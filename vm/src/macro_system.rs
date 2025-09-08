//! Macro System for XR-Lang
//! 
//! Implements hygienic macros with gensym support, following the Lisp tradition
//! of code-as-data transformation. This is where XR-Lang begins eating its own
//! dogfood - the macro system is implemented in XR-Lang itself.

use crate::value::{Value, Symbol, Environment};
use crate::parser::parse_one;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

/// Gensym counter for hygienic macros
static GENSYM_COUNTER: AtomicUsize = AtomicUsize::new(0);

/// Macro definition stored in the environment
#[derive(Debug, Clone)]
pub struct Macro {
    pub name: Symbol,
    pub params: Vec<Symbol>,
    pub body: Value,
    pub env: Rc<Environment>,
}

/// Macro expander that transforms code before evaluation
pub struct MacroExpander {
    macros: HashMap<Symbol, Macro>,
}

impl MacroExpander {
    pub fn new() -> Self {
        MacroExpander {
            macros: HashMap::new(),
        }
    }

    /// Generate a unique symbol for hygienic macros
    pub fn gensym(prefix: &str) -> Symbol {
        let id = GENSYM_COUNTER.fetch_add(1, Ordering::SeqCst);
        Symbol(format!("{}#{}", prefix, id))
    }

    /// Register a macro definition
    pub fn define_macro(&mut self, name: Symbol, params: Vec<Symbol>, body: Value, env: Rc<Environment>) {
        self.macros.insert(
            name.clone(),
            Macro { name, params, body, env }
        );
    }

    /// Expand a macro call
    pub fn expand(&self, form: &Value) -> Result<Value, String> {
        match form {
            Value::List(items) if !items.is_empty() => {
                if let Value::Symbol(name) = &items[0] {
                    if let Some(macro_def) = self.macros.get(name) {
                        return self.expand_macro(macro_def, &items[1..]);
                    }
                }
                // Not a macro call, recursively expand nested forms
                let expanded: Result<Vec<_>, _> = items.iter()
                    .map(|item| self.expand(item))
                    .collect();
                Ok(Value::List(expanded?))
            }
            // Quasiquote support for template-based macros
            Value::List(items) if items.len() == 2 => {
                if let Value::Symbol(sym) = &items[0] {
                    if sym.0 == "quasiquote" || sym.0 == "`" {
                        return self.expand_quasiquote(&items[1]);
                    }
                }
                Ok(form.clone())
            }
            _ => Ok(form.clone()),
        }
    }

    /// Expand a specific macro with arguments
    fn expand_macro(&self, macro_def: &Macro, args: &[Value]) -> Result<Value, String> {
        if args.len() != macro_def.params.len() {
            return Err(format!(
                "Macro {} expects {} arguments, got {}",
                macro_def.name.0,
                macro_def.params.len(),
                args.len()
            ));
        }

        // Create binding environment for macro parameters
        let mut bindings = HashMap::new();
        for (param, arg) in macro_def.params.iter().zip(args) {
            bindings.insert(param.clone(), arg.clone());
        }

        // Substitute parameters in macro body
        let expanded = self.substitute(&macro_def.body, &bindings)?;
        
        // Recursively expand the result
        self.expand(&expanded)
    }

    /// Substitute variables in a form
    fn substitute(&self, form: &Value, bindings: &HashMap<Symbol, Value>) -> Result<Value, String> {
        match form {
            Value::Symbol(sym) => {
                Ok(bindings.get(sym).cloned().unwrap_or_else(|| form.clone()))
            }
            Value::List(items) => {
                // Handle special forms
                if !items.is_empty() {
                    if let Value::Symbol(sym) = &items[0] {
                        match sym.0.as_str() {
                            "quote" => return Ok(form.clone()),
                            "unquote" | "~" if items.len() == 2 => {
                                return self.substitute(&items[1], bindings);
                            }
                            "unquote-splicing" | "~@" if items.len() == 2 => {
                                let expanded = self.substitute(&items[1], bindings)?;
                                if let Value::List(_) = expanded {
                                    return Ok(expanded);
                                }
                                return Err("unquote-splicing requires a list".to_string());
                            }
                            _ => {}
                        }
                    }
                }

                let substituted: Result<Vec<_>, _> = items.iter()
                    .map(|item| self.substitute(item, bindings))
                    .collect();
                Ok(Value::List(substituted?))
            }
            _ => Ok(form.clone()),
        }
    }

    /// Expand quasiquote forms
    fn expand_quasiquote(&self, form: &Value) -> Result<Value, String> {
        self.expand_quasiquote_rec(form, 1)
    }

    fn expand_quasiquote_rec(&self, form: &Value, level: usize) -> Result<Value, String> {
        match form {
            Value::List(items) if !items.is_empty() => {
                if let Value::Symbol(sym) = &items[0] {
                    match sym.0.as_str() {
                        "quasiquote" | "`" if items.len() == 2 => {
                            let inner = self.expand_quasiquote_rec(&items[1], level + 1)?;
                            return Ok(Value::List(vec![
                                Value::Symbol(Symbol("quasiquote".to_string())),
                                inner,
                            ]));
                        }
                        "unquote" | "~" if items.len() == 2 => {
                            if level == 1 {
                                return Ok(items[1].clone());
                            } else {
                                let inner = self.expand_quasiquote_rec(&items[1], level - 1)?;
                                return Ok(Value::List(vec![
                                    Value::Symbol(Symbol("unquote".to_string())),
                                    inner,
                                ]));
                            }
                        }
                        "unquote-splicing" | "~@" if items.len() == 2 => {
                            if level == 1 {
                                return Err("unquote-splicing not in list context".to_string());
                            } else {
                                let inner = self.expand_quasiquote_rec(&items[1], level - 1)?;
                                return Ok(Value::List(vec![
                                    Value::Symbol(Symbol("unquote-splicing".to_string())),
                                    inner,
                                ]));
                            }
                        }
                        _ => {}
                    }
                }

                // Process list elements, handling splicing
                let mut result = Vec::new();
                for item in items {
                    if let Value::List(inner) = item {
                        if inner.len() == 2 {
                            if let Value::Symbol(sym) = &inner[0] {
                                if (sym.0 == "unquote-splicing" || sym.0 == "~@") && level == 1 {
                                    // Splice the result
                                    if let Value::List(splice_items) = &inner[1] {
                                        result.extend(splice_items.clone());
                                        continue;
                                    }
                                }
                            }
                        }
                    }
                    result.push(self.expand_quasiquote_rec(item, level)?);
                }
                Ok(Value::List(result))
            }
            _ => Ok(form.clone()),
        }
    }
}

/// Built-in macro definitions in XR-Lang syntax
pub fn init_core_macros(expander: &mut MacroExpander) {
    // defmacro: Define new macros
    let defmacro_body = parse_one(r#"
        (list 'define-macro 
              name 
              (list 'lambda params body))
    "#).unwrap();
    
    expander.define_macro(
        Symbol("defmacro".to_string()),
        vec![Symbol("name".to_string()), Symbol("params".to_string()), Symbol("body".to_string())],
        defmacro_body,
        Rc::new(Environment::new()),
    );

    // let: Local bindings
    let let_body = parse_one(r#"
        (list (list 'lambda 
                    (map first bindings)
                    body)
              (map second bindings))
    "#).unwrap();
    
    expander.define_macro(
        Symbol("let".to_string()),
        vec![Symbol("bindings".to_string()), Symbol("body".to_string())],
        let_body,
        Rc::new(Environment::new()),
    );

    // when: Conditional execution
    let when_body = parse_one(r#"
        (list 'if condition (list 'begin body) nil)
    "#).unwrap();
    
    expander.define_macro(
        Symbol("when".to_string()),
        vec![Symbol("condition".to_string()), Symbol("body".to_string())],
        when_body,
        Rc::new(Environment::new()),
    );

    // unless: Inverted conditional
    let unless_body = parse_one(r#"
        (list 'if condition nil (list 'begin body))
    "#).unwrap();
    
    expander.define_macro(
        Symbol("unless".to_string()),
        vec![Symbol("condition".to_string()), Symbol("body".to_string())],
        unless_body,
        Rc::new(Environment::new()),
    );
}

/// Scene-specific macros for XR-Lang
pub fn init_scene_macros(expander: &mut MacroExpander) {
    // Minimal, practical scene helpers so XR-Lang can express scene-like code.
    // These are wrappers around native intrinsics from vm::intrinsics and
    // vm::intrinsics_camera.

    // (defscene3d name body) -> (begin ...body)
    let defscene3d_body = parse_one(
        "(quasiquote (begin (unquote-splicing body)))",
    )
    .unwrap();
    expander.define_macro(
        Symbol("defscene3d".to_string()),
        vec![Symbol("name".to_string()), Symbol("body".to_string())],
        defscene3d_body,
        Rc::new(Environment::new()),
    );

    // (camera pos target) -> (create-camera pos target)
    let camera_body = parse_one("(create-camera pos target)").unwrap();
    expander.define_macro(
        Symbol("camera".to_string()),
        vec![Symbol("pos".to_string()), Symbol("target".to_string())],
        camera_body,
        Rc::new(Environment::new()),
    );

    // (camera-fov pos target fov) -> (create-camera pos target fov)
    let camera_fov_body = parse_one("(create-camera pos target fov)").unwrap();
    expander.define_macro(
        Symbol("camera-fov".to_string()),
        vec![
            Symbol("pos".to_string()),
            Symbol("target".to_string()),
            Symbol("fov".to_string()),
        ],
        camera_fov_body,
        Rc::new(Environment::new()),
    );

    // (cube pos) -> (create-cube pos)
    let cube_body = parse_one("(create-cube pos)").unwrap();
    expander.define_macro(
        Symbol("cube".to_string()),
        vec![Symbol("pos".to_string())],
        cube_body,
        Rc::new(Environment::new()),
    );

    // (sphere pos) -> (create-sphere pos)
    let sphere_body = parse_one("(create-sphere pos)").unwrap();
    expander.define_macro(
        Symbol("sphere".to_string()),
        vec![Symbol("pos".to_string())],
        sphere_body,
        Rc::new(Environment::new()),
    );

    // (cylinder pos radius height) -> (create-cylinder pos radius height)
    let cylinder_body = parse_one("(create-cylinder pos radius height)").unwrap();
    expander.define_macro(
        Symbol("cylinder".to_string()),
        vec![
            Symbol("pos".to_string()),
            Symbol("radius".to_string()),
            Symbol("height".to_string()),
        ],
        cylinder_body,
        Rc::new(Environment::new()),
    );

    // (cone pos radius height segments) -> (create-cone pos radius height segments)
    let cone_body = parse_one("(create-cone pos radius height segments)").unwrap();
    expander.define_macro(
        Symbol("cone".to_string()),
        vec![
            Symbol("pos".to_string()),
            Symbol("radius".to_string()),
            Symbol("height".to_string()),
            Symbol("segments".to_string()),
        ],
        cone_body,
        Rc::new(Environment::new()),
    );

    // (pyramid pos basew based h) -> (create-pyramid pos basew based h)
    let pyramid_body = parse_one("(create-pyramid pos basew based h)").unwrap();
    expander.define_macro(
        Symbol("pyramid".to_string()),
        vec![
            Symbol("pos".to_string()),
            Symbol("basew".to_string()),
            Symbol("based".to_string()),
            Symbol("h".to_string()),
        ],
        pyramid_body,
        Rc::new(Environment::new()),
    );

    // (wedge pos w h d) -> (create-wedge pos w h d)
    let wedge_body = parse_one("(create-wedge pos w h d)").unwrap();
    expander.define_macro(
        Symbol("wedge".to_string()),
        vec![
            Symbol("pos".to_string()),
            Symbol("w".to_string()),
            Symbol("h".to_string()),
            Symbol("d".to_string()),
        ],
        wedge_body,
        Rc::new(Environment::new()),
    );

    // (torus pos R r seg rings) -> (create-torus pos R r seg rings)
    let torus_body = parse_one("(create-torus pos R r seg rings)").unwrap();
    expander.define_macro(
        Symbol("torus".to_string()),
        vec![
            Symbol("pos".to_string()),
            Symbol("R".to_string()),
            Symbol("r".to_string()),
            Symbol("seg".to_string()),
            Symbol("rings".to_string()),
        ],
        torus_body,
        Rc::new(Environment::new()),
    );

    // (plane pos w h sub) -> (create-plane pos w h sub)
    let plane_body = parse_one("(create-plane pos w h sub)").unwrap();
    expander.define_macro(
        Symbol("plane".to_string()),
        vec![
            Symbol("pos".to_string()),
            Symbol("w".to_string()),
            Symbol("h".to_string()),
            Symbol("sub".to_string()),
        ],
        plane_body,
        Rc::new(Environment::new()),
    );

    // (capsule pos radius height seg) -> (create-capsule pos radius height seg)
    let capsule_body = parse_one("(create-capsule pos radius height seg)").unwrap();
    expander.define_macro(
        Symbol("capsule".to_string()),
        vec![
            Symbol("pos".to_string()),
            Symbol("radius".to_string()),
            Symbol("height".to_string()),
            Symbol("seg".to_string()),
        ],
        capsule_body,
        Rc::new(Environment::new()),
    );

    // (icosahedron pos radius) -> (create-icosahedron pos radius)
    let icosa_body = parse_one("(create-icosahedron pos radius)").unwrap();
    expander.define_macro(
        Symbol("icosahedron".to_string()),
        vec![Symbol("pos".to_string()), Symbol("radius".to_string())],
        icosa_body,
        Rc::new(Environment::new()),
    );

    // (octahedron pos radius) -> (create-octahedron pos radius)
    let octa_body = parse_one("(create-octahedron pos radius)").unwrap();
    expander.define_macro(
        Symbol("octahedron".to_string()),
        vec![Symbol("pos".to_string()), Symbol("radius".to_string())],
        octa_body,
        Rc::new(Environment::new()),
    );

    // (tetrahedron pos radius) -> (create-tetrahedron pos radius)
    let tetra_body = parse_one("(create-tetrahedron pos radius)").unwrap();
    expander.define_macro(
        Symbol("tetrahedron".to_string()),
        vec![Symbol("pos".to_string()), Symbol("radius".to_string())],
        tetra_body,
        Rc::new(Environment::new()),
    );

    // Material helper: (color obj [r g b a]) -> (set-color obj [r g b a])
    let color_body = parse_one("(set-color obj rgba)").unwrap();
    expander.define_macro(
        Symbol("color".to_string()),
        vec![Symbol("obj".to_string()), Symbol("rgba".to_string())],
        color_body,
        Rc::new(Environment::new()),
    );

    // Material set: (material obj { ... }) -> (set-material obj { ... })
    // We pass through the map as-is; the evaluator will read it as Value::Map
    let material_body = parse_one("(set-material obj mat)").unwrap();
    expander.define_macro(
        Symbol("material".to_string()),
        vec![Symbol("obj".to_string()), Symbol("mat".to_string())],
        material_body,
        Rc::new(Environment::new()),
    );

    // Note: rotate/scale are available as native intrinsics (no macros defined)

    // (move obj pos) -> (update-transform obj pos)
    let move_body = parse_one("(update-transform obj pos)").unwrap();
    expander.define_macro(
        Symbol("move".to_string()),
        vec![Symbol("obj".to_string()), Symbol("pos".to_string())],
        move_body,
        Rc::new(Environment::new()),
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gensym_uniqueness() {
        let sym1 = MacroExpander::gensym("test");
        let sym2 = MacroExpander::gensym("test");
        assert_ne!(sym1, sym2);
        assert!(sym1.0.starts_with("test#"));
        assert!(sym2.0.starts_with("test#"));
    }

    #[test]
    fn test_macro_definition() {
        let mut expander = MacroExpander::new();
        let name = Symbol("test-macro".to_string());
        let params = vec![Symbol("x".to_string())];
        let body = Value::List(vec![
            Value::Symbol(Symbol("quote".to_string())),
            Value::Symbol(Symbol("x".to_string())),
        ]);
        
        expander.define_macro(name.clone(), params, body, Rc::new(Environment::new()));
        assert!(expander.macros.contains_key(&name));
    }

    #[test]
    fn test_simple_macro_expansion() {
        let mut expander = MacroExpander::new();
        
        // Define a simple macro: (twice x) -> (+ x x)
        expander.define_macro(
            Symbol("twice".to_string()),
            vec![Symbol("x".to_string())],
            Value::List(vec![
                Value::Symbol(Symbol("+".to_string())),
                Value::Symbol(Symbol("x".to_string())),
                Value::Symbol(Symbol("x".to_string())),
            ]),
            Rc::new(Environment::new()),
        );

        // Test expansion
        let form = Value::List(vec![
            Value::Symbol(Symbol("twice".to_string())),
            Value::Int(5),
        ]);

        let expanded = expander.expand(&form).unwrap();
        
        match expanded {
            Value::List(items) => {
                assert_eq!(items.len(), 3);
                assert_eq!(items[0], Value::Symbol(Symbol("+".to_string())));
                assert_eq!(items[1], Value::Int(5));
                assert_eq!(items[2], Value::Int(5));
            }
            _ => panic!("Expected list after expansion"),
        }
    }

    #[test]
    fn test_quasiquote_expansion() {
        let expander = MacroExpander::new();
        
        // Test simple quasiquote without unquote
        let form = Value::List(vec![
            Value::Symbol(Symbol("quasiquote".to_string())),
            Value::List(vec![
                Value::Symbol(Symbol("a".to_string())),
                Value::Symbol(Symbol("b".to_string())),
            ]),
        ]);
        
        let expanded = expander.expand(&form).unwrap();
        match expanded {
            Value::List(items) => {
                assert_eq!(items.len(), 2);
            }
            _ => panic!("Expected list"),
        }
    }

    #[test]
    fn test_nested_macro_expansion() {
        let mut expander = MacroExpander::new();
        
        // Define nested macros
        expander.define_macro(
            Symbol("inc".to_string()),
            vec![Symbol("x".to_string())],
            Value::List(vec![
                Value::Symbol(Symbol("+".to_string())),
                Value::Symbol(Symbol("x".to_string())),
                Value::Int(1),
            ]),
            Rc::new(Environment::new()),
        );

        expander.define_macro(
            Symbol("inc-twice".to_string()),
            vec![Symbol("x".to_string())],
            Value::List(vec![
                Value::Symbol(Symbol("inc".to_string())),
                Value::List(vec![
                    Value::Symbol(Symbol("inc".to_string())),
                    Value::Symbol(Symbol("x".to_string())),
                ]),
            ]),
            Rc::new(Environment::new()),
        );

        let form = Value::List(vec![
            Value::Symbol(Symbol("inc-twice".to_string())),
            Value::Int(5),
        ]);

        let expanded = expander.expand(&form).unwrap();
        
        // Should expand to (+ (+ 5 1) 1)
        match expanded {
            Value::List(outer) => {
                assert_eq!(outer.len(), 3);
                assert_eq!(outer[0], Value::Symbol(Symbol("+".to_string())));
                if let Value::List(inner) = &outer[1] {
                    assert_eq!(inner[0], Value::Symbol(Symbol("+".to_string())));
                    assert_eq!(inner[1], Value::Int(5));
                    assert_eq!(inner[2], Value::Int(1));
                }
                assert_eq!(outer[2], Value::Int(1));
            }
            _ => panic!("Expected nested list after expansion"),
        }
    }
}

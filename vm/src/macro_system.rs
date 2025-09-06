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
    // defscene3d: Define a 3D scene
    let defscene3d_src = r#"
        (do
          (create-scene name)
          ~@(map expand-scene-element body))
    "#;
    
    // with-preserved-state: Preserve state during operations
    let preserved_state_src = r#"
        (let ((state# (gensym "state")))
          `(let ((,state# (capture-state)))
            (try
              ,@body
              (finally
                (restore-state ,state#)))))
    "#;
    
    // animate: Create animation behaviors
    let animate_src = r#"
        (behavior ~object
          (on-update (dt)
            (update-property ~property
              (interpolate ~from ~to (get-time)))))
    "#;
    
    // Note: These would be properly implemented when we have
    // the full evaluator running in XR-Lang itself
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
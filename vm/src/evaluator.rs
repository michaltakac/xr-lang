//! Metacircular Evaluator for XR-Lang
//! 
//! This is the heart of Lisp's elegance - an evaluator written in XR-Lang
//! that can interpret XR-Lang code. This proves the language is complete
//! and self-describing.

use crate::value::{Value, Symbol, Environment, Closure, NativeFn};
use crate::macro_system::MacroExpander;
use std::rc::Rc;
use std::collections::HashMap;

/// The metacircular evaluator - evaluates XR-Lang expressions
pub struct Evaluator {
    pub macro_expander: MacroExpander,
    pub global_env: Rc<Environment>,
}

impl Evaluator {
    pub fn new() -> Self {
        let mut evaluator = Evaluator {
            macro_expander: MacroExpander::new(),
            global_env: Rc::new(Environment::new()),
        };
        evaluator.init_primitives();
        evaluator
    }

    /// Initialize primitive functions
    fn init_primitives(&mut self) {
        let env = Rc::get_mut(&mut self.global_env).unwrap();
        
        // Arithmetic primitives
        env.bind(Symbol("+".to_string()), Value::NativeFunction(
            Rc::new(|args| {
                let mut sum = 0i64;
                let mut is_float = false;
                let mut float_sum = 0.0f64;
                
                for arg in args {
                    match arg {
                        Value::Int(n) => {
                            if is_float {
                                float_sum += *n as f64;
                            } else {
                                sum += n;
                            }
                        }
                        Value::Float(f) => {
                            if !is_float {
                                float_sum = sum as f64;
                                is_float = true;
                            }
                            float_sum += f;
                        }
                        _ => return Err("+ expects numbers".to_string()),
                    }
                }
                
                Ok(if is_float {
                    Value::Float(float_sum)
                } else {
                    Value::Int(sum)
                })
            })
        ));

        env.bind(Symbol("-".to_string()), Value::NativeFunction(
            Rc::new(|args| {
                if args.is_empty() {
                    return Err("- expects at least one argument".to_string());
                }
                
                match &args[0] {
                    Value::Int(n) => {
                        if args.len() == 1 {
                            return Ok(Value::Int(-n));
                        }
                        let mut result = *n;
                        for arg in &args[1..] {
                            match arg {
                                Value::Int(m) => result -= m,
                                Value::Float(f) => return Ok(Value::Float(result as f64 - f)),
                                _ => return Err("- expects numbers".to_string()),
                            }
                        }
                        Ok(Value::Int(result))
                    }
                    Value::Float(f) => {
                        if args.len() == 1 {
                            return Ok(Value::Float(-f));
                        }
                        let mut result = *f;
                        for arg in &args[1..] {
                            match arg {
                                Value::Int(n) => result -= *n as f64,
                                Value::Float(g) => result -= g,
                                _ => return Err("- expects numbers".to_string()),
                            }
                        }
                        Ok(Value::Float(result))
                    }
                    _ => Err("- expects numbers".to_string()),
                }
            })
        ));

        env.bind(Symbol("*".to_string()), Value::NativeFunction(
            Rc::new(|args| {
                let mut product = 1i64;
                let mut is_float = false;
                let mut float_product = 1.0f64;
                
                for arg in args {
                    match arg {
                        Value::Int(n) => {
                            if is_float {
                                float_product *= *n as f64;
                            } else {
                                product *= n;
                            }
                        }
                        Value::Float(f) => {
                            if !is_float {
                                float_product = product as f64;
                                is_float = true;
                            }
                            float_product *= f;
                        }
                        _ => return Err("* expects numbers".to_string()),
                    }
                }
                
                Ok(if is_float {
                    Value::Float(float_product)
                } else {
                    Value::Int(product)
                })
            })
        ));

        // Comparison primitives
        env.bind(Symbol("=".to_string()), Value::NativeFunction(
            Rc::new(|args| {
                if args.len() < 2 {
                    return Err("= expects at least 2 arguments".to_string());
                }
                
                for i in 1..args.len() {
                    if args[i] != args[0] {
                        return Ok(Value::Bool(false));
                    }
                }
                Ok(Value::Bool(true))
            })
        ));

        env.bind(Symbol("<".to_string()), Value::NativeFunction(
            Rc::new(|args| {
                if args.len() != 2 {
                    return Err("< expects exactly 2 arguments".to_string());
                }
                
                match (&args[0], &args[1]) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a < b)),
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a < b)),
                    (Value::Int(a), Value::Float(b)) => Ok(Value::Bool((*a as f64) < *b)),
                    (Value::Float(a), Value::Int(b)) => Ok(Value::Bool(*a < (*b as f64))),
                    _ => Err("< expects numbers".to_string()),
                }
            })
        ));

        // List operations
        env.bind(Symbol("cons".to_string()), Value::NativeFunction(
            Rc::new(|args| {
                if args.len() != 2 {
                    return Err("cons expects exactly 2 arguments".to_string());
                }
                
                match &args[1] {
                    Value::List(items) => {
                        let mut new_list = vec![args[0].clone()];
                        new_list.extend(items.clone());
                        Ok(Value::List(new_list))
                    }
                    Value::Nil => Ok(Value::List(vec![args[0].clone()])),
                    _ => Ok(Value::List(vec![args[0].clone(), args[1].clone()])),
                }
            })
        ));

        env.bind(Symbol("car".to_string()), Value::NativeFunction(
            Rc::new(|args| {
                if args.len() != 1 {
                    return Err("car expects exactly 1 argument".to_string());
                }
                
                match &args[0] {
                    Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
                    Value::List(_) => Ok(Value::Nil),
                    _ => Err("car expects a list".to_string()),
                }
            })
        ));

        env.bind(Symbol("cdr".to_string()), Value::NativeFunction(
            Rc::new(|args| {
                if args.len() != 1 {
                    return Err("cdr expects exactly 1 argument".to_string());
                }
                
                match &args[0] {
                    Value::List(items) if items.len() > 1 => {
                        Ok(Value::List(items[1..].to_vec()))
                    }
                    Value::List(_) => Ok(Value::Nil),
                    _ => Err("cdr expects a list".to_string()),
                }
            })
        ));

        env.bind(Symbol("null?".to_string()), Value::NativeFunction(
            Rc::new(|args| {
                if args.len() != 1 {
                    return Err("null? expects exactly 1 argument".to_string());
                }
                
                match &args[0] {
                    Value::Nil => Ok(Value::Bool(true)),
                    Value::List(items) if items.is_empty() => Ok(Value::Bool(true)),
                    _ => Ok(Value::Bool(false)),
                }
            })
        ));

        // Type predicates
        env.bind(Symbol("symbol?".to_string()), Value::NativeFunction(
            Rc::new(|args| {
                if args.len() != 1 {
                    return Err("symbol? expects exactly 1 argument".to_string());
                }
                Ok(Value::Bool(matches!(args[0], Value::Symbol(_))))
            })
        ));

        env.bind(Symbol("number?".to_string()), Value::NativeFunction(
            Rc::new(|args| {
                if args.len() != 1 {
                    return Err("number? expects exactly 1 argument".to_string());
                }
                Ok(Value::Bool(matches!(args[0], Value::Int(_) | Value::Float(_))))
            })
        ));

        env.bind(Symbol("list?".to_string()), Value::NativeFunction(
            Rc::new(|args| {
                if args.len() != 1 {
                    return Err("list? expects exactly 1 argument".to_string());
                }
                Ok(Value::Bool(matches!(args[0], Value::List(_))))
            })
        ));
    }

    /// The core eval function - evaluates XR-Lang expressions
    pub fn eval(&mut self, expr: &Value, env: Rc<Environment>) -> Result<Value, String> {
        // First expand macros
        let expanded = self.macro_expander.expand(expr)?;
        
        // Then evaluate the expanded form
        self.eval_expanded(&expanded, env)
    }

    /// Evaluate an already macro-expanded expression
    fn eval_expanded(&mut self, expr: &Value, env: Rc<Environment>) -> Result<Value, String> {
        match expr {
            // Self-evaluating forms
            Value::Nil | Value::Bool(_) | Value::Int(_) | Value::Float(_) | Value::Str(_) => {
                Ok(expr.clone())
            }
            
            // Quoted expressions
            Value::Quote(inner) => {
                Ok((**inner).clone())
            }
            
            // Variable lookup
            Value::Symbol(sym) => {
                env.lookup(sym)
                    .or_else(|| self.global_env.lookup(sym))
                    .ok_or_else(|| format!("Undefined variable: {}", sym.0))
            }
            
            // List evaluation (function application and special forms)
            Value::List(items) if !items.is_empty() => {
                // Check for special forms
                if let Value::Symbol(sym) = &items[0] {
                    match sym.0.as_str() {
                        "quote" => return self.eval_quote(&items[1..]),
                        "if" => return self.eval_if(&items[1..], env),
                        "lambda" => return self.eval_lambda(&items[1..], env),
                        "define" => return self.eval_define(&items[1..], env),
                        "defmacro" => return self.eval_defmacro(&items[1..], env),
                        "let" => return self.eval_let(&items[1..], env),
                        "begin" => return self.eval_begin(&items[1..], env),
                        "do" => return self.eval_begin(&items[1..], env), // Alias for begin
                        "set!" => return self.eval_set(&items[1..], env),
                        "eval" => return self.eval_eval(&items[1..], env),
                        "apply" => return self.eval_apply(&items[1..], env),
                        _ => {}
                    }
                }
                
                // Function application
                let func = self.eval_expanded(&items[0], env.clone())?;
                let args: Result<Vec<_>, _> = items[1..]
                    .iter()
                    .map(|arg| self.eval_expanded(arg, env.clone()))
                    .collect();
                
                self.apply_function(&func, &args?)
            }
            
            Value::List(_) => Ok(Value::Nil), // Empty list evaluates to nil
            
            // Vectors evaluate their elements
            Value::Vector(items) => {
                let evaluated: Result<Vec<_>, _> = items
                    .iter()
                    .map(|item| self.eval_expanded(item, env.clone()))
                    .collect();
                Ok(Value::Vector(evaluated?))
            }
            
            // Maps evaluate their values
            Value::Map(map) => {
                let mut result = HashMap::new();
                for (k, v) in map {
                    result.insert(k.clone(), self.eval_expanded(v, env.clone())?);
                }
                Ok(Value::Map(result))
            }
            
            _ => Ok(expr.clone()),
        }
    }

    /// Special form: quote
    fn eval_quote(&mut self, args: &[Value]) -> Result<Value, String> {
        if args.len() != 1 {
            return Err("quote expects exactly 1 argument".to_string());
        }
        Ok(args[0].clone())
    }

    /// Special form: if
    fn eval_if(&mut self, args: &[Value], env: Rc<Environment>) -> Result<Value, String> {
        if args.len() < 2 || args.len() > 3 {
            return Err("if expects 2 or 3 arguments".to_string());
        }
        
        let condition = self.eval_expanded(&args[0], env.clone())?;
        
        if condition.is_truthy() {
            self.eval_expanded(&args[1], env)
        } else if args.len() == 3 {
            self.eval_expanded(&args[2], env)
        } else {
            Ok(Value::Nil)
        }
    }

    /// Special form: lambda
    fn eval_lambda(&mut self, args: &[Value], env: Rc<Environment>) -> Result<Value, String> {
        if args.len() < 2 {
            return Err("lambda expects at least 2 arguments".to_string());
        }
        
        // Parse parameters
        let params = match &args[0] {
            Value::List(items) => {
                let mut params = Vec::new();
                for item in items {
                    match item {
                        Value::Symbol(sym) => params.push(sym.clone()),
                        _ => return Err("lambda parameters must be symbols".to_string()),
                    }
                }
                params
            }
            _ => return Err("lambda expects a parameter list".to_string()),
        };
        
        // Create closure
        let body = if args.len() == 2 {
            Box::new(args[1].clone())
        } else {
            // Multiple expressions in body -> implicit begin
            Box::new(Value::List({
                let mut body_exprs = vec![Value::Symbol(Symbol("begin".to_string()))];
                body_exprs.extend(args[1..].to_vec());
                body_exprs
            }))
        };
        
        Ok(Value::Closure(Rc::new(Closure {
            params,
            body,
            env: env.as_ref().clone(),
        })))
    }

    /// Special form: define
    fn eval_define(&mut self, args: &[Value], env: Rc<Environment>) -> Result<Value, String> {
        if args.len() != 2 {
            return Err("define expects exactly 2 arguments".to_string());
        }
        
        match &args[0] {
            Value::Symbol(sym) => {
                let value = self.eval_expanded(&args[1], env.clone())?;
                
                // Clone the environment to modify it
                let mut new_env = (*env).clone();
                new_env.bind(sym.clone(), value.clone());
                
                // This is a bit of a hack - in a real system we'd need
                // mutable environment references
                if let Some(global) = Rc::get_mut(&mut self.global_env) {
                    global.bind(sym.clone(), value.clone());
                }
                
                Ok(value)
            }
            _ => Err("define expects a symbol as first argument".to_string()),
        }
    }

    /// Special form: defmacro
    fn eval_defmacro(&mut self, args: &[Value], env: Rc<Environment>) -> Result<Value, String> {
        if args.len() != 3 {
            return Err("defmacro expects exactly 3 arguments".to_string());
        }
        
        let name = match &args[0] {
            Value::Symbol(sym) => sym.clone(),
            _ => return Err("defmacro expects a symbol as first argument".to_string()),
        };
        
        let params = match &args[1] {
            Value::List(items) => {
                let mut params = Vec::new();
                for item in items {
                    match item {
                        Value::Symbol(sym) => params.push(sym.clone()),
                        _ => return Err("defmacro parameters must be symbols".to_string()),
                    }
                }
                params
            }
            _ => return Err("defmacro expects a parameter list".to_string()),
        };
        
        self.macro_expander.define_macro(name.clone(), params, args[2].clone(), env);
        
        Ok(Value::Symbol(name))
    }

    /// Special form: let
    fn eval_let(&mut self, args: &[Value], env: Rc<Environment>) -> Result<Value, String> {
        if args.len() < 2 {
            return Err("let expects at least 2 arguments".to_string());
        }
        
        // Create new environment for let bindings
        let mut let_env = Environment::with_parent(env);
        
        // Process bindings
        match &args[0] {
            Value::List(bindings) => {
                for binding in bindings {
                    match binding {
                        Value::List(pair) if pair.len() == 2 => {
                            match &pair[0] {
                                Value::Symbol(sym) => {
                                    let value = self.eval_expanded(&pair[1], Rc::new(let_env.clone()))?;
                                    let_env.bind(sym.clone(), value);
                                }
                                _ => return Err("let binding must start with a symbol".to_string()),
                            }
                        }
                        _ => return Err("let bindings must be pairs".to_string()),
                    }
                }
            }
            _ => return Err("let expects a list of bindings".to_string()),
        }
        
        // Evaluate body in new environment
        self.eval_begin(&args[1..], Rc::new(let_env))
    }

    /// Special form: begin (sequential evaluation)
    fn eval_begin(&mut self, args: &[Value], env: Rc<Environment>) -> Result<Value, String> {
        if args.is_empty() {
            return Ok(Value::Nil);
        }
        
        let mut result = Value::Nil;
        for expr in args {
            result = self.eval_expanded(expr, env.clone())?;
        }
        Ok(result)
    }

    /// Special form: set! (mutation)
    fn eval_set(&mut self, args: &[Value], env: Rc<Environment>) -> Result<Value, String> {
        if args.len() != 2 {
            return Err("set! expects exactly 2 arguments".to_string());
        }
        
        match &args[0] {
            Value::Symbol(sym) => {
                let value = self.eval_expanded(&args[1], env.clone())?;
                
                // Try to update in local environment first, then global
                // This is simplified - real implementation would need mutable refs
                if let Some(global) = Rc::get_mut(&mut self.global_env) {
                    global.bind(sym.clone(), value.clone());
                }
                
                Ok(value)
            }
            _ => Err("set! expects a symbol as first argument".to_string()),
        }
    }

    /// Special form: eval (evaluate data as code)
    fn eval_eval(&mut self, args: &[Value], env: Rc<Environment>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err("eval expects exactly 1 argument".to_string());
        }
        
        let expr = self.eval_expanded(&args[0], env.clone())?;
        self.eval(&expr, env)
    }

    /// Special form: apply
    fn eval_apply(&mut self, args: &[Value], env: Rc<Environment>) -> Result<Value, String> {
        if args.len() != 2 {
            return Err("apply expects exactly 2 arguments".to_string());
        }
        
        let func = self.eval_expanded(&args[0], env.clone())?;
        let arg_list = self.eval_expanded(&args[1], env)?;
        
        match arg_list {
            Value::List(items) => self.apply_function(&func, &items),
            _ => Err("apply expects a list as second argument".to_string()),
        }
    }

    /// Apply a function to arguments
    fn apply_function(&mut self, func: &Value, args: &[Value]) -> Result<Value, String> {
        match func {
            Value::NativeFunction(f) => f(args),
            Value::Closure(closure) => {
                if args.len() != closure.params.len() {
                    return Err(format!(
                        "Function expects {} arguments, got {}",
                        closure.params.len(),
                        args.len()
                    ));
                }
                
                // Create new environment with parameter bindings
                let mut call_env = Environment::with_parent(Rc::new(closure.env.clone()));
                for (param, arg) in closure.params.iter().zip(args) {
                    call_env.bind(param.clone(), arg.clone());
                }
                
                // Evaluate body in new environment
                self.eval_expanded(&closure.body, Rc::new(call_env))
            }
            _ => Err(format!("Cannot apply non-function: {:?}", func)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_one;

    #[test]
    fn test_eval_self_evaluating() {
        let mut evaluator = Evaluator::new();
        let env = Rc::new(Environment::new());
        
        assert_eq!(evaluator.eval(&Value::Int(42), env.clone()).unwrap(), Value::Int(42));
        assert_eq!(evaluator.eval(&Value::Bool(true), env.clone()).unwrap(), Value::Bool(true));
        assert_eq!(evaluator.eval(&Value::Nil, env).unwrap(), Value::Nil);
    }

    #[test]
    fn test_eval_arithmetic() {
        let mut evaluator = Evaluator::new();
        let env = Rc::new(Environment::new());
        
        let expr = parse_one("(+ 1 2 3)").unwrap();
        assert_eq!(evaluator.eval(&expr, env).unwrap(), Value::Int(6));
    }

    #[test]
    fn test_eval_if() {
        let mut evaluator = Evaluator::new();
        let env = Rc::new(Environment::new());
        
        let expr1 = parse_one("(if #t 1 2)").unwrap();
        assert_eq!(evaluator.eval(&expr1, env.clone()).unwrap(), Value::Int(1));
        
        let expr2 = parse_one("(if #f 1 2)").unwrap();
        assert_eq!(evaluator.eval(&expr2, env).unwrap(), Value::Int(2));
    }

    #[test]
    fn test_eval_lambda() {
        let mut evaluator = Evaluator::new();
        let env = Rc::new(Environment::new());
        
        let expr = parse_one("((lambda (x y) (+ x y)) 3 4)").unwrap();
        assert_eq!(evaluator.eval(&expr, env).unwrap(), Value::Int(7));
    }

    #[test]
    fn test_eval_let() {
        let mut evaluator = Evaluator::new();
        let env = Rc::new(Environment::new());
        
        let expr = parse_one("(let ((x 3) (y 4)) (+ x y))").unwrap();
        assert_eq!(evaluator.eval(&expr, env).unwrap(), Value::Int(7));
    }

    #[test]
    fn test_eval_list_operations() {
        let mut evaluator = Evaluator::new();
        let env = Rc::new(Environment::new());
        
        let expr1 = parse_one("(cons 1 '(2 3))").unwrap();
        let result1 = evaluator.eval(&expr1, env.clone()).unwrap();
        match result1 {
            Value::List(items) => {
                assert_eq!(items.len(), 3);
                assert_eq!(items[0], Value::Int(1));
                assert_eq!(items[1], Value::Int(2));
                assert_eq!(items[2], Value::Int(3));
            }
            _ => panic!("Expected list"),
        }
        
        let expr2 = parse_one("(car '(1 2 3))").unwrap();
        assert_eq!(evaluator.eval(&expr2, env.clone()).unwrap(), Value::Int(1));
        
        let expr3 = parse_one("(cdr '(1 2 3))").unwrap();
        let result3 = evaluator.eval(&expr3, env).unwrap();
        match result3 {
            Value::List(items) => {
                assert_eq!(items.len(), 2);
                assert_eq!(items[0], Value::Int(2));
            }
            _ => panic!("Expected list"),
        }
    }

    #[test]
    fn test_recursive_function() {
        let mut evaluator = Evaluator::new();
        let env = Rc::new(Environment::new());
        
        // Define factorial recursively
        let define_expr = parse_one(r#"
            (define fact
              (lambda (n)
                (if (< n 2)
                    1
                    (* n (fact (- n 1))))))
        "#).unwrap();
        
        evaluator.eval(&define_expr, env.clone()).unwrap();
        
        // Test factorial
        let test_expr = parse_one("(fact 5)").unwrap();
        assert_eq!(evaluator.eval(&test_expr, env).unwrap(), Value::Int(120));
    }
}
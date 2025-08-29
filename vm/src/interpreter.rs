//! Runtime interpreter for XR-DSL behaviors and expressions

use std::collections::HashMap;
use anyhow::{Result, anyhow};

#[derive(Debug, Clone)]
pub enum Value {
    F32(f32),
    I32(i32),
    Bool(bool),
    String(String),
    Vec3(f32, f32, f32),
    Object(ObjectRef),
    Function(FunctionValue),
    Nil,
}

#[derive(Debug, Clone)]
pub struct ObjectRef {
    pub id: String,
    pub properties: HashMap<String, Value>,
}

#[derive(Debug, Clone)]
pub struct FunctionValue {
    pub params: Vec<String>,
    pub body: dsl::ast::Expr,
    pub env: Environment,
}

#[derive(Debug, Clone)]
pub struct Environment {
    pub vars: HashMap<String, Value>,
    pub parent: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            parent: None,
        }
    }
    
    pub fn with_parent(parent: Environment) -> Self {
        Self {
            vars: HashMap::new(),
            parent: Some(Box::new(parent)),
        }
    }
    
    pub fn get(&self, name: &str) -> Option<Value> {
        self.vars.get(name).cloned().or_else(|| {
            self.parent.as_ref().and_then(|p| p.get(name))
        })
    }
    
    pub fn set(&mut self, name: String, value: Value) {
        self.vars.insert(name, value);
    }
}

pub struct Interpreter {
    pub global_env: Environment,
    pub behaviors: HashMap<String, BehaviorInstance>,
    pub objects: HashMap<String, ObjectRef>,
    pub time: f32,
    pub delta_time: f32,
}

#[derive(Debug, Clone)]
pub struct BehaviorInstance {
    pub name: String,
    pub state: HashMap<String, Value>,
    pub update_fn: Option<FunctionValue>,
    pub on_select_fn: Option<FunctionValue>,
    pub env: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut global_env = Environment::new();
        
        // Register built-in functions
        Self::register_builtins(&mut global_env);
        
        Self {
            global_env,
            behaviors: HashMap::new(),
            objects: HashMap::new(),
            time: 0.0,
            delta_time: 0.0,
        }
    }
    
    fn register_builtins(env: &mut Environment) {
        // Math functions
        env.set("pi".to_string(), Value::F32(std::f32::consts::PI));
        env.set("tau".to_string(), Value::F32(std::f32::consts::TAU));
        
        // These would be native functions in a real implementation
        // For now, we'll handle them specially in eval
    }
    
    pub fn load_behavior(&mut self, behavior: &dsl::ast::Behavior) -> Result<()> {
        let mut state = HashMap::new();
        for (key, value) in &behavior.state {
            state.insert(key.clone(), Value::F32(*value));
        }
        
        let mut env = Environment::with_parent(self.global_env.clone());
        
        // Add state variables to environment
        for (key, value) in &state {
            env.set(key.clone(), value.clone());
        }
        
        let update_fn = Some(FunctionValue {
            params: behavior.update.params.clone(),
            body: behavior.update.body.clone(),
            env: env.clone(),
        });
        
        let on_select_fn = behavior.on_select.as_ref().map(|f| FunctionValue {
            params: f.params.clone(),
            body: f.body.clone(),
            env: env.clone(),
        });
        
        let instance = BehaviorInstance {
            name: behavior.name.clone(),
            state,
            update_fn,
            on_select_fn,
            env,
        };
        
        self.behaviors.insert(behavior.name.clone(), instance);
        Ok(())
    }
    
    pub fn update_behavior(&mut self, name: &str, dt: f32) -> Result<()> {
        self.delta_time = dt;
        
        let behavior = self.behaviors.get(name)
            .ok_or_else(|| anyhow!("Behavior '{}' not found", name))?
            .clone();
        
        if let Some(update_fn) = behavior.update_fn {
            let mut env = update_fn.env.clone();
            
            // Bind parameters (typically just 'dt')
            if !update_fn.params.is_empty() && update_fn.params[0] == "dt" {
                env.set("dt".to_string(), Value::F32(dt));
            }
            
            // Add behavior state to environment
            for (key, value) in &behavior.state {
                env.set(key.clone(), value.clone());
            }
            
            // Execute update function
            let _result = self.eval(&update_fn.body, &mut env)?;
            
            // Update behavior state with any changes
            if let Some(behavior_mut) = self.behaviors.get_mut(name) {
                for (key, value) in env.vars {
                    if behavior_mut.state.contains_key(&key) {
                        behavior_mut.state.insert(key, value);
                    }
                }
            }
        }
        
        Ok(())
    }
    
    pub fn eval(&mut self, expr: &dsl::ast::Expr, env: &mut Environment) -> Result<Value> {
        match expr {
            dsl::ast::Expr::F32(n) => Ok(Value::F32(*n)),
            dsl::ast::Expr::I32(n) => Ok(Value::I32(*n)),
            dsl::ast::Expr::Bool(b) => Ok(Value::Bool(*b)),
            dsl::ast::Expr::Sym(s) => {
                // Look up variable
                env.get(s).ok_or_else(|| anyhow!("Undefined variable: {}", s))
            }
            dsl::ast::Expr::List(list) => {
                if list.is_empty() {
                    return Ok(Value::Nil);
                }
                
                // Get the function/operator
                let head = &list[0];
                
                match head {
                    dsl::ast::Expr::Sym(op) => {
                        self.eval_operation(op, &list[1..], env)
                    }
                    _ => {
                        // Try to evaluate as function call
                        let func = self.eval(head, env)?;
                        self.apply_function(func, &list[1..], env)
                    }
                }
            }
        }
    }
    
    fn eval_operation(&mut self, op: &str, args: &[dsl::ast::Expr], env: &mut Environment) -> Result<Value> {
        match op {
            // Arithmetic operations
            "+" | "add" => {
                let mut sum = 0.0;
                for arg in args {
                    match self.eval(arg, env)? {
                        Value::F32(n) => sum += n,
                        Value::I32(n) => sum += n as f32,
                        _ => return Err(anyhow!("+ expects numbers")),
                    }
                }
                Ok(Value::F32(sum))
            }
            "-" | "sub" => {
                if args.is_empty() {
                    return Err(anyhow!("- needs at least one argument"));
                }
                let first = match self.eval(&args[0], env)? {
                    Value::F32(n) => n,
                    Value::I32(n) => n as f32,
                    _ => return Err(anyhow!("- expects numbers")),
                };
                if args.len() == 1 {
                    Ok(Value::F32(-first))
                } else {
                    let mut result = first;
                    for arg in &args[1..] {
                        match self.eval(arg, env)? {
                            Value::F32(n) => result -= n,
                            Value::I32(n) => result -= n as f32,
                            _ => return Err(anyhow!("- expects numbers")),
                        }
                    }
                    Ok(Value::F32(result))
                }
            }
            "*" | "mul" => {
                let mut product = 1.0;
                for arg in args {
                    match self.eval(arg, env)? {
                        Value::F32(n) => product *= n,
                        Value::I32(n) => product *= n as f32,
                        _ => return Err(anyhow!("* expects numbers")),
                    }
                }
                Ok(Value::F32(product))
            }
            "/" | "div" => {
                if args.is_empty() {
                    return Err(anyhow!("/ needs at least one argument"));
                }
                let first = match self.eval(&args[0], env)? {
                    Value::F32(n) => n,
                    Value::I32(n) => n as f32,
                    _ => return Err(anyhow!("/ expects numbers")),
                };
                if args.len() == 1 {
                    Ok(Value::F32(1.0 / first))
                } else {
                    let mut result = first;
                    for arg in &args[1..] {
                        match self.eval(arg, env)? {
                            Value::F32(n) => result /= n,
                            Value::I32(n) => result /= n as f32,
                            _ => return Err(anyhow!("/ expects numbers")),
                        }
                    }
                    Ok(Value::F32(result))
                }
            }
            
            // Comparison operations
            "=" | "eq" => {
                if args.len() != 2 {
                    return Err(anyhow!("= needs exactly 2 arguments"));
                }
                let a = self.eval(&args[0], env)?;
                let b = self.eval(&args[1], env)?;
                Ok(Value::Bool(self.values_equal(&a, &b)))
            }
            "<" => {
                if args.len() != 2 {
                    return Err(anyhow!("< needs exactly 2 arguments"));
                }
                let a = self.eval_to_f32(&args[0], env)?;
                let b = self.eval_to_f32(&args[1], env)?;
                Ok(Value::Bool(a < b))
            }
            ">" => {
                if args.len() != 2 {
                    return Err(anyhow!("> needs exactly 2 arguments"));
                }
                let a = self.eval_to_f32(&args[0], env)?;
                let b = self.eval_to_f32(&args[1], env)?;
                Ok(Value::Bool(a > b))
            }
            
            // Control flow
            "if" => {
                if args.len() < 2 || args.len() > 3 {
                    return Err(anyhow!("if needs 2 or 3 arguments"));
                }
                let condition = self.eval(&args[0], env)?;
                let is_true = match condition {
                    Value::Bool(b) => b,
                    Value::Nil => false,
                    _ => true,
                };
                
                if is_true {
                    self.eval(&args[1], env)
                } else if args.len() == 3 {
                    self.eval(&args[2], env)
                } else {
                    Ok(Value::Nil)
                }
            }
            
            // Let binding
            "let" => {
                if args.len() != 2 {
                    return Err(anyhow!("let needs bindings and body"));
                }
                
                let dsl::ast::Expr::List(bindings) = &args[0] else {
                    return Err(anyhow!("let bindings must be a list"));
                };
                
                let mut new_env = Environment::with_parent(env.clone());
                
                // Process bindings
                for binding in bindings.chunks(2) {
                    if binding.len() != 2 {
                        return Err(anyhow!("let bindings must be pairs"));
                    }
                    let dsl::ast::Expr::Sym(name) = &binding[0] else {
                        return Err(anyhow!("let binding name must be a symbol"));
                    };
                    let value = self.eval(&binding[1], env)?;
                    new_env.set(name.clone(), value);
                }
                
                // Evaluate body in new environment
                self.eval(&args[1], &mut new_env)
            }
            
            // Set! for mutation
            "set!" => {
                if args.len() != 2 {
                    return Err(anyhow!("set! needs variable and value"));
                }
                let dsl::ast::Expr::Sym(name) = &args[0] else {
                    return Err(anyhow!("set! first argument must be a symbol"));
                };
                let value = self.eval(&args[1], env)?;
                env.set(name.clone(), value.clone());
                Ok(value)
            }
            
            // Object operations
            "rotate-y" => {
                if args.len() != 1 {
                    return Err(anyhow!("rotate-y needs angle"));
                }
                let angle = self.eval_to_f32(&args[0], env)?;
                // Store the rotation delta for the behavior system to apply
                env.set("_rotate_y".to_string(), Value::F32(angle));
                Ok(Value::F32(angle))
            }
            
            "rotate-x" => {
                if args.len() != 1 {
                    return Err(anyhow!("rotate-x needs angle"));
                }
                let angle = self.eval_to_f32(&args[0], env)?;
                Ok(Value::F32(angle))
            }
            
            "rotate-z" => {
                if args.len() != 1 {
                    return Err(anyhow!("rotate-z needs angle"));
                }
                let angle = self.eval_to_f32(&args[0], env)?;
                Ok(Value::F32(angle))
            }
            
            "move" => {
                if args.len() != 3 {
                    return Err(anyhow!("move needs x, y, z"));
                }
                let x = self.eval_to_f32(&args[0], env)?;
                let y = self.eval_to_f32(&args[1], env)?;
                let z = self.eval_to_f32(&args[2], env)?;
                Ok(Value::Vec3(x, y, z))
            }
            
            "scale" => {
                if args.len() == 1 {
                    let s = self.eval_to_f32(&args[0], env)?;
                    Ok(Value::Vec3(s, s, s))
                } else if args.len() == 3 {
                    let x = self.eval_to_f32(&args[0], env)?;
                    let y = self.eval_to_f32(&args[1], env)?;
                    let z = self.eval_to_f32(&args[2], env)?;
                    Ok(Value::Vec3(x, y, z))
                } else {
                    Err(anyhow!("scale needs 1 or 3 arguments"))
                }
            }
            
            // Math functions
            "sin" => {
                if args.len() != 1 {
                    return Err(anyhow!("sin needs 1 argument"));
                }
                let n = self.eval_to_f32(&args[0], env)?;
                Ok(Value::F32(n.sin()))
            }
            
            "cos" => {
                if args.len() != 1 {
                    return Err(anyhow!("cos needs 1 argument"));
                }
                let n = self.eval_to_f32(&args[0], env)?;
                Ok(Value::F32(n.cos()))
            }
            
            "abs" => {
                if args.len() != 1 {
                    return Err(anyhow!("abs needs 1 argument"));
                }
                let n = self.eval_to_f32(&args[0], env)?;
                Ok(Value::F32(n.abs()))
            }
            
            "min" => {
                if args.is_empty() {
                    return Err(anyhow!("min needs at least 1 argument"));
                }
                let mut min_val = self.eval_to_f32(&args[0], env)?;
                for arg in &args[1..] {
                    let val = self.eval_to_f32(arg, env)?;
                    if val < min_val {
                        min_val = val;
                    }
                }
                Ok(Value::F32(min_val))
            }
            
            "max" => {
                if args.is_empty() {
                    return Err(anyhow!("max needs at least 1 argument"));
                }
                let mut max_val = self.eval_to_f32(&args[0], env)?;
                for arg in &args[1..] {
                    let val = self.eval_to_f32(arg, env)?;
                    if val > max_val {
                        max_val = val;
                    }
                }
                Ok(Value::F32(max_val))
            }
            
            _ => Err(anyhow!("Unknown operation: {}", op)),
        }
    }
    
    fn apply_function(&mut self, func: Value, args: &[dsl::ast::Expr], env: &mut Environment) -> Result<Value> {
        match func {
            Value::Function(f) => {
                if f.params.len() != args.len() {
                    return Err(anyhow!("Function expects {} arguments, got {}", 
                        f.params.len(), args.len()));
                }
                
                let mut new_env = Environment::with_parent(f.env);
                
                // Bind arguments
                for (param, arg) in f.params.iter().zip(args) {
                    let value = self.eval(arg, env)?;
                    new_env.set(param.clone(), value);
                }
                
                // Execute function body
                self.eval(&f.body, &mut new_env)
            }
            _ => Err(anyhow!("Not a function")),
        }
    }
    
    fn eval_to_f32(&mut self, expr: &dsl::ast::Expr, env: &mut Environment) -> Result<f32> {
        match self.eval(expr, env)? {
            Value::F32(n) => Ok(n),
            Value::I32(n) => Ok(n as f32),
            _ => Err(anyhow!("Expected number")),
        }
    }
    
    fn values_equal(&self, a: &Value, b: &Value) -> bool {
        match (a, b) {
            (Value::F32(x), Value::F32(y)) => (x - y).abs() < 0.0001,
            (Value::I32(x), Value::I32(y)) => x == y,
            (Value::Bool(x), Value::Bool(y)) => x == y,
            (Value::String(x), Value::String(y)) => x == y,
            (Value::Nil, Value::Nil) => true,
            _ => false,
        }
    }
    
    pub fn get_rotation_for_object(&self, behavior_name: &str) -> Option<f32> {
        self.behaviors.get(behavior_name)
            .and_then(|b| b.state.get("rotation"))
            .and_then(|v| match v {
                Value::F32(r) => Some(*r),
                _ => None,
            })
    }
}
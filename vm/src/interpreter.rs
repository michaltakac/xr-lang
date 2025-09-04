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
    List(Vec<Value>),
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
            // println!(DEBUG: Loading state {} = {} for behavior '{}'", key, value, behavior.name);
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
            // Start with the behavior's persistent environment
            let mut env = behavior.env.clone();
            
            // Debug: Check if speed is in environment
            if let Some(Value::F32(_speed)) = env.get("speed") {
                // println!(DEBUG: Using speed = {} for behavior '{}'", speed, name);
            }
            
            // Debug: Check if rotation.y is already in persistent env
            if let Some(Value::F32(_rot_y)) = env.get("rotation.y") {
                // println!(DEBUG: rotation.y from persistent env = {}", rot_y);
            }
            
            // Bind parameters (typically just 'dt')
            if !update_fn.params.is_empty() && update_fn.params[0] == "dt" {
                env.set("dt".to_string(), Value::F32(dt));
            }
            
            // Add behavior state to environment
            for (key, value) in &behavior.state {
                env.set(key.clone(), value.clone());
                // Debug: Log state values being used
                if key == "speed" {
                    if let Value::F32(_speed) = value {
                        // println!(DEBUG: Using speed = {} for behavior '{}'", speed, name);
                    }
                }
            }
            
            // Debug: Check rotation.y before execution
            if let Some(Value::F32(_rot_y)) = env.get("rotation.y") {
                // println!(DEBUG: rotation.y before = {}", rot_y);
            }
            
            // Debug: Print the body structure
            // println!(DEBUG: Behavior body structure: {:?}", update_fn.body);
            
            // Execute update function with the NEW body (from hot-swap)
            // Handle body that might be a sequence of statements
            let _result = match &update_fn.body {
                dsl::ast::Expr::List(exprs) if !exprs.is_empty() => {
                    // Check if this is a function call or a sequence of statements
                    if let dsl::ast::Expr::Sym(_s) = &exprs[0] {
                        // It's a function call
                        self.eval(&update_fn.body, &mut env)?
                    } else {
                        // It's a sequence of statements - evaluate each one
                        let mut last_result = Value::Nil;
                        for expr in exprs {
                            last_result = self.eval(expr, &mut env)?;
                        }
                        last_result
                    }
                }
                _ => self.eval(&update_fn.body, &mut env)?
            };
            
            // Debug: Check what rotation.y value we have after execution
            if let Some(Value::F32(_rot_y)) = env.get("rotation.y") {
                // println!(DEBUG: rotation.y after = {}", rot_y);
            }
            
            // Debug: Check speed value in env after execution
            if let Some(Value::F32(_speed)) = env.get("speed") {
                // println!(DEBUG: speed in env after execution = {}", speed);
            }
            
            // Update behavior's persistent environment and state with changes
            if let Some(behavior_mut) = self.behaviors.get_mut(name) {
                // Update state values
                for (key, value) in &env.vars {
                    if behavior_mut.state.contains_key(key) {
                        behavior_mut.state.insert(key.clone(), value.clone());
                    }
                    // Also preserve runtime values (like rotation.y) in the behavior's env
                    if key.contains('.') || !behavior_mut.state.contains_key(key) {
                        behavior_mut.env.set(key.clone(), value.clone());
                        if key == "rotation.y" {
                            // println!(DEBUG: Stored rotation.y = {:?} in persistent env", value);
                        }
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
            dsl::ast::Expr::Str(s) => Ok(Value::String(s.clone())),
            dsl::ast::Expr::Sym(s) => {
                // Handle dotted notation for reading (e.g., rotation.y)
                if s.contains('.') {
                    let parts: Vec<&str> = s.split('.').collect();
                    if parts.len() == 2 {
                        // Try to get the dotted value directly first
                        if let Some(val) = env.get(s) {
                            return Ok(val);
                        }
                        
                        // Otherwise, return a default value for known fields
                        let base = parts[0];
                        let field = parts[1];
                        match base {
                            "rotation" => {
                                // Default rotation values
                                match field {
                                    "x" | "y" | "z" => {
                                        // println!(DEBUG: rotation.{} not found, returning default 0.0", field);
                                        Ok(Value::F32(0.0))
                                    },
                                    _ => Err(anyhow!("Unknown rotation field: {}", field)),
                                }
                            }
                            "position" => {
                                match field {
                                    "x" | "y" | "z" => Ok(Value::F32(0.0)),
                                    _ => Err(anyhow!("Unknown position field: {}", field)),
                                }
                            }
                            "scale" => {
                                match field {
                                    "x" | "y" | "z" => Ok(Value::F32(1.0)),
                                    _ => Err(anyhow!("Unknown scale field: {}", field)),
                                }
                            }
                            "color" => {
                                match field {
                                    "r" | "g" | "b" | "a" => Ok(Value::F32(1.0)),
                                    _ => Err(anyhow!("Unknown color field: {}", field)),
                                }
                            }
                            _ => env.get(s).ok_or_else(|| anyhow!("Undefined variable: {}", s))
                        }
                    } else {
                        env.get(s).ok_or_else(|| anyhow!("Undefined variable: {}", s))
                    }
                } else {
                    // Look up variable normally
                    env.get(s).ok_or_else(|| anyhow!("Undefined variable: {}", s))
                }
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
                    dsl::ast::Expr::List(_) => {
                        // If the first element is a list, this is a sequence of statements
                        // Evaluate each statement in order
                        let mut last_result = Value::Nil;
                        for expr in list {
                            last_result = self.eval(expr, env)?;
                        }
                        Ok(last_result)
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
                let mut debug_values = Vec::new();
                for arg in args {
                    match self.eval(arg, env)? {
                        Value::F32(n) => {
                            debug_values.push(n);
                            product *= n;
                        },
                        Value::I32(n) => {
                            debug_values.push(n as f32);
                            product *= n as f32;
                        },
                        _ => return Err(anyhow!("* expects numbers")),
                    }
                }
                // Debug multiplication for rotation calculations
                if debug_values.len() == 2 && (debug_values[0] > 0.5 || debug_values[1] > 0.5) {
                    // println!(DEBUG: * {} × {} = {}", debug_values[0], debug_values[1], product);
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
            "begin" | "do" | "progn" => {
                // Execute each expression in sequence, returning the last result
                let mut result = Value::Nil;
                for arg in args {
                    result = self.eval(arg, env)?;
                }
                Ok(result)
            }
            
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
            
            // Loop constructs
            "dotimes" => {
                // (dotimes (i n) body...)
                if args.len() < 2 {
                    return Err(anyhow!("dotimes needs (var count) and body"));
                }
                
                let dsl::ast::Expr::List(binding) = &args[0] else {
                    return Err(anyhow!("dotimes binding must be a list"));
                };
                
                if binding.len() != 2 {
                    return Err(anyhow!("dotimes binding must be (var count)"));
                }
                
                let dsl::ast::Expr::Sym(var) = &binding[0] else {
                    return Err(anyhow!("dotimes variable must be a symbol"));
                };
                
                let count = match self.eval(&binding[1], env)? {
                    Value::F32(f) => f as i32,
                    Value::I32(i) => i,
                    _ => return Err(anyhow!("dotimes count must be a number")),
                };
                
                let mut result = Value::Nil;
                let mut loop_env = Environment::with_parent(env.clone());
                
                for i in 0..count {
                    loop_env.set(var.clone(), Value::I32(i));
                    for body_expr in &args[1..] {
                        result = self.eval(body_expr, &mut loop_env)?;
                    }
                }
                
                Ok(result)
            }
            
            "loop" => {
                // Simple loop with collect support: (loop for i from 0 to 10 collect (* i i))
                if args.is_empty() {
                    return Err(anyhow!("loop needs clauses"));
                }
                
                let mut i = 0;
                let mut var_name = String::new();
                let mut from = 0;
                let mut to = 0;
                let mut collect_expr = None;
                let mut body_exprs = Vec::new();
                
                // Parse loop clauses
                while i < args.len() {
                    if let dsl::ast::Expr::Sym(keyword) = &args[i] {
                        match keyword.as_str() {
                            "for" => {
                                if i + 1 >= args.len() {
                                    return Err(anyhow!("loop for needs variable"));
                                }
                                if let dsl::ast::Expr::Sym(var) = &args[i + 1] {
                                    var_name = var.clone();
                                    i += 2;
                                } else {
                                    return Err(anyhow!("loop for variable must be symbol"));
                                }
                            }
                            "from" => {
                                if i + 1 >= args.len() {
                                    return Err(anyhow!("loop from needs value"));
                                }
                                from = match self.eval(&args[i + 1], env)? {
                                    Value::F32(f) => f as i32,
                                    Value::I32(n) => n,
                                    _ => return Err(anyhow!("loop from must be number")),
                                };
                                i += 2;
                            }
                            "to" => {
                                if i + 1 >= args.len() {
                                    return Err(anyhow!("loop to needs value"));
                                }
                                to = match self.eval(&args[i + 1], env)? {
                                    Value::F32(f) => f as i32,
                                    Value::I32(n) => n,
                                    _ => return Err(anyhow!("loop to must be number")),
                                };
                                i += 2;
                            }
                            "collect" => {
                                if i + 1 >= args.len() {
                                    return Err(anyhow!("loop collect needs expression"));
                                }
                                collect_expr = Some(&args[i + 1]);
                                i += 2;
                            }
                            "do" => {
                                // Collect remaining expressions as body
                                i += 1;
                                while i < args.len() {
                                    body_exprs.push(&args[i]);
                                    i += 1;
                                }
                            }
                            _ => {
                                return Err(anyhow!("Unknown loop keyword: {}", keyword));
                            }
                        }
                    } else {
                        return Err(anyhow!("Loop clause must start with keyword"));
                    }
                }
                
                // Execute loop
                let mut results = Vec::new();
                let mut loop_env = Environment::with_parent(env.clone());
                
                for idx in from..=to {
                    loop_env.set(var_name.clone(), Value::I32(idx));
                    
                    if let Some(expr) = collect_expr {
                        let val = self.eval(expr, &mut loop_env)?;
                        results.push(val);
                    }
                    
                    for body_expr in &body_exprs {
                        self.eval(body_expr, &mut loop_env)?;
                    }
                }
                
                if !results.is_empty() {
                    Ok(Value::List(results))
                } else {
                    Ok(Value::Nil)
                }
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
                
                // Handle dotted notation (e.g., rotation.y, position.x)
                if name.contains('.') {
                    let parts: Vec<&str> = name.split('.').collect();
                    if parts.len() == 2 {
                        let base = parts[0];
                        let field = parts[1];
                        
                        match base {
                            "rotation" => {
                                // Store rotation components
                                match field {
                                    "x" => env.set("rotation.x".to_string(), value.clone()),
                                    "y" => env.set("rotation.y".to_string(), value.clone()),
                                    "z" => env.set("rotation.z".to_string(), value.clone()),
                                    _ => return Err(anyhow!("Unknown rotation field: {}", field)),
                                }
                            }
                            "position" => {
                                // Store position components
                                match field {
                                    "x" => env.set("position.x".to_string(), value.clone()),
                                    "y" => env.set("position.y".to_string(), value.clone()),
                                    "z" => env.set("position.z".to_string(), value.clone()),
                                    _ => return Err(anyhow!("Unknown position field: {}", field)),
                                }
                            }
                            "scale" => {
                                // Store scale components
                                match field {
                                    "x" => env.set("scale.x".to_string(), value.clone()),
                                    "y" => env.set("scale.y".to_string(), value.clone()),
                                    "z" => env.set("scale.z".to_string(), value.clone()),
                                    _ => return Err(anyhow!("Unknown scale field: {}", field)),
                                }
                            }
                            "color" => {
                                // Store color components
                                match field {
                                    "r" => env.set("color.r".to_string(), value.clone()),
                                    "g" => env.set("color.g".to_string(), value.clone()),
                                    "b" => env.set("color.b".to_string(), value.clone()),
                                    "a" => env.set("color.a".to_string(), value.clone()),
                                    _ => return Err(anyhow!("Unknown color field: {}", field)),
                                }
                            }
                            _ => {
                                // Store as-is for unknown base names
                                env.set(name.clone(), value.clone());
                            }
                        }
                    } else {
                        env.set(name.clone(), value.clone());
                    }
                } else {
                    env.set(name.clone(), value.clone());
                }
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
                    let scale_val = self.eval_to_f32(&args[0], env)?;
                    Ok(Value::Vec3(scale_val, scale_val, scale_val))
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
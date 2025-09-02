//! Scene-specific macro expansion for procedural generation

use crate::ast::Expr;

/// Expand scene macros like dotimes and loop
pub fn expand_scene_forms(forms: Vec<Expr>) -> anyhow::Result<Vec<Expr>> {
    let mut expanded = Vec::new();
    
    for form in forms {
        match expand_scene_form(form.clone())? {
            Some(expanded_forms) => expanded.extend(expanded_forms),
            None => expanded.push(form),
        }
    }
    
    Ok(expanded)
}

/// Expand a single scene form if it's a macro
fn expand_scene_form(form: Expr) -> anyhow::Result<Option<Vec<Expr>>> {
    let Expr::List(ref parts) = form else {
        return Ok(None);
    };
    
    if parts.is_empty() {
        return Ok(None);
    }
    
    let Expr::Sym(ref tag) = parts[0] else {
        return Ok(None);
    };
    
    match tag.as_str() {
        "dotimes" => expand_dotimes(&parts[1..]),
        "loop" => expand_loop(&parts[1..]),
        "let" => expand_let(&parts[1..]),
        "cond" => expand_cond(&parts[1..]),
        _ => Ok(None),
    }
}

/// Expand dotimes macro
/// (dotimes (var count) body...)
fn expand_dotimes(parts: &[Expr]) -> anyhow::Result<Option<Vec<Expr>>> {
    if parts.len() < 2 {
        anyhow::bail!("dotimes requires binding and body");
    }
    
    let Expr::List(ref binding) = parts[0] else {
        anyhow::bail!("dotimes binding must be a list");
    };
    
    if binding.len() != 2 {
        anyhow::bail!("dotimes binding must have variable and count");
    }
    
    let Expr::Sym(ref var) = binding[0] else {
        anyhow::bail!("dotimes variable must be a symbol");
    };
    
    let count = match &binding[1] {
        Expr::I32(n) => *n as usize,
        _ => anyhow::bail!("dotimes count must be an integer"),
    };
    
    // Get the body forms
    let body_forms = &parts[1..];
    
    // Expand the loop
    let mut expanded = Vec::new();
    for i in 0..count {
        // Substitute the loop variable in each body form
        for body_form in body_forms {
            let substituted = substitute_variable(body_form, var, i)?;
            // Check if the substituted form is itself a macro that needs expansion
            if let Some(expanded_forms) = expand_scene_form(substituted.clone())? {
                expanded.extend(expanded_forms);
            } else {
                expanded.push(substituted);
            }
        }
    }
    
    Ok(Some(expanded))
}

/// Expand let binding
/// (let ((var1 val1) (var2 val2) ...) body...)
fn expand_let(parts: &[Expr]) -> anyhow::Result<Option<Vec<Expr>>> {
    // Debug output in REPL style
    if std::env::var("DSL_DEBUG").is_ok() {
        eprintln!(";;   [MACRO] expanding let with {} bindings", parts.len() - 1);
    }
    if parts.is_empty() {
        anyhow::bail!("let requires bindings and body");
    }
    
    let Expr::List(ref bindings) = parts[0] else {
        anyhow::bail!("let bindings must be a list");
    };
    
    // Parse bindings
    let mut env = std::collections::HashMap::new();
    for binding in bindings {
        let Expr::List(ref pair) = binding else {
            anyhow::bail!("let binding must be a list");
        };
        
        if pair.len() != 2 {
            anyhow::bail!("let binding must have variable and value");
        }
        
        let Expr::Sym(ref var) = pair[0] else {
            anyhow::bail!("let variable must be a symbol");
        };
        
        // Evaluate the value expression
        let value = evaluate_expr(&pair[1], &env)?;
        env.insert(var.clone(), value);
    }
    
    // Substitute variables in body and expand any nested macros
    let body_forms = &parts[1..];
    let mut expanded = Vec::new();
    for form in body_forms {
        let substituted = substitute_let_variables(form, &env)?;
        // Check if the substituted form is a cond that needs expansion
        if let Expr::List(ref parts) = substituted {
            if !parts.is_empty() {
                if let Expr::Sym(ref tag) = parts[0] {
                    if tag == "cond" {
                        // Expand cond with the current environment
                        if let Some(cond_expanded) = expand_cond_with_env(&parts[1..], &env)? {
                            expanded.extend(cond_expanded);
                        }
                    } else {
                        expanded.push(substituted);
                    }
                } else {
                    expanded.push(substituted);
                }
            } else {
                expanded.push(substituted);
            }
        } else {
            expanded.push(substituted);
        }
    }
    
    Ok(Some(expanded))
}

/// Expand cond (conditional)
/// (cond (test1 expr1...) (test2 expr2...) ... (else exprN...))
fn expand_cond(parts: &[Expr]) -> anyhow::Result<Option<Vec<Expr>>> {
    // Call the version with empty environment for backward compatibility
    expand_cond_with_env(parts, &std::collections::HashMap::new())
}

/// Expand cond with environment from let bindings
fn expand_cond_with_env(parts: &[Expr], env: &std::collections::HashMap<String, Expr>) -> anyhow::Result<Option<Vec<Expr>>> {
    for clause in parts {
        let Expr::List(ref clause_parts) = clause else {
            anyhow::bail!("cond clause must be a list");
        };
        
        if clause_parts.is_empty() {
            anyhow::bail!("cond clause cannot be empty");
        }
        
        // Check the test condition
        let test = &clause_parts[0];
        
        // Handle 'else' clause
        if let Expr::Sym(s) = test {
            if s == "else" {
                // Return the body of the else clause, substituting any variables
                let mut result = Vec::new();
                for expr in &clause_parts[1..] {
                    result.push(substitute_let_variables(expr, env)?);
                }
                return Ok(Some(result));
            }
        }
        
        // Evaluate test condition with environment
        if evaluate_condition_with_env(test, env)? {
            // Return the body of this clause, substituting any variables
            let mut result = Vec::new();
            for expr in &clause_parts[1..] {
                result.push(substitute_let_variables(expr, env)?);
            }
            return Ok(Some(result));
        }
    }
    
    // No matching condition
    Ok(Some(Vec::new()))
}

/// Evaluate a simple expression for let bindings
fn evaluate_expr(expr: &Expr, env: &std::collections::HashMap<String, Expr>) -> anyhow::Result<Expr> {
    match expr {
        Expr::Sym(s) => {
            // Look up variable in environment
            env.get(s).cloned().ok_or_else(|| anyhow::anyhow!("Undefined variable: {}", s))
        }
        Expr::List(parts) if !parts.is_empty() => {
            // Handle arithmetic and function calls
            if let Expr::Sym(op) = &parts[0] {
                match op.as_str() {
                    "+" | "-" | "*" | "/" | "mod" => evaluate_arithmetic(op, &parts[1..], env),
                    "format" => {
                        // Keep format expressions as-is for now
                        Ok(expr.clone())
                    }
                    _ => Ok(expr.clone()),
                }
            } else {
                Ok(expr.clone())
            }
        }
        _ => Ok(expr.clone()),
    }
}

/// Evaluate arithmetic operations with unlimited arguments
fn evaluate_arithmetic(op: &str, args: &[Expr], env: &std::collections::HashMap<String, Expr>) -> anyhow::Result<Expr> {
    if args.is_empty() {
        anyhow::bail!("{} requires at least one argument", op);
    }
    
    // Special case for subtraction and division - they need at least 2 arguments
    if (op == "-" || op == "/" || op == "mod") && args.len() < 2 {
        anyhow::bail!("{} requires at least 2 arguments", op);
    }
    
    // Evaluate all arguments to numbers
    let mut values = Vec::new();
    for arg in args {
        let evaluated = evaluate_expr(arg, env)?;
        let val = match evaluated {
            Expr::I32(n) => n as f32,
            Expr::F32(f) => f,
            _ => anyhow::bail!("Arithmetic operand must be a number"),
        };
        values.push(val);
    }
    
    // Compute the result based on the operation
    let result = match op {
        "+" => values.iter().sum::<f32>(),
        "-" => {
            // Subtract all subsequent values from the first
            let mut result = values[0];
            for val in &values[1..] {
                result -= val;
            }
            result
        },
        "*" => values.iter().product::<f32>(),
        "/" => {
            // Divide the first by all subsequent values
            let mut result = values[0];
            for val in &values[1..] {
                result /= val;
            }
            result
        },
        "mod" => {
            // For mod, we'll keep it binary for now (most common use case)
            if values.len() != 2 {
                anyhow::bail!("mod requires exactly 2 arguments");
            }
            values[0] % values[1]
        },
        _ => anyhow::bail!("Unknown operator: {}", op),
    };
    
    // Return as integer if whole number
    if result.fract() == 0.0 {
        Ok(Expr::I32(result as i32))
    } else {
        Ok(Expr::F32(result))
    }
}

/// Evaluate a condition for cond
fn evaluate_condition(test: &Expr) -> anyhow::Result<bool> {
    // Call the version with empty environment for backward compatibility
    evaluate_condition_with_env(test, &std::collections::HashMap::new())
}

/// Evaluate a condition with environment
fn evaluate_condition_with_env(test: &Expr, env: &std::collections::HashMap<String, Expr>) -> anyhow::Result<bool> {
    match test {
        Expr::Bool(b) => Ok(*b),
        Expr::List(parts) if parts.len() == 3 => {
            // Handle comparison operators
            if let Expr::Sym(op) = &parts[0] {
                // Evaluate the operands with the environment
                let left = evaluate_expr(&parts[1], env)?;
                let right = evaluate_expr(&parts[2], env)?;
                
                match op.as_str() {
                    "=" | "==" => Ok(exprs_equal(&left, &right)),
                    "<" => {
                        match (&left, &right) {
                            (Expr::I32(a), Expr::I32(b)) => Ok(a < b),
                            (Expr::F32(a), Expr::F32(b)) => Ok(a < b),
                            (Expr::I32(a), Expr::F32(b)) => Ok((*a as f32) < *b),
                            (Expr::F32(a), Expr::I32(b)) => Ok(*a < (*b as f32)),
                            _ => Ok(false),
                        }
                    }
                    ">" => {
                        match (&left, &right) {
                            (Expr::I32(a), Expr::I32(b)) => Ok(a > b),
                            (Expr::F32(a), Expr::F32(b)) => Ok(a > b),
                            (Expr::I32(a), Expr::F32(b)) => Ok((*a as f32) > *b),
                            (Expr::F32(a), Expr::I32(b)) => Ok(*a > (*b as f32)),
                            _ => Ok(false),
                        }
                    }
                    "<=" => {
                        match (&left, &right) {
                            (Expr::I32(a), Expr::I32(b)) => Ok(a <= b),
                            (Expr::F32(a), Expr::F32(b)) => Ok(a <= b),
                            (Expr::I32(a), Expr::F32(b)) => Ok((*a as f32) <= *b),
                            (Expr::F32(a), Expr::I32(b)) => Ok(*a <= (*b as f32)),
                            _ => Ok(false),
                        }
                    }
                    ">=" => {
                        match (&left, &right) {
                            (Expr::I32(a), Expr::I32(b)) => Ok(a >= b),
                            (Expr::F32(a), Expr::F32(b)) => Ok(a >= b),
                            (Expr::I32(a), Expr::F32(b)) => Ok((*a as f32) >= *b),
                            (Expr::F32(a), Expr::I32(b)) => Ok(*a >= (*b as f32)),
                            _ => Ok(false),
                        }
                    }
                    "!=" | "/=" => Ok(!exprs_equal(&left, &right)),
                    _ => Ok(false), // Unknown operator, assume false
                }
            } else {
                Ok(false)
            }
        }
        _ => Ok(false), // Non-boolean, assume false
    }
}

/// Check if two expressions are equal
fn exprs_equal(a: &Expr, b: &Expr) -> bool {
    match (a, b) {
        (Expr::I32(x), Expr::I32(y)) => x == y,
        (Expr::F32(x), Expr::F32(y)) => (x - y).abs() < 0.0001,
        (Expr::I32(x), Expr::F32(y)) | (Expr::F32(y), Expr::I32(x)) => (*x as f32 - y).abs() < 0.0001,
        (Expr::Sym(x), Expr::Sym(y)) => x == y,
        (Expr::Str(x), Expr::Str(y)) => x == y,
        _ => false,
    }
}

/// Substitute let-bound variables in an expression
fn substitute_let_variables(expr: &Expr, env: &std::collections::HashMap<String, Expr>) -> anyhow::Result<Expr> {
    match expr {
        Expr::Sym(s) => {
            // Replace with bound value if it exists
            Ok(env.get(s).cloned().unwrap_or_else(|| expr.clone()))
        }
        Expr::List(parts) => {
            let mut substituted = Vec::new();
            for part in parts {
                substituted.push(substitute_let_variables(part, env)?);
            }
            Ok(Expr::List(substituted))
        }
        _ => Ok(expr.clone()),
    }
}

/// Expand loop macro (simplified version for now)
/// (loop for var from start to end collect body)
fn expand_loop(parts: &[Expr]) -> anyhow::Result<Option<Vec<Expr>>> {
    // Parse loop syntax
    if parts.len() < 7 {
        anyhow::bail!("loop requires: for var from start to end collect body");
    }
    
    let mut i = 0;
    
    // Check 'for'
    if !matches!(&parts[i], Expr::Sym(s) if s == "for") {
        anyhow::bail!("loop must start with 'for'");
    }
    i += 1;
    
    // Get variable
    let Expr::Sym(ref var) = parts[i] else {
        anyhow::bail!("loop variable must be a symbol");
    };
    i += 1;
    
    // Check 'from'
    if !matches!(&parts[i], Expr::Sym(s) if s == "from") {
        anyhow::bail!("expected 'from' in loop");
    }
    i += 1;
    
    // Get start value
    let start = match &parts[i] {
        Expr::I32(n) => *n as usize,
        _ => anyhow::bail!("loop start must be an integer"),
    };
    i += 1;
    
    // Check 'to'
    if !matches!(&parts[i], Expr::Sym(s) if s == "to") {
        anyhow::bail!("expected 'to' in loop");
    }
    i += 1;
    
    // Get end value
    let end = match &parts[i] {
        Expr::I32(n) => *n as usize,
        _ => anyhow::bail!("loop end must be an integer"),
    };
    i += 1;
    
    // Check 'collect' (optional)
    if matches!(&parts[i], Expr::Sym(s) if s == "collect") {
        i += 1;
    }
    
    // Get body forms
    let body_forms = &parts[i..];
    
    // Expand the loop
    let mut expanded = Vec::new();
    for j in start..=end {
        // Substitute the loop variable in each body form
        for body_form in body_forms {
            let substituted = substitute_variable(body_form, var, j)?;
            expanded.push(substituted);
        }
    }
    
    Ok(Some(expanded))
}

/// Substitute a variable with a value in an expression
fn substitute_variable(expr: &Expr, var: &str, value: usize) -> anyhow::Result<Expr> {
    match expr {
        Expr::Sym(s) if s == var => Ok(Expr::I32(value as i32)),
        Expr::List(parts) => {
            // Handle math functions (sin, cos, etc.) with 2 parts
            if parts.len() == 2 {
                if let Expr::Sym(ref func) = parts[0] {
                    // Substitute variable in argument first
                    let arg = substitute_variable(&parts[1], var, value)?;
                    
                    // Try to evaluate if argument is a number
                    let arg_val = match arg {
                        Expr::I32(n) => Some(n as f32),
                        Expr::F32(f) => Some(f),
                        _ => None,
                    };
                    
                    if let Some(val) = arg_val {
                        let result = match func.as_str() {
                            "sin" => val.sin(),
                            "cos" => val.cos(),
                            "tan" => val.tan(),
                            "abs" => val.abs(),
                            "sqrt" => val.sqrt(),
                            "exp" => val.exp(),
                            "ln" => val.ln(),
                            "floor" => val.floor(),
                            "ceil" => val.ceil(),
                            "round" => val.round(),
                            _ => return Ok(Expr::List(vec![parts[0].clone(), arg])),
                        };
                        
                        return Ok(Expr::F32(result));
                    }
                    
                    // If we can't evaluate, return the substituted expression
                    return Ok(Expr::List(vec![parts[0].clone(), arg]));
                }
            }
            
            // Handle arithmetic operations with multiple arguments
            if parts.len() >= 2 {
                if let Expr::Sym(ref op) = parts[0] {
                    // Check if this is an arithmetic operation
                    let is_arithmetic = matches!(op.as_str(), "+" | "-" | "*" | "/" | "mod");
                    let is_binary_only = matches!(op.as_str(), "pow" | "min" | "max");
                    
                    if is_arithmetic || is_binary_only {
                        // Substitute variables in all operands
                        let mut substituted_args = Vec::new();
                        for arg in &parts[1..] {
                            substituted_args.push(substitute_variable(arg, var, value)?);
                        }
                        
                        // Try to evaluate if all are numbers
                        let mut all_numbers = true;
                        let mut values = Vec::new();
                        for arg in &substituted_args {
                            match arg {
                                Expr::I32(n) => values.push(*n as f32),
                                Expr::F32(f) => values.push(*f),
                                _ => {
                                    all_numbers = false;
                                    break;
                                }
                            }
                        }
                        
                        if all_numbers && !values.is_empty() {
                            let result = if is_binary_only {
                                // Binary operations
                                if values.len() != 2 {
                                    return Ok(Expr::List([vec![parts[0].clone()], substituted_args].concat()));
                                }
                                match op.as_str() {
                                    "pow" => values[0].powf(values[1]),
                                    "min" => values[0].min(values[1]),
                                    "max" => values[0].max(values[1]),
                                    _ => return Ok(Expr::List([vec![parts[0].clone()], substituted_args].concat())),
                                }
                            } else {
                                // Multi-argument arithmetic operations
                                match op.as_str() {
                                    "+" => values.iter().sum::<f32>(),
                                    "-" => {
                                        if values.len() == 1 {
                                            -values[0]
                                        } else {
                                            let mut result = values[0];
                                            for val in &values[1..] {
                                                result -= val;
                                            }
                                            result
                                        }
                                    },
                                    "*" => values.iter().product::<f32>(),
                                    "/" => {
                                        let mut result = values[0];
                                        for val in &values[1..] {
                                            result /= val;
                                        }
                                        result
                                    },
                                    "mod" => {
                                        if values.len() != 2 {
                                            return Ok(Expr::List([vec![parts[0].clone()], substituted_args].concat()));
                                        }
                                        values[0] % values[1]
                                    },
                                    _ => return Ok(Expr::List([vec![parts[0].clone()], substituted_args].concat())),
                                }
                            };
                            
                            // Return as integer if it's a whole number
                            if result.fract() == 0.0 && result.is_finite() {
                                return Ok(Expr::I32(result as i32));
                            } else {
                                return Ok(Expr::F32(result));
                            }
                        }
                        
                        // If we can't evaluate, return the substituted expression
                        return Ok(Expr::List([vec![parts[0].clone()], substituted_args].concat()));
                    }
                }
            }
            
            // Handle special case for format strings
            if parts.len() >= 2 {
                if let Expr::Sym(ref tag) = parts[0] {
                    if tag == "format" {
                        // Format string substitution
                        if let Expr::Str(ref template) = parts[1] {
                            // Substitute variables in the arguments first
                            let mut substituted_args = Vec::new();
                            for part in &parts[2..] {
                                substituted_args.push(substitute_variable(part, var, value)?);
                            }
                            
                            // Now format the string with the substituted values
                            let mut formatted = template.clone();
                            for arg in substituted_args {
                                if let Some(pos) = formatted.find("~a") {
                                    let val_str = match arg {
                                        Expr::I32(n) => n.to_string(),
                                        Expr::F32(f) => f.to_string(),
                                        Expr::Sym(s) => s,
                                        Expr::Str(s) => s,
                                        _ => "?".to_string(),
                                    };
                                    formatted.replace_range(pos..pos+2, &val_str);
                                }
                            }
                            return Ok(Expr::Str(formatted));
                        } else if parts.len() == 3 {
                            // Handle format with a simple string template as first arg (not Str variant)
                            // This handles (format "template" var) where "template" is parsed as symbol
                            if let Expr::Sym(ref template) = parts[1] {
                                // First substitute the variable
                                let substituted_arg = substitute_variable(&parts[2], var, value)?;
                                
                                // Format the string
                                let mut formatted = template.clone();
                                if let Some(pos) = formatted.find("~a") {
                                    let val_str = match substituted_arg {
                                        Expr::I32(n) => n.to_string(),
                                        Expr::F32(f) => f.to_string(),
                                        Expr::Sym(s) => s,
                                        Expr::Str(s) => s,
                                        _ => "?".to_string(),
                                    };
                                    formatted.replace_range(pos..pos+2, &val_str);
                                }
                                return Ok(Expr::Str(formatted));
                            }
                        }
                    }
                }
            }
            
            // Regular list substitution
            let mut substituted = Vec::new();
            for part in parts {
                substituted.push(substitute_variable(part, var, value)?);
            }
            Ok(Expr::List(substituted))
        }
        _ => Ok(expr.clone()),
    }
}

/// Handle nested dotimes for grid generation
pub fn expand_nested_dotimes(form: &Expr) -> anyhow::Result<Vec<Expr>> {
    let Expr::List(ref parts) = form else {
        return Ok(vec![form.clone()]);
    };
    
    if parts.is_empty() || !matches!(&parts[0], Expr::Sym(s) if s == "dotimes") {
        return Ok(vec![form.clone()]);
    }
    
    // Parse outer dotimes
    let Expr::List(ref outer_binding) = parts[1] else {
        anyhow::bail!("dotimes binding must be a list");
    };
    
    let Expr::Sym(ref outer_var) = outer_binding[0] else {
        anyhow::bail!("dotimes variable must be a symbol");
    };
    
    let outer_count = match &outer_binding[1] {
        Expr::I32(n) => *n as usize,
        _ => anyhow::bail!("dotimes count must be an integer"),
    };
    
    let mut expanded = Vec::new();
    
    // Check if body contains nested dotimes
    for outer_i in 0..outer_count {
        for body_form in &parts[2..] {
            // Check if this is a nested dotimes
            if let Expr::List(ref inner_parts) = body_form {
                if !inner_parts.is_empty() && matches!(&inner_parts[0], Expr::Sym(s) if s == "dotimes") {
                    // Handle nested dotimes
                    let Expr::List(ref inner_binding) = inner_parts[1] else {
                        continue;
                    };
                    
                    let Expr::Sym(ref inner_var) = inner_binding[0] else {
                        continue;
                    };
                    
                    let inner_count = match &inner_binding[1] {
                        Expr::I32(n) => *n as usize,
                        _ => continue,
                    };
                    
                    // Expand nested loop
                    for inner_i in 0..inner_count {
                        for inner_body in &inner_parts[2..] {
                            // Substitute both variables
                            let mut substituted = substitute_variable(inner_body, outer_var, outer_i)?;
                            substituted = substitute_variable(&substituted, inner_var, inner_i)?;
                            expanded.push(substituted);
                        }
                    }
                } else {
                    // Regular substitution
                    let substituted = substitute_variable(body_form, outer_var, outer_i)?;
                    expanded.push(substituted);
                }
            } else {
                // Regular substitution
                let substituted = substitute_variable(body_form, outer_var, outer_i)?;
                expanded.push(substituted);
            }
        }
    }
    
    Ok(expanded)
}
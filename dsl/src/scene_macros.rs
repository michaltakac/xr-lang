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
            expanded.push(substituted);
        }
    }
    
    Ok(Some(expanded))
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
            
            // Handle arithmetic operations with 3 parts
            if parts.len() == 3 {
                if let Expr::Sym(ref op) = parts[0] {
                    // Check if this is an arithmetic operation
                    let is_arithmetic = matches!(op.as_str(), "+" | "-" | "*" | "/" | "pow" | "min" | "max");
                    if is_arithmetic {
                        // Substitute variables in operands
                        let left = substitute_variable(&parts[1], var, value)?;
                        let right = substitute_variable(&parts[2], var, value)?;
                        
                        // Try to evaluate if both are numbers
                        let left_val = match left {
                            Expr::I32(n) => Some(n as f32),
                            Expr::F32(f) => Some(f),
                            _ => None,
                        };
                        
                        let right_val = match right {
                            Expr::I32(n) => Some(n as f32),
                            Expr::F32(f) => Some(f),
                            _ => None,
                        };
                        
                        if let (Some(l), Some(r)) = (left_val, right_val) {
                            let result = match op.as_str() {
                                "+" => l + r,
                                "-" => l - r,
                                "*" => l * r,
                                "/" => l / r,
                                "pow" => l.powf(r),
                                "min" => l.min(r),
                                "max" => l.max(r),
                                _ => return Ok(Expr::List(vec![parts[0].clone(), left, right])),
                            };
                            
                            // Return as integer if it's a whole number
                            if result.fract() == 0.0 && result.is_finite() {
                                return Ok(Expr::I32(result as i32));
                            } else {
                                return Ok(Expr::F32(result));
                            }
                        }
                        
                        // If we can't evaluate, return the substituted expression
                        return Ok(Expr::List(vec![parts[0].clone(), left, right]));
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
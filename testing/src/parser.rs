//! Parser for test DSL extensions

use crate::ast::*;
use anyhow::Result;
use std::collections::HashMap;

pub fn parse_test(expr: &dsl::ast::Expr) -> Result<TestDef> {
    let dsl::ast::Expr::List(list) = expr else {
        anyhow::bail!("test must be a list");
    };
    
    if list.is_empty() {
        anyhow::bail!("test list cannot be empty");
    }
    
    let dsl::ast::Expr::Sym(head) = &list[0] else {
        anyhow::bail!("test must start with symbol");
    };
    
    if head != "deftest" {
        anyhow::bail!("expected deftest, got {}", head);
    }
    
    if list.len() < 2 {
        anyhow::bail!("deftest needs at least a name");
    }
    
    let name = match &list[1] {
        dsl::ast::Expr::Sym(s) => s.clone(),
        dsl::ast::Expr::List(l) if !l.is_empty() => {
            if let dsl::ast::Expr::Sym(s) = &l[0] {
                s.clone()
            } else {
                anyhow::bail!("test name must be a symbol");
            }
        }
        _ => anyhow::bail!("test name must be a symbol"),
    };
    
    let mut setup = None;
    let mut actions = Vec::new();
    let mut assertions = Vec::new();
    let mut teardown = None;
    let mut meta = None;
    
    for item in &list[2..] {
        let dsl::ast::Expr::List(form) = item else {
            continue;
        };
        
        if form.is_empty() {
            continue;
        }
        
        let dsl::ast::Expr::Sym(tag) = &form[0] else {
            continue;
        };
        
        match tag.as_str() {
            "setup" => {
                setup = Some(parse_setup(&form[1..])?);
            }
            "actions" => {
                for action_expr in &form[1..] {
                    actions.push(parse_action(action_expr)?);
                }
            }
            "assertions" => {
                for assertion_expr in &form[1..] {
                    assertions.push(parse_assertion(assertion_expr)?);
                }
            }
            "teardown" => {
                teardown = Some(parse_teardown(&form[1..])?);
            }
            "meta" => {
                meta = Some(parse_test_meta(&form[1..])?);
            }
            _ => {}
        }
    }
    
    Ok(TestDef {
        name,
        description: None,
        setup,
        actions,
        assertions,
        teardown,
        meta,
    })
}

fn parse_setup(exprs: &[dsl::ast::Expr]) -> Result<TestSetup> {
    let mut steps = Vec::new();
    
    for expr in exprs {
        if let dsl::ast::Expr::List(step_list) = expr {
            steps.push(parse_step(step_list)?);
        }
    }
    
    Ok(TestSetup { steps })
}

fn parse_teardown(exprs: &[dsl::ast::Expr]) -> Result<TestTeardown> {
    let mut steps = Vec::new();
    
    for expr in exprs {
        if let dsl::ast::Expr::List(step_list) = expr {
            steps.push(parse_step(step_list)?);
        }
    }
    
    Ok(TestTeardown { steps })
}

fn parse_step(list: &[dsl::ast::Expr]) -> Result<TestStep> {
    if list.is_empty() {
        anyhow::bail!("step cannot be empty");
    }
    
    let dsl::ast::Expr::Sym(action) = &list[0] else {
        anyhow::bail!("step action must be a symbol");
    };
    
    let mut params = HashMap::new();
    
    for pair in list[1..].chunks(2) {
        if pair.len() == 2 {
            if let dsl::ast::Expr::Sym(key) = &pair[0] {
                params.insert(key.clone(), expr_to_test_value(&pair[1])?);
            }
        }
    }
    
    Ok(TestStep {
        action: action.clone(),
        params,
    })
}

fn parse_action(expr: &dsl::ast::Expr) -> Result<TestAction> {
    let dsl::ast::Expr::List(list) = expr else {
        anyhow::bail!("action must be a list");
    };
    
    if list.is_empty() {
        anyhow::bail!("action cannot be empty");
    }
    
    let dsl::ast::Expr::Sym(action_type) = &list[0] else {
        anyhow::bail!("action type must be a symbol");
    };
    
    match action_type.as_str() {
        "move-camera" => {
            if list.len() < 3 {
                anyhow::bail!("move-camera needs position and target");
            }
            let position = parse_vec3(&list[1])?;
            let target = parse_vec3(&list[2])?;
            Ok(TestAction::MoveCamera { position, target })
        }
        "trigger-hot-reload" => Ok(TestAction::TriggerHotReload),
        "wait" => {
            if list.len() < 2 {
                anyhow::bail!("wait needs duration");
            }
            let duration_ms = match &list[1] {
                dsl::ast::Expr::I32(n) => *n as u32,
                dsl::ast::Expr::F32(n) => *n as u32,
                _ => anyhow::bail!("wait duration must be a number"),
            };
            Ok(TestAction::Wait { duration_ms })
        }
        "record-snapshot" => {
            if list.len() < 2 {
                anyhow::bail!("record-snapshot needs a name");
            }
            let name = match &list[1] {
                dsl::ast::Expr::Sym(s) => s.clone(),
                _ => anyhow::bail!("snapshot name must be a symbol"),
            };
            Ok(TestAction::RecordSnapshot { name })
        }
        _ => anyhow::bail!("unknown action type: {}", action_type),
    }
}

fn parse_assertion(expr: &dsl::ast::Expr) -> Result<TestAssertion> {
    let dsl::ast::Expr::List(list) = expr else {
        anyhow::bail!("assertion must be a list");
    };
    
    if list.is_empty() {
        anyhow::bail!("assertion cannot be empty");
    }
    
    let dsl::ast::Expr::Sym(assertion_type) = &list[0] else {
        anyhow::bail!("assertion type must be a symbol");
    };
    
    match assertion_type.as_str() {
        "assert-eq" => {
            if list.len() < 3 {
                anyhow::bail!("assert-eq needs actual and expected");
            }
            let actual = match &list[1] {
                dsl::ast::Expr::Sym(s) => s.clone(),
                _ => anyhow::bail!("actual must be a symbol"),
            };
            let expected = expr_to_test_value(&list[2])?;
            Ok(TestAssertion::AssertEq { actual, expected })
        }
        "assert-preserved" => {
            if list.len() < 2 {
                anyhow::bail!("assert-preserved needs an object");
            }
            let object = match &list[1] {
                dsl::ast::Expr::Sym(s) => s.clone(),
                _ => anyhow::bail!("object must be a symbol"),
            };
            Ok(TestAssertion::AssertPreserved { object })
        }
        "assert-no-errors" => Ok(TestAssertion::AssertNoErrors),
        _ => anyhow::bail!("unknown assertion type: {}", assertion_type),
    }
}

fn parse_test_meta(exprs: &[dsl::ast::Expr]) -> Result<TestMeta> {
    let mut tags = Vec::new();
    let mut skip = false;
    let mut timeout_ms = None;
    let mut flaky = false;
    let mut device_profiles = Vec::new();
    
    for expr in exprs {
        if let dsl::ast::Expr::List(list) = expr {
            if list.len() >= 2 {
                if let dsl::ast::Expr::Sym(key) = &list[0] {
                    match key.as_str() {
                        "tags" => {
                            for tag_expr in &list[1..] {
                                if let dsl::ast::Expr::Sym(tag) = tag_expr {
                                    tags.push(tag.clone());
                                }
                            }
                        }
                        "skip" => {
                            skip = match &list[1] {
                                dsl::ast::Expr::Bool(b) => *b,
                                _ => false,
                            };
                        }
                        "timeout" => {
                            timeout_ms = match &list[1] {
                                dsl::ast::Expr::I32(n) => Some(*n as u32),
                                dsl::ast::Expr::F32(n) => Some(*n as u32),
                                _ => None,
                            };
                        }
                        "flaky" => {
                            flaky = match &list[1] {
                                dsl::ast::Expr::Bool(b) => *b,
                                _ => false,
                            };
                        }
                        "devices" => {
                            for device_expr in &list[1..] {
                                if let dsl::ast::Expr::Sym(device) = device_expr {
                                    device_profiles.push(device.clone());
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
        }
    }
    
    Ok(TestMeta {
        tags,
        skip,
        timeout_ms,
        flaky,
        device_profiles,
    })
}

fn parse_vec3(expr: &dsl::ast::Expr) -> Result<[f32; 3]> {
    let dsl::ast::Expr::List(list) = expr else {
        anyhow::bail!("vec3 must be a list");
    };
    
    if list.len() != 3 {
        anyhow::bail!("vec3 must have exactly 3 elements");
    }
    
    let x = match &list[0] {
        dsl::ast::Expr::F32(n) => *n,
        dsl::ast::Expr::I32(n) => *n as f32,
        _ => anyhow::bail!("vec3 x must be a number"),
    };
    
    let y = match &list[1] {
        dsl::ast::Expr::F32(n) => *n,
        dsl::ast::Expr::I32(n) => *n as f32,
        _ => anyhow::bail!("vec3 y must be a number"),
    };
    
    let z = match &list[2] {
        dsl::ast::Expr::F32(n) => *n,
        dsl::ast::Expr::I32(n) => *n as f32,
        _ => anyhow::bail!("vec3 z must be a number"),
    };
    
    Ok([x, y, z])
}

fn expr_to_test_value(expr: &dsl::ast::Expr) -> Result<TestValue> {
    match expr {
        dsl::ast::Expr::F32(n) => Ok(TestValue::F32(*n)),
        dsl::ast::Expr::I32(n) => Ok(TestValue::I32(*n)),
        dsl::ast::Expr::Bool(b) => Ok(TestValue::Bool(*b)),
        dsl::ast::Expr::Sym(s) => Ok(TestValue::String(s.clone())),
        dsl::ast::Expr::List(list) => {
            if list.len() == 3 {
                // Try to parse as Vec3
                if let Ok(vec3) = parse_vec3(expr) {
                    return Ok(TestValue::Vec3(vec3));
                }
            }
            // Otherwise, parse as list
            let mut values = Vec::new();
            for item in list {
                values.push(expr_to_test_value(item)?);
            }
            Ok(TestValue::List(values))
        }
    }
}
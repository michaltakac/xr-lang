//! Test 3D examples to ensure scene primitives work correctly

use vm::{
    parser::Parser,
    evaluator::Evaluator,
    value::{Value, Symbol, Environment},
};
use std::fs;
use std::rc::Rc;

#[test]
fn test_3d_scene_basic_example() {
    let source = fs::read_to_string("../examples/3d_scene_basic.xrl")
        .expect("Failed to read 3d_scene_basic.xrl");
    
    let mut parser = Parser::new(&source);
    let mut evaluator = Evaluator::new();
    let env = Rc::new(Environment::new());
    
    let mut object_count = 0;
    let exprs = parser.parse().expect("Failed to parse 3d_scene_basic.xrl");
    for expr in exprs {
        match evaluator.eval(&expr, env.clone()) {
            Ok(val) => {
                // Verify objects are created
                if let Value::Object(id) = &val {
                    println!("Created object: {:?}", id);
                    object_count += 1;
                }
            }
            Err(e) => {
                panic!("Evaluation error in 3d_scene_basic.xrl: {}", e);
            }
        }
    }
    
    // Check that we have some objects created
    assert!(object_count > 0, "Should have created some objects");
}

#[test]
fn test_3d_animated_scene_example() {
    let source = fs::read_to_string("../examples/3d_animated_scene.xrl")
        .expect("Failed to read 3d_animated_scene.xrl");
    
    let mut parser = Parser::new(&source);
    let mut evaluator = Evaluator::new();
    let env = Rc::new(Environment::new());
    
    let mut object_count = 0;
    let exprs = parser.parse().expect("Failed to parse 3d_animated_scene.xrl");
    for expr in exprs {
        match evaluator.eval(&expr, env.clone()) {
            Ok(val) => {
                if let Value::Object(_) = val {
                    object_count += 1;
                }
            }
            Err(e) => {
                panic!("Evaluation error in 3d_animated_scene.xrl: {}", e);
            }
        }
    }
    
    // We should have created at least the camera and some objects
    assert!(object_count > 0, "Should have created at least one object");
}

#[test]
fn test_scene_primitives_directly() {
    let mut evaluator = Evaluator::new();
    let env = Rc::new(Environment::new());
    
    // Test create-camera
    let camera_expr = Value::List(vec![
        Value::Symbol(Symbol("create-camera".to_string())),
        Value::Vector(vec![Value::Int(0), Value::Int(5), Value::Int(10)]),
    ]);
    
    let result = evaluator.eval(&camera_expr, env.clone());
    assert!(result.is_ok(), "create-camera should succeed");
    
    if let Ok(Value::Object(id)) = result {
        println!("Created camera with ID: {:?}", id);
        
        // Test get-position
        let get_pos_expr = Value::List(vec![
            Value::Symbol(Symbol("get-position".to_string())),
            Value::Object(id),
        ]);
        
        let pos_result = evaluator.eval(&get_pos_expr, env.clone());
        assert!(pos_result.is_ok(), "get-position should succeed");
        
        if let Ok(Value::Vector(coords)) = pos_result {
            assert_eq!(coords.len(), 3, "Position should be a 3D vector");
        }
    }
    
    // Test create-cube
    let cube_expr = Value::List(vec![
        Value::Symbol(Symbol("create-cube".to_string())),
        Value::Vector(vec![Value::Int(1), Value::Int(2), Value::Int(3)]),
    ]);
    
    let cube_result = evaluator.eval(&cube_expr, env.clone());
    assert!(cube_result.is_ok(), "create-cube should succeed");
    
    if let Ok(Value::Object(cube_id)) = cube_result {
        // Test update-transform
        let update_expr = Value::List(vec![
            Value::Symbol(Symbol("update-transform".to_string())),
            Value::Object(cube_id),
            Value::Vector(vec![Value::Int(5), Value::Int(0), Value::Int(0)]),
        ]);
        
        let update_result = evaluator.eval(&update_expr, env.clone());
        assert!(update_result.is_ok(), "update-transform should succeed");
        
        // Test rotate
        let rotate_expr = Value::List(vec![
            Value::Symbol(Symbol("rotate".to_string())),
            Value::Object(cube_id),
            Value::Vector(vec![Value::Int(0), Value::Int(45), Value::Int(0)]),
        ]);
        
        let rotate_result = evaluator.eval(&rotate_expr, env.clone());
        assert!(rotate_result.is_ok(), "rotate should succeed");
        
        // Test scale
        let scale_expr = Value::List(vec![
            Value::Symbol(Symbol("scale".to_string())),
            Value::Object(cube_id),
            Value::Vector(vec![Value::Float(2.0), Value::Float(2.0), Value::Float(2.0)]),
        ]);
        
        let scale_result = evaluator.eval(&scale_expr, env.clone());
        assert!(scale_result.is_ok(), "scale should succeed");
    }
}

#[test] 
fn test_math_functions_in_3d_context() {
    let mut evaluator = Evaluator::new();
    let env = Rc::new(Environment::new());
    
    // Test sin function (used in orbit calculations)
    let sin_expr = Value::List(vec![
        Value::Symbol(Symbol("sin".to_string())),
        Value::Float(1.5708), // pi/2
    ]);
    
    let sin_result = evaluator.eval(&sin_expr, env.clone());
    assert!(sin_result.is_ok(), "sin function should work");
    
    if let Ok(Value::Float(val)) = sin_result {
        assert!((val - 1.0).abs() < 0.001, "sin(pi/2) should be ~1.0");
    }
    
    // Test cos function
    let cos_expr = Value::List(vec![
        Value::Symbol(Symbol("cos".to_string())),
        Value::Float(0.0),
    ]);
    
    let cos_result = evaluator.eval(&cos_expr, env.clone());
    assert!(cos_result.is_ok(), "cos function should work");
    
    if let Ok(Value::Float(val)) = cos_result {
        assert!((val - 1.0).abs() < 0.001, "cos(0) should be 1.0");
    }
}
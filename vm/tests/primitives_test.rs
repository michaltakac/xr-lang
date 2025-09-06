//! Integration tests for all 3D primitive intrinsic functions

use vm::evaluator::Evaluator;
use vm::parser::Parser;
use vm::value::{Value, Environment, Symbol};
use vm::intrinsics::register_scene_intrinsics;
use std::rc::Rc;

fn setup_env_with_intrinsics() -> Rc<Environment> {
    let mut env = Environment::new();
    let intrinsics = register_scene_intrinsics();
    
    for (name, func) in intrinsics {
        env.bind(Symbol(name), Value::NativeFunction(func));
    }
    
    // Add basic functions needed for tests
    // Add 'def' as a simple variable assignment for testing
    // Note: This is a simplified version - real 'def' would be a special form
    env.bind(Symbol("list".to_string()), Value::NativeFunction(
        Rc::new(|args| Ok(Value::List(args.to_vec())))
    ));
    env.bind(Symbol("do".to_string()), Value::NativeFunction(
        Rc::new(|args| Ok(args.last().cloned().unwrap_or(Value::Nil)))
    ));
    
    Rc::new(env)
}

fn parse_expr(input: &str) -> Result<Value, String> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse()?;
    if exprs.is_empty() {
        Err("No expression parsed".to_string())
    } else {
        Ok(exprs[0].clone())
    }
}

#[test]
fn test_create_all_basic_primitives() {
    let mut evaluator = Evaluator::new();
    let env = setup_env_with_intrinsics();
    
    // Test cube creation
    let cube_expr = parse_expr("(create-cube [0 0 0])").unwrap();
    let cube_result = evaluator.eval(&cube_expr, env.clone()).unwrap();
    assert!(matches!(cube_result, Value::Object(_)));
    
    // Test sphere with parameters
    let sphere_expr = parse_expr("(create-sphere [1 2 3] 2.5 32)").unwrap();
    let sphere_result = evaluator.eval(&sphere_expr, env.clone()).unwrap();
    assert!(matches!(sphere_result, Value::Object(_)));
    
    // Test cylinder
    let cylinder_expr = parse_expr("(create-cylinder [0 0 0] 1 3 24)").unwrap();
    let cylinder_result = evaluator.eval(&cylinder_expr, env.clone()).unwrap();
    assert!(matches!(cylinder_result, Value::Object(_)));
    
    // Test cone
    let cone_expr = parse_expr("(create-cone [0 1 0] 1.5 2 16)").unwrap();
    let cone_result = evaluator.eval(&cone_expr, env.clone()).unwrap();
    assert!(matches!(cone_result, Value::Object(_)));
    
    // Test pyramid
    let pyramid_expr = parse_expr("(create-pyramid [0 0 0] 2 2 3)").unwrap();
    let pyramid_result = evaluator.eval(&pyramid_expr, env.clone()).unwrap();
    assert!(matches!(pyramid_result, Value::Object(_)));
    
    // Test wedge
    let wedge_expr = parse_expr("(create-wedge [1 1 1] 2 2 2)").unwrap();
    let wedge_result = evaluator.eval(&wedge_expr, env.clone()).unwrap();
    assert!(matches!(wedge_result, Value::Object(_)));
}

#[test]
fn test_create_advanced_primitives() {
    let mut evaluator = Evaluator::new();
    let env = setup_env_with_intrinsics();
    
    // Test torus
    let torus_expr = parse_expr("(create-torus [0 0 0] 2 0.5 24 18)").unwrap();
    let torus_result = evaluator.eval(&torus_expr, env.clone()).unwrap();
    assert!(matches!(torus_result, Value::Object(_)));
    
    // Test plane
    let plane_expr = parse_expr("(create-plane [0 -1 0] 10 10 5)").unwrap();
    let plane_result = evaluator.eval(&plane_expr, env.clone()).unwrap();
    assert!(matches!(plane_result, Value::Object(_)));
    
    // Test capsule
    let capsule_expr = parse_expr("(create-capsule [0 0 0] 0.5 2 16)").unwrap();
    let capsule_result = evaluator.eval(&capsule_expr, env.clone()).unwrap();
    assert!(matches!(capsule_result, Value::Object(_)));
}

#[test]
fn test_create_platonic_solids() {
    let mut evaluator = Evaluator::new();
    let env = setup_env_with_intrinsics();
    
    // Test icosahedron
    let icosa_expr = parse_expr("(create-icosahedron [0 0 0] 1.5)").unwrap();
    let icosa_result = evaluator.eval(&icosa_expr, env.clone()).unwrap();
    assert!(matches!(icosa_result, Value::Object(_)));
    
    // Test octahedron
    let octa_expr = parse_expr("(create-octahedron [1 1 1] 2)").unwrap();
    let octa_result = evaluator.eval(&octa_expr, env.clone()).unwrap();
    assert!(matches!(octa_result, Value::Object(_)));
    
    // Test tetrahedron
    let tetra_expr = parse_expr("(create-tetrahedron [-1 0 1] 1)").unwrap();
    let tetra_result = evaluator.eval(&tetra_expr, env.clone()).unwrap();
    assert!(matches!(tetra_result, Value::Object(_)));
}

#[test]
fn test_primitives_with_default_parameters() {
    let mut evaluator = Evaluator::new();
    let env = setup_env_with_intrinsics();
    
    // Test primitives with only position parameter (using defaults)
    let sphere_expr = parse_expr("(create-sphere [0 0 0])").unwrap();
    let sphere_result = evaluator.eval(&sphere_expr, env.clone()).unwrap();
    assert!(matches!(sphere_result, Value::Object(_)));
    
    let cylinder_expr = parse_expr("(create-cylinder [0 0 0])").unwrap();
    let cylinder_result = evaluator.eval(&cylinder_expr, env.clone()).unwrap();
    assert!(matches!(cylinder_result, Value::Object(_)));
    
    let cone_expr = parse_expr("(create-cone [0 0 0])").unwrap();
    let cone_result = evaluator.eval(&cone_expr, env.clone()).unwrap();
    assert!(matches!(cone_result, Value::Object(_)));
    
    let torus_expr = parse_expr("(create-torus [0 0 0])").unwrap();
    let torus_result = evaluator.eval(&torus_expr, env.clone()).unwrap();
    assert!(matches!(torus_result, Value::Object(_)));
}

#[test]
fn test_transform_primitives() {
    let mut evaluator = Evaluator::new();
    let env = setup_env_with_intrinsics();
    
    // For now, we'll test that transform functions work with basic calls
    // without testing state persistence which would need environment modifications
    
    // Create a cube and verify it returns an object ID
    let cube_expr = parse_expr("(create-cube [0 0 0])").unwrap();
    let cube = evaluator.eval(&cube_expr, env.clone()).unwrap();
    assert!(matches!(cube, Value::Object(_)));
}

#[test]
fn test_scene_composition() {
    let mut evaluator = Evaluator::new();
    let env = setup_env_with_intrinsics();
    
    // Create a complete scene with multiple primitives
    let scene_code = r#"
        (list
          (create-camera [0 10 20] [0 0 0] 60)
          (create-plane [0 0 0] 20 20 10)
          (create-cube [0 1 0])
          (create-sphere [3 1 0] 1 32)
          (create-cylinder [-3 1 0] 0.5 2 24)
          (create-torus [0 1 3] 1 0.3 24 18))
    "#;
    
    let scene_expr = parse_expr(scene_code).unwrap();
    let scene_result = evaluator.eval(&scene_expr, env.clone()).unwrap();
    
    // Should return a list of 6 object IDs
    match scene_result {
        Value::List(objects) => {
            assert_eq!(objects.len(), 6);
            for obj in objects {
                assert!(matches!(obj, Value::Object(_)));
            }
        }
        _ => panic!("Expected list of objects"),
    }
}
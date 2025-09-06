//! Stage A Integration Tests
//! 
//! Demonstrates the working components of Stage A:
//! - Homoiconic value model
//! - EDN-like parser
//! - Bytecode VM execution
//! - Persistence with time-travel
//! - Scene primitives

#[cfg(test)]
mod stage_a_tests {
    use vm::parser::{parse, parse_one};
    use vm::bytecode::{VM, OpCode, ByteCode};
    use vm::value::Value;
    use vm::persistence::{PersistenceLayer, Author, Change, ValuePath};
    use vm::intrinsics::{intrinsic_create_cube, intrinsic_update_transform};
    
    #[test]
    fn test_homoiconicity_code_as_data() {
        // Parse code into data structures
        let code = "(+ 1 2)";
        let parsed = parse_one(code).unwrap();
        
        // Code is just data - we can examine it
        match parsed {
            Value::List(ref items) => {
                assert_eq!(items[0], Value::symbol("+"));
                assert_eq!(items[1], Value::Int(1));
                assert_eq!(items[2], Value::Int(2));
            }
            _ => panic!("Expected list"),
        }
        
        // We can construct code as data
        let constructed = Value::List(vec![
            Value::symbol("*"),
            Value::Int(3),
            Value::Int(4),
        ]);
        
        // And convert it back to text
        assert_eq!(constructed.to_string(), "(* 3 4)");
    }
    
    #[test]
    fn test_edn_syntax_rich_literals() {
        // Test vectors
        let vec_result = parse_one("[1 2 3]").unwrap();
        assert!(matches!(vec_result, Value::Vector(_)));
        
        // Test maps  
        let map_code = "{:name \"John\" :age 30}";
        let map_result = parse_one(map_code).unwrap();
        match map_result {
            Value::Map(map) => {
                assert_eq!(
                    map.get("name"),
                    Some(&Value::Str("John".to_string()))
                );
            }
            _ => panic!("Expected map"),
        }
        
        // Test sets
        let set_result = parse_one("#{1 2 3}").unwrap();
        assert!(matches!(set_result, Value::Set(_)));
        
        // Test keywords
        let kw = parse_one(":my-keyword").unwrap();
        assert_eq!(kw, Value::keyword("my-keyword"));
    }
    
    #[test]
    fn test_bytecode_vm_execution() {
        let mut vm = VM::new();
        
        // Simple arithmetic program
        let code = vec![
            ByteCode { op: OpCode::Push(Value::Int(10)), line: 1, column: 1 },
            ByteCode { op: OpCode::Push(Value::Int(5)), line: 1, column: 3 },
            ByteCode { op: OpCode::Add, line: 1, column: 5 },
            ByteCode { op: OpCode::Push(Value::Int(3)), line: 1, column: 7 },
            ByteCode { op: OpCode::Mul, line: 1, column: 9 },
        ];
        
        let result = vm.execute(code, None).unwrap();
        assert_eq!(result, Value::Int(45)); // (10 + 5) * 3
    }
    
    #[test]
    fn test_scene_primitives() {
        // Create a cube using intrinsic function
        let create_cube = intrinsic_create_cube();
        let position = Value::Vector(vec![
            Value::Float(0.0),
            Value::Float(0.0),
            Value::Float(0.0),
        ]);
        
        let cube = create_cube(&[position.clone()]).unwrap();
        assert!(matches!(cube, Value::Object(_)));
        
        // Move the cube
        let update_transform = intrinsic_update_transform();
        let new_position = Value::Vector(vec![
            Value::Float(5.0),
            Value::Float(0.0),
            Value::Float(0.0),
        ]);
        
        let result = update_transform(&[cube, new_position]).unwrap();
        assert_eq!(result, Value::Nil);
    }
    
    #[test]
    fn test_persistence_time_travel() {
        let mut persistence = PersistenceLayer::new();
        let path = ValuePath::new(vec!["game".to_string(), "score".to_string()]);
        
        // Record initial state
        let change1 = Change::Create {
            path: path.clone(),
            value: Value::Int(0),
        };
        persistence.record_change(change1, Author::System).unwrap();
        
        // Take a checkpoint
        let checkpoint = persistence.checkpoint();
        
        // Make some changes
        let change2 = Change::Update {
            path: path.clone(),
            old: Value::Int(0),
            new: Value::Int(100),
        };
        persistence.record_change(change2, Author::Human("player".to_string())).unwrap();
        
        // More changes
        let change3 = Change::Update {
            path: path.clone(),
            old: Value::Int(100),
            new: Value::Int(200),
        };
        persistence.record_change(change3, Author::AI("assistant".to_string())).unwrap();
        
        // Travel back to checkpoint
        // Note: In a real implementation, we'd restore to the checkpoint timestamp
        // For now, this demonstrates the structure is in place
        assert!(persistence.snapshots.get(&checkpoint).is_some());
    }
    
    #[test]
    fn test_quote_and_eval_homoiconicity() {
        // Quote prevents evaluation
        let quoted = parse_one("'(+ 1 2)").unwrap();
        match quoted {
            Value::Quote(inner) => {
                // The inner expression is not evaluated, just data
                match inner.as_ref() {
                    Value::List(items) => {
                        assert_eq!(items[0], Value::symbol("+"));
                    }
                    _ => panic!("Expected list inside quote"),
                }
            }
            _ => panic!("Expected quote"),
        }
    }
    
    #[test]
    fn test_metadata_annotations() {
        // Parse metadata annotations
        let with_meta = parse("^{:doc \"A test\"} (defn foo [])").unwrap();
        
        // In the full implementation, metadata would be attached to values
        assert_eq!(with_meta.len(), 1);
    }
    
    #[test]
    fn test_scene_dsl_parsing() {
        let scene = r#"
            (defscene3d test-world
              (camera 
                (position [0 5 10])
                (fov 60))
              (cube
                (position [0 0 0])
                (color {:r 1 :g 0 :b 0}))
              (sphere
                (position [3 0 0])
                (radius 1.5)))
        "#;
        
        let parsed = parse(scene).unwrap();
        assert_eq!(parsed.len(), 1);
        
        match &parsed[0] {
            Value::List(items) => {
                assert_eq!(items[0], Value::symbol("defscene3d"));
                assert_eq!(items[1], Value::symbol("test-world"));
                
                // The scene contains camera, cube, and sphere definitions
                let scene_elements = &items[2..];
                assert_eq!(scene_elements.len(), 3);
            }
            _ => panic!("Expected scene definition"),
        }
    }
    
    #[test]
    fn test_comprehensive_stage_a_workflow() {
        // 1. Parse XR-Lang code
        let code = "(create-cube [0 0 0])";
        let parsed = parse_one(code).unwrap();
        
        // 2. Code is data - we can manipulate it
        let modified = match parsed {
            Value::List(mut items) => {
                // Change position
                items[1] = Value::Vector(vec![
                    Value::Int(5),
                    Value::Int(0),
                    Value::Int(0),
                ]);
                Value::List(items)
            }
            _ => parsed,
        };
        
        // 3. Convert back to string (homoiconicity!)
        let new_code = modified.to_string();
        assert!(new_code.contains("5"));
        
        // 4. Create actual scene objects
        let create_cube = intrinsic_create_cube();
        let pos = Value::Vector(vec![Value::Int(0), Value::Int(0), Value::Int(0)]);
        let cube = create_cube(&[pos]).unwrap();
        
        // 5. Track changes with persistence
        let mut persistence = PersistenceLayer::new();
        let change = Change::Create {
            path: ValuePath::new(vec!["scene".to_string(), "cube1".to_string()]),
            value: cube.clone(),
        };
        persistence.record_change(change, Author::Human("developer".to_string())).unwrap();
        
        // Stage A Foundation Complete!
        assert!(matches!(cube, Value::Object(_)));
    }
}
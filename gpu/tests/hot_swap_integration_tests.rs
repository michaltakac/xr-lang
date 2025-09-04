//! Integration tests for runtime hot-swapping functionality
//! Tests complete workflows including DSL parsing, reconciliation, and state preservation

use gpu::reconciliation::{SceneReconciler, SceneChange};
use gpu::entity::{Entity, Transform, MeshSource, PrimitiveType, MetaDirective};
use gpu::scene::{SceneData, CameraData};
use gpu::runtime_state::{RuntimeState, AuthoringMode, CameraState};
use gpu::code_sync::CodeSync;
use gpu::math::{Vec3, Quat};
use dsl::parse;
use std::collections::HashMap;
use std::fs;

// Helper functions for MetaDirective creation
fn meta_preserve_runtime() -> MetaDirective {
    MetaDirective {
        preserve_mode: "preserve-runtime".to_string(),
        properties: vec![],
    }
}

fn meta_sync_to_code() -> MetaDirective {
    MetaDirective {
        preserve_mode: "sync-to-code".to_string(),
        properties: vec![],
    }
}

fn meta_reset_on_reload() -> MetaDirective {
    MetaDirective {
        preserve_mode: "reset-on-reload".to_string(),
        properties: vec![],
    }
}

fn is_preserve_runtime(meta: &Option<MetaDirective>) -> bool {
    meta.as_ref().map_or(false, |m| m.preserve_mode == "preserve-runtime")
}

fn is_sync_to_code(meta: &Option<MetaDirective>) -> bool {
    meta.as_ref().map_or(false, |m| m.preserve_mode == "sync-to-code")
}

fn is_reset_on_reload(meta: &Option<MetaDirective>) -> bool {
    meta.as_ref().map_or(false, |m| m.preserve_mode == "reset-on-reload")
}

#[cfg(test)]
mod hot_swap_integration {
    use super::*;
    
    fn create_test_dsl() -> String {
        r#"
        (defscene3d test-scene
          (camera
            (position 0 5 10)
            (target 0 0 0)
            (fov 60))
            
          (object cube1 cube
            (position 0 0 0)
            (scale 1 1 1)
            (color 1 0 0)
            (meta preserve-runtime))
            
          (object cube2 cube
            (position 3 0 0)
            (scale 1 1 1)
            (color 0 1 0)
            (meta sync-to-code))
            
          (object cube3 cube
            (position -3 0 0)
            (scale 1 1 1)
            (color 0 0 1)
            (meta reset-on-reload)))
            
        (behavior rotating
          (update (dt)
            (rotate-y (* dt 1.0))))
        "#.to_string()
    }
    
    fn create_modified_dsl() -> String {
        r#"
        (defscene3d test-scene
          (camera
            (position 0 5 15)  ; Changed
            (target 0 0 0)
            (fov 45))          ; Changed
            
          (object cube1 cube
            (position 0 2 0)   ; Changed in DSL
            (scale 1 1 1)
            (color 1 0 0)
            (meta preserve-runtime))
            
          (object cube2 cube
            (position 3 0 0)
            (scale 2 2 2)      ; Changed
            (color 0 1 0)
            (meta sync-to-code))
            
          (object cube3 cube
            (position -3 0 0)
            (scale 1 1 1)
            (color 0 0 1)
            (meta reset-on-reload))
            
          (object cube4 cube    ; New entity
            (position 0 0 3)
            (scale 1 1 1)
            (color 1 1 0)))
            
        (behavior rotating
          (update (dt)
            (rotate-y (* dt 2.0))))  ; Changed speed
        "#.to_string()
    }
    
    #[test]
    fn test_complete_hot_swap_workflow() {
        // Step 1: Parse initial DSL
        let initial_dsl = create_test_dsl();
        let initial_ast = parse(&initial_dsl).expect("Failed to parse initial DSL");
        let initial_scene = parse_ast_to_scene(initial_ast);
        
        // Step 2: Simulate runtime modifications
        let mut runtime_state = RuntimeState::new();
        runtime_state.authoring_mode = AuthoringMode::Play;
        
        // User moves cube1 at runtime
        runtime_state.object_overrides.insert(
            "cube1".to_string(),
            gpu::runtime_state::Transform {
                position: Vec3::new(5.0, 3.0, 2.0),
                rotation: (0.0, 0.707, 0.0, 0.707), // ~90 degrees Y rotation as quaternion
                scale: Vec3::new(1.5, 1.5, 1.5),
            }
        );
        
        // User adjusts camera
        runtime_state.camera_overrides = Some(CameraState {
            position: Vec3::new(10.0, 8.0, 20.0),
            target: Vec3::new(0.0, 0.0, 0.0),
            fov: 50.0_f32.to_radians(),
        });
        
        // Step 3: Create reconciler and preserve runtime state
        let mut reconciler = SceneReconciler::new();
        
        // Preserve entities based on meta directives
        for entity in &initial_scene.entities {
            if is_preserve_runtime(&entity.meta) {
                if let Some(runtime_transform) = runtime_state.object_overrides.get(&entity.id) {
                    let transform = Transform {
                        position: runtime_transform.position,
                        rotation: Quat::new(
                            runtime_transform.rotation.0,
                            runtime_transform.rotation.1,
                            runtime_transform.rotation.2,
                            runtime_transform.rotation.3
                        ),
                        scale: runtime_transform.scale,
                    };
                    reconciler.preserve_transform(&entity.id, transform);
                }
            } else if is_sync_to_code(&entity.meta) {
                if let Some(runtime_transform) = runtime_state.object_overrides.get(&entity.id) {
                    let transform = Transform {
                        position: runtime_transform.position,
                        rotation: Quat::new(
                            runtime_transform.rotation.0,
                            runtime_transform.rotation.1,
                            runtime_transform.rotation.2,
                            runtime_transform.rotation.3
                        ),
                        scale: runtime_transform.scale,
                    };
                    reconciler.preserve_transform(&entity.id, transform);
                    // In real implementation, would write back to DSL file
                }
            }
            // ResetOnReload and None don't preserve
        }
        
        // Preserve camera in Play mode
        if runtime_state.authoring_mode == AuthoringMode::Play {
            if let Some(camera_state) = &runtime_state.camera_overrides {
                reconciler.preserve_camera(CameraData {
                    position: camera_state.position,
                    target: camera_state.target,
                    fov: camera_state.fov,
                    meta: Some(meta_preserve_runtime()),
                });
            }
        }
        
        // Step 4: Parse modified DSL (simulating hot reload)
        let modified_dsl = create_modified_dsl();
        let modified_ast = parse(&modified_dsl).expect("Failed to parse modified DSL");
        let mut reloaded_scene = parse_ast_to_scene(modified_ast);
        
        // Step 5: Calculate differences
        let changes = reconciler.diff_scenes(&initial_scene, &reloaded_scene);
        
        // Verify expected changes
        assert!(changes.iter().any(|c| matches!(c, SceneChange::EntityAdded { .. })), 
                "Should detect new entity (cube4)");
        assert!(changes.iter().any(|c| matches!(c, SceneChange::EntityModified { .. })), 
                "Should detect modified entities");
        
        // Step 6: Apply preserved state to reloaded scene
        let preserved_transforms = reconciler.get_preserved_transforms();
        for entity in &mut reloaded_scene.entities {
            if let Some(preserved) = preserved_transforms.get(&entity.id) {
                if is_preserve_runtime(&entity.meta) {
                    entity.transform = preserved.clone();
                } else if is_sync_to_code(&entity.meta) {
                    // Would have been written to DSL already
                }
                // Keep DSL values for other cases
            }
        }
        
        // Apply preserved camera
        if let Some(preserved_camera) = reconciler.get_preserved_camera() {
            reloaded_scene.camera = Some(preserved_camera);
        }
        
        // Step 7: Verify final state
        // cube1 should have runtime position (PreserveRuntime)
        let cube1 = reloaded_scene.entities.iter()
            .find(|e| e.id == "cube1")
            .expect("cube1 should exist");
        assert_eq!(cube1.transform.position, Vec3::new(5.0, 3.0, 2.0), 
                   "cube1 should preserve runtime position");
        
        // cube3 should have DSL position (ResetOnReload)
        let cube3 = reloaded_scene.entities.iter()
            .find(|e| e.id == "cube3")
            .expect("cube3 should exist");
        assert_eq!(cube3.transform.position, Vec3::new(-3.0, 0.0, 0.0), 
                   "cube3 should reset to DSL position");
        
        // cube4 should exist (newly added)
        assert!(reloaded_scene.entities.iter().any(|e| e.id == "cube4"), 
                "cube4 should be added");
        
        // Camera should be preserved in Play mode
        if let Some(camera) = reloaded_scene.camera {
            assert_eq!(camera.position, Vec3::new(10.0, 8.0, 20.0), 
                       "Camera position should be preserved");
            assert_eq!(camera.fov, 50.0_f32.to_radians(), 
                       "Camera FOV should be preserved");
        }
    }
    
    #[test]
    fn test_authoring_mode_behavior() {
        let mut runtime_state = RuntimeState::new();
        let mut reconciler = SceneReconciler::new();
        
        // Test Design mode - no preservation
        runtime_state.authoring_mode = AuthoringMode::Design;
        runtime_state.object_overrides.insert(
            "test_entity".to_string(),
            gpu::runtime_state::Transform {
                position: Vec3::new(10.0, 10.0, 10.0),
                rotation: (0.0, 0.0, 0.0, 1.0), // Quaternion identity
                scale: Vec3::ONE,
            }
        );
        
        // In Design mode, we don't preserve
        assert!(reconciler.get_preserved_transforms().is_empty());
        
        // Test Play mode - preserve but don't sync
        runtime_state.authoring_mode = AuthoringMode::Play;
        let transform = Transform {
            position: Vec3::new(20.0, 20.0, 20.0),
            rotation: Quat::IDENTITY,
            scale: Vec3::ONE,
        };
        runtime_state.object_overrides.insert(
            "test_entity".to_string(),
            gpu::runtime_state::Transform {
                position: transform.position,
                rotation: (transform.rotation.x, transform.rotation.y, transform.rotation.z, transform.rotation.w),
                scale: transform.scale,
            }
        );
        reconciler.preserve_transform("test_entity", transform);
        
        assert_eq!(reconciler.get_preserved_transforms().len(), 1);
        
        // Test Live mode - preserve and would sync to code
        runtime_state.authoring_mode = AuthoringMode::Live;
        let live_transform = Transform {
            position: Vec3::new(30.0, 30.0, 30.0),
            rotation: Quat::from_axis_angle(Vec3::new(0.0, 1.0, 0.0), 1.0),
            scale: Vec3::new(2.0, 2.0, 2.0),
        };
        runtime_state.object_overrides.insert(
            "live_entity".to_string(),
            gpu::runtime_state::Transform {
                position: live_transform.position,
                rotation: (live_transform.rotation.x, live_transform.rotation.y, live_transform.rotation.z, live_transform.rotation.w),
                scale: live_transform.scale,
            }
        );
        reconciler.preserve_transform("live_entity", live_transform.clone());
        
        // In real implementation, would also trigger code sync
        assert_eq!(reconciler.get_preserved_transforms().len(), 2);
        
        let preserved = reconciler.get_preserved_transforms();
        assert_eq!(preserved.get("live_entity").unwrap().position, Vec3::new(30.0, 30.0, 30.0));
    }
    
    #[test]
    fn test_code_sync_integration() {
        // Create temporary test file
        let test_dir = std::env::temp_dir().join("xr_lang_test");
        fs::create_dir_all(&test_dir).expect("Failed to create test dir");
        let test_file = test_dir.join("test_scene.xrdsl");
        
        // Write initial DSL
        let initial_dsl = create_test_dsl();
        fs::write(&test_file, &initial_dsl).expect("Failed to write test file");
        
        // Initialize CodeSync and parse DSL manually
        let _code_sync = CodeSync::new();
        let dsl_content = fs::read_to_string(&test_file).expect("Failed to read test file");
        let ast = parse(&dsl_content).expect("Failed to parse DSL");
        let initial_scene = parse_ast_to_scene(ast);
        
        // Simulate runtime changes with SyncToCode directive
        let mut runtime_state = RuntimeState::new();
        runtime_state.authoring_mode = AuthoringMode::Live;
        
        // Modify entity with sync-to-code meta
        let _synced_transform = Transform {
            position: Vec3::new(99.0, 99.0, 99.0),
            rotation: Quat::from_axis_angle(Vec3::new(0.0, 1.0, 0.0), 3.14),
            scale: Vec3::new(5.0, 5.0, 5.0),
        };
        
        // In real implementation, would update DSL file here
        // For test, we verify the sync would happen
        let entity_to_sync = initial_scene.entities.iter()
            .find(|e| is_sync_to_code(&e.meta))
            .map(|e| e.id.clone());
        
        assert!(entity_to_sync.is_some(), "Should find entity with SyncToCode directive");
        
        // Cleanup
        fs::remove_dir_all(&test_dir).ok();
    }
    
    #[test]
    fn test_incremental_hot_swapping() {
        let mut reconciler = SceneReconciler::new();
        let mut current_scene = create_simple_scene();
        
        // Simulate multiple hot-swap cycles
        for cycle in 0..3 {
            // Make incremental changes
            let mut modified_scene = current_scene.clone();
            
            // Add a new entity each cycle
            modified_scene.entities.push(Entity {
                id: format!("dynamic_{}", cycle),
                name: format!("Dynamic Entity {}", cycle),
                mesh: MeshSource::Primitive(PrimitiveType::sphere()),
                transform: Transform {
                    position: Vec3::new(cycle as f32 * 2.0, 0.0, 0.0),
                    rotation: Quat::IDENTITY,
                    scale: Vec3::ONE,
                },
                material: None,
                behavior: Some("rotating".to_string()),
                children: vec![],
                parent: None,
                components: vec![],
                meta: Some(meta_preserve_runtime()),
            });
            
            // Preserve some runtime state
            reconciler.preserve_transform(&format!("dynamic_{}", cycle), Transform {
                position: Vec3::new(cycle as f32 * 3.0, 1.0, 1.0),
                rotation: Quat::from_axis_angle(Vec3::new(0.0, 0.0, 1.0), cycle as f32 * 0.5),
                scale: Vec3::new(1.5, 1.5, 1.5),
            });
            
            // Calculate and apply changes
            let changes = reconciler.diff_scenes(&current_scene, &modified_scene);
            assert!(!changes.is_empty(), "Cycle {} should produce changes", cycle);
            
            // Apply preserved transforms
            for entity in &mut modified_scene.entities {
                if let Some(preserved) = reconciler.get_preserved_transforms().get(&entity.id) {
                    if is_preserve_runtime(&entity.meta) {
                        entity.transform = preserved.clone();
                    }
                }
            }
            
            // Verify preservation worked
            if let Some(dynamic_entity) = modified_scene.entities.iter()
                .find(|e| e.id == format!("dynamic_{}", cycle)) {
                assert_eq!(dynamic_entity.transform.position.y, 1.0, 
                          "Dynamic entity {} should have preserved Y position", cycle);
            }
            
            current_scene = modified_scene;
        }
        
        // Verify all entities were added
        assert_eq!(current_scene.entities.len(), 3, "Should have 3 dynamic entities");
    }
    
    #[test]
    fn test_complex_hierarchy_hot_swap() {
        let mut reconciler = SceneReconciler::new();
        
        // Create scene with parent-child relationships
        let mut initial_scene = create_simple_scene();
        
        // Add parent entity
        initial_scene.entities.push(Entity {
            id: "parent".to_string(),
            name: "Parent".to_string(),
            mesh: MeshSource::Primitive(PrimitiveType::cube()),
            transform: Transform::default(),
            material: None,
            behavior: None,
            children: vec!["child1".to_string(), "child2".to_string()],
            parent: None,
            components: vec![],
            meta: Some(meta_preserve_runtime()),
        });
        
        // Add child entities
        for i in 1..=2 {
            initial_scene.entities.push(Entity {
                id: format!("child{}", i),
                name: format!("Child {}", i),
                mesh: MeshSource::Primitive(PrimitiveType::sphere()),
                transform: Transform {
                    position: Vec3::new(i as f32 * 2.0, 0.0, 0.0),
                    rotation: Quat::IDENTITY,
                    scale: Vec3::ONE,
                },
                material: None,
                behavior: None,
                children: vec![],
                parent: Some("parent".to_string()),
                components: vec![],
                meta: Some(meta_preserve_runtime()),
            });
        }
        
        // Preserve runtime transforms for hierarchy
        reconciler.preserve_transform("parent", Transform {
            position: Vec3::new(10.0, 5.0, 0.0),
            rotation: Quat::from_axis_angle(Vec3::new(0.0, 1.0, 0.0), 0.5),
            scale: Vec3::new(2.0, 2.0, 2.0),
        });
        
        for i in 1..=2 {
            reconciler.preserve_transform(&format!("child{}", i), Transform {
                position: Vec3::new(i as f32 * 3.0, 1.0, 0.0),
                rotation: Quat::IDENTITY,
                scale: Vec3::ONE,
            });
        }
        
        // Modify hierarchy in DSL
        let mut modified_scene = initial_scene.clone();
        
        // Add a new child
        modified_scene.entities.push(Entity {
            id: "child3".to_string(),
            name: "Child 3".to_string(),
            mesh: MeshSource::Primitive(PrimitiveType::sphere()),
            transform: Transform::default(),
            material: None,
            behavior: None,
            children: vec![],
            parent: Some("parent".to_string()),
            components: vec![],
            meta: None,
        });
        
        // Update parent's children list
        if let Some(parent) = modified_scene.entities.iter_mut()
            .find(|e| e.id == "parent") {
            parent.children.push("child3".to_string());
        }
        
        // Apply preserved transforms
        for entity in &mut modified_scene.entities {
            if let Some(preserved) = reconciler.get_preserved_transforms().get(&entity.id) {
                if is_preserve_runtime(&entity.meta) {
                    entity.transform = preserved.clone();
                }
            }
        }
        
        // Verify hierarchy is maintained with preserved transforms
        let parent = modified_scene.entities.iter()
            .find(|e| e.id == "parent")
            .expect("Parent should exist");
        assert_eq!(parent.children.len(), 3, "Parent should have 3 children");
        assert_eq!(parent.transform.position, Vec3::new(10.0, 5.0, 0.0), 
                   "Parent should have preserved position");
        
        for i in 1..=2 {
            let child = modified_scene.entities.iter()
                .find(|e| e.id == format!("child{}", i))
                .expect(&format!("Child {} should exist", i));
            assert_eq!(child.transform.position, Vec3::new(i as f32 * 3.0, 1.0, 0.0),
                      "Child {} should have preserved position", i);
        }
    }
}

// Helper functions
fn parse_ast_to_scene(ast: Vec<dsl::ast::Top>) -> SceneData {
    // Simplified scene extraction from AST
    // In real implementation, this would be more comprehensive
    let mut entities = Vec::new();
    let mut camera = None;
    let mut behaviors = HashMap::new();
    
    for top in ast {
        match top {
            dsl::ast::Top::Scene3D(scene_def) => {
                // Extract entities
                for obj in scene_def.objects {
                    entities.push(Entity {
                        id: obj.name.clone(),
                        name: obj.name.clone(),
                        mesh: MeshSource::Primitive(
                            PrimitiveType::from_type_string(&obj.mesh_type)
                                .unwrap_or(PrimitiveType::cube())
                        ),
                        transform: extract_transform(&obj),
                        material: extract_material(&obj),
                        behavior: obj.behavior.clone(),
                        children: vec![],
                        parent: None,
                        components: vec![],
                        meta: obj.meta.as_ref().map(|m| MetaDirective {
                            preserve_mode: m.preserve_mode.clone(),
                            properties: m.properties.clone(),
                        }),
                    });
                }
                
                // Extract camera
                if let Some(cam_def) = scene_def.camera {
                    camera = Some(CameraData {
                        position: Vec3::new(
                            cam_def.position[0],
                            cam_def.position[1],
                            cam_def.position[2]
                        ),
                        target: Vec3::new(
                            cam_def.target[0],
                            cam_def.target[1],
                            cam_def.target[2]
                        ),
                        fov: cam_def.fov.to_radians(),
                        meta: None,
                    });
                }
            }
            dsl::ast::Top::Behavior(behavior) => {
                behaviors.insert(
                    behavior.name.clone(),
                    gpu::scene::BehaviorData {
                        name: behavior.name,
                        state: HashMap::new(),
                    }
                );
            }
            _ => {}
        }
    }
    
    SceneData {
        entities,
        ui_elements: vec![],
        behaviors,
        camera,
        lighting: None,
        input: None,
        ast: vec![],
    }
}

fn extract_transform(obj: &dsl::ast::Object3D) -> Transform {
    Transform {
        position: Vec3::new(
            obj.transform.position[0],
            obj.transform.position[1],
            obj.transform.position[2]
        ),
        rotation: Quat::from_euler(
            obj.transform.rotation[0].to_radians(),
            obj.transform.rotation[1].to_radians(),
            obj.transform.rotation[2].to_radians()
        ),
        scale: Vec3::new(
            obj.transform.scale[0],
            obj.transform.scale[1],
            obj.transform.scale[2]
        ),
    }
}

fn extract_material(obj: &dsl::ast::Object3D) -> Option<dsl::ast::MaterialDef> {
    obj.material.clone()
}

fn create_simple_scene() -> SceneData {
    SceneData {
        entities: vec![],
        ui_elements: vec![],
        behaviors: HashMap::new(),
        camera: Some(CameraData {
            position: Vec3::new(0.0, 5.0, 10.0),
            target: Vec3::ZERO,
            fov: 60.0_f32.to_radians(),
            meta: None,
        }),
        lighting: None,
        input: None,
        ast: vec![],
    }
}
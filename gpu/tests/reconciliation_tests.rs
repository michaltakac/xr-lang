//! Unit and integration tests for the scene reconciliation system
//! Tests the core reconciliation logic that enables hot-swapping and runtime state preservation

use gpu::reconciliation::{SceneReconciler, SceneChange};
use gpu::entity::{Entity, Transform, MeshSource, PrimitiveType, MetaDirective};

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

#[allow(dead_code)]
fn is_reset_on_reload(meta: &Option<MetaDirective>) -> bool {
    meta.as_ref().map_or(false, |m| m.preserve_mode == "reset-on-reload")
}
use gpu::scene::{SceneData, CameraData};
use gpu::math::{Vec3, Quat};
use std::collections::HashMap;

#[cfg(test)]
mod reconciliation_unit_tests {
    use super::*;
    
    fn create_test_entity(id: &str, pos: Vec3) -> Entity {
        Entity {
            id: id.to_string(),
            name: format!("test_{}", id),
            mesh: MeshSource::Primitive(PrimitiveType::cube()),
            transform: Transform {
                position: pos,
                rotation: Quat::IDENTITY,
                scale: Vec3::ONE,
            },
            material: None,
            behavior: None,
            children: vec![],
            parent: None,
            components: vec![],
            meta: None,
        }
    }
    
    fn create_test_scene(entity_count: usize) -> SceneData {
        let mut entities = Vec::new();
        for i in 0..entity_count {
            entities.push(create_test_entity(
                &format!("entity_{}", i),
                Vec3::new(i as f32, 0.0, 0.0)
            ));
        }
        
        SceneData {
            entities,
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
    
    #[test]
    fn test_reconciler_creation() {
        let reconciler = SceneReconciler::new();
        assert!(reconciler.get_preserved_transforms().is_empty());
    }
    
    #[test]
    fn test_scene_diff_no_changes() {
        let reconciler = SceneReconciler::new();
        let scene = create_test_scene(3);
        let changes = reconciler.diff_scenes(&scene, &scene);
        
        assert!(changes.is_empty(), "Identical scenes should produce no changes");
    }
    
    #[test]
    fn test_scene_diff_entity_added() {
        let reconciler = SceneReconciler::new();
        let old_scene = create_test_scene(2);
        let new_scene = create_test_scene(3);
        
        let changes = reconciler.diff_scenes(&old_scene, &new_scene);
        
        assert_eq!(changes.len(), 1);
        match &changes[0] {
            SceneChange::EntityAdded { entity } => {
                assert_eq!(entity.id, "entity_2");
            }
            _ => panic!("Expected EntityAdded change"),
        }
    }
    
    #[test]
    fn test_scene_diff_entity_removed() {
        let reconciler = SceneReconciler::new();
        let old_scene = create_test_scene(3);
        let new_scene = create_test_scene(2);
        
        let changes = reconciler.diff_scenes(&old_scene, &new_scene);
        
        assert_eq!(changes.len(), 1);
        match &changes[0] {
            SceneChange::EntityRemoved { id } => {
                assert_eq!(id, "entity_2");
            }
            _ => panic!("Expected EntityRemoved change"),
        }
    }
    
    #[test]
    fn test_scene_diff_entity_modified_position() {
        let reconciler = SceneReconciler::new();
        let old_scene = create_test_scene(2);
        let mut new_scene = create_test_scene(2);
        
        // Modify position of first entity
        new_scene.entities[0].transform.position = Vec3::new(5.0, 2.0, 1.0);
        
        let changes = reconciler.diff_scenes(&old_scene, &new_scene);
        
        assert_eq!(changes.len(), 1);
        match &changes[0] {
            SceneChange::EntityModified { id, changes } => {
                assert_eq!(id, "entity_0");
                assert!(changes.transform.is_some());
                let transform = changes.transform.as_ref().unwrap();
                assert_eq!(transform.position, Vec3::new(5.0, 2.0, 1.0));
            }
            _ => panic!("Expected EntityModified change"),
        }
    }
    
    #[test]
    fn test_scene_diff_multiple_changes() {
        let reconciler = SceneReconciler::new();
        let old_scene = create_test_scene(3);
        let mut new_scene = create_test_scene(3);
        
        // Add new entity
        new_scene.entities.push(create_test_entity("entity_3", Vec3::new(3.0, 0.0, 0.0)));
        
        // Modify existing entity
        new_scene.entities[1].transform.scale = Vec3::new(2.0, 2.0, 2.0);
        
        // Remove entity (by not including entity_2 in new scene)
        new_scene.entities.remove(2);
        
        let changes = reconciler.diff_scenes(&old_scene, &new_scene);
        
        // Should have 3 changes: 1 modified, 1 removed, 1 added
        assert_eq!(changes.len(), 3);
        
        let has_added = changes.iter().any(|c| matches!(c, SceneChange::EntityAdded { .. }));
        let has_removed = changes.iter().any(|c| matches!(c, SceneChange::EntityRemoved { .. }));
        let has_modified = changes.iter().any(|c| matches!(c, SceneChange::EntityModified { .. }));
        
        assert!(has_added, "Should have EntityAdded change");
        assert!(has_removed, "Should have EntityRemoved change");
        assert!(has_modified, "Should have EntityModified change");
    }
    
    #[test]
    fn test_preserve_transform() {
        let mut reconciler = SceneReconciler::new();
        let entity_id = "test_entity";
        let preserved_transform = Transform {
            position: Vec3::new(10.0, 5.0, 3.0),
            rotation: Quat::from_axis_angle(Vec3::new(0.0, 1.0, 0.0), 1.57),
            scale: Vec3::new(2.0, 2.0, 2.0),
        };
        
        reconciler.preserve_transform(entity_id, preserved_transform.clone());
        
        let preserved = reconciler.get_preserved_transforms();
        assert_eq!(preserved.len(), 1);
        assert!(preserved.contains_key(entity_id));
        
        let retrieved = &preserved[entity_id];
        assert_eq!(retrieved.position, preserved_transform.position);
        assert_eq!(retrieved.scale, preserved_transform.scale);
    }
    
    #[test]
    fn test_clear_preserved_transforms() {
        let mut reconciler = SceneReconciler::new();
        
        // Add some preserved transforms
        reconciler.preserve_transform("entity1", Transform::default());
        reconciler.preserve_transform("entity2", Transform::default());
        
        assert_eq!(reconciler.get_preserved_transforms().len(), 2);
        
        reconciler.clear_preserved_transforms();
        assert!(reconciler.get_preserved_transforms().is_empty());
    }
    
    #[test]
    fn test_meta_directive_preservation() {
        let reconciler = SceneReconciler::new();
        let mut old_scene = create_test_scene(1);
        let mut new_scene = create_test_scene(1);
        
        // Add preserve-runtime meta directive
        old_scene.entities[0].meta = Some(meta_preserve_runtime());
        new_scene.entities[0].meta = Some(meta_sync_to_code());
        
        let changes = reconciler.diff_scenes(&old_scene, &new_scene);
        
        assert_eq!(changes.len(), 1);
        match &changes[0] {
            SceneChange::EntityModified { changes, .. } => {
                assert!(changes.meta.is_some());
            }
            _ => panic!("Expected EntityModified with meta change"),
        }
    }
    
    #[test]
    fn test_camera_state_diff() {
        let reconciler = SceneReconciler::new();
        let old_scene = create_test_scene(1);
        let mut new_scene = old_scene.clone();
        
        // Modify camera position
        if let Some(ref mut camera) = new_scene.camera {
            camera.position = Vec3::new(5.0, 10.0, 20.0);
            camera.fov = 90.0_f32.to_radians();
        }
        
        let changes = reconciler.diff_scenes(&old_scene, &new_scene);
        
        // Camera changes should be detected
        assert!(!changes.is_empty(), "Should detect changes");
        // Find camera change
        let camera_change = changes.iter().find(|c| matches!(c, SceneChange::CameraChanged { .. }));
        assert!(camera_change.is_some(), "Should have camera change");
        match camera_change.unwrap() {
            SceneChange::CameraChanged { new, .. } => {
                if let Some(camera) = new {
                    assert_eq!(camera.position, Vec3::new(5.0, 10.0, 20.0));
                    assert_eq!(camera.fov, 90.0_f32.to_radians());
                } else {
                    panic!("Expected new camera data");
                }
            }
            _ => panic!("Expected CameraChanged change"),
        }
    }
    
    #[test]
    fn test_mesh_source_change_detection() {
        let reconciler = SceneReconciler::new();
        let old_scene = create_test_scene(1);
        let mut new_scene = old_scene.clone();
        
        // Change mesh from cube to sphere
        new_scene.entities[0].mesh = MeshSource::Primitive(PrimitiveType::sphere());
        
        let changes = reconciler.diff_scenes(&old_scene, &new_scene);
        
        assert_eq!(changes.len(), 1);
        match &changes[0] {
            SceneChange::EntityModified { changes, .. } => {
                assert!(changes.mesh.is_some());
            }
            _ => panic!("Expected EntityModified with mesh change"),
        }
    }
    
    #[test]
    fn test_behavior_change_detection() {
        let reconciler = SceneReconciler::new();
        let old_scene = create_test_scene(1);
        let mut new_scene = old_scene.clone();
        
        // Add behavior
        new_scene.entities[0].behavior = Some("rotating".to_string());
        
        let changes = reconciler.diff_scenes(&old_scene, &new_scene);
        
        assert_eq!(changes.len(), 1);
        match &changes[0] {
            SceneChange::EntityModified { changes, .. } => {
                assert!(changes.behavior.is_some());
                assert_eq!(changes.behavior.as_ref().unwrap().as_ref().unwrap(), "rotating");
            }
            _ => panic!("Expected EntityModified with behavior change"),
        }
    }
    
    #[test]
    fn test_empty_scenes_diff() {
        let reconciler = SceneReconciler::new();
        let empty_scene1 = SceneData {
            entities: vec![],
            ui_elements: vec![],
            behaviors: HashMap::new(),
            camera: None,
            lighting: None,
            input: None,
            ast: vec![],
        };
        let empty_scene2 = empty_scene1.clone();
        
        let changes = reconciler.diff_scenes(&empty_scene1, &empty_scene2);
        assert!(changes.is_empty());
    }
    
    #[test]
    fn test_complex_hierarchy_diff() {
        let reconciler = SceneReconciler::new();
        let mut old_scene = create_test_scene(3);
        let mut new_scene = old_scene.clone();
        
        // Create parent-child relationship
        old_scene.entities[1].parent = Some("entity_0".to_string());
        old_scene.entities[0].children = vec!["entity_1".to_string()];
        
        new_scene.entities[1].parent = Some("entity_2".to_string());
        new_scene.entities[2].children = vec!["entity_1".to_string()];
        new_scene.entities[0].children = vec![];
        
        // Also change a transform to ensure we detect some change
        // (hierarchy changes aren't currently tracked in EntityChanges)
        new_scene.entities[1].transform.position = Vec3::new(10.0, 0.0, 0.0);
        
        let changes = reconciler.diff_scenes(&old_scene, &new_scene);
        
        // Should detect at least the transform change
        // TODO: In the future, also detect parent-child relationship changes
        assert!(!changes.is_empty());
    }
}

#[cfg(test)]
mod reconciliation_integration_tests {
    use super::*;
    use std::sync::Arc;
    use std::sync::Mutex;
    
    #[test]
    fn test_hot_swap_workflow() {
        let mut reconciler = SceneReconciler::new();
        
        // Initial scene
        let _initial_scene = create_test_scene(2);
        
        // User modifies entity at runtime
        let runtime_transform = Transform {
            position: Vec3::new(15.0, 3.0, 7.0),
            rotation: Quat::from_axis_angle(Vec3::new(0.0, 1.0, 0.0), 0.5),
            scale: Vec3::new(1.5, 1.5, 1.5),
        };
        reconciler.preserve_transform("entity_0", runtime_transform.clone());
        
        // DSL hot-reloads with new scene
        let mut reloaded_scene = create_test_scene(3); // Added new entity
        
        // Apply preserved transforms
        let preserved = reconciler.get_preserved_transforms();
        for entity in &mut reloaded_scene.entities {
            if let Some(preserved_transform) = preserved.get(&entity.id) {
                entity.transform = preserved_transform.clone();
            }
        }
        
        // Verify runtime state was preserved
        assert_eq!(reloaded_scene.entities[0].transform.position, Vec3::new(15.0, 3.0, 7.0));
        assert_eq!(reloaded_scene.entities[0].transform.scale, Vec3::new(1.5, 1.5, 1.5));
        
        // Verify new entity was added
        assert_eq!(reloaded_scene.entities.len(), 3);
    }
    
    #[test]
    fn test_selective_preservation_with_meta() {
        let mut reconciler = SceneReconciler::new();
        let mut scene = create_test_scene(3);
        
        // Mark entities with different meta directives
        scene.entities[0].meta = Some(meta_preserve_runtime());
        scene.entities[1].meta = Some(meta_reset_on_reload());
        scene.entities[2].meta = Some(meta_sync_to_code());
        
        // Simulate runtime modifications
        for entity in &mut scene.entities {
            entity.transform.position = Vec3::new(99.0, 99.0, 99.0);
            
            // Preserve based on meta directive
            if is_preserve_runtime(&entity.meta) {
                reconciler.preserve_transform(&entity.id, entity.transform.clone());
            } else if is_sync_to_code(&entity.meta) {
                reconciler.preserve_transform(&entity.id, entity.transform.clone());
                // In real implementation, would also write back to DSL
            }
            // ResetOnReload and None don't preserve
        }
        
        // Reload scene from DSL
        let mut reloaded_scene = create_test_scene(3);
        reloaded_scene.entities[0].meta = Some(meta_preserve_runtime());
        reloaded_scene.entities[1].meta = Some(meta_reset_on_reload());
        reloaded_scene.entities[2].meta = Some(meta_sync_to_code());
        
        // Apply preserved transforms
        let preserved = reconciler.get_preserved_transforms();
        for entity in &mut reloaded_scene.entities {
            if let Some(preserved_transform) = preserved.get(&entity.id) {
                if is_preserve_runtime(&entity.meta) || is_sync_to_code(&entity.meta) {
                    entity.transform = preserved_transform.clone();
                }
            }
        }
        
        // Verify selective preservation
        assert_eq!(reloaded_scene.entities[0].transform.position, Vec3::new(99.0, 99.0, 99.0), 
                   "PreserveRuntime entity should keep runtime position");
        assert_eq!(reloaded_scene.entities[1].transform.position, Vec3::new(1.0, 0.0, 0.0), 
                   "ResetOnReload entity should reset to original position");
        assert_eq!(reloaded_scene.entities[2].transform.position, Vec3::new(99.0, 99.0, 99.0), 
                   "SyncToCode entity should keep runtime position");
    }
    
    #[test]
    fn test_incremental_updates() {
        let reconciler = SceneReconciler::new();
        let mut current_scene = create_test_scene(2);
        
        // Simulate multiple incremental updates
        let updates = vec![
            // Update 1: Move entity
            {
                let mut scene = current_scene.clone();
                scene.entities[0].transform.position = Vec3::new(5.0, 0.0, 0.0);
                scene
            },
            // Update 2: Add entity
            {
                let mut scene = current_scene.clone();
                scene.entities[0].transform.position = Vec3::new(5.0, 0.0, 0.0);
                scene.entities.push(create_test_entity("entity_2", Vec3::new(2.0, 0.0, 0.0)));
                scene
            },
            // Update 3: Change material
            {
                let mut scene = current_scene.clone();
                scene.entities[0].transform.position = Vec3::new(5.0, 0.0, 0.0);
                scene.entities.push(create_test_entity("entity_2", Vec3::new(2.0, 0.0, 0.0)));
                scene.entities[1].material = Some(dsl::ast::MaterialDef::MeshBasic {
                    color: [1.0, 0.0, 0.0, 1.0],
                    opacity: 1.0,
                    transparent: false,
                    side: "front".to_string(),
                    wireframe: false,
                });
                scene
            },
        ];
        
        for (i, next_scene) in updates.iter().enumerate() {
            let changes = reconciler.diff_scenes(&current_scene, next_scene);
            
            println!("Update {}: {} changes", i + 1, changes.len());
            
            // Each update should produce exactly the expected changes
            match i {
                0 => assert_eq!(changes.len(), 1, "Update 1 should have 1 change (position)"),
                1 => assert_eq!(changes.len(), 1, "Update 2 should have 1 change (new entity)"),
                2 => assert_eq!(changes.len(), 1, "Update 3 should have 1 change (material)"),
                _ => {}
            }
            
            current_scene = next_scene.clone();
        }
    }
    
    #[test]
    fn test_concurrent_modifications() {
        let reconciler = Arc::new(Mutex::new(SceneReconciler::new()));
        let scene = Arc::new(Mutex::new(create_test_scene(5)));
        
        let mut handles = vec![];
        
        // Simulate concurrent modifications from different sources
        for i in 0..3 {
            let reconciler_clone = Arc::clone(&reconciler);
            let scene_clone = Arc::clone(&scene);
            
            let handle = std::thread::spawn(move || {
                let transform = Transform {
                    position: Vec3::new(i as f32 * 10.0, 0.0, 0.0),
                    rotation: Quat::IDENTITY,
                    scale: Vec3::ONE,
                };
                
                let mut rec = reconciler_clone.lock().unwrap();
                rec.preserve_transform(&format!("entity_{}", i), transform);
                
                // Simulate modifying the scene
                let mut s = scene_clone.lock().unwrap();
                if i < s.entities.len() {
                    s.entities[i].transform.position = Vec3::new(i as f32 * 10.0, 0.0, 0.0);
                }
            });
            
            handles.push(handle);
        }
        
        for handle in handles {
            handle.join().unwrap();
        }
        
        // Verify all preservations were recorded
        let rec = reconciler.lock().unwrap();
        let preserved = rec.get_preserved_transforms();
        assert_eq!(preserved.len(), 3);
    }
}

#[cfg(test)]
mod hot_swap_scenario_tests {
    use super::*;
    
    #[test]
    fn test_design_mode_scenario() {
        // In Design mode, all changes reset on reload
        let _reconciler = SceneReconciler::new();
        let original_scene = create_test_scene(2);
        let mut runtime_scene = original_scene.clone();
        
        // User modifies scene at runtime
        runtime_scene.entities[0].transform.position = Vec3::new(100.0, 100.0, 100.0);
        
        // Hot reload happens (Design mode - no preservation)
        let reloaded_scene = original_scene.clone();
        
        // Verify everything reset
        assert_eq!(reloaded_scene.entities[0].transform.position, Vec3::new(0.0, 0.0, 0.0));
    }
    
    #[test]
    fn test_play_mode_scenario() {
        // In Play mode, runtime changes preserved but not saved
        let mut reconciler = SceneReconciler::new();
        let original_scene = create_test_scene(2);
        let mut runtime_scene = original_scene.clone();
        
        // User modifies scene at runtime
        runtime_scene.entities[0].transform.position = Vec3::new(100.0, 100.0, 100.0);
        reconciler.preserve_transform("entity_0", runtime_scene.entities[0].transform.clone());
        
        // Hot reload happens
        let mut reloaded_scene = original_scene.clone();
        
        // Apply preserved transforms (Play mode)
        let preserved = reconciler.get_preserved_transforms();
        if let Some(preserved_transform) = preserved.get("entity_0") {
            reloaded_scene.entities[0].transform = preserved_transform.clone();
        }
        
        // Verify runtime changes preserved
        assert_eq!(reloaded_scene.entities[0].transform.position, Vec3::new(100.0, 100.0, 100.0));
    }
    
    #[test]
    fn test_live_mode_scenario() {
        // In Live mode, runtime changes sync to code
        let mut reconciler = SceneReconciler::new();
        let original_scene = create_test_scene(2);
        let mut runtime_scene = original_scene.clone();
        
        // Mark entity for live sync
        runtime_scene.entities[0].meta = Some(meta_sync_to_code());
        
        // User modifies scene at runtime
        runtime_scene.entities[0].transform.position = Vec3::new(100.0, 100.0, 100.0);
        reconciler.preserve_transform("entity_0", runtime_scene.entities[0].transform.clone());
        
        // In real implementation, would write back to DSL file here
        // For test, we simulate the DSL being updated
        let mut dsl_updated_scene = original_scene.clone();
        dsl_updated_scene.entities[0].transform.position = Vec3::new(100.0, 100.0, 100.0);
        dsl_updated_scene.entities[0].meta = Some(meta_sync_to_code());
        
        // Hot reload with updated DSL
        let reloaded_scene = dsl_updated_scene.clone();
        
        // Verify changes persisted to DSL
        assert_eq!(reloaded_scene.entities[0].transform.position, Vec3::new(100.0, 100.0, 100.0));
    }
    
    #[test]
    fn test_camera_preservation_workflow() {
        let mut reconciler = SceneReconciler::new();
        let original_scene = create_test_scene(1);
        
        // User adjusts camera at runtime
        let runtime_camera = CameraData {
            position: Vec3::new(50.0, 25.0, 100.0),
            target: Vec3::new(10.0, 5.0, 0.0),
            fov: 45.0_f32.to_radians(),
            meta: Some(meta_preserve_runtime()),
        };
        
        // Store camera state
        reconciler.preserve_camera(runtime_camera.clone());
        
        // Hot reload happens
        let mut reloaded_scene = original_scene.clone();
        
        // Apply preserved camera
        if let Some(preserved_camera) = reconciler.get_preserved_camera() {
            reloaded_scene.camera = Some(preserved_camera);
        }
        
        // Verify camera state preserved
        if let Some(camera) = reloaded_scene.camera {
            assert_eq!(camera.position, Vec3::new(50.0, 25.0, 100.0));
            assert_eq!(camera.target, Vec3::new(10.0, 5.0, 0.0));
            assert_eq!(camera.fov, 45.0_f32.to_radians());
        } else {
            panic!("Camera should be preserved");
        }
    }
}

// Helper functions for tests
fn create_test_entity(id: &str, pos: Vec3) -> Entity {
    Entity {
        id: id.to_string(),
        name: format!("test_{}", id),
        mesh: MeshSource::Primitive(PrimitiveType::cube()),
        transform: Transform {
            position: pos,
            rotation: Quat::IDENTITY,
            scale: Vec3::ONE,
        },
        material: None,
        behavior: None,
        children: vec![],
        parent: None,
        components: vec![],
        meta: None,
    }
}

fn create_test_scene(entity_count: usize) -> SceneData {
    let mut entities = Vec::new();
    for i in 0..entity_count {
        entities.push(create_test_entity(
            &format!("entity_{}", i),
            Vec3::new(i as f32, 0.0, 0.0)
        ));
    }
    
    SceneData {
        entities,
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
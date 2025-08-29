//! Scene reconciliation system inspired by React Fiber
//! Efficiently diffs and updates only changed parts of the scene

use crate::scene::SceneData;
use crate::entity::{Entity, MeshSource, Transform, Material, MetaDirective, ModelSource};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

#[derive(Debug, Clone)]
pub enum SceneChange {
    EntityAdded { entity: Entity },
    EntityRemoved { id: String },
    EntityModified { id: String, changes: EntityChanges },
    BehaviorAdded { name: String, data: crate::BehaviorData },
    BehaviorRemoved { name: String },
    BehaviorModified { name: String, state_changes: HashMap<String, f32> },
    CameraChanged { old: Option<crate::CameraData>, new: Option<crate::CameraData> },
    LightingChanged { old: Option<crate::LightingData>, new: Option<crate::LightingData> },
    UIElementAdded { element: crate::UIElementData },
    UIElementRemoved { name: String },
    UIElementModified { name: String, changes: UIElementChanges },
}

#[derive(Debug, Clone)]
pub struct EntityChanges {
    pub transform: Option<Transform>,
    pub material: Option<Material>,
    pub behavior: Option<Option<String>>,
    pub mesh: Option<MeshSource>,
    pub meta: Option<Option<MetaDirective>>,
}

#[derive(Debug, Clone)]
pub struct UIElementChanges {
    pub position: Option<crate::Vec3>,
    pub size: Option<crate::Vec2>,
    pub text: Option<Option<String>>,
    pub color: Option<crate::Vec4>,
}

pub struct SceneReconciler {
    current_scene: Option<SceneData>,
    pending_changes: Vec<SceneChange>,
    model_cache: HashMap<String, Arc<ModelSource>>,
}

impl SceneReconciler {
    pub fn new() -> Self {
        Self {
            current_scene: None,
            pending_changes: Vec::new(),
            model_cache: HashMap::new(),
        }
    }

    pub fn diff_scenes(&mut self, old_scene: &SceneData, new_scene: &SceneData) -> Vec<SceneChange> {
        let mut changes = Vec::new();

        // Diff entities
        let old_entities: HashMap<String, &Entity> = old_scene.entities
            .iter()
            .map(|e| (e.id.clone(), e))
            .collect();
        
        let new_entities: HashMap<String, &Entity> = new_scene.entities
            .iter()
            .map(|e| (e.id.clone(), e))
            .collect();

        let old_ids: HashSet<String> = old_entities.keys().cloned().collect();
        let new_ids: HashSet<String> = new_entities.keys().cloned().collect();

        // Find removed entities
        for id in old_ids.difference(&new_ids) {
            changes.push(SceneChange::EntityRemoved { id: id.clone() });
        }

        // Find added entities
        for id in new_ids.difference(&old_ids) {
            if let Some(entity) = new_entities.get(id) {
                changes.push(SceneChange::EntityAdded { 
                    entity: (*entity).clone() 
                });
            }
        }

        // Find modified entities
        for id in old_ids.intersection(&new_ids) {
            if let (Some(old_entity), Some(new_entity)) = (old_entities.get(id), new_entities.get(id)) {
                if let Some(entity_changes) = self.diff_entities(old_entity, new_entity) {
                    changes.push(SceneChange::EntityModified {
                        id: id.clone(),
                        changes: entity_changes,
                    });
                }
            }
        }

        // Diff behaviors
        let old_behaviors = &old_scene.behaviors;
        let new_behaviors = &new_scene.behaviors;

        for (name, new_behavior) in new_behaviors {
            match old_behaviors.get(name) {
                None => {
                    changes.push(SceneChange::BehaviorAdded {
                        name: name.clone(),
                        data: new_behavior.clone(),
                    });
                }
                Some(old_behavior) => {
                    let state_changes = self.diff_behavior_states(&old_behavior.state, &new_behavior.state);
                    if !state_changes.is_empty() {
                        changes.push(SceneChange::BehaviorModified {
                            name: name.clone(),
                            state_changes,
                        });
                    }
                }
            }
        }

        for name in old_behaviors.keys() {
            if !new_behaviors.contains_key(name) {
                changes.push(SceneChange::BehaviorRemoved { name: name.clone() });
            }
        }

        // Diff camera
        if !self.cameras_equal(&old_scene.camera, &new_scene.camera) {
            changes.push(SceneChange::CameraChanged {
                old: old_scene.camera.clone(),
                new: new_scene.camera.clone(),
            });
        }

        // Diff lighting
        if !self.lighting_equal(&old_scene.lighting, &new_scene.lighting) {
            changes.push(SceneChange::LightingChanged {
                old: old_scene.lighting.clone(),
                new: new_scene.lighting.clone(),
            });
        }

        // Diff UI elements
        let old_ui: HashMap<String, &crate::UIElementData> = old_scene.ui_elements
            .iter()
            .map(|e| (e.name.clone(), e))
            .collect();
        
        let new_ui: HashMap<String, &crate::UIElementData> = new_scene.ui_elements
            .iter()
            .map(|e| (e.name.clone(), e))
            .collect();

        let old_ui_names: HashSet<String> = old_ui.keys().cloned().collect();
        let new_ui_names: HashSet<String> = new_ui.keys().cloned().collect();

        for name in old_ui_names.difference(&new_ui_names) {
            changes.push(SceneChange::UIElementRemoved { name: name.clone() });
        }

        for name in new_ui_names.difference(&old_ui_names) {
            if let Some(element) = new_ui.get(name) {
                changes.push(SceneChange::UIElementAdded { 
                    element: (*element).clone() 
                });
            }
        }

        for name in old_ui_names.intersection(&new_ui_names) {
            if let (Some(old_elem), Some(new_elem)) = (old_ui.get(name), new_ui.get(name)) {
                if let Some(ui_changes) = self.diff_ui_elements(old_elem, new_elem) {
                    changes.push(SceneChange::UIElementModified {
                        name: name.clone(),
                        changes: ui_changes,
                    });
                }
            }
        }

        changes
    }

    fn diff_entities(&self, old: &Entity, new: &Entity) -> Option<EntityChanges> {
        let mut changes = EntityChanges {
            transform: None,
            material: None,
            behavior: None,
            mesh: None,
            meta: None,
        };
        
        let mut has_changes = false;

        // Check if mesh source changed (important for model caching)
        if !self.mesh_sources_equal(&old.mesh, &new.mesh) {
            changes.mesh = Some(new.mesh.clone());
            has_changes = true;
        }

        // Check transform changes
        if old.transform.position != new.transform.position ||
           old.transform.rotation != new.transform.rotation ||
           old.transform.scale != new.transform.scale {
            changes.transform = Some(new.transform.clone());
            has_changes = true;
        }

        // Check material changes
        if old.material.color != new.material.color {
            changes.material = Some(new.material.clone());
            has_changes = true;
        }

        // Check behavior changes
        if old.behavior != new.behavior {
            changes.behavior = Some(new.behavior.clone());
            has_changes = true;
        }

        // Check meta directive changes (for preservation)
        if !self.meta_directives_equal(&old.meta, &new.meta) {
            changes.meta = Some(new.meta.clone());
            has_changes = true;
        }

        if has_changes {
            Some(changes)
        } else {
            None
        }
    }

    fn diff_ui_elements(&self, old: &crate::UIElementData, new: &crate::UIElementData) -> Option<UIElementChanges> {
        let mut changes = UIElementChanges {
            position: None,
            size: None,
            text: None,
            color: None,
        };
        
        let mut has_changes = false;

        if old.position != new.position {
            changes.position = Some(new.position);
            has_changes = true;
        }

        if old.size != new.size {
            changes.size = Some(new.size);
            has_changes = true;
        }

        if old.text != new.text {
            changes.text = Some(new.text.clone());
            has_changes = true;
        }

        if old.color != new.color {
            changes.color = Some(new.color);
            has_changes = true;
        }

        if has_changes {
            Some(changes)
        } else {
            None
        }
    }

    fn diff_behavior_states(&self, old: &HashMap<String, f32>, new: &HashMap<String, f32>) -> HashMap<String, f32> {
        let mut changes = HashMap::new();
        
        for (key, new_val) in new {
            if old.get(key) != Some(new_val) {
                changes.insert(key.clone(), *new_val);
            }
        }
        
        changes
    }

    fn mesh_sources_equal(&self, a: &MeshSource, b: &MeshSource) -> bool {
        match (a, b) {
            (MeshSource::Primitive(p1), MeshSource::Primitive(p2)) => {
                // Compare primitive types - they don't have a direct equality, so compare variants
                std::mem::discriminant(p1) == std::mem::discriminant(p2)
            }
            (MeshSource::Model(m1), MeshSource::Model(m2)) => {
                // Compare model sources - check if both point to the same file
                match (m1, m2) {
                    (ModelSource::GLTF { path: p1 }, ModelSource::GLTF { path: p2 }) => p1 == p2,
                    (ModelSource::GLB { path: p1 }, ModelSource::GLB { path: p2 }) => p1 == p2,
                    (ModelSource::OBJ { path: p1 }, ModelSource::OBJ { path: p2 }) => p1 == p2,
                    (ModelSource::STL { path: p1 }, ModelSource::STL { path: p2 }) => p1 == p2,
                    (ModelSource::PLY { path: p1 }, ModelSource::PLY { path: p2 }) => p1 == p2,
                    (ModelSource::FBX { path: p1 }, ModelSource::FBX { path: p2 }) => p1 == p2,
                    (ModelSource::USD { path: p1 }, ModelSource::USD { path: p2 }) => p1 == p2,
                    (ModelSource::USDC { path: p1 }, ModelSource::USDC { path: p2 }) => p1 == p2,
                    _ => false
                }
            }
            _ => false
        }
    }

    fn cameras_equal(&self, a: &Option<crate::CameraData>, b: &Option<crate::CameraData>) -> bool {
        match (a, b) {
            (None, None) => true,
            (Some(cam_a), Some(cam_b)) => {
                // Check if camera has preserve directive
                if let Some(meta) = &cam_b.meta {
                    if meta.preserve_mode == "always" || meta.preserve_mode == "on_change" {
                        // Don't update camera if it's marked for preservation
                        return true;
                    }
                }
                
                cam_a.position == cam_b.position &&
                cam_a.target == cam_b.target &&
                (cam_a.fov - cam_b.fov).abs() < 0.001
            }
            _ => false
        }
    }

    fn lighting_equal(&self, a: &Option<crate::LightingData>, b: &Option<crate::LightingData>) -> bool {
        match (a, b) {
            (None, None) => true,
            (Some(light_a), Some(light_b)) => {
                light_a.ambient == light_b.ambient &&
                light_a.directional_direction == light_b.directional_direction &&
                light_a.directional_color == light_b.directional_color &&
                (light_a.directional_intensity - light_b.directional_intensity).abs() < 0.001
            }
            _ => false
        }
    }

    fn meta_directives_equal(&self, a: &Option<MetaDirective>, b: &Option<MetaDirective>) -> bool {
        match (a, b) {
            (None, None) => true,
            (Some(meta_a), Some(meta_b)) => {
                meta_a.preserve_mode == meta_b.preserve_mode &&
                meta_a.properties == meta_b.properties
            }
            _ => false
        }
    }

    pub fn batch_changes(&mut self, changes: Vec<SceneChange>) {
        self.pending_changes.extend(changes);
    }

    pub fn flush_changes(&mut self) -> Vec<SceneChange> {
        std::mem::take(&mut self.pending_changes)
    }

    pub fn update_current_scene(&mut self, scene: SceneData) {
        self.current_scene = Some(scene);
    }

    pub fn get_current_scene(&self) -> Option<&SceneData> {
        self.current_scene.as_ref()
    }

    pub fn should_recompile_behavior(&self, change: &SceneChange) -> bool {
        matches!(change, 
            SceneChange::BehaviorAdded { .. } | 
            SceneChange::BehaviorModified { .. } |
            SceneChange::BehaviorRemoved { .. }
        )
    }

    pub fn should_reload_model(&self, change: &SceneChange) -> bool {
        if let SceneChange::EntityModified { changes, .. } = change {
            changes.mesh.is_some()
        } else if let SceneChange::EntityAdded { entity } = change {
            matches!(entity.mesh, MeshSource::Model(_))
        } else {
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_entity_diff_detects_transform_change() {
        let reconciler = SceneReconciler::new();
        
        let old_entity = Entity {
            id: "test".to_string(),
            name: "test".to_string(),
            mesh: MeshSource::Primitive(crate::entity::PrimitiveType::cube()),
            transform: Transform {
                position: crate::Vec3::new(0.0, 0.0, 0.0),
                rotation: crate::Quat::IDENTITY,
                scale: crate::Vec3::ONE,
            },
            material: Material::default(),
            behavior: None,
            children: Vec::new(),
            parent: None,
            components: Vec::new(),
            meta: None,
        };

        let mut new_entity = old_entity.clone();
        new_entity.transform.position = crate::Vec3::new(1.0, 0.0, 0.0);

        let changes = reconciler.diff_entities(&old_entity, &new_entity);
        assert!(changes.is_some());
        assert!(changes.unwrap().transform.is_some());
    }

    #[test]
    fn test_scene_diff_detects_entity_addition() {
        let mut reconciler = SceneReconciler::new();
        
        let old_scene = SceneData::default();
        let mut new_scene = old_scene.clone();
        
        new_scene.entities.push(Entity {
            id: "new_entity".to_string(),
            name: "new_entity".to_string(),
            mesh: MeshSource::Primitive(crate::entity::PrimitiveType::sphere()),
            transform: Transform::default(),
            material: Material::default(),
            behavior: None,
            children: Vec::new(),
            parent: None,
            components: Vec::new(),
            meta: None,
        });

        let changes = reconciler.diff_scenes(&old_scene, &new_scene);
        assert_eq!(changes.len(), 1);
        assert!(matches!(changes[0], SceneChange::EntityAdded { .. }));
    }
}
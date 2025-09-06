//! Simplified Hot-Reload System
//! 
//! Replaces complex reconciliation with event-sourced approach and explicit preservation policies.
//! This addresses the complexity issues from the previous React-style reconciliation system.

use std::collections::HashMap;
use crate::value::Value;
use crate::persistence::{PersistenceLayer, Change, ValuePath, Author};

/// Explicit preservation policy for hot-reload
#[derive(Debug, Clone, PartialEq)]
pub enum ReloadPolicy {
    /// Keep runtime state across reloads
    Preserve,
    /// Reset to source definition
    Reset,
    /// Write runtime state back to source code
    SyncToCode,
}

/// Object-specific preservation metadata
#[derive(Debug, Clone)]
pub struct PreservationMeta {
    pub object_id: String,
    pub policy: ReloadPolicy,
    pub properties: Vec<String>, // Specific properties to preserve
}

/// Simple hot-reload manager using event-sourced approach
pub struct HotReloadManager {
    /// Preserved state across reloads
    preserved_state: HashMap<String, HashMap<String, Value>>,
    
    /// Preservation policies per object
    policies: HashMap<String, PreservationMeta>,
    
    /// Reference to persistence layer for event tracking
    persistence: PersistenceLayer,
    
    /// Current scene state
    pub current_scene: HashMap<String, Value>,
}

impl HotReloadManager {
    pub fn new() -> Self {
        Self {
            preserved_state: HashMap::new(),
            policies: HashMap::new(),
            persistence: PersistenceLayer::new(),
            current_scene: HashMap::new(),
        }
    }
    
    /// Register a preservation policy for an object
    pub fn register_policy(&mut self, meta: PreservationMeta) {
        self.policies.insert(meta.object_id.clone(), meta);
    }
    
    /// Save state for objects marked with Preserve policy
    pub fn save_preserved_state(&mut self) {
        self.preserved_state.clear();
        
        for (id, policy) in &self.policies {
            if policy.policy == ReloadPolicy::Preserve {
                if let Some(object) = self.current_scene.get(id) {
                    let mut preserved = HashMap::new();
                    
                    // Extract specific properties to preserve
                    if let Value::Map(props) = object {
                        for prop_name in &policy.properties {
                            if let Some(value) = props.get(prop_name) {
                                preserved.insert(prop_name.clone(), value.clone());
                            }
                        }
                    }
                    
                    if !preserved.is_empty() {
                        self.preserved_state.insert(id.clone(), preserved);
                    }
                }
            }
        }
        
        // Record preservation event in journal
        let path = ValuePath::new(vec!["hotreload".to_string(), "preserve".to_string()]);
        let change = Change::Create {
            path,
            value: Value::Map(
                self.preserved_state
                    .iter()
                    .map(|(k, v)| {
                        (k.clone(), Value::Map(v.clone()))
                    })
                    .collect()
            ),
        };
        let _ = self.persistence.record_change(change, Author::System);
    }
    
    /// Load a new scene from source
    pub fn load_new_scene(&mut self, new_scene: HashMap<String, Value>) {
        // Record scene load event
        let path = ValuePath::new(vec!["hotreload".to_string(), "load".to_string()]);
        let change = Change::Update {
            path,
            old: Value::Map(self.current_scene.clone()),
            new: Value::Map(new_scene.clone()),
        };
        let _ = self.persistence.record_change(change, Author::System);
        
        // Replace current scene
        self.current_scene = new_scene;
    }
    
    /// Restore preserved state to reloaded scene
    pub fn restore_preserved_state(&mut self) {
        for (id, preserved_props) in &self.preserved_state {
            if let Some(object) = self.current_scene.get_mut(id) {
                if let Value::Map(props) = object {
                    // Restore preserved properties
                    for (prop_name, value) in preserved_props {
                        props.insert(prop_name.clone(), value.clone());
                    }
                }
            }
        }
        
        // Record restoration event
        let path = ValuePath::new(vec!["hotreload".to_string(), "restore".to_string()]);
        let change = Change::Create {
            path,
            value: Value::Map(
                self.preserved_state
                    .iter()
                    .map(|(k, v)| {
                        (k.clone(), Value::Map(v.clone()))
                    })
                    .collect()
            ),
        };
        let _ = self.persistence.record_change(change, Author::System);
    }
    
    /// Sync runtime state back to code for objects with SyncToCode policy
    pub fn sync_to_code(&self) -> HashMap<String, HashMap<String, Value>> {
        let mut code_updates = HashMap::new();
        
        for (id, policy) in &self.policies {
            if policy.policy == ReloadPolicy::SyncToCode {
                if let Some(object) = self.current_scene.get(id) {
                    if let Value::Map(props) = object {
                        let mut synced_props = HashMap::new();
                        
                        // Sync specific properties back to code
                        for prop_name in &policy.properties {
                            if let Some(value) = props.get(prop_name) {
                                synced_props.insert(prop_name.clone(), value.clone());
                            }
                        }
                        
                        if !synced_props.is_empty() {
                            code_updates.insert(id.clone(), synced_props);
                        }
                    }
                }
            }
        }
        
        code_updates
    }
    
    /// Main hot-reload operation
    pub fn reload(&mut self, new_scene: HashMap<String, Value>) -> Result<(), String> {
        // Step 1: Save state for objects marked "preserve"
        self.save_preserved_state();
        
        // Step 2: Load new scene from source
        self.load_new_scene(new_scene);
        
        // Step 3: Restore preserved state
        self.restore_preserved_state();
        
        // Step 4: Create checkpoint for time-travel
        let _checkpoint = self.persistence.checkpoint();
        
        Ok(())
    }
    
    /// Parse meta directives from scene DSL
    pub fn parse_meta_directives(&mut self, scene_value: &Value) {
        if let Value::List(items) = scene_value {
            for item in items {
                if let Value::List(expr) = item {
                    if expr.len() >= 2 {
                        if let Value::Symbol(sym) = &expr[0] {
                            if sym.0 == "meta" {
                                self.parse_single_meta(&expr[1..]);
                            }
                        }
                    }
                }
            }
        }
    }
    
    fn parse_single_meta(&mut self, meta_args: &[Value]) {
        if meta_args.is_empty() {
            return;
        }
        
        if let Value::Symbol(directive) = &meta_args[0] {
            match directive.0.as_str() {
                "preserve-runtime" => {
                    if meta_args.len() >= 2 {
                        if let Value::Symbol(id) = &meta_args[1] {
                            let mut properties = Vec::new();
                            if meta_args.len() >= 3 {
                                if let Value::Vector(props) = &meta_args[2] {
                                    for prop in props {
                                        if let Value::Symbol(p) = prop {
                                            properties.push(p.0.clone());
                                        }
                                    }
                                }
                            }
                            
                            self.register_policy(PreservationMeta {
                                object_id: id.0.clone(),
                                policy: ReloadPolicy::Preserve,
                                properties,
                            });
                        }
                    }
                }
                "sync-to-code" => {
                    if meta_args.len() >= 2 {
                        if let Value::Symbol(id) = &meta_args[1] {
                            let mut properties = Vec::new();
                            if meta_args.len() >= 3 {
                                if let Value::Vector(props) = &meta_args[2] {
                                    for prop in props {
                                        if let Value::Symbol(p) = prop {
                                            properties.push(p.0.clone());
                                        }
                                    }
                                }
                            }
                            
                            self.register_policy(PreservationMeta {
                                object_id: id.0.clone(),
                                policy: ReloadPolicy::SyncToCode,
                                properties,
                            });
                        }
                    }
                }
                "reset-on-reload" => {
                    if meta_args.len() >= 2 {
                        if let Value::Symbol(id) = &meta_args[1] {
                            self.register_policy(PreservationMeta {
                                object_id: id.0.clone(),
                                policy: ReloadPolicy::Reset,
                                properties: Vec::new(),
                            });
                        }
                    }
                }
                _ => {}
            }
        }
    }
    
    /// Get current scene state
    pub fn get_current_scene(&self) -> &HashMap<String, Value> {
        &self.current_scene
    }
    
    /// Get preservation policies
    pub fn get_policies(&self) -> &HashMap<String, PreservationMeta> {
        &self.policies
    }
    
    /// Time-travel to a previous state using persistence layer
    pub fn time_travel_to(&mut self, timestamp: u64) -> Result<(), String> {
        if let Some(snapshot) = self.persistence.snapshots.find_before(timestamp) {
            // Restore scene from snapshot
            let path = ValuePath::new(vec!["scene".to_string()]);
            if let Some(Value::Map(scene)) = snapshot.state.get(&path) {
                self.current_scene = scene.clone();
                Ok(())
            } else {
                Err("No scene data in snapshot".to_string())
            }
        } else {
            Err(format!("No snapshot found for timestamp {}", timestamp))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::Symbol;
    
    fn create_test_scene() -> HashMap<String, Value> {
        let mut scene = HashMap::new();
        
        // Create a cube with position
        let mut cube_props = HashMap::new();
        cube_props.insert("position".to_string(), Value::Vector(vec![
            Value::Float(0.0),
            Value::Float(0.0),
            Value::Float(0.0),
        ]));
        cube_props.insert("color".to_string(), Value::Str("red".to_string()));
        scene.insert("cube1".to_string(), Value::Map(cube_props));
        
        // Create a camera
        let mut camera_props = HashMap::new();
        camera_props.insert("position".to_string(), Value::Vector(vec![
            Value::Float(0.0),
            Value::Float(5.0),
            Value::Float(10.0),
        ]));
        camera_props.insert("fov".to_string(), Value::Float(60.0));
        scene.insert("camera".to_string(), Value::Map(camera_props));
        
        scene
    }
    
    #[test]
    fn test_preserve_policy() {
        let mut manager = HotReloadManager::new();
        
        // Load initial scene
        let scene = create_test_scene();
        manager.load_new_scene(scene.clone());
        
        // Register preservation policy for camera position
        manager.register_policy(PreservationMeta {
            object_id: "camera".to_string(),
            policy: ReloadPolicy::Preserve,
            properties: vec!["position".to_string()],
        });
        
        // Modify camera position at runtime
        if let Some(Value::Map(camera)) = manager.current_scene.get_mut("camera") {
            camera.insert("position".to_string(), Value::Vector(vec![
                Value::Float(5.0),
                Value::Float(10.0),
                Value::Float(15.0),
            ]));
        }
        
        // Create new scene from source (with original camera position)
        let new_scene = create_test_scene();
        
        // Hot-reload
        manager.reload(new_scene).unwrap();
        
        // Check that camera position was preserved
        if let Some(Value::Map(camera)) = manager.current_scene.get("camera") {
            if let Some(Value::Vector(pos)) = camera.get("position") {
                assert_eq!(pos[0], Value::Float(5.0));
                assert_eq!(pos[1], Value::Float(10.0));
                assert_eq!(pos[2], Value::Float(15.0));
            } else {
                panic!("Camera position not found");
            }
        } else {
            panic!("Camera not found");
        }
    }
    
    #[test]
    fn test_reset_policy() {
        let mut manager = HotReloadManager::new();
        
        // Load initial scene
        let scene = create_test_scene();
        manager.load_new_scene(scene.clone());
        
        // Register reset policy for cube
        manager.register_policy(PreservationMeta {
            object_id: "cube1".to_string(),
            policy: ReloadPolicy::Reset,
            properties: vec![],
        });
        
        // Modify cube position at runtime
        if let Some(Value::Map(cube)) = manager.current_scene.get_mut("cube1") {
            cube.insert("position".to_string(), Value::Vector(vec![
                Value::Float(10.0),
                Value::Float(0.0),
                Value::Float(0.0),
            ]));
        }
        
        // Create new scene from source
        let new_scene = create_test_scene();
        
        // Hot-reload
        manager.reload(new_scene).unwrap();
        
        // Check that cube position was reset to source
        if let Some(Value::Map(cube)) = manager.current_scene.get("cube1") {
            if let Some(Value::Vector(pos)) = cube.get("position") {
                assert_eq!(pos[0], Value::Float(0.0));
                assert_eq!(pos[1], Value::Float(0.0));
                assert_eq!(pos[2], Value::Float(0.0));
            }
        }
    }
    
    #[test]
    fn test_sync_to_code() {
        let mut manager = HotReloadManager::new();
        
        // Load scene
        let scene = create_test_scene();
        manager.load_new_scene(scene);
        
        // Register sync-to-code policy for cube position
        manager.register_policy(PreservationMeta {
            object_id: "cube1".to_string(),
            policy: ReloadPolicy::SyncToCode,
            properties: vec!["position".to_string()],
        });
        
        // Modify cube position at runtime
        if let Some(Value::Map(cube)) = manager.current_scene.get_mut("cube1") {
            cube.insert("position".to_string(), Value::Vector(vec![
                Value::Float(3.0),
                Value::Float(4.0),
                Value::Float(5.0),
            ]));
        }
        
        // Get values to sync back to code
        let code_updates = manager.sync_to_code();
        
        assert!(code_updates.contains_key("cube1"));
        let cube_updates = &code_updates["cube1"];
        assert!(cube_updates.contains_key("position"));
        
        if let Some(Value::Vector(pos)) = cube_updates.get("position") {
            assert_eq!(pos[0], Value::Float(3.0));
            assert_eq!(pos[1], Value::Float(4.0));
            assert_eq!(pos[2], Value::Float(5.0));
        }
    }
    
    #[test]
    fn test_parse_meta_directives() {
        let mut manager = HotReloadManager::new();
        
        // Create scene with meta directives
        let scene_dsl = Value::List(vec![
            Value::List(vec![
                Value::Symbol(Symbol("meta".to_string())),
                Value::Symbol(Symbol("preserve-runtime".to_string())),
                Value::Symbol(Symbol("camera".to_string())),
                Value::Vector(vec![
                    Value::Symbol(Symbol("position".to_string())),
                    Value::Symbol(Symbol("target".to_string())),
                ]),
            ]),
            Value::List(vec![
                Value::Symbol(Symbol("meta".to_string())),
                Value::Symbol(Symbol("sync-to-code".to_string())),
                Value::Symbol(Symbol("cube1".to_string())),
                Value::Vector(vec![
                    Value::Symbol(Symbol("position".to_string())),
                ]),
            ]),
            Value::List(vec![
                Value::Symbol(Symbol("meta".to_string())),
                Value::Symbol(Symbol("reset-on-reload".to_string())),
                Value::Symbol(Symbol("light1".to_string())),
            ]),
        ]);
        
        manager.parse_meta_directives(&scene_dsl);
        
        let policies = manager.get_policies();
        assert_eq!(policies.len(), 3);
        
        assert_eq!(policies["camera"].policy, ReloadPolicy::Preserve);
        assert_eq!(policies["camera"].properties, vec!["position", "target"]);
        
        assert_eq!(policies["cube1"].policy, ReloadPolicy::SyncToCode);
        assert_eq!(policies["cube1"].properties, vec!["position"]);
        
        assert_eq!(policies["light1"].policy, ReloadPolicy::Reset);
    }
}
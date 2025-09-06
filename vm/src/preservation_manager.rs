//! Preservation State Manager
//! 
//! Manages runtime state preservation across hot-reloads.
//! Works with the hot-reload manager and scene differ to provide
//! predictable state management without complex reconciliation.

use std::collections::HashMap;
use crate::value::Value;
use crate::persistence::{PersistenceLayer, Change, ValuePath, Author};

/// Runtime state that can be preserved across reloads
#[derive(Debug, Clone)]
pub struct PreservedState {
    /// Object states indexed by ID
    pub objects: HashMap<String, ObjectState>,
    
    /// Global runtime values (e.g., time, frame count)
    pub globals: HashMap<String, Value>,
    
    /// Camera state for viewport preservation
    pub camera: Option<CameraState>,
    
    /// Animation states
    pub animations: HashMap<String, AnimationState>,
}

impl PreservedState {
    pub fn new() -> Self {
        Self {
            objects: HashMap::new(),
            globals: HashMap::new(),
            camera: None,
            animations: HashMap::new(),
        }
    }
}

/// State of a single object
#[derive(Debug, Clone)]
pub struct ObjectState {
    pub id: String,
    pub properties: HashMap<String, Value>,
    pub metadata: HashMap<String, Value>,
}

/// Camera state for preserving viewport
#[derive(Debug, Clone)]
pub struct CameraState {
    pub position: Vec<f64>,
    pub target: Vec<f64>,
    pub fov: f64,
    pub near: f64,
    pub far: f64,
}

/// Animation state
#[derive(Debug, Clone)]
pub struct AnimationState {
    pub id: String,
    pub current_time: f64,
    pub is_playing: bool,
    pub loop_count: i32,
}

/// Manager for preserving state across hot-reloads
pub struct PreservationManager {
    /// Current preserved state
    state: PreservedState,
    
    /// History of preserved states for time-travel
    pub history: Vec<(u64, PreservedState)>,
    
    /// Maximum history size
    max_history_size: usize,
    
    /// Persistence layer for durable storage
    persistence: PersistenceLayer,
}

impl PreservationManager {
    pub fn new() -> Self {
        Self {
            state: PreservedState::new(),
            history: Vec::new(),
            max_history_size: 100,
            persistence: PersistenceLayer::new(),
        }
    }
    
    /// Capture current state for preservation
    pub fn capture_state(&mut self, scene: &HashMap<String, Value>, preserve_list: &[String]) {
        // Clear current preserved objects
        self.state.objects.clear();
        
        // Capture specified objects
        for id in preserve_list {
            if let Some(value) = scene.get(id) {
                self.capture_object_state(id, value);
            }
        }
        
        // Record capture event
        let path = ValuePath::new(vec!["preservation".to_string(), "capture".to_string()]);
        let change = Change::Create {
            path,
            value: self.state_to_value(),
        };
        let _ = self.persistence.record_change(change, Author::System);
        
        // Add to history
        self.add_to_history();
    }
    
    /// Capture state of a single object
    fn capture_object_state(&mut self, id: &str, value: &Value) {
        let mut object_state = ObjectState {
            id: id.to_string(),
            properties: HashMap::new(),
            metadata: HashMap::new(),
        };
        
        // Extract properties from Map values
        if let Value::Map(props) = value {
            object_state.properties = props.clone();
        }
        
        // Extract metadata if present
        if let Value::WithMeta { value: _, metadata } = value {
            object_state.metadata = metadata.clone();
        }
        
        self.state.objects.insert(id.to_string(), object_state);
    }
    
    /// Restore preserved state to a scene
    pub fn restore_state(&mut self, scene: &mut HashMap<String, Value>) {
        for (id, object_state) in &self.state.objects {
            // Only restore if object exists in new scene
            if let Some(scene_value) = scene.get_mut(id) {
                self.restore_object_state(scene_value, object_state);
            }
        }
        
        // Record restoration event
        let path = ValuePath::new(vec!["preservation".to_string(), "restore".to_string()]);
        let change = Change::Create {
            path,
            value: self.state_to_value(),
        };
        let _ = self.persistence.record_change(change, Author::System);
    }
    
    /// Restore state to a single object
    fn restore_object_state(&self, scene_value: &mut Value, object_state: &ObjectState) {
        if let Value::Map(props) = scene_value {
            // Restore properties
            for (key, value) in &object_state.properties {
                props.insert(key.clone(), value.clone());
            }
        }
    }
    
    /// Preserve camera state
    pub fn capture_camera(&mut self, position: Vec<f64>, target: Vec<f64>, fov: f64) {
        self.state.camera = Some(CameraState {
            position,
            target,
            fov,
            near: 0.1,
            far: 1000.0,
        });
    }
    
    /// Restore camera state
    pub fn restore_camera(&self) -> Option<&CameraState> {
        self.state.camera.as_ref()
    }
    
    /// Preserve global runtime value
    pub fn set_global(&mut self, key: String, value: Value) {
        self.state.globals.insert(key, value);
    }
    
    /// Get preserved global value
    pub fn get_global(&self, key: &str) -> Option<&Value> {
        self.state.globals.get(key)
    }
    
    /// Preserve animation state
    pub fn capture_animation(&mut self, id: String, current_time: f64, is_playing: bool) {
        self.state.animations.insert(id.clone(), AnimationState {
            id,
            current_time,
            is_playing,
            loop_count: 0,
        });
    }
    
    /// Restore animation state
    pub fn restore_animation(&self, id: &str) -> Option<&AnimationState> {
        self.state.animations.get(id)
    }
    
    /// Clear all preserved state
    pub fn clear(&mut self) {
        self.state = PreservedState::new();
    }
    
    /// Add current state to history
    fn add_to_history(&mut self) {
        let timestamp = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs();
        
        self.history.push((timestamp, self.state.clone()));
        
        // Trim history if too large
        if self.history.len() > self.max_history_size {
            self.history.remove(0);
        }
    }
    
    /// Get state from history
    pub fn get_from_history(&self, timestamp: u64) -> Option<&PreservedState> {
        self.history
            .iter()
            .find(|(ts, _)| *ts == timestamp)
            .map(|(_, state)| state)
    }
    
    /// Time-travel to a previous state
    pub fn restore_from_history(&mut self, timestamp: u64) -> bool {
        if let Some(state) = self.get_from_history(timestamp) {
            self.state = state.clone();
            true
        } else {
            false
        }
    }
    
    /// Convert preserved state to Value for persistence
    fn state_to_value(&self) -> Value {
        let mut state_map = HashMap::new();
        
        // Add objects
        let objects_map: HashMap<String, Value> = self.state.objects
            .iter()
            .map(|(k, v)| {
                let mut obj_map = HashMap::new();
                obj_map.insert("properties".to_string(), Value::Map(v.properties.clone()));
                obj_map.insert("metadata".to_string(), Value::Map(v.metadata.clone()));
                (k.clone(), Value::Map(obj_map))
            })
            .collect();
        state_map.insert("objects".to_string(), Value::Map(objects_map));
        
        // Add globals
        state_map.insert("globals".to_string(), Value::Map(self.state.globals.clone()));
        
        // Add camera if present
        if let Some(cam) = &self.state.camera {
            let mut cam_map = HashMap::new();
            cam_map.insert("position".to_string(), Value::Vector(
                cam.position.iter().map(|v| Value::Float(*v)).collect()
            ));
            cam_map.insert("target".to_string(), Value::Vector(
                cam.target.iter().map(|v| Value::Float(*v)).collect()
            ));
            cam_map.insert("fov".to_string(), Value::Float(cam.fov));
            state_map.insert("camera".to_string(), Value::Map(cam_map));
        }
        
        Value::Map(state_map)
    }
    
    /// Create a checkpoint for the current state
    pub fn checkpoint(&mut self) -> u64 {
        let snapshot_id = self.persistence.checkpoint();
        // Extract timestamp from the snapshot ID string
        // Format is "snapshot_{timestamp}"
        if let Some(timestamp_str) = snapshot_id.0.strip_prefix("snapshot_") {
            timestamp_str.parse().unwrap_or(0)
        } else {
            0
        }
    }
    
    /// Get current preserved state
    pub fn get_state(&self) -> &PreservedState {
        &self.state
    }
    
    /// Set maximum history size
    pub fn set_max_history_size(&mut self, size: usize) {
        self.max_history_size = size;
        
        // Trim existing history if needed
        while self.history.len() > self.max_history_size {
            self.history.remove(0);
        }
    }
}

/// Utility to determine what should be preserved based on heuristics
pub struct PreservationHeuristics;

impl PreservationHeuristics {
    /// Determine if an object should be preserved based on its properties
    pub fn should_preserve(value: &Value) -> bool {
        if let Value::Map(props) = value {
            // Preserve if has runtime-specific properties
            props.contains_key("runtime_state") ||
            props.contains_key("animation_time") ||
            props.contains_key("user_position") ||
            props.contains_key("interaction_state")
        } else {
            false
        }
    }
    
    /// Get list of properties that should typically be preserved
    pub fn default_preserved_properties() -> Vec<String> {
        vec![
            "position".to_string(),
            "rotation".to_string(),
            "scale".to_string(),
            "animation_time".to_string(),
            "user_data".to_string(),
            "interaction_state".to_string(),
        ]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    fn create_test_scene() -> HashMap<String, Value> {
        let mut scene = HashMap::new();
        
        // Create cube with position
        let mut cube = HashMap::new();
        cube.insert("position".to_string(), Value::Vector(vec![
            Value::Float(1.0),
            Value::Float(2.0),
            Value::Float(3.0),
        ]));
        cube.insert("color".to_string(), Value::Str("red".to_string()));
        scene.insert("cube1".to_string(), Value::Map(cube));
        
        // Create sphere
        let mut sphere = HashMap::new();
        sphere.insert("position".to_string(), Value::Vector(vec![
            Value::Float(0.0),
            Value::Float(0.0),
            Value::Float(0.0),
        ]));
        scene.insert("sphere1".to_string(), Value::Map(sphere));
        
        scene
    }
    
    #[test]
    fn test_capture_and_restore() {
        let mut manager = PreservationManager::new();
        let mut scene = create_test_scene();
        
        // Capture state of cube1
        manager.capture_state(&scene, &["cube1".to_string()]);
        
        // Modify scene
        if let Some(Value::Map(cube)) = scene.get_mut("cube1") {
            cube.insert("position".to_string(), Value::Vector(vec![
                Value::Float(10.0),
                Value::Float(20.0),
                Value::Float(30.0),
            ]));
        }
        
        // Create new scene from source
        let mut new_scene = create_test_scene();
        
        // Restore preserved state
        manager.restore_state(&mut new_scene);
        
        // Check that cube1 position was preserved
        if let Some(Value::Map(cube)) = new_scene.get("cube1") {
            if let Some(Value::Vector(pos)) = cube.get("position") {
                // Should have original captured position, not modified position
                assert_eq!(pos[0], Value::Float(1.0));
                assert_eq!(pos[1], Value::Float(2.0));
                assert_eq!(pos[2], Value::Float(3.0));
            }
        }
    }
    
    #[test]
    fn test_camera_preservation() {
        let mut manager = PreservationManager::new();
        
        // Capture camera state
        manager.capture_camera(
            vec![5.0, 10.0, 15.0],
            vec![0.0, 0.0, 0.0],
            75.0,
        );
        
        // Restore camera
        let camera = manager.restore_camera().unwrap();
        assert_eq!(camera.position, vec![5.0, 10.0, 15.0]);
        assert_eq!(camera.target, vec![0.0, 0.0, 0.0]);
        assert_eq!(camera.fov, 75.0);
    }
    
    #[test]
    fn test_global_values() {
        let mut manager = PreservationManager::new();
        
        // Set global values
        manager.set_global("frame_count".to_string(), Value::Int(100));
        manager.set_global("time".to_string(), Value::Float(3.14));
        
        // Retrieve globals
        assert_eq!(manager.get_global("frame_count"), Some(&Value::Int(100)));
        assert_eq!(manager.get_global("time"), Some(&Value::Float(3.14)));
    }
    
    #[test]
    fn test_animation_state() {
        let mut manager = PreservationManager::new();
        
        // Capture animation state
        manager.capture_animation("anim1".to_string(), 2.5, true);
        
        // Restore animation
        let anim = manager.restore_animation("anim1").unwrap();
        assert_eq!(anim.current_time, 2.5);
        assert!(anim.is_playing);
    }
    
    #[test]
    fn test_history() {
        let mut manager = PreservationManager::new();
        manager.set_max_history_size(3);
        
        let scene = create_test_scene();
        
        // Capture multiple states
        for i in 0..5 {
            manager.set_global("counter".to_string(), Value::Int(i));
            manager.capture_state(&scene, &[]);
        }
        
        // History should be limited to 3 entries
        assert_eq!(manager.history.len(), 3);
    }
    
    #[test]
    fn test_preservation_heuristics() {
        let mut obj = HashMap::new();
        obj.insert("position".to_string(), Value::Vector(vec![Value::Float(0.0)]));
        obj.insert("runtime_state".to_string(), Value::Map(HashMap::new()));
        
        let value = Value::Map(obj);
        assert!(PreservationHeuristics::should_preserve(&value));
        
        let mut obj2 = HashMap::new();
        obj2.insert("type".to_string(), Value::Str("cube".to_string()));
        
        let value2 = Value::Map(obj2);
        assert!(!PreservationHeuristics::should_preserve(&value2));
    }
}
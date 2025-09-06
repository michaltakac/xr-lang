//! Hot-Swap Coordinator
//! 
//! Coordinates the simplified hot-swapping system, bringing together
//! hot-reload, scene diffing, and state preservation without complex reconciliation.

use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use crate::value::Value;
use crate::parser::parse_one;
use crate::hotreload::{HotReloadManager, ReloadPolicy, PreservationMeta};
use crate::scene_differ::{SceneDiffer, DiffResult, SceneChange};
use crate::preservation_manager::{PreservationManager, PreservationHeuristics};

/// Authoring modes for hot-swapping
#[derive(Debug, Clone, PartialEq)]
pub enum AuthoringMode {
    /// Design mode - no preservation, always reset
    Design,
    /// Play mode - preserve runtime state
    Play,
    /// Live mode - sync runtime changes back to code
    Live,
}

/// Configuration for hot-swap behavior
#[derive(Debug, Clone)]
pub struct HotSwapConfig {
    /// Current authoring mode
    pub mode: AuthoringMode,
    
    /// Whether to use heuristics for auto-preservation
    pub use_heuristics: bool,
    
    /// Whether to emit diff events
    pub emit_diff_events: bool,
    
    /// Maximum number of checkpoints to keep
    pub max_checkpoints: usize,
}

impl Default for HotSwapConfig {
    fn default() -> Self {
        Self {
            mode: AuthoringMode::Play,
            use_heuristics: true,
            emit_diff_events: true,
            max_checkpoints: 10,
        }
    }
}

/// Main coordinator for hot-swapping functionality
pub struct HotSwapCoordinator {
    /// Hot-reload manager
    pub hot_reload: HotReloadManager,
    
    /// Scene differ
    pub differ: SceneDiffer,
    
    /// Preservation manager
    pub preservation: PreservationManager,
    
    /// Configuration
    pub config: HotSwapConfig,
    
    /// Current scene representation
    pub current_scene: HashMap<String, Value>,
    
    /// Source scene (from DSL)
    pub source_scene: HashMap<String, Value>,
    
    /// Event listeners
    event_listeners: Vec<Box<dyn Fn(&HotSwapEvent) + Send>>,
    
    /// Checkpoint timestamps
    pub checkpoints: Vec<u64>,
    
    /// Thread-safe shared state for async operations
    shared_state: Arc<Mutex<SharedState>>,
}

/// Shared state for thread-safe operations
struct SharedState {
    is_reloading: bool,
    last_reload_time: Option<u64>,
    reload_count: u64,
}

/// Events emitted during hot-swap operations
#[derive(Debug, Clone)]
pub enum HotSwapEvent {
    /// Reload started
    ReloadStarted { timestamp: u64 },
    
    /// Diff computed
    DiffComputed { changes: Vec<SceneChange> },
    
    /// State preserved
    StatePreserved { object_ids: Vec<String> },
    
    /// State restored
    StateRestored { object_ids: Vec<String> },
    
    /// Reload completed
    ReloadCompleted { 
        timestamp: u64,
        changes_applied: usize,
    },
    
    /// Error occurred
    Error { message: String },
    
    /// Mode changed
    ModeChanged { 
        old_mode: AuthoringMode,
        new_mode: AuthoringMode,
    },
    
    /// Code synced
    CodeSynced { 
        updates: HashMap<String, HashMap<String, Value>>,
    },
}

impl HotSwapCoordinator {
    pub fn new() -> Self {
        Self {
            hot_reload: HotReloadManager::new(),
            differ: SceneDiffer::new(),
            preservation: PreservationManager::new(),
            config: HotSwapConfig::default(),
            current_scene: HashMap::new(),
            source_scene: HashMap::new(),
            event_listeners: Vec::new(),
            checkpoints: Vec::new(),
            shared_state: Arc::new(Mutex::new(SharedState {
                is_reloading: false,
                last_reload_time: None,
                reload_count: 0,
            })),
        }
    }
    
    /// Set configuration
    pub fn set_config(&mut self, config: HotSwapConfig) {
        let old_mode = self.config.mode.clone();
        self.config = config;
        
        if old_mode != self.config.mode {
            self.emit_event(HotSwapEvent::ModeChanged {
                old_mode,
                new_mode: self.config.mode.clone(),
            });
        }
    }
    
    /// Get current authoring mode
    pub fn get_mode(&self) -> &AuthoringMode {
        &self.config.mode
    }
    
    /// Set authoring mode
    pub fn set_mode(&mut self, mode: AuthoringMode) {
        if self.config.mode != mode {
            let old_mode = self.config.mode.clone();
            self.config.mode = mode.clone();
            
            self.emit_event(HotSwapEvent::ModeChanged {
                old_mode,
                new_mode: mode,
            });
            
            // Update preservation policies based on mode
            self.update_policies_for_mode();
        }
    }
    
    /// Load scene from DSL source
    pub fn load_from_dsl(&mut self, dsl_source: &str) -> Result<(), String> {
        // Parse DSL
        let parsed = parse_one(dsl_source)
            .map_err(|e| format!("Parse error: {:?}", e))?;
        
        // Convert parsed value to scene
        let new_scene = self.dsl_to_scene(&parsed)?;
        
        // Store as source scene
        self.source_scene = new_scene.clone();
        
        // Parse meta directives
        self.hot_reload.parse_meta_directives(&parsed);
        
        // Initial load
        if self.current_scene.is_empty() {
            self.current_scene = new_scene;
            Ok(())
        } else {
            // Hot-reload
            self.hot_reload_scene(new_scene)
        }
    }
    
    /// Main hot-reload operation
    pub fn hot_reload_scene(&mut self, new_scene: HashMap<String, Value>) -> Result<(), String> {
        // Acquire lock
        {
            let mut state = self.shared_state.lock().unwrap();
            if state.is_reloading {
                return Err("Reload already in progress".to_string());
            }
            state.is_reloading = true;
        }
        
        let timestamp = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs();
        
        self.emit_event(HotSwapEvent::ReloadStarted { timestamp });
        
        // Compute diff
        let diff_result = self.differ.diff(&self.current_scene, &new_scene);
        
        if self.config.emit_diff_events {
            self.emit_event(HotSwapEvent::DiffComputed {
                changes: diff_result.changes.clone(),
            });
        }
        
        // Determine what to preserve based on mode
        let preserve_list = self.get_preserve_list(&diff_result);
        
        // Preserve state if needed
        if !preserve_list.is_empty() {
            self.preservation.capture_state(&self.current_scene, &preserve_list);
            self.emit_event(HotSwapEvent::StatePreserved {
                object_ids: preserve_list.clone(),
            });
        }
        
        // Apply changes or reload entirely
        if self.should_incremental_update(&diff_result) {
            // Apply only the changes
            SceneDiffer::apply_changes(&mut self.current_scene, &diff_result.changes);
        } else {
            // Full reload
            self.current_scene = new_scene;
        }
        
        // Restore preserved state
        if !preserve_list.is_empty() {
            self.preservation.restore_state(&mut self.current_scene);
            self.emit_event(HotSwapEvent::StateRestored {
                object_ids: preserve_list,
            });
        }
        
        // Sync to code if in Live mode
        if self.config.mode == AuthoringMode::Live {
            let code_updates = self.hot_reload.sync_to_code();
            if !code_updates.is_empty() {
                self.emit_event(HotSwapEvent::CodeSynced {
                    updates: code_updates,
                });
            }
        }
        
        // Create checkpoint
        self.create_checkpoint();
        
        // Update shared state
        {
            let mut state = self.shared_state.lock().unwrap();
            state.is_reloading = false;
            state.last_reload_time = Some(timestamp);
            state.reload_count += 1;
        }
        
        self.emit_event(HotSwapEvent::ReloadCompleted {
            timestamp,
            changes_applied: diff_result.changes.len(),
        });
        
        Ok(())
    }
    
    /// Determine what objects should be preserved based on mode and policies
    fn get_preserve_list(&self, diff_result: &DiffResult) -> Vec<String> {
        let mut preserve_list = Vec::new();
        
        match self.config.mode {
            AuthoringMode::Design => {
                // Never preserve in Design mode
            }
            AuthoringMode::Play => {
                // Preserve objects with explicit preserve policy
                for (id, meta) in self.hot_reload.get_policies() {
                    if meta.policy == ReloadPolicy::Preserve {
                        preserve_list.push(id.clone());
                    }
                }
                
                // Use heuristics if enabled
                if self.config.use_heuristics {
                    for (id, value) in &self.current_scene {
                        if !preserve_list.contains(id) && 
                           PreservationHeuristics::should_preserve(value) {
                            preserve_list.push(id.clone());
                        }
                    }
                }
            }
            AuthoringMode::Live => {
                // Preserve everything that's been modified
                for id in &diff_result.modified_ids {
                    preserve_list.push(id.clone());
                }
            }
        }
        
        preserve_list
    }
    
    /// Determine if we should do incremental update or full reload
    pub fn should_incremental_update(&self, diff_result: &DiffResult) -> bool {
        // Full reload if too many changes
        if diff_result.changes.len() > 50 {
            return false;
        }
        
        // Full reload if any type changes
        for change in &diff_result.changes {
            if matches!(change, SceneChange::TypeChanged { .. }) {
                return false;
            }
        }
        
        // Otherwise, incremental is fine
        true
    }
    
    /// Update preservation policies based on current mode
    fn update_policies_for_mode(&mut self) {
        match self.config.mode {
            AuthoringMode::Design => {
                // Reset all to Reset policy
                for (id, _) in self.current_scene.clone() {
                    self.hot_reload.register_policy(PreservationMeta {
                        object_id: id,
                        policy: ReloadPolicy::Reset,
                        properties: vec![],
                    });
                }
            }
            AuthoringMode::Play => {
                // Keep existing policies
            }
            AuthoringMode::Live => {
                // Set all to SyncToCode
                for (id, _) in self.current_scene.clone() {
                    let properties = PreservationHeuristics::default_preserved_properties();
                    self.hot_reload.register_policy(PreservationMeta {
                        object_id: id,
                        policy: ReloadPolicy::SyncToCode,
                        properties,
                    });
                }
            }
        }
    }
    
    /// Convert DSL value to scene representation
    pub fn dsl_to_scene(&self, value: &Value) -> Result<HashMap<String, Value>, String> {
        let mut scene = HashMap::new();
        
        // Extract scene objects from DSL structure
        if let Value::List(items) = value {
            for item in items {
                if let Value::List(expr) = item {
                    if expr.len() >= 2 {
                        if let (Value::Symbol(type_sym), Value::Symbol(id_sym)) = (&expr[0], &expr[1]) {
                            // Skip meta directives
                            if type_sym.0 == "meta" {
                                continue;
                            }
                            
                            // Create object from expression
                            let mut obj_props = HashMap::new();
                            obj_props.insert("type".to_string(), Value::Str(type_sym.0.clone()));
                            
                            // Parse additional properties
                            for prop_expr in &expr[2..] {
                                if let Value::List(prop) = prop_expr {
                                    if prop.len() >= 2 {
                                        if let Value::Symbol(prop_name) = &prop[0] {
                                            obj_props.insert(prop_name.0.clone(), prop[1].clone());
                                        }
                                    }
                                }
                            }
                            
                            scene.insert(id_sym.0.clone(), Value::Map(obj_props));
                        }
                    }
                }
            }
        }
        
        Ok(scene)
    }
    
    /// Create a checkpoint
    pub fn create_checkpoint(&mut self) {
        let checkpoint = self.preservation.checkpoint();
        self.checkpoints.push(checkpoint);
        
        // Limit checkpoints
        if self.checkpoints.len() > self.config.max_checkpoints {
            self.checkpoints.remove(0);
        }
    }
    
    /// Time-travel to a checkpoint
    pub fn restore_checkpoint(&mut self, index: usize) -> Result<(), String> {
        if index >= self.checkpoints.len() {
            return Err("Invalid checkpoint index".to_string());
        }
        
        let timestamp = self.checkpoints[index];
        if self.preservation.restore_from_history(timestamp) {
            self.preservation.restore_state(&mut self.current_scene);
            Ok(())
        } else {
            Err("Failed to restore checkpoint".to_string())
        }
    }
    
    /// Add event listener
    pub fn add_listener<F>(&mut self, listener: F) 
    where
        F: Fn(&HotSwapEvent) + Send + 'static
    {
        self.event_listeners.push(Box::new(listener));
    }
    
    /// Emit event to listeners
    fn emit_event(&self, event: HotSwapEvent) {
        for listener in &self.event_listeners {
            listener(&event);
        }
    }
    
    /// Get current scene
    pub fn get_current_scene(&self) -> &HashMap<String, Value> {
        &self.current_scene
    }
    
    /// Get source scene
    pub fn get_source_scene(&self) -> &HashMap<String, Value> {
        &self.source_scene
    }
    
    /// Get reload statistics
    pub fn get_stats(&self) -> HotSwapStats {
        let state = self.shared_state.lock().unwrap();
        HotSwapStats {
            reload_count: state.reload_count,
            last_reload_time: state.last_reload_time,
            checkpoint_count: self.checkpoints.len(),
            current_mode: self.config.mode.clone(),
        }
    }
}

/// Statistics about hot-swap operations
#[derive(Debug, Clone)]
pub struct HotSwapStats {
    pub reload_count: u64,
    pub last_reload_time: Option<u64>,
    pub checkpoint_count: usize,
    pub current_mode: AuthoringMode,
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_mode_switching() {
        let mut coordinator = HotSwapCoordinator::new();
        
        assert_eq!(*coordinator.get_mode(), AuthoringMode::Play);
        
        coordinator.set_mode(AuthoringMode::Design);
        assert_eq!(*coordinator.get_mode(), AuthoringMode::Design);
        
        coordinator.set_mode(AuthoringMode::Live);
        assert_eq!(*coordinator.get_mode(), AuthoringMode::Live);
    }
    
    #[test]
    fn test_load_from_dsl() {
        let mut coordinator = HotSwapCoordinator::new();
        
        let dsl = r#"
            ((meta preserve-runtime camera [position target])
             (cube cube1 
               (position [0 0 0])
               (color "red"))
             (camera main
               (position [0 5 10])
               (fov 60)))
        "#;
        
        coordinator.load_from_dsl(dsl).unwrap();
        
        let scene = coordinator.get_current_scene();
        assert!(scene.contains_key("cube1"));
        assert!(scene.contains_key("main"));
    }
    
    #[test]
    fn test_hot_reload_with_preservation() {
        let mut coordinator = HotSwapCoordinator::new();
        coordinator.set_mode(AuthoringMode::Play);
        
        // Initial scene
        let mut scene1 = HashMap::new();
        let mut cube = HashMap::new();
        cube.insert("position".to_string(), Value::Vector(vec![
            Value::Float(0.0), Value::Float(0.0), Value::Float(0.0)
        ]));
        scene1.insert("cube1".to_string(), Value::Map(cube));
        
        coordinator.current_scene = scene1;
        
        // Register preservation policy
        coordinator.hot_reload.register_policy(PreservationMeta {
            object_id: "cube1".to_string(),
            policy: ReloadPolicy::Preserve,
            properties: vec!["position".to_string()],
        });
        
        // Modify position at runtime
        if let Some(Value::Map(cube)) = coordinator.current_scene.get_mut("cube1") {
            cube.insert("position".to_string(), Value::Vector(vec![
                Value::Float(5.0), Value::Float(0.0), Value::Float(0.0)
            ]));
        }
        
        // New scene from source
        let mut scene2 = HashMap::new();
        let mut cube2 = HashMap::new();
        cube2.insert("position".to_string(), Value::Vector(vec![
            Value::Float(0.0), Value::Float(0.0), Value::Float(0.0)
        ]));
        cube2.insert("color".to_string(), Value::Str("blue".to_string()));
        scene2.insert("cube1".to_string(), Value::Map(cube2));
        
        // Hot reload
        coordinator.hot_reload_scene(scene2).unwrap();
        
        // Check position was preserved
        if let Some(Value::Map(cube)) = coordinator.current_scene.get("cube1") {
            if let Some(Value::Vector(pos)) = cube.get("position") {
                assert_eq!(pos[0], Value::Float(5.0));
            }
            // Check new property was added
            assert_eq!(cube.get("color"), Some(&Value::Str("blue".to_string())));
        } else {
            panic!("Cube not found");
        }
    }
    
    #[test]
    fn test_event_emission() {
        let mut coordinator = HotSwapCoordinator::new();
        
        let events = Arc::new(Mutex::new(Vec::new()));
        let events_clone = events.clone();
        
        coordinator.add_listener(move |event| {
            events_clone.lock().unwrap().push(format!("{:?}", event));
        });
        
        coordinator.set_mode(AuthoringMode::Live);
        
        let captured_events = events.lock().unwrap();
        assert!(!captured_events.is_empty());
        assert!(captured_events[0].contains("ModeChanged"));
    }
    
    #[test]
    fn test_checkpoint_creation() {
        let mut coordinator = HotSwapCoordinator::new();
        coordinator.config.max_checkpoints = 3;
        
        let scene = HashMap::new();
        
        // Create multiple checkpoints
        for _ in 0..5 {
            coordinator.current_scene = scene.clone();
            coordinator.create_checkpoint();
        }
        
        // Should be limited to 3
        assert_eq!(coordinator.checkpoints.len(), 3);
    }
}
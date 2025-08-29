//! Behavior system that integrates with the VM interpreter for hot-swappable code

use std::collections::HashMap;
use anyhow::Result;

pub struct BehaviorSystem {
    pub interpreter: vm::Interpreter,
    pub behavior_ast: HashMap<String, dsl::ast::Behavior>,
    pub active_behaviors: HashMap<String, String>, // object_id -> behavior_name
}

impl BehaviorSystem {
    pub fn new() -> Self {
        Self {
            interpreter: vm::Interpreter::new(),
            behavior_ast: HashMap::new(),
            active_behaviors: HashMap::new(),
        }
    }
    
    /// Hot-swap behaviors from new AST
    pub fn hot_swap_behaviors(&mut self, ast: &[dsl::ast::Top]) -> Result<()> {
        println!("🔥 Hot-swapping behaviors...");
        
        // Clear old behaviors but preserve state where possible
        let old_states = self.preserve_behavior_states();
        
        // Load new behaviors
        for item in ast {
            if let dsl::ast::Top::Behavior(behavior) = item {
                println!("  📦 Loading behavior: {}", behavior.name);
                
                // Store the AST for the behavior
                self.behavior_ast.insert(behavior.name.clone(), behavior.clone());
                
                // Load into interpreter
                self.interpreter.load_behavior(behavior)?;
                
                // Restore state if this behavior existed before
                if let Some(old_state) = old_states.get(&behavior.name) {
                    self.restore_behavior_state(&behavior.name, old_state)?;
                    println!("    ✅ Preserved state for '{}'", behavior.name);
                }
            }
        }
        
        println!("✨ Behaviors hot-swapped successfully!");
        Ok(())
    }
    
    /// Preserve current behavior states before hot-swap
    fn preserve_behavior_states(&self) -> HashMap<String, HashMap<String, vm::Value>> {
        let mut states = HashMap::new();
        
        for (name, behavior) in &self.interpreter.behaviors {
            let mut state = HashMap::new();
            for (key, value) in &behavior.state {
                state.insert(key.clone(), value.clone());
            }
            states.insert(name.clone(), state);
        }
        
        states
    }
    
    /// Restore behavior state after hot-swap
    fn restore_behavior_state(&mut self, name: &str, state: &HashMap<String, vm::Value>) -> Result<()> {
        if let Some(behavior) = self.interpreter.behaviors.get_mut(name) {
            for (key, value) in state {
                // Only restore if the key still exists in the new behavior
                if behavior.state.contains_key(key) {
                    behavior.state.insert(key.clone(), value.clone());
                }
            }
        }
        Ok(())
    }
    
    /// Update a behavior for a specific object
    pub fn update_behavior(&mut self, object_id: &str, behavior_name: &str, dt: f32) -> Result<BehaviorUpdate> {
        // Execute the behavior in the interpreter
        self.interpreter.update_behavior(behavior_name, dt)?;
        
        // Get the results of execution
        let mut update = BehaviorUpdate::default();
        
        // Check for rotation updates
        if let Some(behavior) = self.interpreter.behaviors.get(behavior_name) {
            // Check for rotation in state
            if let Some(vm::Value::F32(rotation)) = behavior.state.get("rotation") {
                update.rotation = Some(*rotation);
            }
            
            // Check for position updates
            if let Some(vm::Value::Vec3(x, y, z)) = behavior.state.get("position") {
                update.position = Some((*x, *y, *z));
            }
            
            // Check for scale updates
            if let Some(vm::Value::Vec3(x, y, z)) = behavior.state.get("scale") {
                update.scale = Some((*x, *y, *z));
            }
            
            // Handle any side effects from the update function
            // This is where we'd capture rotate-y, move, scale calls etc.
            if let Some(vm::Value::F32(angle)) = behavior.state.get("_rotate_y") {
                update.rotation_delta = Some(*angle);
                // Clear the temporary state
                if let Some(b) = self.interpreter.behaviors.get_mut(behavior_name) {
                    b.state.remove("_rotate_y");
                }
            }
        }
        
        Ok(update)
    }
    
    /// Assign a behavior to an object
    pub fn assign_behavior(&mut self, object_id: String, behavior_name: String) {
        self.active_behaviors.insert(object_id, behavior_name);
    }
    
    /// Get all active behavior assignments
    pub fn get_assignments(&self) -> &HashMap<String, String> {
        &self.active_behaviors
    }
    
    /// Check if a behavior exists
    pub fn has_behavior(&self, name: &str) -> bool {
        self.behavior_ast.contains_key(name)
    }
    
    /// Get behavior state for debugging
    pub fn get_behavior_state(&self, name: &str) -> Option<HashMap<String, String>> {
        self.interpreter.behaviors.get(name).map(|b| {
            let mut state = HashMap::new();
            for (key, value) in &b.state {
                state.insert(key.clone(), format!("{:?}", value));
            }
            state
        })
    }
}

#[derive(Debug, Default, Clone)]
pub struct BehaviorUpdate {
    pub rotation: Option<f32>,
    pub rotation_delta: Option<f32>,
    pub position: Option<(f32, f32, f32)>,
    pub scale: Option<(f32, f32, f32)>,
    pub color: Option<(f32, f32, f32)>,
}
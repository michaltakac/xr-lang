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
        
        // Collect runtime states to preserve (dotted notation values and runtime-modified state)
        let mut runtime_states_to_preserve = HashMap::new();
        let mut runtime_modified_state = HashMap::new();
        
        for item in ast {
            if let dsl::ast::Top::Behavior(new_behavior) = item {
                if let Some(current_behavior) = self.interpreter.behaviors.get(&new_behavior.name) {
                    let mut runtime_state = HashMap::new();
                    let mut modified_state = HashMap::new();
                    
                    // Preserve dotted notation values (these are runtime-computed values)
                    for (key, value) in &current_behavior.env.vars {
                        if key.contains('.') {
                            runtime_state.insert(key.clone(), value.clone());
                        }
                    }
                    
                    // Check if state values have been modified at runtime
                    // (different from initial values in AST)
                    if let Some(old_ast_behavior) = self.behavior_ast.get(&new_behavior.name) {
                        for (key, current_value) in &current_behavior.state {
                            // Check if this is a runtime-modified value (like "time" that accumulates)
                            // or if it's different from the old AST initial value
                            if key == "time" || key.starts_with("_") {
                                // Always preserve time and internal state variables
                                modified_state.insert(key.clone(), current_value.clone());
                            } else {
                                // Find the old initial value in the state vector
                                let old_initial = old_ast_behavior.state.iter()
                                    .find(|(k, _)| k == key)
                                    .map(|(_, v)| *v);
                                
                                if let Some(old_val) = old_initial {
                                    // Check if the current runtime value differs from the old initial value
                                    if let vm::Value::F32(current_f32) = current_value {
                                        if (*current_f32 - old_val).abs() > 0.001 {
                                            // This value has been modified at runtime, preserve it
                                            modified_state.insert(key.clone(), current_value.clone());
                                        }
                                    }
                                }
                            }
                        }
                    }
                    
                    if !runtime_state.is_empty() {
                        runtime_states_to_preserve.insert(new_behavior.name.clone(), runtime_state);
                    }
                    if !modified_state.is_empty() {
                        runtime_modified_state.insert(new_behavior.name.clone(), modified_state);
                    }
                }
            }
        }
        
        // Load new behaviors with updated AST
        for item in ast {
            if let dsl::ast::Top::Behavior(behavior) = item {
                println!("  📦 Loading behavior: {}", behavior.name);
                
                // Check what changed in the behavior state
                let state_changes = if let Some(old_behavior) = self.behavior_ast.get(&behavior.name) {
                    let mut changes = Vec::new();
                    for (key, new_val) in &behavior.state {
                        // Find the old value in the state vector
                        let old_val = old_behavior.state.iter()
                            .find(|(k, _)| k == key)
                            .map(|(_, v)| *v);
                        
                        if let Some(old_v) = old_val {
                            if (new_val - old_v).abs() > 0.001 {
                                changes.push(format!("{}:{:.2}→{:.2}", key, old_v, new_val));
                                println!("    🔄 State change: {} = {} (was {})", key, new_val, old_v);
                            }
                        } else {
                            changes.push(format!("{}:+{:.2}", key, new_val));
                            println!("    ➕ New state: {} = {}", key, new_val);
                        }
                    }
                    changes
                } else {
                    Vec::new()
                };
                
                // Update the AST
                self.behavior_ast.insert(behavior.name.clone(), behavior.clone());
                
                // Load into interpreter (this uses the new initial values from AST)
                self.interpreter.load_behavior(behavior)?;
                
                // Restore runtime computed values (dotted notation)
                if let Some(runtime_state) = runtime_states_to_preserve.get(&behavior.name) {
                    if let Some(behavior_mut) = self.interpreter.behaviors.get_mut(&behavior.name) {
                        for (key, value) in runtime_state {
                            behavior_mut.env.set(key.clone(), value.clone());
                        }
                        println!("    ✅ Preserved runtime values for '{}'", behavior.name);
                    }
                }
                
                // Restore runtime-modified state values (like accumulated time)
                if let Some(modified_state) = runtime_modified_state.get(&behavior.name) {
                    if let Some(behavior_mut) = self.interpreter.behaviors.get_mut(&behavior.name) {
                        for (key, value) in modified_state {
                            behavior_mut.state.insert(key.clone(), value.clone());
                            // IMPORTANT: Also update the environment so the value is accessible during eval
                            behavior_mut.env.set(key.clone(), value.clone());
                        }
                        println!("    ✅ Preserved runtime state for '{}'", behavior.name);
                    }
                }
                
                // Ensure all new state values from AST are in the environment
                // This is crucial for hot-swapping to work properly
                if let Some(behavior_mut) = self.interpreter.behaviors.get_mut(&behavior.name) {
                    for (key, value) in &behavior_mut.state {
                        // Only update if not already set (to preserve runtime values)
                        if behavior_mut.env.get(key).is_none() {
                            behavior_mut.env.set(key.clone(), value.clone());
                        }
                    }
                }
                
                // Log the state changes for debugging
                if !state_changes.is_empty() {
                    println!("    📝 Applied state changes: {}", state_changes.join(", "));
                }
            }
        }
        
        println!("✨ Behaviors hot-swapped successfully!");
        Ok(())
    }
    
    
    /// Update a behavior for a specific object
    pub fn update_behavior(&mut self, object_id: &str, behavior_name: &str, dt: f32) -> Result<BehaviorUpdate> {
        // Execute the behavior in the interpreter
        self.interpreter.update_behavior(behavior_name, dt)?;
        
        // Get the results of execution
        let mut update = BehaviorUpdate {
            properties: HashMap::new(),
            ..Default::default()
        };
        
        if let Some(behavior) = self.interpreter.behaviors.get(behavior_name) {
            // Collect ALL properties that have been set in the environment
            // This makes the system completely generic
            for (key, value) in &behavior.env.vars {
                // Only collect properties with dot notation (e.g., rotation.x, position.y)
                if key.contains('.') {
                    update.properties.insert(key.clone(), value.clone());
                    println!("        DEBUG: Found {} = {:?} in behavior '{}'", key, value, behavior_name);
                }
            }
            
            // Also check state for any non-dotted properties
            for (key, value) in &behavior.state {
                if !key.starts_with('_') {  // Skip internal state
                    update.properties.insert(key.clone(), value.clone());
                }
            }
            
            
            if update.properties.is_empty() {
                println!("        DEBUG: No properties found for behavior '{}'", behavior_name);
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
    // Generic property updates from behaviors
    pub properties: HashMap<String, vm::Value>,
}
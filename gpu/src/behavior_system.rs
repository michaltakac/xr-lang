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
        
        // Collect runtime states to preserve (only dotted notation values like rotation.y)
        let mut runtime_states_to_preserve = HashMap::new();
        
        for item in ast {
            if let dsl::ast::Top::Behavior(new_behavior) = item {
                if let Some(current_behavior) = self.interpreter.behaviors.get(&new_behavior.name) {
                    let mut runtime_state = HashMap::new();
                    
                    // Only preserve dotted notation values (these are runtime-computed values)
                    for (key, value) in &current_behavior.env.vars {
                        if key.contains('.') {
                            runtime_state.insert(key.clone(), value.clone());
                        }
                    }
                    
                    if !runtime_state.is_empty() {
                        runtime_states_to_preserve.insert(new_behavior.name.clone(), runtime_state);
                    }
                }
            }
        }
        
        // Load new behaviors with updated AST
        for item in ast {
            if let dsl::ast::Top::Behavior(behavior) = item {
                println!("  📦 Loading behavior: {}", behavior.name);
                
                // Update the AST
                self.behavior_ast.insert(behavior.name.clone(), behavior.clone());
                
                // Load into interpreter (this uses the new initial values from AST)
                self.interpreter.load_behavior(behavior)?;
                
                // Restore only runtime computed values (dotted notation)
                if let Some(runtime_state) = runtime_states_to_preserve.get(&behavior.name) {
                    if let Some(behavior_mut) = self.interpreter.behaviors.get_mut(&behavior.name) {
                        for (key, value) in runtime_state {
                            behavior_mut.env.set(key.clone(), value.clone());
                        }
                        println!("    ✅ Preserved runtime values for '{}'", behavior.name);
                    }
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
        let mut update = BehaviorUpdate::default();
        
        // Check for rotation updates
        if let Some(behavior) = self.interpreter.behaviors.get(behavior_name) {
            // Check for rotation.y in the environment (this is what kitchen_sink uses)
            if let Some(vm::Value::F32(rotation_y)) = behavior.env.get("rotation.y") {
                update.rotation = Some(rotation_y);
            } else if let Some(vm::Value::F32(rotation)) = behavior.state.get("rotation") {
                update.rotation = Some(*rotation);
            }
            
            // Check for position components
            let mut pos_changed = false;
            let mut pos_x = 0.0;
            let mut pos_y = 0.0; 
            let mut pos_z = 0.0;
            
            if let Some(vm::Value::F32(x)) = behavior.env.get("position.x") {
                pos_x = x;
                pos_changed = true;
            }
            if let Some(vm::Value::F32(y)) = behavior.env.get("position.y") {
                pos_y = y;
                pos_changed = true;
            }
            if let Some(vm::Value::F32(z)) = behavior.env.get("position.z") {
                pos_z = z;
                pos_changed = true;
            }
            
            if pos_changed {
                update.position = Some((pos_x, pos_y, pos_z));
            } else if let Some(vm::Value::Vec3(x, y, z)) = behavior.state.get("position") {
                update.position = Some((*x, *y, *z));
            }
            
            // Check for scale components
            let mut scale_changed = false;
            let mut scale_x = 1.0;
            let mut scale_y = 1.0;
            let mut scale_z = 1.0;
            
            if let Some(vm::Value::F32(x)) = behavior.env.get("scale.x") {
                scale_x = x;
                scale_changed = true;
            }
            if let Some(vm::Value::F32(y)) = behavior.env.get("scale.y") {
                scale_y = y;
                scale_changed = true;
            }
            if let Some(vm::Value::F32(z)) = behavior.env.get("scale.z") {
                scale_z = z;
                scale_changed = true;
            }
            
            if scale_changed {
                update.scale = Some((scale_x, scale_y, scale_z));
            } else if let Some(vm::Value::Vec3(x, y, z)) = behavior.state.get("scale") {
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
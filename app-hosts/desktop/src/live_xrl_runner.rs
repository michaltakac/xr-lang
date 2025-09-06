//! Live XRL file runner - supports true liveness without restarts
//! 
//! This runner maintains the entire VM state across code changes,
//! allowing you to modify functions while preserving all runtime state.

use anyhow::{Result, Context};
use std::path::{Path, PathBuf};
use std::collections::HashMap;
use vm::{
    parser::Parser,
    value::{Value, Symbol, ObjectId},
    intrinsics::SCENE,
    live_image::{LiveEvaluator, LiveSnapshot},
};
use gpu::scene::{SceneData, CameraData};
use gpu::entity::{Entity, PrimitiveType, MeshSource, Transform};
use gpu::{Vec3, Quat};

/// Live XRL Runner with true liveness support
pub struct LiveXrlRunner {
    /// The live evaluator that maintains state
    evaluator: LiveEvaluator,
    
    /// Current file being executed
    current_file: Option<PathBuf>,
    
    /// Source code cache for incremental updates
    source_cache: HashMap<PathBuf, String>,
    
    /// Update history for debugging
    update_history: Vec<UpdateEvent>,
    
    /// Whether live mode is enabled
    live_mode_enabled: bool,
}

/// Event representing a live update
#[derive(Clone, Debug)]
pub struct UpdateEvent {
    pub timestamp: u64,
    pub file: PathBuf,
    pub change_type: ChangeType,
    pub affected_definitions: Vec<Symbol>,
}

#[derive(Clone, Debug)]
pub enum ChangeType {
    FunctionRedefinition,
    MacroRedefinition,
    StructureChange,
    NewDefinition,
}

impl LiveXrlRunner {
    pub fn new() -> Self {
        let mut evaluator = LiveEvaluator::new();
        
        // Set up redefinition callback to track live updates
        evaluator.base.set_redefinition_callback({
            move |name, _value| {
                println!("[LIVE] Redefined: {}", name.0);
            }
        });
        
        Self {
            evaluator,
            current_file: None,
            source_cache: HashMap::new(),
            update_history: Vec::new(),
            live_mode_enabled: true,
        }
    }
    
    /// Enable or disable live mode
    pub fn set_live_mode(&mut self, enabled: bool) {
        self.live_mode_enabled = enabled;
        if enabled {
            println!("[LIVE] Live mode enabled - changes will be applied without restart");
        } else {
            println!("[LIVE] Live mode disabled - changes will require restart");
        }
    }
    
    /// Load and evaluate an XRL file for the first time
    pub fn load_xrl_file(&mut self, path: &Path) -> Result<SceneData> {
        let source = std::fs::read_to_string(path)
            .with_context(|| format!("Failed to read XRL file: {}", path.display()))?;
        
        // Cache the source for incremental updates
        self.source_cache.insert(path.to_path_buf(), source.clone());
        self.current_file = Some(path.to_path_buf());
        
        // Clear scene for initial load
        SCENE.with(|scene| {
            let mut scene = scene.borrow_mut();
            scene.nodes.clear();
            scene.cameras.clear();
            scene.next_id = 1;
        });
        
        // Parse and evaluate the entire file
        self.evaluate_source(&source)?;
        
        // Extract scene data
        Ok(self.extract_scene_data())
    }
    
    /// Apply live updates from changed source
    pub fn apply_live_update(&mut self, path: &Path) -> Result<SceneData> {
        if !self.live_mode_enabled {
            // Fall back to full reload if live mode is disabled
            return self.load_xrl_file(path);
        }
        
        let new_source = std::fs::read_to_string(path)
            .with_context(|| format!("Failed to read XRL file: {}", path.display()))?;
        
        // Get the old source for comparison
        let old_source = self.source_cache.get(path).cloned().unwrap_or_default();
        
        if new_source == old_source {
            // No changes, return current scene
            return Ok(self.extract_scene_data());
        }
        
        // Find changed definitions
        let changes = self.find_changed_definitions(&old_source, &new_source)?;
        
        // Apply only the changed definitions
        for (name, expr) in changes {
            println!("[LIVE] Updating definition: {}", name.0);
            
            // Evaluate the changed definition with live update
            self.evaluator.eval_live(&expr)
                .map_err(|e| anyhow::anyhow!("Failed to apply live update: {}", e))?;
            
            // Record the update
            self.update_history.push(UpdateEvent {
                timestamp: std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap()
                    .as_secs(),
                file: path.to_path_buf(),
                change_type: ChangeType::FunctionRedefinition,
                affected_definitions: vec![name],
            });
        }
        
        // Update source cache
        self.source_cache.insert(path.to_path_buf(), new_source);
        
        // Return updated scene (only scene nodes might have changed)
        Ok(self.extract_scene_data())
    }
    
    /// Find definitions that changed between two sources
    fn find_changed_definitions(&self, old_source: &str, new_source: &str) -> Result<Vec<(Symbol, Value)>> {
        let mut changes = Vec::new();
        
        // Parse both sources
        let mut old_parser = Parser::new(old_source);
        let old_exprs = old_parser.parse()
            .map_err(|e| anyhow::anyhow!("Failed to parse old source: {}", e))?;
        
        let mut new_parser = Parser::new(new_source);
        let new_exprs = new_parser.parse()
            .map_err(|e| anyhow::anyhow!("Failed to parse new source: {}", e))?;
        
        // Build maps of definitions
        let old_defs = self.extract_definitions(&old_exprs);
        let new_defs = self.extract_definitions(&new_exprs);
        
        // Find changed or new definitions
        for (name, new_expr) in new_defs {
            if let Some(old_expr) = old_defs.get(&name) {
                // Check if definition changed
                if !self.expressions_equal(old_expr, &new_expr) {
                    changes.push((name, new_expr));
                }
            } else {
                // New definition
                changes.push((name, new_expr));
            }
        }
        
        Ok(changes)
    }
    
    /// Extract all definitions from expressions
    fn extract_definitions(&self, exprs: &[Value]) -> HashMap<Symbol, Value> {
        let mut defs = HashMap::new();
        
        for expr in exprs {
            if let Value::List(items) = expr {
                if items.len() >= 3 {
                    if let Value::Symbol(sym) = &items[0] {
                        if sym.0 == "define" {
                            if let Value::Symbol(name) = &items[1] {
                                // Store the entire define expression
                                defs.insert(name.clone(), expr.clone());
                            }
                        }
                    }
                }
            }
        }
        
        defs
    }
    
    /// Check if two expressions are equal
    fn expressions_equal(&self, a: &Value, b: &Value) -> bool {
        // Simple equality check - could be made more sophisticated
        a == b
    }
    
    /// Evaluate source code
    fn evaluate_source(&mut self, source: &str) -> Result<()> {
        let mut parser = Parser::new(source);
        let expressions = parser.parse()
            .map_err(|e| anyhow::anyhow!("Failed to parse XRL source: {}", e))?;
        
        for expr in expressions {
            self.evaluator.eval_live(&expr)
                .map_err(|e| anyhow::anyhow!("Failed to evaluate expression: {:?} - {}", expr, e))?;
        }
        
        Ok(())
    }
    
    /// Extract scene data from the VM
    fn extract_scene_data(&self) -> SceneData {
        let mut scene_data = SceneData::default();
        
        // Add default camera controls for XRL scenes
        scene_data.input = Some(gpu::scene::InputData {
            camera_controls: Some(gpu::scene::CameraControlsData {
                move_speed: 5.0,
                rotate_speed: 2.0,
                movement_keys: gpu::scene::MovementKeysData {
                    forward: "W".to_string(),
                    backward: "S".to_string(),
                    left: "A".to_string(),
                    right: "D".to_string(),
                    up: "Space".to_string(),
                    down: "Shift".to_string(),
                },
                rotation_keys: gpu::scene::RotationKeysData {
                    pitch_up: "Up".to_string(),
                    pitch_down: "Down".to_string(),
                    yaw_left: "Left".to_string(),
                    yaw_right: "Right".to_string(),
                },
                orbit_controls: Some(gpu::scene::OrbitControlsData {
                    enabled: true,
                    sensitivity: 1.0,
                    damping: 0.05,
                    min_distance: 1.0,
                    max_distance: 50.0,
                    min_polar_angle: 0.1,
                    max_polar_angle: 3.0,
                    enable_zoom: true,
                    zoom_speed: 1.0,
                }),
            }),
        });
        
        SCENE.with(|scene| {
            let scene = scene.borrow();
            
            // Convert cameras
            if let Some((_, camera)) = scene.cameras.iter().next() {
                scene_data.camera = Some(CameraData {
                    position: Vec3::new(camera.position.x, camera.position.y, camera.position.z),
                    target: Vec3::new(camera.target.x, camera.target.y, camera.target.z),
                    fov: camera.fov,
                    meta: None,
                });
            }
            
            // Convert scene nodes to entities
            for (ObjectId(id), node) in scene.nodes.iter() {
                if let Some(mesh_type) = &node.mesh_type {
                    let primitive = match mesh_type.as_str() {
                        "cube" => PrimitiveType::Box { 
                            width: 1.0, 
                            height: 1.0, 
                            depth: 1.0 
                        },
                        "sphere" => PrimitiveType::Sphere { 
                            radius: 0.5, 
                            segments: 32 
                        },
                        _ => PrimitiveType::Box { 
                            width: 1.0, 
                            height: 1.0, 
                            depth: 1.0 
                        },
                    };
                    
                    let mut transform = Transform::default();
                    transform.position = Vec3::new(
                        node.transform.position.x,
                        node.transform.position.y,
                        node.transform.position.z,
                    );
                    transform.rotation = Quat::from_euler(
                        node.transform.rotation.x.to_radians(),
                        node.transform.rotation.y.to_radians(),
                        node.transform.rotation.z.to_radians()
                    );
                    transform.scale = Vec3::new(
                        node.transform.scale.x,
                        node.transform.scale.y,
                        node.transform.scale.z,
                    );
                    
                    scene_data.entities.push(Entity {
                        id: format!("object_{}", id),
                        name: node.name.clone(),
                        mesh: MeshSource::Primitive(primitive),
                        transform,
                        material: None,
                        behavior: None,
                        children: Vec::new(),
                        parent: None,
                        components: Vec::new(),
                        meta: None,
                    });
                }
            }
        });
        
        scene_data
    }
    
    /// Get a snapshot of the current VM state
    pub fn snapshot(&self) -> LiveSnapshot {
        self.evaluator.snapshot()
    }
    
    /// Restore from a snapshot
    pub fn restore(&mut self, snapshot: LiveSnapshot) {
        self.evaluator.restore(snapshot);
    }
    
    /// Get update history
    pub fn get_update_history(&self) -> &[UpdateEvent] {
        &self.update_history
    }
    
    /// Clear update history
    pub fn clear_history(&mut self) {
        self.update_history.clear();
    }
}
//! Hot-reload system for live DSL development

use anyhow::Result;
use notify::{Event, RecommendedWatcher, RecursiveMode, Watcher};
use std::path::Path;
use std::sync::mpsc::{self, Receiver};
use std::time::{Duration, Instant};

#[derive(Debug, Clone)]
pub enum SceneUpdate {
    Full(gpu::SceneData),
    Incremental { 
        scene_data: gpu::SceneData, 
        changes: Vec<gpu::reconciliation::SceneChange> 
    },
}

pub struct HotReloader {
    _watcher: RecommendedWatcher,
    pub receiver: Receiver<String>,
    last_reload: Instant,
    debounce_duration: Duration,
}

impl HotReloader {
    pub fn new<P: AsRef<Path>>(watch_path: P) -> Result<Self> {
        let (tx, rx) = mpsc::channel();
        
        let mut watcher = notify::recommended_watcher(move |res: Result<Event, notify::Error>| {
            match res {
                Ok(event) => {
                    if let Some(path) = event.paths.first() {
                        if let Some(extension) = path.extension() {
                            if extension == "xrdsl" {
                                if let Some(path_str) = path.to_str() {
                                    let _ = tx.send(path_str.to_string());
                                }
                            }
                        }
                    }
                }
                Err(e) => log::error!("File watcher error: {:?}", e),
            }
        })?;

        watcher.watch(watch_path.as_ref(), RecursiveMode::Recursive)?;
        
        println!("📁 Watching for changes in: {}", watch_path.as_ref().display());
        
        Ok(Self {
            _watcher: watcher,
            receiver: rx,
            last_reload: Instant::now(),
            debounce_duration: Duration::from_millis(500),
        })
    }
    
    pub fn check_for_changes(&mut self) -> Option<String> {
        // Check if we have any pending changes
        let mut latest_file = None;
        
        // Drain all pending messages, keeping only the latest
        while let Ok(file_path) = self.receiver.try_recv() {
            latest_file = Some(file_path);
        }
        
        // Apply debouncing to avoid reloading too frequently
        if let Some(file_path) = latest_file {
            let now = Instant::now();
            if now.duration_since(self.last_reload) >= self.debounce_duration {
                self.last_reload = now;
                return Some(file_path);
            }
        }
        
        None
    }
}

pub struct SceneLoader {
    current_scene: Option<String>,
    reconciler: gpu::reconciliation::SceneReconciler,
    last_scene_data: Option<gpu::SceneData>,
}

impl SceneLoader {
    pub fn new() -> Self {
        Self {
            current_scene: None,
            reconciler: gpu::reconciliation::SceneReconciler::new(),
            last_scene_data: None,
        }
    }
    
    pub fn load_scene_from_file(&mut self, file_path: &str) -> Result<Option<SceneUpdate>> {
        println!("🔄 Reloading scene from: {}", file_path);
        
        // Read the file
        let content = std::fs::read_to_string(file_path)?;
        
        // Check if content has changed
        if self.current_scene.as_ref() == Some(&content) {
            return Ok(None); // No change
        }
        
        self.current_scene = Some(content.clone());
        
        // Parse DSL content with detailed error reporting
        println!("🔍 Parsing DSL content ({} chars)...", content.len());
        match dsl::parse(&content) {
            Ok(ast) => {
                println!("✅ DSL Parse successful! Found {} top-level items", ast.len());
                
                // Log what we found
                for (i, item) in ast.iter().enumerate() {
                    match item {
                        dsl::ast::Top::Behavior(b) => println!("  [{i}] Behavior: '{}' with {} state vars", b.name, b.state.len()),
                        dsl::ast::Top::Scene3D(s) => println!("  [{i}] Scene3D: '{}' with {} objects", s.name, s.objects.len()),
                    }
                }
                
                // Convert AST to scene data with error handling
                match self.extract_scene_from_ast_with_validation(&ast) {
                    Ok(Some(mut scene_data)) => {
                        println!("✅ Scene conversion successful!");
                        // Add the AST to the scene data for hot-swapping
                        scene_data.ast = ast.clone();
                        
                        // Perform reconciliation if we have a previous scene
                        let update = if let Some(ref last_scene) = self.last_scene_data {
                            let changes = self.reconciler.diff_scenes(last_scene, &scene_data);
                            println!("🔍 Reconciliation found {} changes", changes.len());
                            
                            // Log changes for debugging
                            for change in &changes {
                                match change {
                                    gpu::reconciliation::SceneChange::EntityAdded { entity } => {
                                        println!("  + Entity added: {}", entity.name);
                                    }
                                    gpu::reconciliation::SceneChange::EntityRemoved { id } => {
                                        println!("  - Entity removed: {}", id);
                                    }
                                    gpu::reconciliation::SceneChange::EntityModified { id, .. } => {
                                        println!("  ~ Entity modified: {}", id);
                                    }
                                    gpu::reconciliation::SceneChange::BehaviorModified { name, .. } => {
                                        println!("  ~ Behavior modified: {}", name);
                                    }
                                    _ => {}
                                }
                            }
                            
                            SceneUpdate::Incremental { scene_data, changes }
                        } else {
                            println!("📦 Initial scene load");
                            SceneUpdate::Full(scene_data)
                        };
                        
                        // Update stored scene
                        if let SceneUpdate::Full(ref data) = update {
                            self.last_scene_data = Some(data.clone());
                            self.reconciler.update_current_scene(data.clone());
                        } else if let SceneUpdate::Incremental { ref scene_data, .. } = update {
                            self.last_scene_data = Some(scene_data.clone());
                            self.reconciler.update_current_scene(scene_data.clone());
                        }
                        
                        return Ok(Some(update));
                    }
                    Ok(None) => {
                        println!("⚠️ No valid Scene3D found in AST");
                        println!("🔧 Available items: {:?}", ast.iter().map(|item| match item {
                            dsl::ast::Top::Behavior(b) => format!("Behavior({})", b.name),
                            dsl::ast::Top::Scene3D(s) => format!("Scene3D({})", s.name),
                        }).collect::<Vec<_>>());
                        
                        // Since parsing worked but no Scene3D found, create default scene
                        let fallback_scene = gpu::SceneData {
                            entities: Vec::new(),  // Empty entities for fallback
                            ui_elements: vec![],
                            behaviors: self.extract_behaviors_from_ast(&ast),
                            camera: None,
                            lighting: None,
                            input: None,
                            ast: ast.clone(),
                        };
                        
                        let update = if self.last_scene_data.is_some() {
                            let changes = self.reconciler.diff_scenes(self.last_scene_data.as_ref().unwrap(), &fallback_scene);
                            SceneUpdate::Incremental { scene_data: fallback_scene.clone(), changes }
                        } else {
                            SceneUpdate::Full(fallback_scene.clone())
                        };
                        
                        self.last_scene_data = Some(fallback_scene.clone());
                        self.reconciler.update_current_scene(fallback_scene);
                        return Ok(Some(update));
                    }
                    Err(validation_error) => {
                        println!("❌ Scene validation failed: {}", validation_error);
                        println!("🔧 Creating fallback scene with parsed behaviors");
                        
                        let fallback_scene = gpu::SceneData {
                            entities: Vec::new(),  // Empty entities for fallback
                            ui_elements: vec![],
                            behaviors: self.extract_behaviors_from_ast(&ast),
                            camera: None,
                            lighting: None,
                            input: None,
                            ast: ast.clone(),
                        };
                        
                        let update = if self.last_scene_data.is_some() {
                            let changes = self.reconciler.diff_scenes(self.last_scene_data.as_ref().unwrap(), &fallback_scene);
                            SceneUpdate::Incremental { scene_data: fallback_scene.clone(), changes }
                        } else {
                            SceneUpdate::Full(fallback_scene.clone())
                        };
                        
                        self.last_scene_data = Some(fallback_scene.clone());
                        self.reconciler.update_current_scene(fallback_scene);
                        return Ok(Some(update));
                    }
                }
            }
            Err(e) => {
                println!("❌ DSL Parse Error Details:");
                println!("   Error: {:?}", e);
                println!("   File: {}", file_path);
                println!("   Content preview (first 200 chars):");
                println!("   │ {}", content.chars().take(200).collect::<String>().replace('\n', "\n   │ "));
                
                // Try to give more context about where the error occurred
                if let Some(error_str) = format!("{:?}", e).split("at line").nth(1) {
                    if let Some(line_num_str) = error_str.split_whitespace().next() {
                        if let Ok(line_num) = line_num_str.parse::<usize>() {
                            println!("   Context around line {}:", line_num);
                            let lines: Vec<&str> = content.lines().collect();
                            let start = if line_num > 3 { line_num - 3 } else { 0 };
                            let end = std::cmp::min(line_num + 3, lines.len());
                            for i in start..end {
                                let marker = if i + 1 == line_num { ">>> " } else { "    " };
                                println!("   {}{:3}: {}", marker, i + 1, lines.get(i).unwrap_or(&""));
                            }
                        }
                    }
                }
                
                println!("🔧 Creating fallback scene due to parse error");
                let fallback_scene = gpu::SceneData {
                    entities: Vec::new(),  // Empty entities for fallback
                    ui_elements: vec![],
                    behaviors: std::collections::HashMap::new(),
                    camera: None,
                    lighting: None,
                    input: None,
                    ast: Vec::new(),
                };
                
                let update = if self.last_scene_data.is_some() {
                    let changes = self.reconciler.diff_scenes(self.last_scene_data.as_ref().unwrap(), &fallback_scene);
                    SceneUpdate::Incremental { scene_data: fallback_scene.clone(), changes }
                } else {
                    SceneUpdate::Full(fallback_scene.clone())
                };
                
                self.last_scene_data = Some(fallback_scene.clone());
                self.reconciler.update_current_scene(fallback_scene);
                return Ok(Some(update));
            }
        }
    }
    
    fn extract_behaviors_from_ast(&self, ast: &Vec<dsl::ast::Top>) -> std::collections::HashMap<String, gpu::BehaviorData> {
        let mut behaviors = std::collections::HashMap::new();
        
        for item in ast {
            if let dsl::ast::Top::Behavior(behavior) = item {
                let mut state = std::collections::HashMap::new();
                for (key, value) in &behavior.state {
                    state.insert(key.clone(), *value);
                }
                
                behaviors.insert(behavior.name.clone(), gpu::BehaviorData {
                    name: behavior.name.clone(),
                    state,
                });
                println!("📋 Extracted behavior: '{}' with state {:?}", behavior.name, behavior.state);
            }
        }
        
        behaviors
    }
    
    fn extract_scene_from_ast_with_validation(&self, ast: &Vec<dsl::ast::Top>) -> Result<Option<gpu::SceneData>> {
        let behaviors = self.extract_behaviors_from_ast(ast);
        
        // Find Scene3D
        for item in ast {
            if let dsl::ast::Top::Scene3D(scene) = item {
                println!("🎬 Processing Scene3D: '{}'", scene.name);
                
                // Validate all behavior references
                for obj in &scene.objects {
                    if let Some(ref behavior_name) = obj.behavior {
                        if !behaviors.contains_key(behavior_name) {
                            return Err(anyhow::anyhow!(
                                "Object '{}' references undefined behavior '{}'. Available behaviors: [{}]",
                                obj.name,
                                behavior_name,
                                behaviors.keys().cloned().collect::<Vec<_>>().join(", ")
                            ));
                        }
                        println!("✓ Object '{}' → behavior '{}' (valid)", obj.name, behavior_name);
                    }
                }
                
                let scene_data = self.convert_scene_to_data_with_validation(scene, &behaviors, ast)?;
                return Ok(Some(scene_data));
            }
        }
        
        Ok(None)
    }
    
    fn convert_scene_to_data_with_validation(&self, scene: &dsl::ast::Scene3D, behaviors: &std::collections::HashMap<String, gpu::BehaviorData>, ast: &[dsl::ast::Top]) -> Result<gpu::SceneData> {
        let mut entities = Vec::new();
        let mut ui_elements = Vec::new();
        
        println!("🔧 Converting {} objects and {} UI elements to scene data", 
            scene.objects.len(), scene.ui_elements.len());
        
        // Extract entities from scene
        for object in &scene.objects {
            println!("  Processing object: '{}' ({})", object.name, object.mesh_type);
            
            // Determine mesh source - either a primitive or a model file
            let mesh_source = if let Some(primitive) = gpu::entity::PrimitiveType::from_type_string(&object.mesh_type) {
                // It's a primitive
                gpu::entity::MeshSource::Primitive(primitive)
            } else if object.mesh_type.contains('.') {
                // It looks like a file path - detect format and create model source
                let path = std::path::PathBuf::from(&object.mesh_type);
                if let Some(model_source) = gpu::model_loader::detect_format(&path) {
                    gpu::entity::MeshSource::Model(model_source)
                } else {
                    println!("  ⚠️ Unknown model format for: {}", object.mesh_type);
                    continue; // Skip this object
                }
            } else {
                println!("  ⚠️ Unknown mesh type: {}", object.mesh_type);
                continue; // Skip this object
            };
            
            // Now create the entity with the determined mesh source
            let meta = object.meta.as_ref().map(|m| gpu::entity::MetaDirective {
                preserve_mode: m.preserve_mode.clone(),
                properties: m.properties.clone(),
            });
            
            let entity = gpu::entity::Entity {
                id: object.name.clone(),
                name: object.name.clone(),
                mesh: mesh_source,
                transform: gpu::entity::Transform {
                    position: gpu::Vec3::from(object.transform.position),
                    rotation: gpu::Quat::IDENTITY,  // TODO: Parse rotation
                    scale: gpu::Vec3::from(object.transform.scale),
                },
                material: gpu::entity::Material {
                    color: [0.8, 0.8, 0.8, 1.0],  // TODO: Parse from material def
                    ..gpu::entity::Material::default()
                },
                behavior: object.behavior.clone(),
                children: Vec::new(),
                parent: None,
                components: Vec::new(),
                meta,
            };
            
            println!("    ✓ Entity '{}': type={:?}, pos({:.1}, {:.1}, {:.1}), behavior: {:?}", 
                entity.name, object.mesh_type,
                entity.transform.position.x, entity.transform.position.y, entity.transform.position.z,
                entity.behavior);
            
            entities.push(entity);
        }
        
        // Parse camera if available
        let camera = scene.camera.as_ref().map(|cam| {
            println!("📹 Camera: pos({:.1}, {:.1}, {:.1}), target({:.1}, {:.1}, {:.1}), fov: {:.1}°", 
                cam.position[0], cam.position[1], cam.position[2],
                cam.target[0], cam.target[1], cam.target[2],
                cam.fov.to_degrees());
            
            let meta = cam.meta.as_ref().map(|m| gpu::entity::MetaDirective {
                preserve_mode: m.preserve_mode.clone(),
                properties: m.properties.clone(),
            });
            
            if let Some(ref meta) = meta {
                println!("    📌 Camera Meta: preserve_mode={}, properties={:?}", 
                    meta.preserve_mode, meta.properties);
            }
            
            gpu::CameraData {
                position: gpu::Vec3::from(cam.position),
                target: gpu::Vec3::from(cam.target),
                fov: cam.fov,
                meta,
            }
        });
        
        // Parse lighting if available
        let lighting = scene.lighting.as_ref().map(|light| {
            println!("💡 Lighting: ambient({:.1}, {:.1}, {:.1})", 
                light.ambient[0], light.ambient[1], light.ambient[2]);
            if let Some(dir) = &light.directional {
                println!("    ☀️ Directional: dir({:.1}, {:.1}, {:.1}), color({:.1}, {:.1}, {:.1}), intensity: {:.1}", 
                    dir.direction[0], dir.direction[1], dir.direction[2],
                    dir.color[0], dir.color[1], dir.color[2],
                    dir.intensity);
            }
            gpu::LightingData {
                ambient: gpu::Vec3::from(light.ambient),
                directional_direction: light.directional.as_ref().map_or(gpu::Vec3::new(1.0, 1.0, 1.0), |d| gpu::Vec3::from(d.direction)),
                directional_color: light.directional.as_ref().map_or(gpu::Vec3::ONE, |d| gpu::Vec3::from(d.color)),
                directional_intensity: light.directional.as_ref().map_or(1.0, |d| d.intensity),
            }
        });
        
        // Parse UI elements
        for ui in &scene.ui_elements {
            println!("  📱 UI Element: '{}' ({})", ui.name, ui.ui_type);
            
            let ui_data = gpu::UIElementData {
                name: ui.name.clone(),
                ui_type: ui.ui_type.clone(),
                position: gpu::Vec3::from(ui.position),
                size: ui.size,
                text: ui.text.clone(),
                color: ui.color,
                behavior: ui.behavior.clone(),
            };
            
            if let Some(ref text) = ui.text {
                println!("    📝 Text: '{}'", text);
            }
            println!("    📍 Position: ({:.1}, {:.1}, {:.1})", ui.position[0], ui.position[1], ui.position[2]);
            
            ui_elements.push(ui_data);
        }
        
        // No fallback needed for entities
        
        // Convert input configuration
        let input = scene.input.as_ref().map(|input_def| {
            gpu::InputData {
                camera_controls: input_def.camera_controls.as_ref().map(|controls| {
                    gpu::CameraControlsData {
                        move_speed: controls.move_speed,
                        rotate_speed: controls.rotate_speed,
                        movement_keys: gpu::MovementKeysData {
                            forward: controls.movement_keys.forward.clone(),
                            backward: controls.movement_keys.backward.clone(),
                            left: controls.movement_keys.left.clone(),
                            right: controls.movement_keys.right.clone(),
                            up: controls.movement_keys.up.clone(),
                            down: controls.movement_keys.down.clone(),
                        },
                        rotation_keys: gpu::RotationKeysData {
                            pitch_up: controls.rotation_keys.pitch_up.clone(),
                            pitch_down: controls.rotation_keys.pitch_down.clone(),
                            yaw_left: controls.rotation_keys.yaw_left.clone(),
                            yaw_right: controls.rotation_keys.yaw_right.clone(),
                        },
                        orbit_controls: controls.orbit_controls.as_ref().map(|oc| {
                            gpu::OrbitControlsData {
                                enabled: oc.enabled,
                                sensitivity: oc.sensitivity,
                                damping: oc.damping,
                                min_distance: oc.min_distance,
                                max_distance: oc.max_distance,
                                min_polar_angle: oc.min_polar_angle,
                                max_polar_angle: oc.max_polar_angle,
                                enable_zoom: oc.enable_zoom,
                                zoom_speed: oc.zoom_speed,
                            }
                        }),
                    }
                }),
            }
        });
        
        println!("✅ Scene conversion complete: {} entities, {} UI elements, {} behaviors", 
            entities.len(), ui_elements.len(), behaviors.len());
        
        Ok(gpu::SceneData { 
            entities,
            ui_elements,
            behaviors: behaviors.clone(),
            camera,
            lighting,
            input,
            ast: ast.to_vec(),
        })
    }

    fn extract_scene_from_ast(&self, ast: &Vec<dsl::ast::Top>) -> Option<gpu::SceneData> {
        let mut behaviors = std::collections::HashMap::new();
        let mut scene_data = None;
        
        // First pass: extract all behaviors
        for item in ast {
            if let dsl::ast::Top::Behavior(behavior) = item {
                let mut state = std::collections::HashMap::new();
                for (key, value) in &behavior.state {
                    state.insert(key.clone(), *value);
                }
                
                behaviors.insert(behavior.name.clone(), gpu::BehaviorData {
                    name: behavior.name.clone(),
                    state,
                });
            }
        }
        
        // Second pass: extract scene
        for item in ast {
            if let dsl::ast::Top::Scene3D(scene) = item {
                scene_data = Some(self.convert_scene_to_data(scene, &behaviors, ast));
                break;
            }
        }
        
        scene_data
    }
    
    fn convert_scene_to_data(&self, scene: &dsl::ast::Scene3D, behaviors: &std::collections::HashMap<String, gpu::BehaviorData>, ast: &[dsl::ast::Top]) -> gpu::SceneData {
        let mut entities = Vec::new();
        
        // Extract entities from scene
        for object in &scene.objects {
            // Determine mesh source - either a primitive or a model file
            let mesh_source = if let Some(primitive) = gpu::entity::PrimitiveType::from_type_string(&object.mesh_type) {
                // It's a primitive
                gpu::entity::MeshSource::Primitive(primitive)
            } else if object.mesh_type.contains('.') {
                // It looks like a file path - detect format and create model source
                let path = std::path::PathBuf::from(&object.mesh_type);
                if let Some(model_source) = gpu::model_loader::detect_format(&path) {
                    gpu::entity::MeshSource::Model(model_source)
                } else {
                    continue; // Skip unknown formats
                }
            } else {
                continue; // Skip unknown mesh types
            };
            
            {
                let meta = object.meta.as_ref().map(|m| gpu::entity::MetaDirective {
                    preserve_mode: m.preserve_mode.clone(),
                    properties: m.properties.clone(),
                });
                
                let entity = gpu::entity::Entity {
                    id: object.name.clone(),
                    name: object.name.clone(),
                    mesh: mesh_source,
                    transform: gpu::entity::Transform {
                        position: gpu::Vec3::from(object.transform.position),
                        rotation: gpu::Quat::IDENTITY,
                        scale: gpu::Vec3::from(object.transform.scale),
                    },
                    material: gpu::entity::Material {
                        color: self.generate_color_for_entity(&object.name),
                        ..gpu::entity::Material::default()
                    },
                    behavior: object.behavior.clone(),
                    children: Vec::new(),
                    parent: None,
                    components: Vec::new(),
                    meta,
                };
                
                entities.push(entity);
            }
        }
        
        // Parse camera if available
        let camera = scene.camera.as_ref().map(|cam| {
            let meta = cam.meta.as_ref().map(|m| gpu::entity::MetaDirective {
                preserve_mode: m.preserve_mode.clone(),
                properties: m.properties.clone(),
            });
            
            gpu::CameraData {
                position: gpu::Vec3::from(cam.position),
                target: gpu::Vec3::from(cam.target),
                fov: cam.fov,
                meta,
            }
        });
        
        // Parse lighting if available
        let lighting = scene.lighting.as_ref().map(|light| gpu::LightingData {
            ambient: gpu::Vec3::from(light.ambient),
            directional_direction: light.directional.as_ref().map_or(gpu::Vec3::new(1.0, 1.0, 1.0), |d| gpu::Vec3::from(d.direction)),
            directional_color: light.directional.as_ref().map_or(gpu::Vec3::ONE, |d| gpu::Vec3::from(d.color)),
            directional_intensity: light.directional.as_ref().map_or(1.0, |d| d.intensity),
        });
        
        // No fallback needed for entities
        
        gpu::SceneData { 
            entities,
            ui_elements: vec![],
            behaviors: behaviors.clone(),
            camera,
            lighting,
            input: None,
            ast: ast.to_vec(),
        }
    }
    
    fn generate_color_for_entity(&self, name: &str) -> [f32; 4] {
        // Generate consistent colors based on entity name
        let hash = name.chars().map(|c| c as u32).sum::<u32>();
        [
            0.3 + ((hash * 17) % 100) as f32 / 150.0,
            0.3 + ((hash * 31) % 100) as f32 / 150.0,
            0.3 + ((hash * 43) % 100) as f32 / 150.0,
            1.0,
        ]
    }
    
    fn parse_vec3_from_transform(&self, transform: &dsl::ast::TransformDef, property: &str) -> Option<gpu::Vec3> {
        // Simple parsing based on transform definition
        // This is a simplified version - in reality we'd need full transform parsing
        match property {
            "position" => Some(gpu::Vec3::from(transform.position)),
            "scale" => Some(gpu::Vec3::from(transform.scale)),
            _ => None,
        }
    }
    
}
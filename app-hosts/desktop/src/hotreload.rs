//! Hot-reload system for live DSL development

use anyhow::Result;
use notify::{Event, RecommendedWatcher, RecursiveMode, Watcher};
use std::path::Path;
use std::sync::mpsc::{self, Receiver};
use std::time::{Duration, Instant};

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
}

impl SceneLoader {
    pub fn new() -> Self {
        Self {
            current_scene: None,
        }
    }
    
    pub fn load_scene_from_file(&mut self, file_path: &str) -> Result<Option<gpu::SceneData>> {
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
                    Ok(Some(scene_data)) => {
                        println!("✅ Scene conversion successful!");
                        return Ok(Some(scene_data));
                    }
                    Ok(None) => {
                        println!("⚠️ No valid Scene3D found in AST");
                        println!("🔧 Available items: {:?}", ast.iter().map(|item| match item {
                            dsl::ast::Top::Behavior(b) => format!("Behavior({})", b.name),
                            dsl::ast::Top::Scene3D(s) => format!("Scene3D({})", s.name),
                        }).collect::<Vec<_>>());
                        
                        // Since parsing worked but no Scene3D found, create default scene
                        let fallback_scene = gpu::SceneData {
                            cubes: self.parse_cubes_from_raw_scene(),
                            ui_elements: vec![],
                            behaviors: self.extract_behaviors_from_ast(&ast),
                            camera: None,
                            lighting: None,
                        };
                        return Ok(Some(fallback_scene));
                    }
                    Err(validation_error) => {
                        println!("❌ Scene validation failed: {}", validation_error);
                        println!("🔧 Creating fallback scene with parsed behaviors");
                        
                        let fallback_scene = gpu::SceneData {
                            cubes: self.parse_cubes_from_raw_scene(),
                            ui_elements: vec![],
                            behaviors: self.extract_behaviors_from_ast(&ast),
                            camera: None,
                            lighting: None,
                        };
                        return Ok(Some(fallback_scene));
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
                    cubes: self.parse_cubes_from_raw_scene(),
                    ui_elements: vec![],
                    behaviors: std::collections::HashMap::new(),
                    camera: None,
                    lighting: None,
                };
                return Ok(Some(fallback_scene));
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
                
                let scene_data = self.convert_scene_to_data_with_validation(scene, &behaviors)?;
                return Ok(Some(scene_data));
            }
        }
        
        Ok(None)
    }
    
    fn convert_scene_to_data_with_validation(&self, scene: &dsl::ast::Scene3D, behaviors: &std::collections::HashMap<String, gpu::BehaviorData>) -> Result<gpu::SceneData> {
        let mut cubes = Vec::new();
        let mut ui_elements = Vec::new();
        
        println!("🔧 Converting {} objects and {} UI elements to scene data", 
            scene.objects.len(), scene.ui_elements.len());
        
        // Extract cube objects from scene
        for object in &scene.objects {
            println!("  Processing object: '{}' ({})", object.name, object.mesh_type);
            
            if object.mesh_type == "cube" {
                let cube = gpu::CubeData {
                    name: object.name.clone(),
                    position: gpu::Vec3::from(object.transform.position),
                    scale: gpu::Vec3::from(object.transform.scale),
                    color: self.generate_color_for_cube(&object.name),
                    behavior: object.behavior.clone(),
                };
                
                println!("    ✓ Cube: pos({:.1}, {:.1}, {:.1}), scale({:.1}, {:.1}, {:.1}), behavior: {:?}", 
                    cube.position.x, cube.position.y, cube.position.z,
                    cube.scale.x, cube.scale.y, cube.scale.z,
                    cube.behavior);
                
                cubes.push(cube);
            } else if object.mesh_type == "plane" {
                println!("    ⊡ Plane object '{}' - currently not rendered but parsed", object.name);
            } else {
                println!("    ⚠️ Unknown mesh type '{}' for object '{}'", object.mesh_type, object.name);
            }
        }
        
        // Parse camera if available
        let camera = scene.camera.as_ref().map(|cam| {
            println!("📹 Camera: pos({:.1}, {:.1}, {:.1}), target({:.1}, {:.1}, {:.1}), fov: {:.1}°", 
                cam.position[0], cam.position[1], cam.position[2],
                cam.target[0], cam.target[1], cam.target[2],
                cam.fov.to_degrees());
            gpu::CameraData {
                position: gpu::Vec3::from(cam.position),
                target: gpu::Vec3::from(cam.target),
                fov: cam.fov,
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
        
        // If no cubes found, fall back to text parsing
        if cubes.is_empty() {
            println!("⚠️ No cubes found in Scene3D, falling back to text parsing");
            cubes = self.parse_cubes_from_raw_scene();
        }
        
        println!("✅ Scene conversion complete: {} cubes, {} UI elements, {} behaviors", 
            cubes.len(), ui_elements.len(), behaviors.len());
        
        Ok(gpu::SceneData { 
            cubes,
            ui_elements,
            behaviors: behaviors.clone(),
            camera,
            lighting,
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
                scene_data = Some(self.convert_scene_to_data(scene, &behaviors));
                break;
            }
        }
        
        scene_data
    }
    
    fn convert_scene_to_data(&self, scene: &dsl::ast::Scene3D, behaviors: &std::collections::HashMap<String, gpu::BehaviorData>) -> gpu::SceneData {
        let mut cubes = Vec::new();
        
        // Extract cube objects from scene
        for object in &scene.objects {
            if object.mesh_type == "cube" {
                let cube = gpu::CubeData {
                    name: object.name.clone(),
                    position: gpu::Vec3::from(object.transform.position),
                    scale: gpu::Vec3::from(object.transform.scale),
                    color: self.generate_color_for_cube(&object.name),
                    behavior: object.behavior.clone(),
                };
                
                cubes.push(cube);
            }
        }
        
        // Parse camera if available
        let camera = scene.camera.as_ref().map(|cam| gpu::CameraData {
            position: gpu::Vec3::from(cam.position),
            target: gpu::Vec3::from(cam.target),
            fov: cam.fov,
        });
        
        // Parse lighting if available
        let lighting = scene.lighting.as_ref().map(|light| gpu::LightingData {
            ambient: gpu::Vec3::from(light.ambient),
            directional_direction: light.directional.as_ref().map_or(gpu::Vec3::new(1.0, 1.0, 1.0), |d| gpu::Vec3::from(d.direction)),
            directional_color: light.directional.as_ref().map_or(gpu::Vec3::ONE, |d| gpu::Vec3::from(d.color)),
            directional_intensity: light.directional.as_ref().map_or(1.0, |d| d.intensity),
        });
        
        // If no cubes found, fall back to text parsing
        if cubes.is_empty() {
            cubes = self.parse_cubes_from_raw_scene();
        }
        
        gpu::SceneData { 
            cubes,
            ui_elements: vec![],
            behaviors: behaviors.clone(),
            camera,
            lighting,
        }
    }
    
    fn generate_color_for_cube(&self, name: &str) -> gpu::Vec3 {
        // Generate consistent colors based on cube name
        let hash = name.chars().map(|c| c as u32).sum::<u32>();
        gpu::Vec3::new(
            0.3 + ((hash * 17) % 100) as f32 / 150.0,
            0.3 + ((hash * 31) % 100) as f32 / 150.0,
            0.3 + ((hash * 43) % 100) as f32 / 150.0,
        )
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
    
    fn parse_cubes_from_raw_scene(&self) -> Vec<gpu::CubeData> {
        if let Some(ref content) = self.current_scene {
            return self.parse_cubes_from_text(content);
        }
        
        // Fallback to default cubes  
        vec![
            gpu::CubeData {
                name: "cube1".to_string(),
                position: gpu::Vec3::new(-3.0, 0.0, 0.0),
                scale: gpu::Vec3::ONE,
                color: gpu::Vec3::new(1.0, 0.2, 0.2), // Red
                behavior: Some("spin".to_string()),
            },
            gpu::CubeData {
                name: "cube2".to_string(),
                position: gpu::Vec3::new(0.0, 0.0, 0.0),
                scale: gpu::Vec3::new(1.5, 1.5, 1.5),
                color: gpu::Vec3::new(0.2, 1.0, 0.2), // Green
                behavior: Some("spin".to_string()),
            },
        ]
    }
    
    fn parse_cubes_from_text(&self, content: &str) -> Vec<gpu::CubeData> {
        let mut cubes = Vec::new();
        let mut current_name = String::new();
        let mut current_position = None;
        let mut current_scale = None;
        let mut current_behavior = None;
        let mut parsing_object = false;
        let mut object_is_cube = false;
        
        // Simple regex-like parsing for object definitions
        for line in content.lines() {
            let line = line.trim();
            
            if line.starts_with("(object") && line.contains("cube") {
                parsing_object = true;
                object_is_cube = true;
                current_position = None;
                current_scale = None;
                current_behavior = None;
                
                // Extract object name
                let parts: Vec<&str> = line.split_whitespace().collect();
                if parts.len() >= 2 {
                    current_name = parts[1].to_string();
                }
            } else if parsing_object && line.starts_with("(position") {
                if let Some(pos) = self.parse_vec3_from_line(line) {
                    current_position = Some(pos);
                }
            } else if parsing_object && line.starts_with("(scale") {
                if let Some(scale) = self.parse_vec3_from_line(line) {
                    current_scale = Some(scale);
                }
            } else if parsing_object && line.starts_with("(behavior") {
                let parts: Vec<&str> = line.split_whitespace().collect();
                if parts.len() >= 2 {
                    current_behavior = Some(parts[1].replace(")", ""));
                }
            } else if parsing_object && line.contains(")") && !line.contains("(") {
                // End of object definition
                if object_is_cube {
                    cubes.push(gpu::CubeData {
                        name: if current_name.is_empty() { format!("cube{}", cubes.len() + 1) } else { current_name.clone() },
                        position: current_position.unwrap_or(gpu::Vec3::ZERO),
                        scale: current_scale.unwrap_or(gpu::Vec3::ONE),
                        color: gpu::Vec3::new(
                            0.5 + (cubes.len() as f32 * 0.3) % 1.0,
                            0.3 + (cubes.len() as f32 * 0.5) % 1.0,
                            0.7 + (cubes.len() as f32 * 0.7) % 1.0,
                        ),
                        behavior: current_behavior.clone(),
                    });
                }
                parsing_object = false;
                object_is_cube = false;
            }
        }
        
        println!("📦 Parsed {} cubes from raw text", cubes.len());
        cubes
    }
    
    fn parse_vec3_from_line(&self, line: &str) -> Option<gpu::Vec3> {
        // Parse lines like "(position -4 0 0)" or "(scale 1.2 1.2 1.2)"
        let parts: Vec<&str> = line.split_whitespace().collect();
        if parts.len() >= 4 {
            if let (Ok(x), Ok(y), Ok(z)) = (
                parts[1].parse::<f32>(),
                parts[2].parse::<f32>(),
                parts[3].replace(")", "").parse::<f32>()
            ) {
                return Some(gpu::Vec3::new(x, y, z));
            }
        }
        None
    }
}
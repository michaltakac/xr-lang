//! Runtime state preservation system for hot-reload

use crate::math::Vec3;
use crate::scene::{CameraData, SceneData};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct RuntimeState {
    pub camera_overrides: Option<CameraState>,
    pub object_overrides: HashMap<String, Transform>,
    pub preserve_flags: HashMap<String, PreserveMode>,
    pub authoring_mode: AuthoringMode,
}

#[derive(Debug, Clone, Copy)]
pub struct CameraState {
    pub position: Vec3,
    pub target: Vec3,
    pub fov: f32,
}

#[derive(Debug, Clone, Copy)]
pub struct Transform {
    pub position: Vec3,
    pub rotation: (f32, f32, f32, f32), // Quaternion as tuple
    pub scale: Vec3,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PreserveMode {
    Reset,      // Always reset on reload (default)
    Preserve,   // Keep runtime changes  
    Sync,       // Write changes back to DSL
    Volatile,   // Never persist
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AuthoringMode {
    Design,  // All changes reset on reload (current behavior)
    Play,    // Runtime changes preserved, not saved
    Live,    // Runtime changes sync to code
    Replay,  // Playback recorded interactions
}

impl Default for RuntimeState {
    fn default() -> Self {
        Self {
            camera_overrides: None,
            object_overrides: HashMap::new(),
            preserve_flags: HashMap::new(),
            authoring_mode: AuthoringMode::Play,  // Default to Play mode for camera preservation
        }
    }
}

impl RuntimeState {
    pub fn new() -> Self {
        Self::default()
    }
    
    pub fn toggle_preservation_mode(&mut self) {
        self.authoring_mode = match self.authoring_mode {
            AuthoringMode::Design => AuthoringMode::Play,
            AuthoringMode::Play => AuthoringMode::Live,
            AuthoringMode::Live => AuthoringMode::Design,
            AuthoringMode::Replay => AuthoringMode::Design,
        };
        println!("🔄 Authoring mode: {:?}", self.authoring_mode);
    }
    
    pub fn preserve_camera(&mut self, position: Vec3, target: Vec3, fov: f32) {
        if matches!(self.authoring_mode, AuthoringMode::Play | AuthoringMode::Live) {
            self.camera_overrides = Some(CameraState {
                position,
                target,
                fov,
            });
        }
    }
    
    pub fn should_preserve_camera(&self) -> bool {
        matches!(self.authoring_mode, AuthoringMode::Play | AuthoringMode::Live)
    }
    
    pub fn apply_to_scene(&self, scene: &mut SceneData) {
        // Apply camera overrides if preservation is enabled
        if let Some(camera_state) = &self.camera_overrides {
            if self.should_preserve_camera() {
                if let Some(ref mut camera) = scene.camera {
                    camera.position = camera_state.position;
                    camera.target = camera_state.target;
                    camera.fov = camera_state.fov;
                } else {
                    // Create camera data if it doesn't exist
                    scene.camera = Some(CameraData {
                        position: camera_state.position,
                        target: camera_state.target,
                        fov: camera_state.fov,
                        meta: None,  // Preserve the existing meta
                    });
                }
                println!("📌 Applied preserved camera state");
            }
        }
        
        // Apply object overrides based on meta directives
        for (id, transform) in &self.object_overrides {
            if let Some(preserve_mode) = self.preserve_flags.get(id) {
                if matches!(preserve_mode, PreserveMode::Preserve | PreserveMode::Sync) {
                    // Apply transform to matching entities
                    for entity in &mut scene.entities {
                        if entity.id == *id {
                            entity.transform.position = transform.position;
                            entity.transform.scale = transform.scale;
                            // TODO: Apply rotation quaternion
                        }
                    }
                }
            }
        }
    }
    
    pub fn extract_from_scene(&mut self, scene: &SceneData) {
        // Extract camera state if preservation is enabled
        if self.should_preserve_camera() {
            if let Some(ref camera) = scene.camera {
                self.preserve_camera(camera.position, camera.target, camera.fov);
            }
        }
        
        // Extract object states based on meta directives
        for entity in &scene.entities {
            if let Some(preserve_mode) = self.preserve_flags.get(&entity.id) {
                if matches!(preserve_mode, PreserveMode::Preserve | PreserveMode::Sync) {
                    self.object_overrides.insert(
                        entity.id.clone(),
                        Transform {
                            position: entity.transform.position,
                            rotation: (0.0, 0.0, 0.0, 1.0), // TODO: Convert from Quat
                            scale: entity.transform.scale,
                        },
                    );
                }
            }
        }
    }
    
    pub fn set_preserve_mode(&mut self, object_id: &str, mode: PreserveMode) {
        self.preserve_flags.insert(object_id.to_string(), mode);
    }
    
    pub fn clear(&mut self) {
        self.camera_overrides = None;
        self.object_overrides.clear();
    }
}
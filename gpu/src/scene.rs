//! Scene data structures for DSL-driven 3D scenes

use crate::math::*;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct SceneData {
    pub cubes: Vec<CubeData>,
    pub ui_elements: Vec<UIElementData>,
    pub behaviors: HashMap<String, BehaviorData>,
    pub camera: Option<CameraData>,
    pub lighting: Option<LightingData>,
}

#[derive(Debug, Clone)]
pub struct CubeData {
    pub name: String,
    pub position: Vec3,
    pub scale: Vec3,
    pub color: Vec3,
    pub behavior: Option<String>,
}

#[derive(Debug, Clone)]
pub struct BehaviorData {
    pub name: String,
    pub state: HashMap<String, f32>,
}

#[derive(Debug, Clone)]
pub struct CameraData {
    pub position: Vec3,
    pub target: Vec3,
    pub fov: f32,
}

#[derive(Debug, Clone)]
pub struct LightingData {
    pub ambient: Vec3,
    pub directional_direction: Vec3,
    pub directional_color: Vec3,
    pub directional_intensity: f32,
}

#[derive(Debug, Clone)]
pub struct UIElementData {
    pub name: String,
    pub ui_type: String,
    pub position: Vec3,
    pub size: Vec2,
    pub text: Option<String>,
    pub color: Vec4,
    pub behavior: Option<String>,
}

impl Default for SceneData {
    fn default() -> Self {
        let mut behaviors = HashMap::new();
        let mut spin_state = HashMap::new();
        spin_state.insert("speed".to_string(), 1.0);
        behaviors.insert("spin".to_string(), BehaviorData {
            name: "spin".to_string(),
            state: spin_state,
        });

        Self {
            cubes: vec![
                CubeData {
                    name: "cube1".to_string(),
                    position: Vec3::new(-2.0, 0.0, 0.0),
                    scale: Vec3::ONE,
                    color: Vec3::new(1.0, 0.5, 0.0),
                    behavior: Some("spin".to_string()),
                },
                CubeData {
                    name: "cube2".to_string(),
                    position: Vec3::new(2.0, 0.0, 0.0),
                    scale: Vec3::ONE,
                    color: Vec3::new(0.0, 1.0, 0.5),
                    behavior: Some("spin".to_string()),
                },
            ],
            ui_elements: vec![],
            behaviors,
            camera: None,
            lighting: None,
        }
    }
}
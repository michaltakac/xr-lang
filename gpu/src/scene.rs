//! Scene data structures for DSL-driven 3D scenes

use crate::math::*;
use crate::entity::{Entity, MetaDirective};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct SceneData {
    pub entities: Vec<Entity>,  // All entities in the scene (replaces cubes)
    pub ui_elements: Vec<UIElementData>,
    pub behaviors: HashMap<String, BehaviorData>,
    pub camera: Option<CameraData>,
    pub lighting: Option<LightingData>,
    pub input: Option<InputData>,
    pub ast: Vec<dsl::ast::Top>,  // Store AST for hot-swapping
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
    pub meta: Option<MetaDirective>,
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

#[derive(Debug, Clone)]
pub struct InputData {
    pub camera_controls: Option<CameraControlsData>,
}

#[derive(Debug, Clone)]
pub struct CameraControlsData {
    pub move_speed: f32,
    pub rotate_speed: f32,
    pub movement_keys: MovementKeysData,
    pub rotation_keys: RotationKeysData,
    pub orbit_controls: Option<OrbitControlsData>,
}

#[derive(Debug, Clone)]
pub struct OrbitControlsData {
    pub enabled: bool,
    pub sensitivity: f32,
    pub damping: f32,
    pub min_distance: f32,
    pub max_distance: f32,
    pub min_polar_angle: f32,
    pub max_polar_angle: f32,
    pub enable_zoom: bool,
    pub zoom_speed: f32,
}

#[derive(Debug, Clone)]
pub struct MovementKeysData {
    pub forward: String,
    pub backward: String,
    pub left: String,
    pub right: String,
    pub up: String,
    pub down: String,
}

#[derive(Debug, Clone)]
pub struct RotationKeysData {
    pub pitch_up: String,
    pub pitch_down: String,
    pub yaw_left: String,
    pub yaw_right: String,
}

impl Default for SceneData {
    fn default() -> Self {
        use crate::entity::{Entity, PrimitiveType, MeshSource, Transform, Material};
        
        let mut behaviors = HashMap::new();
        let mut spin_state = HashMap::new();
        spin_state.insert("speed".to_string(), 1.0);
        behaviors.insert("spin".to_string(), BehaviorData {
            name: "spin".to_string(),
            state: spin_state,
        });

        // Create default entities using various primitives
        let entities = vec![
            Entity {
                id: "cube1".to_string(),
                name: "cube1".to_string(),
                mesh: MeshSource::Primitive(PrimitiveType::cube()),
                transform: Transform {
                    position: Vec3::new(-2.0, 0.0, 0.0),
                    rotation: Quat::IDENTITY,
                    scale: Vec3::ONE,
                },
                material: Material {
                    color: [1.0, 0.5, 0.0, 1.0],
                    ..Material::default()
                },
                behavior: Some("spin".to_string()),
                children: Vec::new(),
                parent: None,
                components: Vec::new(),
                meta: None,
            },
            Entity {
                id: "sphere1".to_string(),
                name: "sphere1".to_string(),
                mesh: MeshSource::Primitive(PrimitiveType::sphere()),
                transform: Transform {
                    position: Vec3::new(2.0, 0.0, 0.0),
                    rotation: Quat::IDENTITY,
                    scale: Vec3::ONE,
                },
                material: Material {
                    color: [0.0, 1.0, 0.5, 1.0],
                    ..Material::default()
                },
                behavior: Some("spin".to_string()),
                children: Vec::new(),
                parent: None,
                components: Vec::new(),
                meta: None,
            },
        ];

        Self {
            entities,
            ui_elements: vec![],
            behaviors,
            camera: None,
            lighting: None,
            input: None,
            ast: Vec::new(),
        }
    }
}
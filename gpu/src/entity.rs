//! Entity system for 3D primitives and models

use crate::math::*;
use std::path::PathBuf;

/// All supported 3D primitive types
#[derive(Debug, Clone, PartialEq)]
pub enum PrimitiveType {
    Box { width: f32, height: f32, depth: f32 },
    Sphere { radius: f32, segments: u32 },
    Cylinder { radius: f32, height: f32, segments: u32 },
    Cone { radius: f32, height: f32, segments: u32 },
    Pyramid { base_width: f32, base_depth: f32, height: f32 },
    Wedge { width: f32, height: f32, depth: f32 },
    Torus { major_radius: f32, minor_radius: f32, segments: u32, rings: u32 },
    Plane { width: f32, height: f32, subdivisions: u32 },
    Capsule { radius: f32, height: f32, segments: u32 },
    Icosahedron { radius: f32 },
    Octahedron { radius: f32 },
    Tetrahedron { radius: f32 },
}

/// External model formats that can be loaded
#[derive(Debug, Clone)]
pub enum ModelSource {
    GLTF { path: PathBuf },
    GLB { path: PathBuf },
    OBJ { path: PathBuf },
    STL { path: PathBuf },
    PLY { path: PathBuf },
    FBX { path: PathBuf },
    USD { path: PathBuf },
    USDC { path: PathBuf },
}

/// Mesh data source - either a primitive or loaded model
#[derive(Debug, Clone)]
pub enum MeshSource {
    Primitive(PrimitiveType),
    Model(ModelSource),
    Procedural { generator: String, params: Vec<f32> },
}

/// Transform component for entities
#[derive(Debug, Clone)]
pub struct Transform {
    pub position: Vec3,
    pub rotation: Quat,
    pub scale: Vec3,
}

impl Default for Transform {
    fn default() -> Self {
        Self {
            position: Vec3::ZERO,
            rotation: Quat::IDENTITY,
            scale: Vec3::ONE,
        }
    }
}

/// Material properties for rendering
#[derive(Debug, Clone)]
pub struct Material {
    pub color: Vec4,
    pub metallic: f32,
    pub roughness: f32,
    pub emissive: Vec3,
    pub texture: Option<String>,
    pub normal_map: Option<String>,
}

impl Default for Material {
    fn default() -> Self {
        Self {
            color: [1.0, 1.0, 1.0, 1.0],
            metallic: 0.0,
            roughness: 0.5,
            emissive: Vec3::ZERO,
            texture: None,
            normal_map: None,
        }
    }
}

/// A generic 3D entity in the scene
#[derive(Debug, Clone)]
pub struct Entity {
    pub id: String,
    pub name: String,
    pub mesh: MeshSource,
    pub transform: Transform,
    pub material: Material,
    pub behavior: Option<String>,
    pub children: Vec<String>,  // IDs of child entities
    pub parent: Option<String>,  // ID of parent entity
    pub components: Vec<Component>,  // Additional ECS components
    pub meta: Option<MetaDirective>,
}

/// Additional components that can be attached to entities
#[derive(Debug, Clone)]
pub enum Component {
    Collider(ColliderComponent),
    RigidBody(RigidBodyComponent),
    Light(LightComponent),
    AudioSource(AudioSourceComponent),
    ParticleEmitter(ParticleEmitterComponent),
    Script(ScriptComponent),
    Tag(String),
    Custom { name: String, data: Vec<u8> },
}

#[derive(Debug, Clone)]
pub struct ColliderComponent {
    pub shape: ColliderShape,
    pub is_trigger: bool,
    pub offset: Vec3,
}

#[derive(Debug, Clone)]
pub enum ColliderShape {
    Box { half_extents: Vec3 },
    Sphere { radius: f32 },
    Capsule { radius: f32, height: f32 },
    Mesh,  // Use the entity's mesh as collider
}

#[derive(Debug, Clone)]
pub struct RigidBodyComponent {
    pub mass: f32,
    pub velocity: Vec3,
    pub angular_velocity: Vec3,
    pub is_kinematic: bool,
}

#[derive(Debug, Clone)]
pub struct LightComponent {
    pub light_type: LightType,
    pub color: Vec3,
    pub intensity: f32,
}

#[derive(Debug, Clone)]
pub enum LightType {
    Point { range: f32 },
    Directional,
    Spot { angle: f32, range: f32 },
}

#[derive(Debug, Clone)]
pub struct AudioSourceComponent {
    pub source: String,
    pub volume: f32,
    pub is_spatial: bool,
    pub loop_audio: bool,
}

#[derive(Debug, Clone)]
pub struct ParticleEmitterComponent {
    pub max_particles: u32,
    pub emission_rate: f32,
    pub lifetime: f32,
    pub start_velocity: Vec3,
    pub start_size: f32,
}

#[derive(Debug, Clone)]
pub struct ScriptComponent {
    pub script: String,  // DSL script code
    pub state: std::collections::HashMap<String, f32>,
}

#[derive(Debug, Clone)]
pub struct MetaDirective {
    pub preserve_mode: String,
    pub properties: Vec<String>,
}

impl PrimitiveType {
    /// Create a simple unit cube
    pub fn cube() -> Self {
        PrimitiveType::Box { width: 1.0, height: 1.0, depth: 1.0 }
    }
    
    /// Create a unit sphere
    pub fn sphere() -> Self {
        PrimitiveType::Sphere { radius: 0.5, segments: 32 }
    }
    
    /// Parse from a string type name
    pub fn from_type_string(type_str: &str) -> Option<Self> {
        match type_str.to_lowercase().as_str() {
            "cube" | "box" => Some(Self::cube()),
            "sphere" => Some(Self::sphere()),
            "cylinder" => Some(PrimitiveType::Cylinder { radius: 0.5, height: 1.0, segments: 32 }),
            "cone" => Some(PrimitiveType::Cone { radius: 0.5, height: 1.0, segments: 32 }),
            "pyramid" => Some(PrimitiveType::Pyramid { base_width: 1.0, base_depth: 1.0, height: 1.0 }),
            "wedge" => Some(PrimitiveType::Wedge { width: 1.0, height: 1.0, depth: 1.0 }),
            "torus" | "donut" => Some(PrimitiveType::Torus { major_radius: 1.0, minor_radius: 0.3, segments: 32, rings: 16 }),
            "plane" => Some(PrimitiveType::Plane { width: 10.0, height: 10.0, subdivisions: 1 }),
            "capsule" => Some(PrimitiveType::Capsule { radius: 0.5, height: 1.0, segments: 32 }),
            "icosahedron" => Some(PrimitiveType::Icosahedron { radius: 1.0 }),
            "octahedron" => Some(PrimitiveType::Octahedron { radius: 1.0 }),
            "tetrahedron" => Some(PrimitiveType::Tetrahedron { radius: 1.0 }),
            _ => None,
        }
    }
}
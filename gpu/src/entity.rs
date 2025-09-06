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
    pub material: Option<dsl::ast::MaterialDef>,
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
    
    /// Parse from a string type name, supports both simple names and parameterized formats
    /// Simple: "sphere" 
    /// Parameterized: "sphere:1.5:32" (radius:segments)
    pub fn from_type_string(type_str: &str) -> Option<Self> {
        // Split by colon to get type and optional parameters
        let parts: Vec<&str> = type_str.split(':').collect();
        let base_type = parts[0].to_lowercase();
        
        match base_type.as_str() {
            "cube" | "box" => {
                if parts.len() >= 4 {
                    // box:width:height:depth
                    let width = parts[1].parse().unwrap_or(1.0);
                    let height = parts[2].parse().unwrap_or(1.0);
                    let depth = parts[3].parse().unwrap_or(1.0);
                    Some(PrimitiveType::Box { width, height, depth })
                } else {
                    Some(Self::cube())
                }
            },
            "sphere" => {
                if parts.len() >= 3 {
                    // sphere:radius:segments
                    let radius = parts[1].parse().unwrap_or(1.0);
                    let segments = parts[2].parse().unwrap_or(32);
                    Some(PrimitiveType::Sphere { radius, segments })
                } else {
                    Some(Self::sphere())
                }
            },
            "cylinder" => {
                if parts.len() >= 4 {
                    // cylinder:radius:height:segments
                    let radius = parts[1].parse().unwrap_or(0.5);
                    let height = parts[2].parse().unwrap_or(1.0);
                    let segments = parts[3].parse().unwrap_or(32);
                    Some(PrimitiveType::Cylinder { radius, height, segments })
                } else {
                    Some(PrimitiveType::Cylinder { radius: 0.5, height: 1.0, segments: 32 })
                }
            },
            "cone" => {
                if parts.len() >= 4 {
                    // cone:radius:height:segments
                    let radius = parts[1].parse().unwrap_or(0.5);
                    let height = parts[2].parse().unwrap_or(1.0);
                    let segments = parts[3].parse().unwrap_or(32);
                    Some(PrimitiveType::Cone { radius, height, segments })
                } else {
                    Some(PrimitiveType::Cone { radius: 0.5, height: 1.0, segments: 32 })
                }
            },
            "pyramid" => {
                if parts.len() >= 4 {
                    // pyramid:base_width:base_depth:height
                    let base_width = parts[1].parse().unwrap_or(1.0);
                    let base_depth = parts[2].parse().unwrap_or(1.0);
                    let height = parts[3].parse().unwrap_or(1.0);
                    Some(PrimitiveType::Pyramid { base_width, base_depth, height })
                } else {
                    Some(PrimitiveType::Pyramid { base_width: 1.0, base_depth: 1.0, height: 1.0 })
                }
            },
            "wedge" => {
                if parts.len() >= 4 {
                    // wedge:width:height:depth
                    let width = parts[1].parse().unwrap_or(1.0);
                    let height = parts[2].parse().unwrap_or(1.0);
                    let depth = parts[3].parse().unwrap_or(1.0);
                    Some(PrimitiveType::Wedge { width, height, depth })
                } else {
                    Some(PrimitiveType::Wedge { width: 1.0, height: 1.0, depth: 1.0 })
                }
            },
            "torus" | "donut" => {
                if parts.len() >= 5 {
                    // torus:major_radius:minor_radius:segments:rings
                    let major_radius = parts[1].parse().unwrap_or(1.0);
                    let minor_radius = parts[2].parse().unwrap_or(0.3);
                    let segments = parts[3].parse().unwrap_or(32);
                    let rings = parts[4].parse().unwrap_or(16);
                    Some(PrimitiveType::Torus { major_radius, minor_radius, segments, rings })
                } else {
                    Some(PrimitiveType::Torus { major_radius: 1.0, minor_radius: 0.3, segments: 32, rings: 16 })
                }
            },
            "plane" => {
                if parts.len() >= 4 {
                    // plane:width:height:subdivisions
                    let width = parts[1].parse().unwrap_or(10.0);
                    let height = parts[2].parse().unwrap_or(10.0);
                    let subdivisions = parts[3].parse().unwrap_or(1);
                    Some(PrimitiveType::Plane { width, height, subdivisions })
                } else {
                    Some(PrimitiveType::Plane { width: 10.0, height: 10.0, subdivisions: 1 })
                }
            },
            "capsule" => {
                if parts.len() >= 4 {
                    // capsule:radius:height:segments
                    let radius = parts[1].parse().unwrap_or(0.5);
                    let height = parts[2].parse().unwrap_or(1.0);
                    let segments = parts[3].parse().unwrap_or(32);
                    Some(PrimitiveType::Capsule { radius, height, segments })
                } else {
                    Some(PrimitiveType::Capsule { radius: 0.5, height: 1.0, segments: 32 })
                }
            },
            "icosahedron" => {
                if parts.len() >= 2 {
                    // icosahedron:radius
                    let radius = parts[1].parse().unwrap_or(1.0);
                    Some(PrimitiveType::Icosahedron { radius })
                } else {
                    Some(PrimitiveType::Icosahedron { radius: 1.0 })
                }
            },
            "octahedron" => {
                if parts.len() >= 2 {
                    // octahedron:radius
                    let radius = parts[1].parse().unwrap_or(1.0);
                    Some(PrimitiveType::Octahedron { radius })
                } else {
                    Some(PrimitiveType::Octahedron { radius: 1.0 })
                }
            },
            "tetrahedron" => {
                if parts.len() >= 2 {
                    // tetrahedron:radius
                    let radius = parts[1].parse().unwrap_or(1.0);
                    Some(PrimitiveType::Tetrahedron { radius })
                } else {
                    Some(PrimitiveType::Tetrahedron { radius: 1.0 })
                }
            },
            _ => None,
        }
    }
}
//! Abstract Syntax Tree for XR-DSL

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Expr {
    F32(f32),
    I32(i32),
    Bool(bool),
    Sym(String),
    List(Vec<Expr>),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Behavior {
    pub name: String,
    pub state: Vec<(String, f32)>,
    pub update: FnDef,
    pub on_select: Option<FnDef>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FnDef {
    pub params: Vec<String>,
    pub body: Expr,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Top {
    Behavior(Behavior),
    Scene3D(Scene3D),
    // Future: DefShader, DefMacro, etc.
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Scene3D {
    pub name: String,
    pub objects: Vec<Object3D>,
    pub camera: Option<CameraDef>,
    pub lighting: Option<LightingDef>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Object3D {
    pub name: String,
    pub mesh_type: String, // "cube", "sphere", "plane", etc.
    pub transform: TransformDef,
    pub material: Option<MaterialDef>,
    pub behavior: Option<String>, // reference to behavior
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TransformDef {
    pub position: [f32; 3],
    pub rotation: [f32; 3], // Euler angles
    pub scale: [f32; 3],
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CameraDef {
    pub position: [f32; 3],
    pub target: [f32; 3],
    pub up: [f32; 3],
    pub fov: f32,
    pub near: f32,
    pub far: f32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LightingDef {
    pub ambient: [f32; 3],
    pub directional: Option<DirectionalLight>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DirectionalLight {
    pub direction: [f32; 3],
    pub color: [f32; 3],
    pub intensity: f32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MaterialDef {
    pub base_color: [f32; 4],
    pub metallic: f32,
    pub roughness: f32,
    pub emissive: [f32; 3],
}

impl Expr {
    pub fn is_symbol(&self, name: &str) -> bool {
        matches!(self, Expr::Sym(s) if s == name)
    }
    
    pub fn as_symbol(&self) -> Option<&str> {
        match self {
            Expr::Sym(s) => Some(s),
            _ => None,
        }
    }
    
    pub fn as_list(&self) -> Option<&Vec<Expr>> {
        match self {
            Expr::List(v) => Some(v),
            _ => None,
        }
    }
    
    pub fn as_f32(&self) -> Option<f32> {
        match self {
            Expr::F32(f) => Some(*f),
            _ => None,
        }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::F32(x) => write!(f, "{}", x),
            Expr::I32(x) => write!(f, "{}", x),
            Expr::Bool(x) => write!(f, "{}", x),
            Expr::Sym(s) => write!(f, "{}", s),
            Expr::List(v) => {
                write!(f, "(")?;
                for (i, e) in v.iter().enumerate() {
                    if i > 0 { write!(f, " ")?; }
                    write!(f, "{}", e)?;
                }
                write!(f, ")")
            }
        }
    }
}
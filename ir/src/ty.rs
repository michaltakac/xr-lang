//! Type system for XR-DSL IR

use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Ty {
    Unit,
    F32,
    I32,
    Bool,
    Ptr,
    // Vector types for GPU operations
    Vec2F32,
    Vec3F32,
    Vec4F32,
    // Matrix types
    Mat4F32,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Sig {
    pub params: Vec<Ty>,
    pub ret: Ty,
}

impl Ty {
    pub fn size_bytes(self) -> usize {
        match self {
            Ty::Unit => 0,
            Ty::F32 | Ty::I32 | Ty::Bool => 4,
            Ty::Ptr => 8, // 64-bit pointers
            Ty::Vec2F32 => 8,
            Ty::Vec3F32 => 12,
            Ty::Vec4F32 => 16,
            Ty::Mat4F32 => 64,
        }
    }
    
    pub fn is_numeric(self) -> bool {
        matches!(self, Ty::F32 | Ty::I32 | Ty::Vec2F32 | Ty::Vec3F32 | Ty::Vec4F32)
    }
    
    pub fn is_float(self) -> bool {
        matches!(self, Ty::F32 | Ty::Vec2F32 | Ty::Vec3F32 | Ty::Vec4F32)
    }
    
    pub fn is_int(self) -> bool {
        matches!(self, Ty::I32 | Ty::Bool)
    }
}
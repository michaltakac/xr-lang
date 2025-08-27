//! 3D math utilities for XR-DSL

pub mod vec3;
pub mod mat4;
pub mod transform;

pub use vec3::*;
pub use mat4::*;
pub use transform::*;

// Additional vector types for UI
pub type Vec2 = [f32; 2];
pub type Vec4 = [f32; 4];

// use bytemuck::{Pod, Zeroable};
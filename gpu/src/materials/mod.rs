//! Material system for XR-lang

pub mod mesh_basic_material;

pub use mesh_basic_material::{MeshBasicMaterial, Side, MeshBasicMaterialUniforms};

use wgpu::*;

/// Trait for all materials in the system
pub trait Material {
    /// Get the render pipeline for this material
    fn get_pipeline(&self) -> &RenderPipeline;
    
    /// Get the bind group for this material
    fn get_bind_group(&self) -> &BindGroup;
    
    /// Update material uniforms if needed
    fn update(&mut self, queue: &Queue, time: f32);
    
    /// Check if material should render both sides
    fn is_double_sided(&self) -> bool;
}
//! 3D transform utilities

use super::{Vec3, Mat4, Quat};
use bytemuck::{Pod, Zeroable};

#[repr(C)]
#[derive(Debug, Clone, Copy, Pod, Zeroable)]
pub struct Transform {
    pub position: Vec3,
    pub _pad1: f32,
    pub rotation: Quat,
    pub scale: Vec3,
    pub _pad3: f32,
}

impl Transform {
    pub const IDENTITY: Self = Self {
        position: Vec3::ZERO,
        _pad1: 0.0,
        rotation: Quat::IDENTITY,
        scale: Vec3::ONE,
        _pad3: 0.0,
    };
    
    pub fn new(position: Vec3, rotation: Quat, scale: Vec3) -> Self {
        Self {
            position,
            _pad1: 0.0,
            rotation,
            scale,
            _pad3: 0.0,
        }
    }
    
    pub fn with_position(position: Vec3) -> Self {
        Self {
            position,
            ..Self::IDENTITY
        }
    }
    
    pub fn with_rotation(rotation: Quat) -> Self {
        Self {
            rotation,
            ..Self::IDENTITY
        }
    }
    
    pub fn with_scale(scale: Vec3) -> Self {
        Self {
            scale,
            ..Self::IDENTITY
        }
    }
    
    pub fn to_matrix(&self) -> Mat4 {
        let translation = Mat4::from_translation(self.position);
        let rotation = Mat4::from_quat(self.rotation);
        let scale = Mat4::from_scale(self.scale);
        
        translation * rotation * scale
    }
}

impl Default for Transform {
    fn default() -> Self {
        Self::IDENTITY
    }
}

#[repr(C)]
#[derive(Debug, Clone, Copy, Pod, Zeroable)]
pub struct Camera {
    pub view: Mat4,
    pub projection: Mat4,
    pub view_projection: Mat4,
    pub position: Vec3,
    pub _pad: f32,
}

impl Camera {
    pub fn perspective(position: Vec3, target: Vec3, up: Vec3, fovy: f32, aspect: f32, near: f32, far: f32) -> Self {
        let view = Mat4::look_at(position, target, up);
        let projection = Mat4::perspective(fovy, aspect, near, far);
        let view_projection = projection * view;
        
        Self {
            view,
            projection,
            view_projection,
            position,
            _pad: 0.0,
        }
    }
    
    pub fn update(&mut self, position: Vec3, target: Vec3, up: Vec3) {
        self.position = position;
        self.view = Mat4::look_at(position, target, up);
        self.view_projection = self.projection * self.view;
    }
    
    pub fn set_aspect_ratio(&mut self, aspect: f32) {
        // Recreate projection with new aspect ratio
        // Assuming the original was perspective, we need to extract the fov and near/far
        // For now, use common values
        self.projection = Mat4::perspective(std::f32::consts::FRAC_PI_4, aspect, 0.1, 100.0);
        self.view_projection = self.projection * self.view;
    }
}
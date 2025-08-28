//! Quaternion implementation for 3D rotations

use bytemuck::{Pod, Zeroable};
use super::Vec3;

#[repr(C)]
#[derive(Debug, Clone, Copy, Pod, Zeroable)]
pub struct Quat {
    pub x: f32,
    pub y: f32,
    pub z: f32,
    pub w: f32,
}

impl Quat {
    pub const IDENTITY: Self = Self { x: 0.0, y: 0.0, z: 0.0, w: 1.0 };
    
    pub fn new(x: f32, y: f32, z: f32, w: f32) -> Self {
        Self { x, y, z, w }
    }
    
    pub fn from_axis_angle(axis: Vec3, angle: f32) -> Self {
        let half_angle = angle * 0.5;
        let s = half_angle.sin();
        let c = half_angle.cos();
        
        let normalized_axis = axis.normalize();
        
        Self {
            x: normalized_axis.x * s,
            y: normalized_axis.y * s,
            z: normalized_axis.z * s,
            w: c,
        }
    }
    
    pub fn from_euler(pitch: f32, yaw: f32, roll: f32) -> Self {
        let half_pitch = pitch * 0.5;
        let half_yaw = yaw * 0.5;
        let half_roll = roll * 0.5;
        
        let sin_pitch = half_pitch.sin();
        let cos_pitch = half_pitch.cos();
        let sin_yaw = half_yaw.sin();
        let cos_yaw = half_yaw.cos();
        let sin_roll = half_roll.sin();
        let cos_roll = half_roll.cos();
        
        Self {
            x: sin_pitch * cos_yaw * cos_roll - cos_pitch * sin_yaw * sin_roll,
            y: cos_pitch * sin_yaw * cos_roll + sin_pitch * cos_yaw * sin_roll,
            z: cos_pitch * cos_yaw * sin_roll - sin_pitch * sin_yaw * cos_roll,
            w: cos_pitch * cos_yaw * cos_roll + sin_pitch * sin_yaw * sin_roll,
        }
    }
    
    pub fn normalize(&self) -> Self {
        let mag = (self.x * self.x + self.y * self.y + self.z * self.z + self.w * self.w).sqrt();
        if mag > 0.0 {
            Self {
                x: self.x / mag,
                y: self.y / mag,
                z: self.z / mag,
                w: self.w / mag,
            }
        } else {
            Self::IDENTITY
        }
    }
    
    pub fn conjugate(&self) -> Self {
        Self {
            x: -self.x,
            y: -self.y,
            z: -self.z,
            w: self.w,
        }
    }
    
    pub fn rotate_vector(&self, v: Vec3) -> Vec3 {
        let qv = Vec3::new(self.x, self.y, self.z);
        let t = qv.cross(v) * 2.0;
        v + t * self.w + qv.cross(t)
    }
}

impl std::ops::Mul for Quat {
    type Output = Self;
    
    fn mul(self, other: Self) -> Self {
        Self {
            x: self.w * other.x + self.x * other.w + self.y * other.z - self.z * other.y,
            y: self.w * other.y - self.x * other.z + self.y * other.w + self.z * other.x,
            z: self.w * other.z + self.x * other.y - self.y * other.x + self.z * other.w,
            w: self.w * other.w - self.x * other.x - self.y * other.y - self.z * other.z,
        }
    }
}

impl Default for Quat {
    fn default() -> Self {
        Self::IDENTITY
    }
}
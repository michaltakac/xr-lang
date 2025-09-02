//! Frustum culling for efficient rendering of large scenes
//! Only renders objects visible within the camera's view frustum

use crate::math::{Vec3, Mat4};

#[derive(Debug, Clone)]
pub struct Frustum {
    planes: [Plane; 6],  // near, far, left, right, top, bottom
}

#[derive(Debug, Clone, Copy)]
struct Plane {
    normal: Vec3,
    distance: f32,
}

impl Plane {
    fn new(normal: Vec3, point: Vec3) -> Self {
        let normal = normal.normalize();
        let distance = -normal.dot(point);
        Self { normal, distance }
    }
    
    /// Distance from plane to point (positive = in front, negative = behind)
    fn distance_to_point(&self, point: Vec3) -> f32 {
        self.normal.dot(point) + self.distance
    }
}

impl Frustum {
    /// Create frustum from view-projection matrix
    pub fn from_view_projection(view_proj: &Mat4) -> Self {
        // Extract frustum planes from view-projection matrix
        // Using the Gribb & Hartmann method
        let m = view_proj.cols;
        
        // Left plane
        let left = Plane {
            normal: Vec3::new(
                m[0][3] + m[0][0],
                m[1][3] + m[1][0],
                m[2][3] + m[2][0],
            ).normalize(),
            distance: m[3][3] + m[3][0],
        };
        
        // Right plane
        let right = Plane {
            normal: Vec3::new(
                m[0][3] - m[0][0],
                m[1][3] - m[1][0],
                m[2][3] - m[2][0],
            ).normalize(),
            distance: m[3][3] - m[3][0],
        };
        
        // Bottom plane
        let bottom = Plane {
            normal: Vec3::new(
                m[0][3] + m[0][1],
                m[1][3] + m[1][1],
                m[2][3] + m[2][1],
            ).normalize(),
            distance: m[3][3] + m[3][1],
        };
        
        // Top plane
        let top = Plane {
            normal: Vec3::new(
                m[0][3] - m[0][1],
                m[1][3] - m[1][1],
                m[2][3] - m[2][1],
            ).normalize(),
            distance: m[3][3] - m[3][1],
        };
        
        // Near plane
        let near = Plane {
            normal: Vec3::new(
                m[0][3] + m[0][2],
                m[1][3] + m[1][2],
                m[2][3] + m[2][2],
            ).normalize(),
            distance: m[3][3] + m[3][2],
        };
        
        // Far plane
        let far = Plane {
            normal: Vec3::new(
                m[0][3] - m[0][2],
                m[1][3] - m[1][2],
                m[2][3] - m[2][2],
            ).normalize(),
            distance: m[3][3] - m[3][2],
        };
        
        Self {
            planes: [near, far, left, right, top, bottom],
        }
    }
    
    /// Test if a sphere is inside the frustum
    pub fn contains_sphere(&self, center: Vec3, radius: f32) -> bool {
        for plane in &self.planes {
            if plane.distance_to_point(center) < -radius {
                return false;  // Sphere is completely behind this plane
            }
        }
        true
    }
    
    /// Test if an axis-aligned bounding box is inside the frustum
    pub fn contains_aabb(&self, min: Vec3, max: Vec3) -> bool {
        for plane in &self.planes {
            // Find the corner of the AABB that is furthest in the direction of the plane normal
            let positive_vertex = Vec3::new(
                if plane.normal.x >= 0.0 { max.x } else { min.x },
                if plane.normal.y >= 0.0 { max.y } else { min.y },
                if plane.normal.z >= 0.0 { max.z } else { min.z },
            );
            
            if plane.distance_to_point(positive_vertex) < 0.0 {
                return false;  // AABB is completely behind this plane
            }
        }
        true
    }
    
    /// Quick test if a point is inside the frustum
    pub fn contains_point(&self, point: Vec3) -> bool {
        for plane in &self.planes {
            if plane.distance_to_point(point) < 0.0 {
                return false;
            }
        }
        true
    }
}

/// Bounding volume for culling
#[derive(Debug, Clone, Copy)]
pub struct BoundingVolume {
    pub center: Vec3,
    pub radius: f32,
    pub aabb_min: Vec3,
    pub aabb_max: Vec3,
}

impl BoundingVolume {
    /// Create bounding volume for a mesh at a given transform
    pub fn from_mesh_transform(vertices: &[Vec3], transform: &Mat4) -> Self {
        if vertices.is_empty() {
            return Self {
                center: Vec3::ZERO,
                radius: 0.0,
                aabb_min: Vec3::ZERO,
                aabb_max: Vec3::ZERO,
            };
        }
        
        // Transform vertices and compute bounds
        let mut min = Vec3::new(f32::MAX, f32::MAX, f32::MAX);
        let mut max = Vec3::new(f32::MIN, f32::MIN, f32::MIN);
        
        for vertex in vertices {
            // Transform point manually: transform * vec4(vertex, 1.0)
            let m = transform.cols;
            let x = m[0][0] * vertex.x + m[1][0] * vertex.y + m[2][0] * vertex.z + m[3][0];
            let y = m[0][1] * vertex.x + m[1][1] * vertex.y + m[2][1] * vertex.z + m[3][1];
            let z = m[0][2] * vertex.x + m[1][2] * vertex.y + m[2][2] * vertex.z + m[3][2];
            let w = m[0][3] * vertex.x + m[1][3] * vertex.y + m[2][3] * vertex.z + m[3][3];
            
            let transformed = Vec3::new(x / w, y / w, z / w);
            
            // Manual min/max
            min.x = min.x.min(transformed.x);
            min.y = min.y.min(transformed.y);
            min.z = min.z.min(transformed.z);
            
            max.x = max.x.max(transformed.x);
            max.y = max.y.max(transformed.y);
            max.z = max.z.max(transformed.z);
        }
        
        let center = (min + max) * 0.5;
        let radius = (max - min).length() * 0.5;
        
        Self {
            center,
            radius,
            aabb_min: min,
            aabb_max: max,
        }
    }
    
    /// Simple bounding volume for common primitives
    pub fn from_primitive(primitive_type: &str, transform: &Mat4, scale: Vec3) -> Self {
        // Approximate bounds for common primitives
        let base_radius = match primitive_type {
            "sphere" => 0.5,
            "cube" | "box" => 0.866,  // sqrt(3) / 2 for cube diagonal
            "cylinder" => 0.707,  // sqrt(2) / 2
            "cone" | "pyramid" => 0.707,
            "plane" => 1.0,
            _ => 1.0,
        };
        
        let max_scale = scale.x.max(scale.y).max(scale.z);
        let world_radius = base_radius * max_scale;
        
        // Transform origin point
        let m = transform.cols;
        let x = m[3][0];
        let y = m[3][1];
        let z = m[3][2];
        let w = m[3][3];
        let center = Vec3::new(x / w, y / w, z / w);
        
        Self {
            center,
            radius: world_radius,
            aabb_min: center - Vec3::splat(world_radius),
            aabb_max: center + Vec3::splat(world_radius),
        }
    }
}
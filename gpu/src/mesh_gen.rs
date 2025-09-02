//! Mesh generation for 3D primitives and model loading

use crate::math::*;
use crate::entity::{PrimitiveType, MeshSource};

/// Vertex data for mesh generation
#[repr(C)]
#[derive(Copy, Clone, Debug, bytemuck::Pod, bytemuck::Zeroable)]
pub struct Vertex {
    pub position: [f32; 3],
    pub normal: [f32; 3],
    pub tex_coords: [f32; 2],
}

/// Generated mesh data
pub struct MeshData {
    pub vertices: Vec<Vertex>,
    pub indices: Vec<u32>,
}

impl MeshData {
    /// Generate mesh data from a mesh source (primitive or model)
    pub fn from_source(source: &MeshSource) -> anyhow::Result<Self> {
        match source {
            MeshSource::Primitive(primitive) => Ok(Self::from_primitive(primitive)),
            MeshSource::Model(model_source) => {
                // Use the model loader to load external models
                crate::model_loader::load_model(model_source)
            }
            MeshSource::Procedural { .. } => {
                // Procedural generation not yet implemented
                Err(anyhow::anyhow!("Procedural mesh generation not yet implemented"))
            }
        }
    }
    
    /// Generate mesh data for a primitive type
    pub fn from_primitive(primitive: &PrimitiveType) -> Self {
        match primitive {
            PrimitiveType::Box { width, height, depth } => {
                Self::generate_box(*width, *height, *depth)
            }
            PrimitiveType::Sphere { radius, segments } => {
                Self::generate_sphere(*radius, *segments)
            }
            PrimitiveType::Cylinder { radius, height, segments } => {
                Self::generate_cylinder(*radius, *height, *segments)
            }
            PrimitiveType::Cone { radius, height, segments } => {
                Self::generate_cone(*radius, *height, *segments)
            }
            PrimitiveType::Pyramid { base_width, base_depth, height } => {
                Self::generate_pyramid(*base_width, *base_depth, *height)
            }
            PrimitiveType::Wedge { width, height, depth } => {
                Self::generate_wedge(*width, *height, *depth)
            }
            PrimitiveType::Torus { major_radius, minor_radius, segments, rings } => {
                Self::generate_torus(*major_radius, *minor_radius, *segments, *rings)
            }
            PrimitiveType::Plane { width, height, subdivisions } => {
                Self::generate_plane(*width, *height, *subdivisions)
            }
            PrimitiveType::Capsule { radius, height, segments } => {
                Self::generate_capsule(*radius, *height, *segments)
            }
            PrimitiveType::Icosahedron { radius } => {
                Self::generate_icosahedron(*radius)
            }
            PrimitiveType::Octahedron { radius } => {
                Self::generate_octahedron(*radius)
            }
            PrimitiveType::Tetrahedron { radius } => {
                Self::generate_tetrahedron(*radius)
            }
        }
    }
    
    /// Generate a box/cube mesh
    pub fn generate_box(width: f32, height: f32, depth: f32) -> Self {
        let hw = width / 2.0;
        let hh = height / 2.0;
        let hd = depth / 2.0;
        
        let vertices = vec![
            // Front face
            Vertex { position: [-hw, -hh,  hd], normal: [0.0, 0.0, 1.0], tex_coords: [0.0, 1.0] },
            Vertex { position: [ hw, -hh,  hd], normal: [0.0, 0.0, 1.0], tex_coords: [1.0, 1.0] },
            Vertex { position: [ hw,  hh,  hd], normal: [0.0, 0.0, 1.0], tex_coords: [1.0, 0.0] },
            Vertex { position: [-hw,  hh,  hd], normal: [0.0, 0.0, 1.0], tex_coords: [0.0, 0.0] },
            // Back face
            Vertex { position: [ hw, -hh, -hd], normal: [0.0, 0.0, -1.0], tex_coords: [0.0, 1.0] },
            Vertex { position: [-hw, -hh, -hd], normal: [0.0, 0.0, -1.0], tex_coords: [1.0, 1.0] },
            Vertex { position: [-hw,  hh, -hd], normal: [0.0, 0.0, -1.0], tex_coords: [1.0, 0.0] },
            Vertex { position: [ hw,  hh, -hd], normal: [0.0, 0.0, -1.0], tex_coords: [0.0, 0.0] },
            // Top face
            Vertex { position: [-hw,  hh,  hd], normal: [0.0, 1.0, 0.0], tex_coords: [0.0, 1.0] },
            Vertex { position: [ hw,  hh,  hd], normal: [0.0, 1.0, 0.0], tex_coords: [1.0, 1.0] },
            Vertex { position: [ hw,  hh, -hd], normal: [0.0, 1.0, 0.0], tex_coords: [1.0, 0.0] },
            Vertex { position: [-hw,  hh, -hd], normal: [0.0, 1.0, 0.0], tex_coords: [0.0, 0.0] },
            // Bottom face
            Vertex { position: [-hw, -hh, -hd], normal: [0.0, -1.0, 0.0], tex_coords: [0.0, 1.0] },
            Vertex { position: [ hw, -hh, -hd], normal: [0.0, -1.0, 0.0], tex_coords: [1.0, 1.0] },
            Vertex { position: [ hw, -hh,  hd], normal: [0.0, -1.0, 0.0], tex_coords: [1.0, 0.0] },
            Vertex { position: [-hw, -hh,  hd], normal: [0.0, -1.0, 0.0], tex_coords: [0.0, 0.0] },
            // Right face
            Vertex { position: [ hw, -hh,  hd], normal: [1.0, 0.0, 0.0], tex_coords: [0.0, 1.0] },
            Vertex { position: [ hw, -hh, -hd], normal: [1.0, 0.0, 0.0], tex_coords: [1.0, 1.0] },
            Vertex { position: [ hw,  hh, -hd], normal: [1.0, 0.0, 0.0], tex_coords: [1.0, 0.0] },
            Vertex { position: [ hw,  hh,  hd], normal: [1.0, 0.0, 0.0], tex_coords: [0.0, 0.0] },
            // Left face
            Vertex { position: [-hw, -hh, -hd], normal: [-1.0, 0.0, 0.0], tex_coords: [0.0, 1.0] },
            Vertex { position: [-hw, -hh,  hd], normal: [-1.0, 0.0, 0.0], tex_coords: [1.0, 1.0] },
            Vertex { position: [-hw,  hh,  hd], normal: [-1.0, 0.0, 0.0], tex_coords: [1.0, 0.0] },
            Vertex { position: [-hw,  hh, -hd], normal: [-1.0, 0.0, 0.0], tex_coords: [0.0, 0.0] },
        ];
        
        let indices = vec![
            0,  1,  2,  2,  3,  0,  // front
            4,  5,  6,  6,  7,  4,  // back
            8,  9,  10, 10, 11, 8,  // top
            12, 13, 14, 14, 15, 12, // bottom
            16, 17, 18, 18, 19, 16, // right
            20, 21, 22, 22, 23, 20, // left
        ];
        
        Self { vertices, indices }
    }
    
    /// Generate a sphere mesh using UV sphere algorithm
    pub fn generate_sphere(radius: f32, segments: u32) -> Self {
        let mut vertices = Vec::new();
        let mut indices = Vec::new();
        
        let pi = std::f32::consts::PI;
        let two_pi = 2.0 * pi;
        
        for lat in 0..=segments {
            let theta = lat as f32 * pi / segments as f32;
            let sin_theta = theta.sin();
            let cos_theta = theta.cos();
            
            for lon in 0..=segments {
                let phi = lon as f32 * two_pi / segments as f32;
                let sin_phi = phi.sin();
                let cos_phi = phi.cos();
                
                let x = sin_theta * cos_phi;
                let y = cos_theta;
                let z = sin_theta * sin_phi;
                
                vertices.push(Vertex {
                    position: [x * radius, y * radius, z * radius],
                    normal: [x, y, z],
                    tex_coords: [lon as f32 / segments as f32, lat as f32 / segments as f32],
                });
            }
        }
        
        for lat in 0..segments {
            for lon in 0..segments {
                let current = lat * (segments + 1) + lon;
                let next = current + segments + 1;
                
                indices.push(current);
                indices.push(current + 1);
                indices.push(next);
                
                indices.push(current + 1);
                indices.push(next + 1);
                indices.push(next);
            }
        }
        
        Self { vertices, indices }
    }
    
    /// Generate a cylinder mesh
    pub fn generate_cylinder(radius: f32, height: f32, segments: u32) -> Self {
        let mut vertices = Vec::new();
        let mut indices = Vec::new();
        
        let half_height = height / 2.0;
        let angle_step = 2.0 * std::f32::consts::PI / segments as f32;
        
        // Generate vertices for the sides
        for i in 0..=segments {
            let angle = i as f32 * angle_step;
            let x = angle.cos() * radius;
            let z = angle.sin() * radius;
            let u = i as f32 / segments as f32;
            
            // Top vertex
            vertices.push(Vertex {
                position: [x, half_height, z],
                normal: [x / radius, 0.0, z / radius],
                tex_coords: [u, 0.0],
            });
            
            // Bottom vertex
            vertices.push(Vertex {
                position: [x, -half_height, z],
                normal: [x / radius, 0.0, z / radius],
                tex_coords: [u, 1.0],
            });
        }
        
        // Generate indices for the sides
        for i in 0..segments {
            let top_left = i * 2;
            let bottom_left = top_left + 1;
            let top_right = top_left + 2;
            let bottom_right = top_right + 1;
            
            indices.push(top_left);
            indices.push(bottom_left);
            indices.push(top_right);
            
            indices.push(bottom_left);
            indices.push(bottom_right);
            indices.push(top_right);
        }
        
        // Add top and bottom caps
        let top_center_idx = vertices.len() as u32;
        vertices.push(Vertex {
            position: [0.0, half_height, 0.0],
            normal: [0.0, 1.0, 0.0],
            tex_coords: [0.5, 0.5],
        });
        
        let bottom_center_idx = vertices.len() as u32;
        vertices.push(Vertex {
            position: [0.0, -half_height, 0.0],
            normal: [0.0, -1.0, 0.0],
            tex_coords: [0.5, 0.5],
        });
        
        for i in 0..segments {
            let angle = i as f32 * angle_step;
            let next_angle = (i + 1) as f32 * angle_step;
            
            // Top cap
            let x1 = angle.cos() * radius;
            let z1 = angle.sin() * radius;
            let x2 = next_angle.cos() * radius;
            let z2 = next_angle.sin() * radius;
            
            let top_vertex_1 = vertices.len() as u32;
            vertices.push(Vertex {
                position: [x1, half_height, z1],
                normal: [0.0, 1.0, 0.0],
                tex_coords: [(x1 / radius + 1.0) / 2.0, (z1 / radius + 1.0) / 2.0],
            });
            
            let top_vertex_2 = vertices.len() as u32;
            vertices.push(Vertex {
                position: [x2, half_height, z2],
                normal: [0.0, 1.0, 0.0],
                tex_coords: [(x2 / radius + 1.0) / 2.0, (z2 / radius + 1.0) / 2.0],
            });
            
            indices.push(top_center_idx);
            indices.push(top_vertex_1);
            indices.push(top_vertex_2);
            
            // Bottom cap
            let bottom_vertex_1 = vertices.len() as u32;
            vertices.push(Vertex {
                position: [x1, -half_height, z1],
                normal: [0.0, -1.0, 0.0],
                tex_coords: [(x1 / radius + 1.0) / 2.0, (z1 / radius + 1.0) / 2.0],
            });
            
            let bottom_vertex_2 = vertices.len() as u32;
            vertices.push(Vertex {
                position: [x2, -half_height, z2],
                normal: [0.0, -1.0, 0.0],
                tex_coords: [(x2 / radius + 1.0) / 2.0, (z2 / radius + 1.0) / 2.0],
            });
            
            indices.push(bottom_center_idx);
            indices.push(bottom_vertex_2);
            indices.push(bottom_vertex_1);
        }
        
        Self { vertices, indices }
    }
    
    /// Generate a cone mesh
    pub fn generate_cone(radius: f32, height: f32, segments: u32) -> Self {
        let mut vertices = Vec::new();
        let mut indices = Vec::new();
        
        let angle_step = 2.0 * std::f32::consts::PI / segments as f32;
        
        // Apex vertex
        let apex_idx = 0;
        vertices.push(Vertex {
            position: [0.0, height / 2.0, 0.0],
            normal: [0.0, 1.0, 0.0],
            tex_coords: [0.5, 0.0],
        });
        
        // Base vertices and side faces
        for i in 0..=segments {
            let angle = i as f32 * angle_step;
            let x = angle.cos() * radius;
            let z = angle.sin() * radius;
            
            let normal = Vec3::new(x, radius, z).normalize();
            vertices.push(Vertex {
                position: [x, -height / 2.0, z],
                normal: [normal.x, normal.y, normal.z],
                tex_coords: [i as f32 / segments as f32, 1.0],
            });
        }
        
        // Side face indices
        for i in 0..segments {
            indices.push(apex_idx);
            indices.push(i + 1);
            indices.push(i + 2);
        }
        
        // Base cap
        let base_center_idx = vertices.len() as u32;
        vertices.push(Vertex {
            position: [0.0, -height / 2.0, 0.0],
            normal: [0.0, -1.0, 0.0],
            tex_coords: [0.5, 0.5],
        });
        
        for i in 0..segments {
            let angle = i as f32 * angle_step;
            let next_angle = (i + 1) as f32 * angle_step;
            
            let x1 = angle.cos() * radius;
            let z1 = angle.sin() * radius;
            let x2 = next_angle.cos() * radius;
            let z2 = next_angle.sin() * radius;
            
            let v1 = vertices.len() as u32;
            vertices.push(Vertex {
                position: [x1, -height / 2.0, z1],
                normal: [0.0, -1.0, 0.0],
                tex_coords: [(x1 / radius + 1.0) / 2.0, (z1 / radius + 1.0) / 2.0],
            });
            
            let v2 = vertices.len() as u32;
            vertices.push(Vertex {
                position: [x2, -height / 2.0, z2],
                normal: [0.0, -1.0, 0.0],
                tex_coords: [(x2 / radius + 1.0) / 2.0, (z2 / radius + 1.0) / 2.0],
            });
            
            indices.push(base_center_idx);
            indices.push(v2);
            indices.push(v1);
        }
        
        Self { vertices, indices }
    }
    
    /// Generate a pyramid mesh
    pub fn generate_pyramid(base_width: f32, base_depth: f32, height: f32) -> Self {
        let hw = base_width / 2.0;
        let hd = base_depth / 2.0;
        let hh = height / 2.0;
        
        let vertices = vec![
            // Apex
            Vertex { position: [0.0, hh, 0.0], normal: [0.0, 1.0, 0.0], tex_coords: [0.5, 0.0] },
            // Base corners
            Vertex { position: [-hw, -hh, -hd], normal: [0.0, -1.0, 0.0], tex_coords: [0.0, 1.0] },
            Vertex { position: [ hw, -hh, -hd], normal: [0.0, -1.0, 0.0], tex_coords: [1.0, 1.0] },
            Vertex { position: [ hw, -hh,  hd], normal: [0.0, -1.0, 0.0], tex_coords: [1.0, 0.0] },
            Vertex { position: [-hw, -hh,  hd], normal: [0.0, -1.0, 0.0], tex_coords: [0.0, 0.0] },
        ];
        
        let indices = vec![
            // Front face
            0, 4, 3,
            // Right face
            0, 3, 2,
            // Back face
            0, 2, 1,
            // Left face
            0, 1, 4,
            // Base
            1, 2, 3,
            3, 4, 1,
        ];
        
        Self { vertices, indices }
    }
    
    /// Generate a wedge mesh
    pub fn generate_wedge(width: f32, height: f32, depth: f32) -> Self {
        let hw = width / 2.0;
        let hh = height / 2.0;
        let hd = depth / 2.0;
        
        let vertices = vec![
            // Bottom face
            Vertex { position: [-hw, -hh,  hd], normal: [0.0, -1.0, 0.0], tex_coords: [0.0, 0.0] },
            Vertex { position: [ hw, -hh,  hd], normal: [0.0, -1.0, 0.0], tex_coords: [1.0, 0.0] },
            Vertex { position: [ hw, -hh, -hd], normal: [0.0, -1.0, 0.0], tex_coords: [1.0, 1.0] },
            Vertex { position: [-hw, -hh, -hd], normal: [0.0, -1.0, 0.0], tex_coords: [0.0, 1.0] },
            // Top edge
            Vertex { position: [-hw,  hh, -hd], normal: [0.0, 0.0, -1.0], tex_coords: [0.0, 0.0] },
            Vertex { position: [ hw,  hh, -hd], normal: [0.0, 0.0, -1.0], tex_coords: [1.0, 0.0] },
        ];
        
        let indices = vec![
            // Bottom face
            0, 1, 2,
            2, 3, 0,
            // Front slant
            0, 4, 5,
            5, 1, 0,
            // Back face
            2, 5, 4,
            4, 3, 2,
            // Left face
            0, 3, 4,
            // Right face
            1, 5, 2,
        ];
        
        Self { vertices, indices }
    }
    
    /// Generate a torus (donut) mesh
    pub fn generate_torus(major_radius: f32, minor_radius: f32, segments: u32, rings: u32) -> Self {
        let mut vertices = Vec::new();
        let mut indices = Vec::new();
        
        let two_pi = 2.0 * std::f32::consts::PI;
        
        for i in 0..=rings {
            let theta = i as f32 * two_pi / rings as f32;
            let cos_theta = theta.cos();
            let sin_theta = theta.sin();
            
            for j in 0..=segments {
                let phi = j as f32 * two_pi / segments as f32;
                let cos_phi = phi.cos();
                let sin_phi = phi.sin();
                
                let x = (major_radius + minor_radius * cos_phi) * cos_theta;
                let y = minor_radius * sin_phi;
                let z = (major_radius + minor_radius * cos_phi) * sin_theta;
                
                let nx = cos_phi * cos_theta;
                let ny = sin_phi;
                let nz = cos_phi * sin_theta;
                
                vertices.push(Vertex {
                    position: [x, y, z],
                    normal: [nx, ny, nz],
                    tex_coords: [i as f32 / rings as f32, j as f32 / segments as f32],
                });
            }
        }
        
        for i in 0..rings {
            for j in 0..segments {
                let current = i * (segments + 1) + j;
                let next = (i + 1) * (segments + 1) + j;
                
                indices.push(current);
                indices.push(current + 1);
                indices.push(next);
                
                indices.push(current + 1);
                indices.push(next + 1);
                indices.push(next);
            }
        }
        
        Self { vertices, indices }
    }
    
    /// Generate a plane mesh
    pub fn generate_plane(width: f32, height: f32, subdivisions: u32) -> Self {
        let mut vertices = Vec::new();
        let mut indices = Vec::new();
        
        let hw = width / 2.0;
        let hh = height / 2.0;
        let step_x = width / subdivisions as f32;
        let step_z = height / subdivisions as f32;
        
        for i in 0..=subdivisions {
            for j in 0..=subdivisions {
                let x = -hw + i as f32 * step_x;
                let z = -hh + j as f32 * step_z;
                
                vertices.push(Vertex {
                    position: [x, 0.0, z],
                    normal: [0.0, 1.0, 0.0],
                    tex_coords: [i as f32 / subdivisions as f32, j as f32 / subdivisions as f32],
                });
            }
        }
        
        for i in 0..subdivisions {
            for j in 0..subdivisions {
                let top_left = i * (subdivisions + 1) + j;
                let top_right = top_left + 1;
                let bottom_left = (i + 1) * (subdivisions + 1) + j;
                let bottom_right = bottom_left + 1;
                
                indices.push(top_left);
                indices.push(bottom_left);
                indices.push(top_right);
                
                indices.push(top_right);
                indices.push(bottom_left);
                indices.push(bottom_right);
            }
        }
        
        Self { vertices, indices }
    }
    
    /// Generate a capsule mesh
    pub fn generate_capsule(radius: f32, height: f32, segments: u32) -> Self {
        // A capsule is a cylinder with hemisphere caps
        // For simplicity, we'll reuse sphere generation for caps
        let indices: Vec<u32> = Vec::new();
        
        // Generate cylinder middle part
        let cylinder_height = height - 2.0 * radius;
        let half_height = cylinder_height / 2.0;
        
        // Similar to cylinder generation but adjusted for capsule
        // ... (implementation details)
        
        // For now, return a simple cylinder as placeholder
        Self::generate_cylinder(radius, height, segments)
    }
    
    /// Generate an icosahedron mesh
    pub fn generate_icosahedron(radius: f32) -> Self {
        let phi = (1.0 + 5.0_f32.sqrt()) / 2.0; // Golden ratio
        let a = radius / (phi * phi + 1.0).sqrt();
        let b = a * phi;
        
        let vertices = vec![
            Vertex { position: [-a, b, 0.0], normal: [-a, b, 0.0], tex_coords: [0.0, 0.0] },
            Vertex { position: [a, b, 0.0], normal: [a, b, 0.0], tex_coords: [0.0, 0.0] },
            Vertex { position: [-a, -b, 0.0], normal: [-a, -b, 0.0], tex_coords: [0.0, 0.0] },
            Vertex { position: [a, -b, 0.0], normal: [a, -b, 0.0], tex_coords: [0.0, 0.0] },
            Vertex { position: [0.0, -a, b], normal: [0.0, -a, b], tex_coords: [0.0, 0.0] },
            Vertex { position: [0.0, a, b], normal: [0.0, a, b], tex_coords: [0.0, 0.0] },
            Vertex { position: [0.0, -a, -b], normal: [0.0, -a, -b], tex_coords: [0.0, 0.0] },
            Vertex { position: [0.0, a, -b], normal: [0.0, a, -b], tex_coords: [0.0, 0.0] },
            Vertex { position: [b, 0.0, -a], normal: [b, 0.0, -a], tex_coords: [0.0, 0.0] },
            Vertex { position: [b, 0.0, a], normal: [b, 0.0, a], tex_coords: [0.0, 0.0] },
            Vertex { position: [-b, 0.0, -a], normal: [-b, 0.0, -a], tex_coords: [0.0, 0.0] },
            Vertex { position: [-b, 0.0, a], normal: [-b, 0.0, a], tex_coords: [0.0, 0.0] },
        ];
        
        let indices = vec![
            0, 11, 5,   0, 5, 1,    0, 1, 7,    0, 7, 10,   0, 10, 11,
            1, 5, 9,    5, 11, 4,   11, 10, 2,  10, 7, 6,   7, 1, 8,
            3, 9, 4,    3, 4, 2,    3, 2, 6,    3, 6, 8,    3, 8, 9,
            4, 9, 5,    2, 4, 11,   6, 2, 10,   8, 6, 7,    9, 8, 1,
        ];
        
        Self { vertices, indices }
    }
    
    /// Generate an octahedron mesh
    pub fn generate_octahedron(radius: f32) -> Self {
        let vertices = vec![
            Vertex { position: [radius, 0.0, 0.0], normal: [1.0, 0.0, 0.0], tex_coords: [0.0, 0.5] },
            Vertex { position: [-radius, 0.0, 0.0], normal: [-1.0, 0.0, 0.0], tex_coords: [1.0, 0.5] },
            Vertex { position: [0.0, radius, 0.0], normal: [0.0, 1.0, 0.0], tex_coords: [0.5, 0.0] },
            Vertex { position: [0.0, -radius, 0.0], normal: [0.0, -1.0, 0.0], tex_coords: [0.5, 1.0] },
            Vertex { position: [0.0, 0.0, radius], normal: [0.0, 0.0, 1.0], tex_coords: [0.25, 0.5] },
            Vertex { position: [0.0, 0.0, -radius], normal: [0.0, 0.0, -1.0], tex_coords: [0.75, 0.5] },
        ];
        
        let indices = vec![
            0, 2, 4,  0, 4, 3,  0, 3, 5,  0, 5, 2,
            1, 4, 2,  1, 3, 4,  1, 5, 3,  1, 2, 5,
        ];
        
        Self { vertices, indices }
    }
    
    /// Generate a tetrahedron mesh
    pub fn generate_tetrahedron(radius: f32) -> Self {
        let a = radius * 2.0 / 3.0_f32.sqrt();
        let h = radius * (2.0 / 3.0_f32).sqrt();
        
        let vertices = vec![
            Vertex { position: [0.0, radius, 0.0], normal: [0.0, 1.0, 0.0], tex_coords: [0.5, 0.0] },
            Vertex { position: [-a, -h, a], normal: [-1.0, -1.0, 1.0], tex_coords: [0.0, 1.0] },
            Vertex { position: [a, -h, a], normal: [1.0, -1.0, 1.0], tex_coords: [1.0, 1.0] },
            Vertex { position: [0.0, -h, -a * 2.0_f32.sqrt()], normal: [0.0, -1.0, -1.0], tex_coords: [0.5, 1.0] },
        ];
        
        let indices = vec![
            0, 1, 2,  0, 2, 3,  0, 3, 1,  1, 3, 2,
        ];
        
        Self { vertices, indices }
    }
}
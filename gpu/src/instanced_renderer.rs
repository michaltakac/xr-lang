//! Instanced rendering for optimized GPU performance
//! Groups similar meshes and renders them in a single draw call

use wgpu::*;
use std::collections::HashMap;
use crate::entity::{Entity, MeshSource, PrimitiveType};
use crate::math::*;
use crate::frustum::{Frustum, BoundingVolume};

/// Key for grouping similar meshes
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct InstanceKey {
    mesh_type: String,  // sphere, cube, plane, etc.
    material_type: String,  // mesh-basic, etc.
}

/// Instance data for each object
#[repr(C)]
#[derive(Copy, Clone, Debug, bytemuck::Pod, bytemuck::Zeroable)]
pub struct InstanceData {
    model_matrix: [[f32; 4]; 4],
    color: [f32; 4],
}

impl InstanceData {
    pub fn new(transform: &Mat4, color: [f32; 4]) -> Self {
        Self {
            model_matrix: transform.cols,
            color,
        }
    }
}

/// Manages instanced rendering for batched draw calls
pub struct InstancedRenderer {
    instance_groups: HashMap<InstanceKey, Vec<InstanceData>>,
    instance_buffers: HashMap<InstanceKey, Vec<Buffer>>,  // Multiple buffers per key for chunking
    max_instances_per_buffer: usize,
    frustum: Option<Frustum>,  // Current view frustum for culling
    culling_enabled: bool,
    culled_count: usize,  // Track how many objects were culled
    buffers_dirty: bool,  // Only update buffers when data changes
    last_instance_count: usize,  // Track if instance count changed
}

impl InstancedRenderer {
    pub fn new() -> Self {
        Self {
            instance_groups: HashMap::new(),
            instance_buffers: HashMap::new(),
            max_instances_per_buffer: 65536,  // Increased for better batching
            frustum: None,
            culling_enabled: true,
            culled_count: 0,
            buffers_dirty: true,  // Start dirty to force initial buffer creation
            last_instance_count: 0,
        }
    }
    
    /// Update the view frustum for culling
    pub fn update_frustum(&mut self, view_proj: &Mat4) {
        self.frustum = Some(Frustum::from_view_projection(view_proj));
        self.culled_count = 0;  // Reset counter
    }
    
    /// Enable or disable frustum culling
    pub fn set_culling_enabled(&mut self, enabled: bool) {
        self.culling_enabled = enabled;
    }
    
    /// Clear all instance data for the next frame
    pub fn clear(&mut self) {
        // Track if we're changing the instance count
        let current_count: usize = self.instance_groups.values().map(|g| g.len()).sum();
        if current_count != self.last_instance_count {
            self.buffers_dirty = true;
            self.last_instance_count = current_count;
        }
        
        for group in self.instance_groups.values_mut() {
            group.clear();
        }
    }
    
    /// Mark buffers as needing update (for dynamic scenes)
    pub fn mark_dirty(&mut self) {
        self.buffers_dirty = true;
    }
    
    /// Add an entity to be rendered with instancing (with frustum culling)
    pub fn add_instance(&mut self, entity: &Entity, transform: &Mat4, color: [f32; 4]) {
        // Perform frustum culling if enabled
        if self.culling_enabled {
            if let Some(ref frustum) = self.frustum {
                // Get bounding volume for the entity
                let mesh_type_str = match &entity.mesh {
                    MeshSource::Primitive(prim) => {
                        match prim {
                            PrimitiveType::Sphere { .. } => "sphere",
                            PrimitiveType::Box { .. } => "box",
                            PrimitiveType::Cylinder { .. } => "cylinder",
                            PrimitiveType::Plane { .. } => "plane",
                            PrimitiveType::Cone { .. } => "cone",
                            PrimitiveType::Pyramid { .. } => "pyramid",
                            PrimitiveType::Wedge { .. } => "wedge",
                            PrimitiveType::Torus { .. } => "torus",
                            PrimitiveType::Capsule { .. } => "capsule",
                            PrimitiveType::Icosahedron { .. } => "icosahedron",
                            PrimitiveType::Octahedron { .. } => "octahedron",
                            PrimitiveType::Tetrahedron { .. } => "tetrahedron",
                        }
                    },
                    _ => "default",
                };
                
                // Extract scale from transform matrix
                let cols = transform.cols;
                let scale = Vec3::new(
                    Vec3::new(cols[0][0], cols[0][1], cols[0][2]).length(),
                    Vec3::new(cols[1][0], cols[1][1], cols[1][2]).length(),
                    Vec3::new(cols[2][0], cols[2][1], cols[2][2]).length(),
                );
                
                let bounds = BoundingVolume::from_primitive(mesh_type_str, transform, scale);
                
                // Cull if outside frustum
                if !frustum.contains_sphere(bounds.center, bounds.radius) {
                    self.culled_count += 1;
                    return;  // Skip this instance
                }
            }
        }
        
        // Get mesh type as string
        let mesh_type = match &entity.mesh {
            MeshSource::Primitive(prim) => {
                match prim {
                    PrimitiveType::Sphere { .. } => "sphere",
                    PrimitiveType::Box { .. } => "box",
                    PrimitiveType::Cylinder { .. } => "cylinder",
                    PrimitiveType::Plane { .. } => "plane",
                    PrimitiveType::Cone { .. } => "cone",
                    PrimitiveType::Pyramid { .. } => "pyramid",
                    PrimitiveType::Wedge { .. } => "wedge",
                    PrimitiveType::Torus { .. } => "torus",
                    PrimitiveType::Capsule { .. } => "capsule",
                    PrimitiveType::Icosahedron { .. } => "icosahedron",
                    PrimitiveType::Octahedron { .. } => "octahedron",
                    PrimitiveType::Tetrahedron { .. } => "tetrahedron",
                }
            },
            MeshSource::Model(_) => "model",
            MeshSource::Procedural { .. } => "procedural",
        }.to_string();
        
        // Get material type as string
        let material_type = entity.material.as_ref()
            .map(|m| {
                match m {
                    dsl::ast::MaterialDef::Standard { .. } => "standard",
                    dsl::ast::MaterialDef::MeshBasic { .. } => "mesh-basic",
                }
            })
            .unwrap_or("default")
            .to_string();
        
        let key = InstanceKey {
            mesh_type,
            material_type,
        };
        
        let instance_data = InstanceData::new(transform, color);
        
        self.instance_groups
            .entry(key)
            .or_insert_with(Vec::new)
            .push(instance_data);
    }
    
    /// Update GPU buffers with instance data (only when dirty)
    pub fn update_buffers(&mut self, device: &Device, queue: &Queue) {
        // Skip update if buffers are clean
        if !self.buffers_dirty {
            return;
        }
        
        for (key, instances) in &self.instance_groups {
            if instances.is_empty() {
                continue;
            }
            
            // Calculate how many buffers we need for this instance group
            let chunks_needed = (instances.len() + self.max_instances_per_buffer - 1) / self.max_instances_per_buffer;
            
            // Get or create buffer list for this key
            let buffers = self.instance_buffers.entry(key.clone()).or_insert_with(Vec::new);
            
            // Ensure we have enough buffers
            while buffers.len() < chunks_needed {
                let buffer = device.create_buffer(&BufferDescriptor {
                    label: Some(&format!("Instance Buffer: {:?} [{}]", key, buffers.len())),
                    size: (std::mem::size_of::<InstanceData>() * self.max_instances_per_buffer) as u64,
                    usage: BufferUsages::VERTEX | BufferUsages::COPY_DST,
                    mapped_at_creation: false,
                });
                buffers.push(buffer);
            }
            
            // Write instance data to buffers in chunks
            for (chunk_idx, chunk) in instances.chunks(self.max_instances_per_buffer).enumerate() {
                let data = bytemuck::cast_slice(chunk);
                queue.write_buffer(&buffers[chunk_idx], 0, data);
            }
        }
        
        // Mark buffers as clean
        self.buffers_dirty = false;
    }
    
    /// Get instance buffers and counts for a specific mesh type (now returns multiple buffers)
    pub fn get_instance_chunks(&self, key: &InstanceKey) -> Vec<(&Buffer, u32)> {
        match (self.instance_buffers.get(key), self.instance_groups.get(key)) {
            (Some(buffers), Some(instances)) => {
                let mut result = Vec::new();
                for (chunk_idx, chunk) in instances.chunks(self.max_instances_per_buffer).enumerate() {
                    if chunk_idx < buffers.len() {
                        result.push((&buffers[chunk_idx], chunk.len() as u32));
                    }
                }
                result
            },
            _ => Vec::new(),
        }
    }
    
    /// Get all instance groups for rendering (now returns chunks)
    pub fn get_all_chunks(&self) -> Vec<(&InstanceKey, &Buffer, u32)> {
        let mut result = Vec::new();
        
        for (key, instances) in &self.instance_groups {
            if let Some(buffers) = self.instance_buffers.get(key) {
                for (chunk_idx, chunk) in instances.chunks(self.max_instances_per_buffer).enumerate() {
                    if chunk_idx < buffers.len() {
                        result.push((key, &buffers[chunk_idx], chunk.len() as u32));
                    }
                }
            }
        }
        
        result
    }
    
    /// Get statistics about instancing
    pub fn get_stats(&self) -> InstanceStats {
        let total_instances: usize = self.instance_groups.values()
            .map(|g| g.len())
            .sum();
        
        InstanceStats {
            total_instances,
            instance_groups: self.instance_groups.len(),
            draw_calls_saved: total_instances.saturating_sub(self.instance_groups.len()),
            objects_culled: self.culled_count,
        }
    }
}

pub struct InstanceStats {
    pub total_instances: usize,
    pub instance_groups: usize,
    pub draw_calls_saved: usize,
    pub objects_culled: usize,
}

/// Vertex buffer layout for instance data
pub fn instance_buffer_layout() -> VertexBufferLayout<'static> {
    VertexBufferLayout {
        array_stride: std::mem::size_of::<InstanceData>() as BufferAddress,
        step_mode: VertexStepMode::Instance,
        attributes: &[
            // Model matrix (4x4)
            VertexAttribute {
                offset: 0,
                shader_location: 3,
                format: VertexFormat::Float32x4,
            },
            VertexAttribute {
                offset: 16,
                shader_location: 4,
                format: VertexFormat::Float32x4,
            },
            VertexAttribute {
                offset: 32,
                shader_location: 5,
                format: VertexFormat::Float32x4,
            },
            VertexAttribute {
                offset: 48,
                shader_location: 6,
                format: VertexFormat::Float32x4,
            },
            // Color
            VertexAttribute {
                offset: 64,
                shader_location: 7,
                format: VertexFormat::Float32x4,
            },
        ],
    }
}
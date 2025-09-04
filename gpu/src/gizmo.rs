//! Transform gizmo system for interactive object manipulation

use crate::math::*;
use wgpu::*;
use bytemuck::{Pod, Zeroable};
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum GizmoMode {
    Translate,
    Rotate,
    Scale,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum GizmoAxis {
    X,
    Y,
    Z,
    XY,
    XZ,
    YZ,
    XYZ, // For uniform scale or free rotation
}

#[repr(C)]
#[derive(Debug, Clone, Copy, Pod, Zeroable)]
pub struct GizmoVertex {
    pub position: [f32; 3],
    pub color: [f32; 4],
}

#[repr(C)]
#[derive(Debug, Clone, Copy, Pod, Zeroable)]
pub struct GizmoUniforms {
    pub view_proj: Mat4,
    pub model: Mat4,
    pub scale_factor: f32,  // For screen-space scaling
    pub _padding: [f32; 3],
}

/// Gizmo component attached to interactive objects
pub struct TransformGizmo {
    pub mode: GizmoMode,
    pub active_axis: Option<GizmoAxis>,
    pub visible: bool,
    pub object_transform: Transform,
    pub world_position: Vec3,
    
    // Rendering resources
    pub vertex_buffer: Buffer,
    pub index_buffer: Buffer,
    pub uniform_buffer: Buffer,
    pub bind_group: BindGroup,
    pub pipeline: RenderPipeline,
    
    // Interaction state
    pub is_dragging: bool,
    pub drag_start_point: Option<Vec3>,
    pub drag_start_transform: Option<Transform>,
    
    // Gizmo geometry
    pub translate_vertices: Vec<GizmoVertex>,
    pub translate_indices: Vec<u16>,
    pub rotate_vertices: Vec<GizmoVertex>,
    pub rotate_indices: Vec<u16>,
    pub scale_vertices: Vec<GizmoVertex>,
    pub scale_indices: Vec<u16>,
}

impl TransformGizmo {
    pub fn new(device: &Device, config: &SurfaceConfiguration) -> Self {
        // Create shader module
        let shader = device.create_shader_module(ShaderModuleDescriptor {
            label: Some("Gizmo Shader"),
            source: ShaderSource::Wgsl(include_str!("shaders/gizmo.wgsl").into()),
        });
        
        // Create bind group layout
        let bind_group_layout = device.create_bind_group_layout(&BindGroupLayoutDescriptor {
            label: Some("Gizmo Bind Group Layout"),
            entries: &[
                BindGroupLayoutEntry {
                    binding: 0,
                    visibility: ShaderStages::VERTEX,
                    ty: BindingType::Buffer {
                        ty: BufferBindingType::Uniform,
                        has_dynamic_offset: false,
                        min_binding_size: None,
                    },
                    count: None,
                },
            ],
        });
        
        // Create pipeline layout
        let pipeline_layout = device.create_pipeline_layout(&PipelineLayoutDescriptor {
            label: Some("Gizmo Pipeline Layout"),
            bind_group_layouts: &[&bind_group_layout],
            push_constant_ranges: &[],
        });
        
        // Create render pipeline
        let pipeline = device.create_render_pipeline(&RenderPipelineDescriptor {
            label: Some("Gizmo Pipeline"),
            layout: Some(&pipeline_layout),
            vertex: VertexState {
                module: &shader,
                entry_point: "vs_main",
                buffers: &[VertexBufferLayout {
                    array_stride: std::mem::size_of::<GizmoVertex>() as BufferAddress,
                    step_mode: VertexStepMode::Vertex,
                    attributes: &[
                        VertexAttribute {
                            offset: 0,
                            shader_location: 0,
                            format: VertexFormat::Float32x3,
                        },
                        VertexAttribute {
                            offset: std::mem::size_of::<[f32; 3]>() as BufferAddress,
                            shader_location: 1,
                            format: VertexFormat::Float32x4,
                        },
                    ],
                }],
                compilation_options: Default::default(),
            },
            fragment: Some(FragmentState {
                module: &shader,
                entry_point: "fs_main",
                targets: &[Some(ColorTargetState {
                    format: config.format,
                    blend: Some(BlendState::ALPHA_BLENDING),
                    write_mask: ColorWrites::ALL,
                })],
                compilation_options: Default::default(),
            }),
            primitive: PrimitiveState {
                topology: PrimitiveTopology::LineList,
                strip_index_format: None,
                front_face: FrontFace::Ccw,
                cull_mode: None,
                unclipped_depth: false,
                polygon_mode: PolygonMode::Fill,
                conservative: false,
            },
            depth_stencil: Some(DepthStencilState {
                format: TextureFormat::Depth32Float,
                depth_write_enabled: false,
                depth_compare: CompareFunction::Less,
                stencil: StencilState::default(),
                bias: DepthBiasState::default(),
            }),
            multisample: MultisampleState::default(),
            multiview: None,
        });
        
        // Create geometry for each mode
        let (translate_vertices, translate_indices) = Self::create_translate_gizmo();
        let (rotate_vertices, rotate_indices) = Self::create_rotate_gizmo();
        let (scale_vertices, scale_indices) = Self::create_scale_gizmo();
        
        // Create buffers
        let vertex_buffer = device.create_buffer(&BufferDescriptor {
            label: Some("Gizmo Vertex Buffer"),
            size: (std::mem::size_of::<GizmoVertex>() * 1000) as u64, // Pre-allocate
            usage: BufferUsages::VERTEX | BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });
        
        let index_buffer = device.create_buffer(&BufferDescriptor {
            label: Some("Gizmo Index Buffer"),
            size: (std::mem::size_of::<u16>() * 1000) as u64, // Pre-allocate
            usage: BufferUsages::INDEX | BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });
        
        let uniform_buffer = device.create_buffer(&BufferDescriptor {
            label: Some("Gizmo Uniform Buffer"),
            size: std::mem::size_of::<GizmoUniforms>() as u64,
            usage: BufferUsages::UNIFORM | BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });
        
        let bind_group = device.create_bind_group(&BindGroupDescriptor {
            label: Some("Gizmo Bind Group"),
            layout: &bind_group_layout,
            entries: &[
                BindGroupEntry {
                    binding: 0,
                    resource: uniform_buffer.as_entire_binding(),
                },
            ],
        });
        
        Self {
            mode: GizmoMode::Translate,
            active_axis: None,
            visible: true,
            object_transform: Transform::default(),
            world_position: Vec3::ZERO,
            
            vertex_buffer,
            index_buffer,
            uniform_buffer,
            bind_group,
            pipeline,
            
            is_dragging: false,
            drag_start_point: None,
            drag_start_transform: None,
            
            translate_vertices,
            translate_indices,
            rotate_vertices,
            rotate_indices,
            scale_vertices,
            scale_indices,
        }
    }
    
    /// Create translate gizmo geometry (arrows for X, Y, Z axes)
    fn create_translate_gizmo() -> (Vec<GizmoVertex>, Vec<u16>) {
        let mut vertices = Vec::new();
        let mut indices = Vec::new();
        
        let arrow_length = 1.0;
        let arrow_tip_size = 0.15;
        
        // X axis (red)
        let x_color = [1.0, 0.0, 0.0, 1.0];
        let x_start_idx = vertices.len() as u16;
        
        // Line
        vertices.push(GizmoVertex { position: [0.0, 0.0, 0.0], color: x_color });
        vertices.push(GizmoVertex { position: [arrow_length, 0.0, 0.0], color: x_color });
        indices.extend_from_slice(&[x_start_idx, x_start_idx + 1]);
        
        // Arrow tip (cone)
        vertices.push(GizmoVertex { position: [arrow_length, 0.0, 0.0], color: x_color });
        vertices.push(GizmoVertex { position: [arrow_length - arrow_tip_size, arrow_tip_size, 0.0], color: x_color });
        vertices.push(GizmoVertex { position: [arrow_length - arrow_tip_size, -arrow_tip_size, 0.0], color: x_color });
        indices.extend_from_slice(&[x_start_idx + 2, x_start_idx + 3, x_start_idx + 2, x_start_idx + 4]);
        
        // Y axis (green)
        let y_color = [0.0, 1.0, 0.0, 1.0];
        let y_start_idx = vertices.len() as u16;
        
        vertices.push(GizmoVertex { position: [0.0, 0.0, 0.0], color: y_color });
        vertices.push(GizmoVertex { position: [0.0, arrow_length, 0.0], color: y_color });
        indices.extend_from_slice(&[y_start_idx, y_start_idx + 1]);
        
        vertices.push(GizmoVertex { position: [0.0, arrow_length, 0.0], color: y_color });
        vertices.push(GizmoVertex { position: [arrow_tip_size, arrow_length - arrow_tip_size, 0.0], color: y_color });
        vertices.push(GizmoVertex { position: [-arrow_tip_size, arrow_length - arrow_tip_size, 0.0], color: y_color });
        indices.extend_from_slice(&[y_start_idx + 2, y_start_idx + 3, y_start_idx + 2, y_start_idx + 4]);
        
        // Z axis (blue)
        let z_color = [0.0, 0.0, 1.0, 1.0];
        let z_start_idx = vertices.len() as u16;
        
        vertices.push(GizmoVertex { position: [0.0, 0.0, 0.0], color: z_color });
        vertices.push(GizmoVertex { position: [0.0, 0.0, arrow_length], color: z_color });
        indices.extend_from_slice(&[z_start_idx, z_start_idx + 1]);
        
        vertices.push(GizmoVertex { position: [0.0, 0.0, arrow_length], color: z_color });
        vertices.push(GizmoVertex { position: [arrow_tip_size, 0.0, arrow_length - arrow_tip_size], color: z_color });
        vertices.push(GizmoVertex { position: [-arrow_tip_size, 0.0, arrow_length - arrow_tip_size], color: z_color });
        indices.extend_from_slice(&[z_start_idx + 2, z_start_idx + 3, z_start_idx + 2, z_start_idx + 4]);
        
        (vertices, indices)
    }
    
    /// Create rotate gizmo geometry (circles for rotation)
    fn create_rotate_gizmo() -> (Vec<GizmoVertex>, Vec<u16>) {
        let mut vertices = Vec::new();
        let mut indices = Vec::new();
        
        let radius = 1.0;
        let segments = 32;
        
        // X rotation (red circle in YZ plane)
        let x_color = [1.0, 0.0, 0.0, 0.8];
        let x_start_idx = vertices.len() as u16;
        
        for i in 0..segments {
            let angle = (i as f32 / segments as f32) * std::f32::consts::TAU;
            let y = angle.cos() * radius;
            let z = angle.sin() * radius;
            vertices.push(GizmoVertex { position: [0.0, y, z], color: x_color });
        }
        
        for i in 0..segments {
            indices.push(x_start_idx + i);
            indices.push(x_start_idx + ((i + 1) % segments));
        }
        
        // Y rotation (green circle in XZ plane)
        let y_color = [0.0, 1.0, 0.0, 0.8];
        let y_start_idx = vertices.len() as u16;
        
        for i in 0..segments {
            let angle = (i as f32 / segments as f32) * std::f32::consts::TAU;
            let x = angle.cos() * radius;
            let z = angle.sin() * radius;
            vertices.push(GizmoVertex { position: [x, 0.0, z], color: y_color });
        }
        
        for i in 0..segments {
            indices.push(y_start_idx + i);
            indices.push(y_start_idx + ((i + 1) % segments));
        }
        
        // Z rotation (blue circle in XY plane)
        let z_color = [0.0, 0.0, 1.0, 0.8];
        let z_start_idx = vertices.len() as u16;
        
        for i in 0..segments {
            let angle = (i as f32 / segments as f32) * std::f32::consts::TAU;
            let x = angle.cos() * radius;
            let y = angle.sin() * radius;
            vertices.push(GizmoVertex { position: [x, y, 0.0], color: z_color });
        }
        
        for i in 0..segments {
            indices.push(z_start_idx + i);
            indices.push(z_start_idx + ((i + 1) % segments));
        }
        
        (vertices, indices)
    }
    
    /// Create scale gizmo geometry (boxes at the end of axes)
    fn create_scale_gizmo() -> (Vec<GizmoVertex>, Vec<u16>) {
        let mut vertices = Vec::new();
        let mut indices = Vec::new();
        
        let axis_length = 0.8;
        let box_size = 0.1;
        
        // X axis scale (red)
        let x_color = [1.0, 0.0, 0.0, 1.0];
        let x_start_idx = vertices.len() as u16;
        
        // Line
        vertices.push(GizmoVertex { position: [0.0, 0.0, 0.0], color: x_color });
        vertices.push(GizmoVertex { position: [axis_length, 0.0, 0.0], color: x_color });
        indices.extend_from_slice(&[x_start_idx, x_start_idx + 1]);
        
        // Box at end
        let x_box_start = vertices.len() as u16;
        vertices.push(GizmoVertex { position: [axis_length - box_size, -box_size, -box_size], color: x_color });
        vertices.push(GizmoVertex { position: [axis_length + box_size, -box_size, -box_size], color: x_color });
        vertices.push(GizmoVertex { position: [axis_length + box_size, box_size, -box_size], color: x_color });
        vertices.push(GizmoVertex { position: [axis_length - box_size, box_size, -box_size], color: x_color });
        
        // Box edges
        indices.extend_from_slice(&[
            x_box_start, x_box_start + 1,
            x_box_start + 1, x_box_start + 2,
            x_box_start + 2, x_box_start + 3,
            x_box_start + 3, x_box_start,
        ]);
        
        // Y axis scale (green)
        let y_color = [0.0, 1.0, 0.0, 1.0];
        let y_start_idx = vertices.len() as u16;
        
        vertices.push(GizmoVertex { position: [0.0, 0.0, 0.0], color: y_color });
        vertices.push(GizmoVertex { position: [0.0, axis_length, 0.0], color: y_color });
        indices.extend_from_slice(&[y_start_idx, y_start_idx + 1]);
        
        let y_box_start = vertices.len() as u16;
        vertices.push(GizmoVertex { position: [-box_size, axis_length - box_size, -box_size], color: y_color });
        vertices.push(GizmoVertex { position: [box_size, axis_length - box_size, -box_size], color: y_color });
        vertices.push(GizmoVertex { position: [box_size, axis_length + box_size, -box_size], color: y_color });
        vertices.push(GizmoVertex { position: [-box_size, axis_length + box_size, -box_size], color: y_color });
        
        indices.extend_from_slice(&[
            y_box_start, y_box_start + 1,
            y_box_start + 1, y_box_start + 2,
            y_box_start + 2, y_box_start + 3,
            y_box_start + 3, y_box_start,
        ]);
        
        // Z axis scale (blue)
        let z_color = [0.0, 0.0, 1.0, 1.0];
        let z_start_idx = vertices.len() as u16;
        
        vertices.push(GizmoVertex { position: [0.0, 0.0, 0.0], color: z_color });
        vertices.push(GizmoVertex { position: [0.0, 0.0, axis_length], color: z_color });
        indices.extend_from_slice(&[z_start_idx, z_start_idx + 1]);
        
        let z_box_start = vertices.len() as u16;
        vertices.push(GizmoVertex { position: [-box_size, -box_size, axis_length - box_size], color: z_color });
        vertices.push(GizmoVertex { position: [box_size, -box_size, axis_length - box_size], color: z_color });
        vertices.push(GizmoVertex { position: [box_size, box_size, axis_length - box_size], color: z_color });
        vertices.push(GizmoVertex { position: [-box_size, box_size, axis_length - box_size], color: z_color });
        
        indices.extend_from_slice(&[
            z_box_start, z_box_start + 1,
            z_box_start + 1, z_box_start + 2,
            z_box_start + 2, z_box_start + 3,
            z_box_start + 3, z_box_start,
        ]);
        
        // Center uniform scale box
        let center_color = [1.0, 1.0, 1.0, 0.8];
        let center_size = 0.15;
        let center_start = vertices.len() as u16;
        
        vertices.push(GizmoVertex { position: [-center_size, -center_size, -center_size], color: center_color });
        vertices.push(GizmoVertex { position: [center_size, -center_size, -center_size], color: center_color });
        vertices.push(GizmoVertex { position: [center_size, center_size, -center_size], color: center_color });
        vertices.push(GizmoVertex { position: [-center_size, center_size, -center_size], color: center_color });
        
        indices.extend_from_slice(&[
            center_start, center_start + 1,
            center_start + 1, center_start + 2,
            center_start + 2, center_start + 3,
            center_start + 3, center_start,
        ]);
        
        (vertices, indices)
    }
    
    /// Update gizmo with current object transform
    pub fn update(&mut self, object_transform: Transform, camera: &Camera, queue: &Queue) {
        self.object_transform = object_transform;
        self.world_position = object_transform.position;
        
        // Calculate screen-space scale factor to maintain constant gizmo size
        let distance_to_camera = (camera.position - self.world_position).length();
        let scale_factor = distance_to_camera * 0.1; // Adjust this multiplier as needed
        
        // Update uniforms
        let uniforms = GizmoUniforms {
            view_proj: camera.view_projection,
            model: Mat4::from_translation(self.world_position) * Mat4::from_scale(Vec3::splat(scale_factor)),
            scale_factor,
            _padding: [0.0; 3],
        };
        
        queue.write_buffer(&self.uniform_buffer, 0, bytemuck::cast_slice(&[uniforms]));
        
        // Update vertex buffer based on current mode
        let (vertices, indices) = match self.mode {
            GizmoMode::Translate => (&self.translate_vertices, &self.translate_indices),
            GizmoMode::Rotate => (&self.rotate_vertices, &self.rotate_indices),
            GizmoMode::Scale => (&self.scale_vertices, &self.scale_indices),
        };
        
        queue.write_buffer(&self.vertex_buffer, 0, bytemuck::cast_slice(vertices));
        queue.write_buffer(&self.index_buffer, 0, bytemuck::cast_slice(indices));
    }
    
    /// Handle mouse ray intersection with gizmo
    pub fn intersect_ray(&self, ray_origin: Vec3, ray_direction: Vec3) -> Option<(GizmoAxis, f32)> {
        // Simple ray-cylinder intersection for axis handles
        // This is a simplified version - a full implementation would be more complex
        
        let threshold = 0.1; // Hit threshold
        let mut closest_hit: Option<(GizmoAxis, f32)> = None;
        
        // Check X axis
        let x_axis = Vec3::X;
        let x_distance = self.ray_axis_distance(ray_origin, ray_direction, self.world_position, x_axis);
        if x_distance < threshold {
            closest_hit = Some((GizmoAxis::X, x_distance));
        }
        
        // Check Y axis  
        let y_axis = Vec3::Y;
        let y_distance = self.ray_axis_distance(ray_origin, ray_direction, self.world_position, y_axis);
        if y_distance < threshold {
            if closest_hit.is_none() || y_distance < closest_hit.unwrap().1 {
                closest_hit = Some((GizmoAxis::Y, y_distance));
            }
        }
        
        // Check Z axis
        let z_axis = Vec3::Z;
        let z_distance = self.ray_axis_distance(ray_origin, ray_direction, self.world_position, z_axis);
        if z_distance < threshold {
            if closest_hit.is_none() || z_distance < closest_hit.unwrap().1 {
                closest_hit = Some((GizmoAxis::Z, z_distance));
            }
        }
        
        closest_hit
    }
    
    /// Calculate distance from ray to axis
    fn ray_axis_distance(&self, ray_origin: Vec3, ray_direction: Vec3, axis_origin: Vec3, axis_direction: Vec3) -> f32 {
        let w = ray_origin - axis_origin;
        let a = ray_direction.dot(ray_direction);
        let b = ray_direction.dot(axis_direction);
        let c = axis_direction.dot(axis_direction);
        let d = ray_direction.dot(w);
        let e = axis_direction.dot(w);
        
        let denom = a * c - b * b;
        
        if denom.abs() < 0.0001 {
            return f32::MAX;
        }
        
        let s = (b * e - c * d) / denom;
        let t = (a * e - b * d) / denom;
        
        let ray_point = ray_origin + ray_direction * s;
        let axis_point = axis_origin + axis_direction * t;
        
        (ray_point - axis_point).length()
    }
    
    /// Apply transformation based on dragging
    pub fn apply_drag(&mut self, current_ray_origin: Vec3, current_ray_direction: Vec3) -> Option<Transform> {
        if !self.is_dragging || self.active_axis.is_none() {
            return None;
        }
        
        let axis = self.active_axis.unwrap();
        let start_transform = self.drag_start_transform.as_ref()?;
        
        match self.mode {
            GizmoMode::Translate => {
                // Calculate translation along axis
                let axis_vec = match axis {
                    GizmoAxis::X => Vec3::X,
                    GizmoAxis::Y => Vec3::Y,
                    GizmoAxis::Z => Vec3::Z,
                    _ => return None, // Plane translations not implemented yet
                };
                
                // Project current ray onto axis to get translation amount
                let plane_normal = axis_vec.cross(current_ray_direction).normalize();
                let plane_d = -plane_normal.dot(self.world_position);
                
                let t = -(plane_normal.dot(current_ray_origin) + plane_d) / plane_normal.dot(current_ray_direction);
                let intersection = current_ray_origin + current_ray_direction * t;
                
                let delta = intersection - self.drag_start_point?;
                let axis_delta = delta.dot(axis_vec) * axis_vec;
                
                let mut new_transform = *start_transform;
                new_transform.position = start_transform.position + axis_delta;
                
                Some(new_transform)
            }
            GizmoMode::Rotate => {
                // Calculate rotation around axis
                // Simplified - full implementation would be more complex
                let rotation_axis = match axis {
                    GizmoAxis::X => Vec3::X,
                    GizmoAxis::Y => Vec3::Y,
                    GizmoAxis::Z => Vec3::Z,
                    _ => return None,
                };
                
                // Calculate rotation angle based on mouse movement
                // This is simplified - proper implementation would project onto rotation plane
                let angle = 0.01; // Placeholder
                
                let mut new_transform = *start_transform;
                new_transform.rotation = Quat::from_axis_angle(rotation_axis, angle) * start_transform.rotation;
                
                Some(new_transform)
            }
            GizmoMode::Scale => {
                // Calculate scale along axis
                let scale_axis = match axis {
                    GizmoAxis::X => Vec3::X,
                    GizmoAxis::Y => Vec3::Y, 
                    GizmoAxis::Z => Vec3::Z,
                    GizmoAxis::XYZ => Vec3::ONE, // Uniform scale
                    _ => return None,
                };
                
                // Calculate scale factor based on mouse movement
                // Simplified - proper implementation would project properly
                let scale_delta = 0.01; // Placeholder
                
                let mut new_transform = *start_transform;
                new_transform.scale = start_transform.scale + scale_axis * scale_delta;
                
                Some(new_transform)
            }
        }
    }
    
    /// Start dragging operation
    pub fn start_drag(&mut self, axis: GizmoAxis, _ray_origin: Vec3, _ray_direction: Vec3) {
        self.is_dragging = true;
        self.active_axis = Some(axis);
        self.drag_start_transform = Some(self.object_transform);
        
        // Calculate intersection point as drag start
        // Simplified - proper implementation would calculate actual intersection
        self.drag_start_point = Some(self.world_position);
    }
    
    /// Stop dragging operation
    pub fn stop_drag(&mut self) {
        self.is_dragging = false;
        self.active_axis = None;
        self.drag_start_point = None;
        self.drag_start_transform = None;
    }
    
    /// Switch gizmo mode
    pub fn set_mode(&mut self, mode: GizmoMode) {
        self.mode = mode;
    }
    
    /// Toggle visibility
    pub fn toggle_visible(&mut self) {
        self.visible = !self.visible;
    }
}

/// Manager for all gizmos in the scene
pub struct GizmoSystem {
    pub gizmos: HashMap<String, TransformGizmo>,
    pub selected_object: Option<String>,
}

impl GizmoSystem {
    pub fn new() -> Self {
        Self {
            gizmos: HashMap::new(),
            selected_object: None,
        }
    }
    
    /// Add a gizmo for an object
    pub fn add_gizmo(&mut self, object_name: String, device: &Device, config: &SurfaceConfiguration) {
        let gizmo = TransformGizmo::new(device, config);
        self.gizmos.insert(object_name, gizmo);
    }
    
    /// Remove a gizmo
    pub fn remove_gizmo(&mut self, object_name: &str) {
        self.gizmos.remove(object_name);
    }
    
    /// Select an object (shows its gizmo)
    pub fn select_object(&mut self, object_name: Option<String>) {
        // Hide previous selection
        if let Some(prev) = &self.selected_object {
            if let Some(gizmo) = self.gizmos.get_mut(prev) {
                gizmo.visible = false;
            }
        }
        
        // Show new selection
        if let Some(name) = &object_name {
            if let Some(gizmo) = self.gizmos.get_mut(name) {
                gizmo.visible = true;
            }
        }
        
        self.selected_object = object_name;
    }
    
    /// Update all gizmos
    pub fn update(&mut self, transforms: &HashMap<String, Transform>, camera: &Camera, queue: &Queue) {
        for (name, gizmo) in &mut self.gizmos {
            if let Some(transform) = transforms.get(name) {
                gizmo.update(*transform, camera, queue);
            }
        }
    }
    
    /// Handle mouse input
    pub fn handle_mouse(&mut self, ray_origin: Vec3, ray_direction: Vec3, button_pressed: bool) -> Option<String> {
        if let Some(selected) = &self.selected_object {
            if let Some(gizmo) = self.gizmos.get_mut(selected) {
                if button_pressed {
                    if !gizmo.is_dragging {
                        // Start drag if hitting a gizmo axis
                        if let Some((axis, _)) = gizmo.intersect_ray(ray_origin, ray_direction) {
                            gizmo.start_drag(axis, ray_origin, ray_direction);
                            return Some(selected.clone());
                        }
                    } else {
                        // Continue dragging
                        if let Some(_new_transform) = gizmo.apply_drag(ray_origin, ray_direction) {
                            // Return the object name that needs its transform updated
                            return Some(selected.clone());
                        }
                    }
                } else {
                    // Stop dragging
                    gizmo.stop_drag();
                }
            }
        }
        
        None
    }
}
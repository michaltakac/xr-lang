//! GPU-driven rendering pipeline inspired by UE5's Nanite
//! All culling, LOD selection, and animation happens on GPU

use wgpu::*;
use wgpu::util::DeviceExt;
use bytemuck::{Pod, Zeroable};
use std::num::NonZeroU64;

const MAX_INSTANCES: usize = 1_000_000;  // Support up to 1M instances
const WORKGROUP_SIZE: u32 = 64;

#[repr(C)]
#[derive(Copy, Clone, Debug, Pod, Zeroable)]
pub struct GPUInstanceData {
    pub model_matrix: [[f32; 4]; 4],  // 64 bytes
    pub color: [f32; 4],              // 16 bytes
    pub animation_time: f32,          // 4 bytes
    pub animation_speed: f32,         // 4 bytes
    pub animation_amplitude: f32,     // 4 bytes
    pub bounding_radius: f32,         // 4 bytes
    pub visible: u32,                 // 4 bytes
    pub _padding: [u32; 3],           // 12 bytes
}  // Total: 112 bytes

#[repr(C)]
#[derive(Copy, Clone, Debug, Pod, Zeroable)]
pub struct AnimationParams {
    pub delta_time: f32,
    pub total_time: f32,
    pub instance_count: u32,
    pub _padding: u32,
}

#[repr(C)]
#[derive(Copy, Clone, Debug, Pod, Zeroable)]
pub struct CullingUniforms {
    pub view_proj: [[f32; 4]; 4],
    pub camera_pos: [f32; 3],
    pub instance_count: u32,
    pub frustum_planes: [[f32; 4]; 6],
    pub lod_distances: [f32; 4],
}

#[repr(C)]
#[derive(Copy, Clone, Debug, Pod, Zeroable)]
pub struct DrawIndirectArgs {
    pub vertex_count: u32,
    pub instance_count: u32,
    pub first_vertex: u32,
    pub first_instance: u32,
}

pub struct GPUDrivenRenderer {
    // Instance data (persistent, double-buffered)
    instance_buffer_a: Buffer,
    instance_buffer_b: Buffer,
    current_buffer: bool,  // Toggle between A and B
    
    // Animation system
    animation_pipeline: ComputePipeline,
    animation_bind_group_a: BindGroup,
    animation_bind_group_b: BindGroup,
    animation_params_buffer: Buffer,
    
    // Culling system
    culling_pipeline: ComputePipeline,
    culling_bind_group_a: BindGroup,
    culling_bind_group_b: BindGroup,
    culling_uniforms_buffer: Buffer,
    visible_instances_buffer: Buffer,
    visible_count_buffer: Buffer,
    
    // Indirect rendering
    indirect_buffer: Buffer,
    indirect_commands_pipeline: ComputePipeline,
    
    // Render pipeline
    render_pipeline: RenderPipeline,
    
    // Mesh data
    vertex_buffer: Buffer,  // Shared vertex buffer for primitives
    index_buffer: Buffer,   // Shared index buffer
    vertex_count: u32,
    index_count: u32,
    
    // Bind groups and buffers for rendering
    camera_buffer: Buffer,
    camera_bind_group: BindGroup,
    material_buffer: Buffer,
    material_bind_group: BindGroup,
    
    // Statistics
    instance_count: usize,
    visible_count: usize,
    frame_count: u64,
}

impl GPUDrivenRenderer {
    // Extract frustum planes from view-projection matrix
    fn extract_frustum_planes(view_proj: &[[f32; 4]; 4]) -> [[f32; 4]; 6] {
        let mut planes = [[0.0; 4]; 6];
        
        // Left plane
        planes[0][0] = view_proj[0][3] + view_proj[0][0];
        planes[0][1] = view_proj[1][3] + view_proj[1][0];
        planes[0][2] = view_proj[2][3] + view_proj[2][0];
        planes[0][3] = view_proj[3][3] + view_proj[3][0];
        
        // Right plane
        planes[1][0] = view_proj[0][3] - view_proj[0][0];
        planes[1][1] = view_proj[1][3] - view_proj[1][0];
        planes[1][2] = view_proj[2][3] - view_proj[2][0];
        planes[1][3] = view_proj[3][3] - view_proj[3][0];
        
        // Bottom plane
        planes[2][0] = view_proj[0][3] + view_proj[0][1];
        planes[2][1] = view_proj[1][3] + view_proj[1][1];
        planes[2][2] = view_proj[2][3] + view_proj[2][1];
        planes[2][3] = view_proj[3][3] + view_proj[3][1];
        
        // Top plane
        planes[3][0] = view_proj[0][3] - view_proj[0][1];
        planes[3][1] = view_proj[1][3] - view_proj[1][1];
        planes[3][2] = view_proj[2][3] - view_proj[2][1];
        planes[3][3] = view_proj[3][3] - view_proj[3][1];
        
        // Near plane
        planes[4][0] = view_proj[0][3] + view_proj[0][2];
        planes[4][1] = view_proj[1][3] + view_proj[1][2];
        planes[4][2] = view_proj[2][3] + view_proj[2][2];
        planes[4][3] = view_proj[3][3] + view_proj[3][2];
        
        // Far plane
        planes[5][0] = view_proj[0][3] - view_proj[0][2];
        planes[5][1] = view_proj[1][3] - view_proj[1][2];
        planes[5][2] = view_proj[2][3] - view_proj[2][2];
        planes[5][3] = view_proj[3][3] - view_proj[3][2];
        
        // Normalize planes
        for plane in &mut planes {
            let length = (plane[0] * plane[0] + plane[1] * plane[1] + plane[2] * plane[2]).sqrt();
            if length > 0.0 {
                plane[0] /= length;
                plane[1] /= length;
                plane[2] /= length;
                plane[3] /= length;
            }
        }
        
        planes
    }

    pub fn new(device: &Device, surface_format: TextureFormat) -> Self {
        // Create persistent instance buffers (double-buffered for async updates)
        let instance_buffer_a = device.create_buffer(&BufferDescriptor {
            label: Some("GPU Instance Buffer A"),
            size: (std::mem::size_of::<GPUInstanceData>() * MAX_INSTANCES) as u64,
            usage: BufferUsages::STORAGE | BufferUsages::VERTEX | BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });
        
        let instance_buffer_b = device.create_buffer(&BufferDescriptor {
            label: Some("GPU Instance Buffer B"),
            size: (std::mem::size_of::<GPUInstanceData>() * MAX_INSTANCES) as u64,
            usage: BufferUsages::STORAGE | BufferUsages::VERTEX | BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });
        
        // Animation parameters buffer
        let animation_params_buffer = device.create_buffer(&BufferDescriptor {
            label: Some("Animation Params"),
            size: std::mem::size_of::<AnimationParams>() as u64,
            usage: BufferUsages::UNIFORM | BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });
        
        // Culling uniforms buffer
        let culling_uniforms_buffer = device.create_buffer(&BufferDescriptor {
            label: Some("Culling Uniforms"),
            size: std::mem::size_of::<CullingUniforms>() as u64,
            usage: BufferUsages::UNIFORM | BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });
        
        // Visible instances buffer (indices of visible instances)
        let visible_instances_buffer = device.create_buffer(&BufferDescriptor {
            label: Some("Visible Instances"),
            size: (std::mem::size_of::<u32>() * MAX_INSTANCES) as u64,
            usage: BufferUsages::STORAGE | BufferUsages::COPY_SRC,
            mapped_at_creation: false,
        });
        
        // Atomic counter for visible instances (with padding for alignment)
        let visible_count_buffer = device.create_buffer(&BufferDescriptor {
            label: Some("Visible Count"),
            size: 16, // 4 u32s for alignment
            usage: BufferUsages::STORAGE | BufferUsages::COPY_DST | BufferUsages::COPY_SRC,
            mapped_at_creation: false,
        });
        
        // Indirect draw buffer
        let indirect_buffer = device.create_buffer(&BufferDescriptor {
            label: Some("Indirect Draw Buffer"),
            size: std::mem::size_of::<DrawIndirectArgs>() as u64 * 16,  // Support up to 16 draw calls
            usage: BufferUsages::STORAGE | BufferUsages::INDIRECT | BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });
        
        // Create animation compute pipeline
        let animation_shader = device.create_shader_module(ShaderModuleDescriptor {
            label: Some("GPU Animation Shader"),
            source: ShaderSource::Wgsl(include_str!("shaders/gpu_animation.wgsl").into()),
        });
        
        let animation_bind_group_layout = device.create_bind_group_layout(&BindGroupLayoutDescriptor {
            label: Some("Animation Bind Group Layout"),
            entries: &[
                BindGroupLayoutEntry {
                    binding: 0,
                    visibility: ShaderStages::COMPUTE,
                    ty: BindingType::Buffer {
                        ty: BufferBindingType::Storage { read_only: false },
                        has_dynamic_offset: false,
                        min_binding_size: None,
                    },
                    count: None,
                },
                BindGroupLayoutEntry {
                    binding: 1,
                    visibility: ShaderStages::COMPUTE,
                    ty: BindingType::Buffer {
                        ty: BufferBindingType::Uniform,
                        has_dynamic_offset: false,
                        min_binding_size: None,
                    },
                    count: None,
                },
            ],
        });
        
        let animation_pipeline_layout = device.create_pipeline_layout(&PipelineLayoutDescriptor {
            label: Some("Animation Pipeline Layout"),
            bind_group_layouts: &[&animation_bind_group_layout],
            push_constant_ranges: &[],
        });
        
        let animation_pipeline = device.create_compute_pipeline(&ComputePipelineDescriptor {
            label: Some("Animation Pipeline"),
            layout: Some(&animation_pipeline_layout),
            module: &animation_shader,
            entry_point: "animate_instances",
            compilation_options: Default::default(),
        });
        
        // Create bind groups for double buffering
        let animation_bind_group_a = device.create_bind_group(&BindGroupDescriptor {
            label: Some("Animation Bind Group A"),
            layout: &animation_bind_group_layout,
            entries: &[
                BindGroupEntry {
                    binding: 0,
                    resource: instance_buffer_a.as_entire_binding(),
                },
                BindGroupEntry {
                    binding: 1,
                    resource: animation_params_buffer.as_entire_binding(),
                },
            ],
        });
        
        let animation_bind_group_b = device.create_bind_group(&BindGroupDescriptor {
            label: Some("Animation Bind Group B"),
            layout: &animation_bind_group_layout,
            entries: &[
                BindGroupEntry {
                    binding: 0,
                    resource: instance_buffer_b.as_entire_binding(),
                },
                BindGroupEntry {
                    binding: 1,
                    resource: animation_params_buffer.as_entire_binding(),
                },
            ],
        });
        
        // Create culling compute pipeline
        let culling_shader = device.create_shader_module(ShaderModuleDescriptor {
            label: Some("GPU Culling Shader"),
            source: ShaderSource::Wgsl(include_str!("shaders/gpu_culling.wgsl").into()),
        });
        
        // Create proper culling bind group layout
        let culling_bind_group_layout = device.create_bind_group_layout(&BindGroupLayoutDescriptor {
            label: Some("Culling Bind Group Layout"),
            entries: &[
                // Instances buffer (read/write)
                BindGroupLayoutEntry {
                    binding: 0,
                    visibility: ShaderStages::COMPUTE,
                    ty: BindingType::Buffer {
                        ty: BufferBindingType::Storage { read_only: false },
                        has_dynamic_offset: false,
                        min_binding_size: None,
                    },
                    count: None,
                },
                // Culling uniforms
                BindGroupLayoutEntry {
                    binding: 1,
                    visibility: ShaderStages::COMPUTE,
                    ty: BindingType::Buffer {
                        ty: BufferBindingType::Uniform,
                        has_dynamic_offset: false,
                        min_binding_size: None,
                    },
                    count: None,
                },
                // Visible instances buffer
                BindGroupLayoutEntry {
                    binding: 2,
                    visibility: ShaderStages::COMPUTE,
                    ty: BindingType::Buffer {
                        ty: BufferBindingType::Storage { read_only: false },
                        has_dynamic_offset: false,
                        min_binding_size: None,
                    },
                    count: None,
                },
                // Draw commands buffer
                BindGroupLayoutEntry {
                    binding: 3,
                    visibility: ShaderStages::COMPUTE,
                    ty: BindingType::Buffer {
                        ty: BufferBindingType::Storage { read_only: false },
                        has_dynamic_offset: false,
                        min_binding_size: None,
                    },
                    count: None,
                },
                // Visible count atomic
                BindGroupLayoutEntry {
                    binding: 4,
                    visibility: ShaderStages::COMPUTE,
                    ty: BindingType::Buffer {
                        ty: BufferBindingType::Storage { read_only: false },
                        has_dynamic_offset: false,
                        min_binding_size: None,
                    },
                    count: None,
                },
            ],
        });
        
        let culling_pipeline_layout = device.create_pipeline_layout(&PipelineLayoutDescriptor {
            label: Some("Culling Pipeline Layout"),
            bind_group_layouts: &[&culling_bind_group_layout],
            push_constant_ranges: &[],
        });
        
        let culling_pipeline = device.create_compute_pipeline(&ComputePipelineDescriptor {
            label: Some("Culling Pipeline"),
            layout: Some(&culling_pipeline_layout),
            module: &culling_shader,
            entry_point: "cull_instances",
            compilation_options: Default::default(),
        });
        
        let indirect_commands_pipeline = device.create_compute_pipeline(&ComputePipelineDescriptor {
            label: Some("Indirect Commands Pipeline"),
            layout: Some(&culling_pipeline_layout),
            module: &culling_shader,
            entry_point: "generate_draw_commands",
            compilation_options: Default::default(),
        });
        
        // Create culling bind groups for double buffering
        let culling_bind_group_a = device.create_bind_group(&BindGroupDescriptor {
            label: Some("Culling Bind Group A"),
            layout: &culling_bind_group_layout,
            entries: &[
                BindGroupEntry {
                    binding: 0,
                    resource: instance_buffer_a.as_entire_binding(),
                },
                BindGroupEntry {
                    binding: 1,
                    resource: culling_uniforms_buffer.as_entire_binding(),
                },
                BindGroupEntry {
                    binding: 2,
                    resource: visible_instances_buffer.as_entire_binding(),
                },
                BindGroupEntry {
                    binding: 3,
                    resource: indirect_buffer.as_entire_binding(),
                },
                BindGroupEntry {
                    binding: 4,
                    resource: visible_count_buffer.as_entire_binding(),
                },
            ],
        });
        
        let culling_bind_group_b = device.create_bind_group(&BindGroupDescriptor {
            label: Some("Culling Bind Group B"),
            layout: &culling_bind_group_layout,
            entries: &[
                BindGroupEntry {
                    binding: 0,
                    resource: instance_buffer_b.as_entire_binding(),
                },
                BindGroupEntry {
                    binding: 1,
                    resource: culling_uniforms_buffer.as_entire_binding(),
                },
                BindGroupEntry {
                    binding: 2,
                    resource: visible_instances_buffer.as_entire_binding(),
                },
                BindGroupEntry {
                    binding: 3,
                    resource: indirect_buffer.as_entire_binding(),
                },
                BindGroupEntry {
                    binding: 4,
                    resource: visible_count_buffer.as_entire_binding(),
                },
            ],
        });
        
        // Create camera uniform buffer first (needed for bind groups)
        let camera_buffer = device.create_buffer(&BufferDescriptor {
            label: Some("GPU Driven Camera Buffer"),
            size: 144,  // view mat4 (64) + proj mat4 (64) + position vec3 + padding (16) = 144 bytes
            usage: BufferUsages::UNIFORM | BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });
        
        // Create material uniform buffer  
        let material_buffer = device.create_buffer(&BufferDescriptor {
            label: Some("GPU Driven Material Buffer"),
            size: 48,  // vec4 + f32 + padding
            usage: BufferUsages::UNIFORM | BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });
        
        // Create bind group layouts (matching the shader expectations)
        let camera_bind_group_layout = device.create_bind_group_layout(&BindGroupLayoutDescriptor {
            label: Some("Camera Bind Group Layout"),
            entries: &[
                BindGroupLayoutEntry {
                    binding: 0,
                    visibility: ShaderStages::VERTEX | ShaderStages::FRAGMENT,
                    ty: BindingType::Buffer {
                        ty: BufferBindingType::Uniform,
                        has_dynamic_offset: false,
                        min_binding_size: Some(NonZeroU64::new(144).unwrap()),
                    },
                    count: None,
                },
            ],
        });
        
        let material_bind_group_layout = device.create_bind_group_layout(&BindGroupLayoutDescriptor {
            label: Some("Material Bind Group Layout"),
            entries: &[
                BindGroupLayoutEntry {
                    binding: 0,
                    visibility: ShaderStages::FRAGMENT,
                    ty: BindingType::Buffer {
                        ty: BufferBindingType::Uniform,
                        has_dynamic_offset: false,
                        min_binding_size: None,
                    },
                    count: None,
                },
            ],
        });
        
        // Create render pipeline
        let shader = device.create_shader_module(ShaderModuleDescriptor {
            label: Some("GPU Driven Render Shader"),
            source: ShaderSource::Wgsl(include_str!("shaders/mesh_basic_instanced.wgsl").into()),
        });
        
        // Define vertex buffer layout for mesh data
        let vertex_buffer_layout = VertexBufferLayout {
            array_stride: std::mem::size_of::<[f32; 8]>() as BufferAddress,  // pos(3) + normal(3) + uv(2)
            step_mode: VertexStepMode::Vertex,
            attributes: &[
                VertexAttribute {
                    format: VertexFormat::Float32x3,
                    offset: 0,
                    shader_location: 0,  // position
                },
                VertexAttribute {
                    format: VertexFormat::Float32x3,
                    offset: 12,
                    shader_location: 1,  // normal
                },
                VertexAttribute {
                    format: VertexFormat::Float32x2,
                    offset: 24,
                    shader_location: 2,  // uv
                },
            ],
        };
        
        // Define instance buffer layout
        let instance_buffer_layout = VertexBufferLayout {
            array_stride: std::mem::size_of::<GPUInstanceData>() as BufferAddress,
            step_mode: VertexStepMode::Instance,
            attributes: &[
                // Model matrix (4x vec4)
                VertexAttribute {
                    format: VertexFormat::Float32x4,
                    offset: 0,
                    shader_location: 3,
                },
                VertexAttribute {
                    format: VertexFormat::Float32x4,
                    offset: 16,
                    shader_location: 4,
                },
                VertexAttribute {
                    format: VertexFormat::Float32x4,
                    offset: 32,
                    shader_location: 5,
                },
                VertexAttribute {
                    format: VertexFormat::Float32x4,
                    offset: 48,
                    shader_location: 6,
                },
                // Color
                VertexAttribute {
                    format: VertexFormat::Float32x4,
                    offset: 64,
                    shader_location: 7,
                },
            ],
        };
        
        // Create pipeline layout for render pipeline
        let render_pipeline_layout = device.create_pipeline_layout(&PipelineLayoutDescriptor {
            label: Some("GPU Driven Render Pipeline Layout"),
            bind_group_layouts: &[&camera_bind_group_layout, &material_bind_group_layout],
            push_constant_ranges: &[],
        });

        let render_pipeline = device.create_render_pipeline(&RenderPipelineDescriptor {
            label: Some("GPU Driven Render Pipeline"),
            layout: Some(&render_pipeline_layout),
            vertex: VertexState {
                module: &shader,
                entry_point: "vs_main",
                compilation_options: Default::default(),
                buffers: &[vertex_buffer_layout, instance_buffer_layout],
            },
            fragment: Some(FragmentState {
                module: &shader,
                entry_point: "fs_main",
                compilation_options: Default::default(),
                targets: &[Some(ColorTargetState {
                    format: surface_format,
                    blend: Some(BlendState::REPLACE),
                    write_mask: ColorWrites::ALL,
                })],
            }),
            primitive: PrimitiveState {
                topology: PrimitiveTopology::TriangleList,
                ..Default::default()
            },
            depth_stencil: Some(DepthStencilState {
                format: TextureFormat::Depth32Float,
                depth_write_enabled: true,
                depth_compare: CompareFunction::Less,
                stencil: StencilState::default(),
                bias: DepthBiasState::default(),
            }),
            multisample: MultisampleState::default(),
            multiview: None,
        });
        
        
        // Create bind groups
        let camera_bind_group = device.create_bind_group(&BindGroupDescriptor {
            label: Some("Camera Bind Group"),
            layout: &camera_bind_group_layout,
            entries: &[
                BindGroupEntry {
                    binding: 0,
                    resource: camera_buffer.as_entire_binding(),
                },
            ],
        });
        
        let material_bind_group = device.create_bind_group(&BindGroupDescriptor {
            label: Some("Material Bind Group"),
            layout: &material_bind_group_layout,
            entries: &[
                BindGroupEntry {
                    binding: 0,
                    resource: material_buffer.as_entire_binding(),
                },
            ],
        });
        
        // Create a simple sphere mesh for testing (will be replaced with proper mesh management)
        let (vertices, indices) = create_sphere_mesh(1.0, 16, 16);
        
        let vertex_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("GPU Driven Vertex Buffer"),
            contents: bytemuck::cast_slice(&vertices),
            usage: BufferUsages::VERTEX,
        });
        
        let index_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("GPU Driven Index Buffer"),
            contents: bytemuck::cast_slice(&indices),
            usage: BufferUsages::INDEX,
        });
        
        let vertex_count = vertices.len() as u32;
        let index_count = indices.len() as u32;
        
        Self {
            instance_buffer_a,
            instance_buffer_b,
            current_buffer: false,
            vertex_buffer,
            index_buffer,
            vertex_count,
            index_count,
            animation_pipeline,
            animation_bind_group_a,
            animation_bind_group_b,
            animation_params_buffer,
            culling_pipeline,
            culling_bind_group_a,
            culling_bind_group_b,
            culling_uniforms_buffer,
            visible_instances_buffer,
            visible_count_buffer,
            indirect_buffer,
            indirect_commands_pipeline,
            render_pipeline,
            camera_buffer,
            camera_bind_group,
            material_buffer,
            material_bind_group,
            instance_count: 0,
            visible_count: 0,
            frame_count: 0,
        }
    }
    
    /// Update camera uniforms
    pub fn update_camera(&mut self, queue: &Queue, view: [[f32; 4]; 4], proj: [[f32; 4]; 4], position: [f32; 3]) {
        // Pack camera data
        let mut camera_data = Vec::new();
        // View matrix (64 bytes)
        for row in view {
            camera_data.extend_from_slice(&row);
        }
        // Projection matrix (64 bytes)
        for row in proj {
            camera_data.extend_from_slice(&row);
        }
        // Position + padding (16 bytes)
        camera_data.extend_from_slice(&position);
        camera_data.push(0.0); // padding
        
        queue.write_buffer(&self.camera_buffer, 0, bytemuck::cast_slice(&camera_data));
    }
    
    /// Update material uniforms
    pub fn update_material(&mut self, queue: &Queue, color: [f32; 4], opacity: f32) {
        let material_data = [
            color[0], color[1], color[2], color[3],
            opacity, 0.0, 0.0, 0.0,  // opacity + padding
            0.0, 0.0, 0.0, 0.0,      // more padding for alignment
        ];
        queue.write_buffer(&self.material_buffer, 0, bytemuck::cast_slice(&material_data));
    }
    
    /// Update mesh with a different primitive type
    pub fn set_mesh(&mut self, device: &Device, primitive: PrimitiveType) {
        let (vertices, indices) = create_primitive_mesh(primitive);
        
        self.vertex_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("GPU Driven Vertex Buffer"),
            contents: bytemuck::cast_slice(&vertices),
            usage: BufferUsages::VERTEX,
        });
        
        self.index_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("GPU Driven Index Buffer"),
            contents: bytemuck::cast_slice(&indices),
            usage: BufferUsages::INDEX,
        });
        
        self.vertex_count = vertices.len() as u32 / 8; // 8 floats per vertex
        self.index_count = indices.len() as u32;
    }
    
    /// Upload initial instance data (only done once or when scene changes)
    pub fn upload_instances(&mut self, instances: &[GPUInstanceData], queue: &Queue) {
        self.instance_count = instances.len();
        println!("\x1b[36mGPU\x1b[0m Uploading {} instances", instances.len());
        
        // Upload to both buffers initially
        let data = bytemuck::cast_slice(instances);
        queue.write_buffer(&self.instance_buffer_a, 0, data);
        queue.write_buffer(&self.instance_buffer_b, 0, data);
    }
    
    /// Execute GPU-driven rendering pipeline
    pub fn render(
        &mut self,
        encoder: &mut CommandEncoder,
        view: &TextureView,
        depth_view: &TextureView,
        camera: &CullingUniforms,
        delta_time: f32,
        queue: &Queue,
    ) {
        self.frame_count += 1;
        
        // Log performance periodically
        if self.frame_count % 60 == 0 {
            println!("🎮 GPU-driven: Frame {}, {} instances active", self.frame_count, self.instance_count);
        }
        
        // Update animation parameters
        let animation_params = AnimationParams {
            delta_time,
            total_time: self.frame_count as f32 * delta_time,
            instance_count: self.instance_count as u32,
            _padding: 0,
        };
        queue.write_buffer(&self.animation_params_buffer, 0, bytemuck::cast_slice(&[animation_params]));
        
        // Update culling uniforms with properly extracted frustum planes
        let mut culling_uniforms = *camera;
        culling_uniforms.frustum_planes = Self::extract_frustum_planes(&camera.view_proj);
        culling_uniforms.instance_count = self.instance_count as u32;
        
        // Debug: Check if matrices and frustum planes are valid
        let view_proj_valid = camera.view_proj.iter()
            .any(|row| row.iter().any(|&v| v != 0.0));
        if !view_proj_valid {
            println!("⚠️ WARNING: View-projection matrix is all zero!");
        } else if self.frame_count % 60 == 0 {
            // Print debug info every 60 frames
            println!("\x1b[2mDEBUG\x1b[0m View-proj[0][0]: {:.3}, Camera pos: ({:.1}, {:.1}, {:.1})", 
                camera.view_proj[0][0],
                camera.camera_pos[0], camera.camera_pos[1], camera.camera_pos[2]);
            println!("  Frustum plane[0]: ({:.3}, {:.3}, {:.3}, {:.3})",
                culling_uniforms.frustum_planes[0][0],
                culling_uniforms.frustum_planes[0][1],
                culling_uniforms.frustum_planes[0][2],
                culling_uniforms.frustum_planes[0][3]);
        }
        
        queue.write_buffer(&self.culling_uniforms_buffer, 0, bytemuck::cast_slice(&[culling_uniforms]));
        
        // Reset visible count
        queue.write_buffer(&self.visible_count_buffer, 0, bytemuck::cast_slice(&[0u32, 0, 0, 0]));
        
        // 1. Animation compute pass
        {
            let mut compute_pass = encoder.begin_compute_pass(&ComputePassDescriptor {
                label: Some("Animation Pass"),
                timestamp_writes: None,
            });
            
            compute_pass.set_pipeline(&self.animation_pipeline);
            let bind_group = if self.current_buffer {
                &self.animation_bind_group_b
            } else {
                &self.animation_bind_group_a
            };
            compute_pass.set_bind_group(0, bind_group, &[]);
            
            let workgroups = (self.instance_count as u32 + WORKGROUP_SIZE - 1) / WORKGROUP_SIZE;
            compute_pass.dispatch_workgroups(workgroups, 1, 1);
        }
        
        // 2. Culling compute pass
        {
            let mut compute_pass = encoder.begin_compute_pass(&ComputePassDescriptor {
                label: Some("Culling Pass"),
                timestamp_writes: None,
            });
            
            compute_pass.set_pipeline(&self.culling_pipeline);
            let bind_group = if self.current_buffer {
                &self.culling_bind_group_b
            } else {
                &self.culling_bind_group_a
            };
            compute_pass.set_bind_group(0, bind_group, &[]);
            
            let workgroups = (self.instance_count as u32 + WORKGROUP_SIZE - 1) / WORKGROUP_SIZE;
            compute_pass.dispatch_workgroups(workgroups, 1, 1);
        }
        
        // 3. Generate indirect draw commands
        {
            let mut compute_pass = encoder.begin_compute_pass(&ComputePassDescriptor {
                label: Some("Indirect Commands Pass"),
                timestamp_writes: None,
            });
            
            compute_pass.set_pipeline(&self.indirect_commands_pipeline);
            let bind_group = if self.current_buffer {
                &self.culling_bind_group_b
            } else {
                &self.culling_bind_group_a
            };
            compute_pass.set_bind_group(0, bind_group, &[]);
            compute_pass.dispatch_workgroups(1, 1, 1);
        }
        
        // 4. Render pass with indirect drawing
        {
            let mut render_pass = encoder.begin_render_pass(&RenderPassDescriptor {
                label: Some("GPU Driven Render Pass"),
                color_attachments: &[Some(RenderPassColorAttachment {
                    view,
                    resolve_target: None,
                    ops: Operations {
                        load: LoadOp::Clear(Color {
                            r: 0.1,
                            g: 0.2,
                            b: 0.3,
                            a: 1.0,
                        }),
                        store: StoreOp::Store,
                    },
                })],
                depth_stencil_attachment: Some(RenderPassDepthStencilAttachment {
                    view: depth_view,
                    depth_ops: Some(Operations {
                        load: LoadOp::Clear(1.0),
                        store: StoreOp::Store,
                    }),
                    stencil_ops: None,
                }),
                ..Default::default()
            });
            
            render_pass.set_pipeline(&self.render_pipeline);
            
            // Use current instance buffer
            let instance_buffer = if self.current_buffer {
                &self.instance_buffer_b
            } else {
                &self.instance_buffer_a
            };
            
            // Set vertex and instance buffers
            render_pass.set_vertex_buffer(0, self.vertex_buffer.slice(..));
            render_pass.set_vertex_buffer(1, instance_buffer.slice(..));
            render_pass.set_index_buffer(self.index_buffer.slice(..), IndexFormat::Uint32);
            
            // Set bind groups for camera and material
            render_pass.set_bind_group(0, &self.camera_bind_group, &[]);
            render_pass.set_bind_group(1, &self.material_bind_group, &[]);
            
            // Only draw if we have instances
            if self.instance_count > 0 {
                // Use indirect drawing with GPU-generated commands
                render_pass.draw_indexed_indirect(&self.indirect_buffer, 0);
            }
        }
        
        // Toggle buffer for next frame
        self.current_buffer = !self.current_buffer;
    }
    
    pub fn get_stats(&self) -> GPUDrivenStats {
        GPUDrivenStats {
            total_instances: self.instance_count,
            visible_instances: self.visible_count,
            culled_instances: self.instance_count - self.visible_count,
            frame_count: self.frame_count,
        }
    }
}

pub struct GPUDrivenStats {
    pub total_instances: usize,
    pub visible_instances: usize,
    pub culled_instances: usize,
    pub frame_count: u64,
}

// Mesh creation helpers for different primitive types
pub enum PrimitiveType {
    Sphere { radius: f32, sectors: u32, stacks: u32 },
    Cube { size: f32 },
    Cylinder { radius: f32, height: f32, segments: u32 },
    Cone { radius: f32, height: f32, segments: u32 },
}

fn create_primitive_mesh(primitive: PrimitiveType) -> (Vec<f32>, Vec<u32>) {
    match primitive {
        PrimitiveType::Sphere { radius, sectors, stacks } => create_sphere_mesh(radius, sectors, stacks),
        PrimitiveType::Cube { size } => create_cube_mesh(size),
        PrimitiveType::Cylinder { radius, height, segments } => create_cylinder_mesh(radius, height, segments),
        PrimitiveType::Cone { radius, height, segments } => create_cone_mesh(radius, height, segments),
    }
}

fn create_sphere_mesh(radius: f32, sectors: u32, stacks: u32) -> (Vec<f32>, Vec<u32>) {
    let mut vertices = Vec::new();
    let mut indices = Vec::new();
    
    let sector_step = 2.0 * std::f32::consts::PI / sectors as f32;
    let stack_step = std::f32::consts::PI / stacks as f32;
    
    // Generate vertices
    for i in 0..=stacks {
        let stack_angle = std::f32::consts::PI / 2.0 - i as f32 * stack_step;
        let xy = radius * stack_angle.cos();
        let z = radius * stack_angle.sin();
        
        for j in 0..=sectors {
            let sector_angle = j as f32 * sector_step;
            let x = xy * sector_angle.cos();
            let y = xy * sector_angle.sin();
            
            // Position
            vertices.push(x);
            vertices.push(y);
            vertices.push(z);
            
            // Normal (normalized position for sphere)
            let len = (x * x + y * y + z * z).sqrt();
            vertices.push(x / len);
            vertices.push(y / len);
            vertices.push(z / len);
            
            // UV
            vertices.push(j as f32 / sectors as f32);
            vertices.push(i as f32 / stacks as f32);
        }
    }
    
    // Generate indices
    for i in 0..stacks {
        let k1 = i * (sectors + 1);
        let k2 = k1 + sectors + 1;
        
        for j in 0..sectors {
            if i != 0 {
                indices.push(k1 + j);
                indices.push(k2 + j);
                indices.push(k1 + j + 1);
            }
            
            if i != (stacks - 1) {
                indices.push(k1 + j + 1);
                indices.push(k2 + j);
                indices.push(k2 + j + 1);
            }
        }
    }
    
    (vertices, indices)
}

fn create_cube_mesh(size: f32) -> (Vec<f32>, Vec<u32>) {
    let h = size / 2.0;
    
    // Vertices: position (3) + normal (3) + uv (2) = 8 floats per vertex
    let vertices = vec![
        // Front face
        -h, -h,  h,  0.0, 0.0, 1.0,  0.0, 0.0,
         h, -h,  h,  0.0, 0.0, 1.0,  1.0, 0.0,
         h,  h,  h,  0.0, 0.0, 1.0,  1.0, 1.0,
        -h,  h,  h,  0.0, 0.0, 1.0,  0.0, 1.0,
        // Back face
        -h, -h, -h,  0.0, 0.0, -1.0,  0.0, 0.0,
        -h,  h, -h,  0.0, 0.0, -1.0,  0.0, 1.0,
         h,  h, -h,  0.0, 0.0, -1.0,  1.0, 1.0,
         h, -h, -h,  0.0, 0.0, -1.0,  1.0, 0.0,
        // Top face
        -h,  h, -h,  0.0, 1.0, 0.0,  0.0, 0.0,
        -h,  h,  h,  0.0, 1.0, 0.0,  0.0, 1.0,
         h,  h,  h,  0.0, 1.0, 0.0,  1.0, 1.0,
         h,  h, -h,  0.0, 1.0, 0.0,  1.0, 0.0,
        // Bottom face
        -h, -h, -h,  0.0, -1.0, 0.0,  0.0, 0.0,
         h, -h, -h,  0.0, -1.0, 0.0,  1.0, 0.0,
         h, -h,  h,  0.0, -1.0, 0.0,  1.0, 1.0,
        -h, -h,  h,  0.0, -1.0, 0.0,  0.0, 1.0,
        // Right face
         h, -h, -h,  1.0, 0.0, 0.0,  0.0, 0.0,
         h,  h, -h,  1.0, 0.0, 0.0,  0.0, 1.0,
         h,  h,  h,  1.0, 0.0, 0.0,  1.0, 1.0,
         h, -h,  h,  1.0, 0.0, 0.0,  1.0, 0.0,
        // Left face
        -h, -h, -h,  -1.0, 0.0, 0.0,  0.0, 0.0,
        -h, -h,  h,  -1.0, 0.0, 0.0,  1.0, 0.0,
        -h,  h,  h,  -1.0, 0.0, 0.0,  1.0, 1.0,
        -h,  h, -h,  -1.0, 0.0, 0.0,  0.0, 1.0,
    ];
    
    let indices = vec![
        0,  1,  2,  0,  2,  3,   // front
        4,  5,  6,  4,  6,  7,   // back
        8,  9,  10, 8,  10, 11,  // top
        12, 13, 14, 12, 14, 15,  // bottom
        16, 17, 18, 16, 18, 19,  // right
        20, 21, 22, 20, 22, 23,  // left
    ];
    
    (vertices, indices)
}

fn create_cylinder_mesh(radius: f32, height: f32, segments: u32) -> (Vec<f32>, Vec<u32>) {
    let mut vertices = Vec::new();
    let mut indices = Vec::new();
    
    let h = height / 2.0;
    let angle_step = 2.0 * std::f32::consts::PI / segments as f32;
    
    // Generate vertices for top and bottom circles
    for i in 0..=segments {
        let angle = i as f32 * angle_step;
        let x = radius * angle.cos();
        let z = radius * angle.sin();
        let u = i as f32 / segments as f32;
        
        // Top circle
        vertices.extend_from_slice(&[x, h, z, x/radius, 0.0, z/radius, u, 1.0]);
        // Bottom circle  
        vertices.extend_from_slice(&[x, -h, z, x/radius, 0.0, z/radius, u, 0.0]);
    }
    
    // Generate indices for the cylinder sides
    for i in 0..segments {
        let top1 = i * 2;
        let bottom1 = i * 2 + 1;
        let top2 = (i + 1) * 2;
        let bottom2 = (i + 1) * 2 + 1;
        
        indices.extend_from_slice(&[top1, bottom1, top2]);
        indices.extend_from_slice(&[bottom1, bottom2, top2]);
    }
    
    (vertices, indices)
}

fn create_cone_mesh(radius: f32, height: f32, segments: u32) -> (Vec<f32>, Vec<u32>) {
    let mut vertices = Vec::new();
    let mut indices = Vec::new();
    
    // Apex vertex
    vertices.extend_from_slice(&[0.0, height/2.0, 0.0, 0.0, 1.0, 0.0, 0.5, 1.0]);
    
    // Base circle vertices
    let angle_step = 2.0 * std::f32::consts::PI / segments as f32;
    for i in 0..=segments {
        let angle = i as f32 * angle_step;
        let x = radius * angle.cos();
        let z = radius * angle.sin();
        let u = i as f32 / segments as f32;
        
        vertices.extend_from_slice(&[x, -height/2.0, z, x/radius, 0.0, z/radius, u, 0.0]);
    }
    
    // Generate indices
    for i in 0..segments {
        indices.extend_from_slice(&[0, i + 1, i + 2]);
    }
    
    (vertices, indices)
}
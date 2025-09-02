//! MeshBasicMaterial implementation similar to Three.js

use wgpu::*;
use wgpu::util::DeviceExt;
use bytemuck::{Pod, Zeroable};

/// Side rendering mode
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Side {
    Front,
    Back,
    Double,
}

/// MeshBasicMaterial properties
#[derive(Debug)]
pub struct MeshBasicMaterial {
    pub color: [f32; 4],
    pub opacity: f32,
    pub transparent: bool,
    pub side: Side,
    pub wireframe: bool,
    pub visible: bool,
    
    // WebGPU resources (not cloneable)
    pipeline: Option<RenderPipeline>,
    bind_group: Option<BindGroup>,
    uniform_buffer: Option<Buffer>,
    bind_group_layout: Option<BindGroupLayout>,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, Pod, Zeroable)]
pub struct MeshBasicMaterialUniforms {
    pub color: [f32; 4],
    pub opacity: f32,
    pub _padding: [f32; 3],
}

impl Default for MeshBasicMaterial {
    fn default() -> Self {
        Self {
            color: [1.0, 1.0, 1.0, 1.0],
            opacity: 1.0,
            transparent: false,
            side: Side::Front,
            wireframe: false,
            visible: true,
            pipeline: None,
            bind_group: None,
            uniform_buffer: None,
            bind_group_layout: None,
        }
    }
}

impl MeshBasicMaterial {
    pub fn new() -> Self {
        Self::default()
    }
    
    pub fn with_color(mut self, r: f32, g: f32, b: f32) -> Self {
        self.color = [r, g, b, 1.0];
        self
    }
    
    pub fn with_opacity(mut self, opacity: f32) -> Self {
        self.opacity = opacity;
        self.transparent = opacity < 1.0;
        self
    }
    
    pub fn with_side(mut self, side: Side) -> Self {
        self.side = side;
        self
    }
    
    pub fn init(
        &mut self, 
        device: &Device,
        format: TextureFormat,
        camera_bind_group_layout: &BindGroupLayout,
    ) {
        // Create material bind group layout
        let bind_group_layout = device.create_bind_group_layout(&BindGroupLayoutDescriptor {
            label: Some("MeshBasicMaterial Bind Group Layout"),
            entries: &[
                BindGroupLayoutEntry {
                    binding: 0,
                    visibility: ShaderStages::VERTEX | ShaderStages::FRAGMENT,
                    ty: BindingType::Buffer {
                        ty: BufferBindingType::Uniform,
                        has_dynamic_offset: false,
                        min_binding_size: None,
                    },
                    count: None,
                },
            ],
        });
        
        // Create uniform buffer
        let uniforms = MeshBasicMaterialUniforms {
            color: self.color,
            opacity: self.opacity,
            _padding: [0.0; 3],
        };
        
        let uniform_buffer = device.create_buffer_init(&util::BufferInitDescriptor {
            label: Some("MeshBasicMaterial Uniform Buffer"),
            contents: bytemuck::cast_slice(&[uniforms]),
            usage: BufferUsages::UNIFORM | BufferUsages::COPY_DST,
        });
        
        // Create bind group
        let bind_group = device.create_bind_group(&BindGroupDescriptor {
            label: Some("MeshBasicMaterial Bind Group"),
            layout: &bind_group_layout,
            entries: &[
                BindGroupEntry {
                    binding: 0,
                    resource: uniform_buffer.as_entire_binding(),
                },
            ],
        });
        
        // Load shader
        let shader = device.create_shader_module(ShaderModuleDescriptor {
            label: Some("MeshBasicMaterial Shader"),
            source: ShaderSource::Wgsl(include_str!("../shaders/mesh_basic_material.wgsl").into()),
        });
        
        // Configure cull mode based on side property
        let cull_mode = match self.side {
            Side::Front => Some(Face::Back),
            Side::Back => Some(Face::Front),
            Side::Double => None,
        };
        
        // Create render pipeline
        let pipeline_layout = device.create_pipeline_layout(&PipelineLayoutDescriptor {
            label: Some("MeshBasicMaterial Pipeline Layout"),
            bind_group_layouts: &[camera_bind_group_layout, &bind_group_layout],
            push_constant_ranges: &[],
        });
        
        // Check if wireframe is supported
        let supports_wireframe = device.features().contains(Features::POLYGON_MODE_LINE);
        
        // Adjust polygon mode based on device capabilities
        let polygon_mode = if self.wireframe && supports_wireframe {
            PolygonMode::Line
        } else {
            if self.wireframe && !supports_wireframe {
                log::warn!("Wireframe mode requested but POLYGON_MODE_LINE feature not available. Using fill mode.");
            }
            PolygonMode::Fill
        };
        
        // For wireframe without feature support, we can use line topology as a fallback
        let topology = if self.wireframe && !supports_wireframe {
            PrimitiveTopology::LineList
        } else {
            PrimitiveTopology::TriangleList
        };
        
        let pipeline = device.create_render_pipeline(&RenderPipelineDescriptor {
            label: Some("MeshBasicMaterial Pipeline"),
            layout: Some(&pipeline_layout),
            vertex: VertexState {
                module: &shader,
                entry_point: "vs_main",
                buffers: &[crate::render3d::Vertex3D::desc()],
                compilation_options: PipelineCompilationOptions::default(),
            },
            fragment: Some(FragmentState {
                module: &shader,
                entry_point: "fs_main",
                targets: &[Some(ColorTargetState {
                    format,
                    blend: if self.transparent {
                        Some(BlendState::ALPHA_BLENDING)
                    } else {
                        Some(BlendState::REPLACE)
                    },
                    write_mask: ColorWrites::ALL,
                })],
                compilation_options: PipelineCompilationOptions::default(),
            }),
            primitive: PrimitiveState {
                topology,
                strip_index_format: None,
                front_face: FrontFace::Ccw,
                cull_mode,
                unclipped_depth: false,
                polygon_mode,
                conservative: false,
            },
            depth_stencil: Some(DepthStencilState {
                format: TextureFormat::Depth32Float,
                depth_write_enabled: !self.transparent,
                depth_compare: CompareFunction::Less,
                stencil: StencilState::default(),
                bias: DepthBiasState::default(),
            }),
            multisample: MultisampleState {
                count: 1,
                mask: !0,
                alpha_to_coverage_enabled: false,
            },
            multiview: None,
        });
        
        self.pipeline = Some(pipeline);
        self.bind_group = Some(bind_group);
        self.uniform_buffer = Some(uniform_buffer);
        self.bind_group_layout = Some(bind_group_layout);
    }
    
    pub fn update(&self, queue: &Queue) {
        if let Some(uniform_buffer) = &self.uniform_buffer {
            let uniforms = MeshBasicMaterialUniforms {
                color: self.color,
                opacity: self.opacity,
                _padding: [0.0; 3],
            };
            queue.write_buffer(
                uniform_buffer,
                0,
                bytemuck::cast_slice(&[uniforms]),
            );
        }
    }
    
    pub fn get_pipeline(&self) -> Option<&RenderPipeline> {
        self.pipeline.as_ref()
    }
    
    pub fn get_bind_group(&self) -> Option<&BindGroup> {
        self.bind_group.as_ref()
    }
}
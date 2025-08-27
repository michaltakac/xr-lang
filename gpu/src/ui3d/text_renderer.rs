//! 3D Text rendering system for UI components

use crate::math::*;
use wgpu::*;
use bytemuck::{Pod, Zeroable};

#[repr(C)]
#[derive(Debug, Clone, Copy, Pod, Zeroable)]
pub struct TextVertex {
    pub position: [f32; 3],
    pub uv: [f32; 2],
    pub color: [f32; 4],
}

#[repr(C)]
#[derive(Debug, Clone, Copy, Pod, Zeroable)]
pub struct TextUniforms {
    pub transform: Mat4,
    pub _pad: [f32; 3],
}

/// 3D Text renderer using bitmap fonts
pub struct TextRenderer3D {
    pub pipeline: RenderPipeline,
    pub uniform_buffer: Buffer,
    pub uniform_bind_group: BindGroup,
    pub font_atlas: Texture,
    pub font_sampler: Sampler,
    
    // Font metrics
    pub char_width: f32,
    pub char_height: f32,
    pub chars_per_row: u32,
}

impl TextRenderer3D {
    pub fn new(device: &Device, config: &SurfaceConfiguration) -> Self {
        // Create simple bitmap font texture (8x8 ASCII grid)
        let font_atlas = Self::create_bitmap_font(device);
        
        let font_sampler = device.create_sampler(&SamplerDescriptor {
            label: Some("Font Sampler"),
            address_mode_u: AddressMode::ClampToEdge,
            address_mode_v: AddressMode::ClampToEdge,
            address_mode_w: AddressMode::ClampToEdge,
            mag_filter: FilterMode::Nearest,
            min_filter: FilterMode::Nearest,
            mipmap_filter: FilterMode::Nearest,
            ..Default::default()
        });
        
        let uniform_buffer = device.create_buffer(&BufferDescriptor {
            label: Some("Text Uniform Buffer"),
            size: std::mem::size_of::<TextUniforms>() as u64,
            usage: BufferUsages::UNIFORM | BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });
        
        let bind_group_layout = device.create_bind_group_layout(&BindGroupLayoutDescriptor {
            label: Some("Text Bind Group Layout"),
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
                BindGroupLayoutEntry {
                    binding: 1,
                    visibility: ShaderStages::FRAGMENT,
                    ty: BindingType::Texture {
                        sample_type: TextureSampleType::Float { filterable: true },
                        view_dimension: TextureViewDimension::D2,
                        multisampled: false,
                    },
                    count: None,
                },
                BindGroupLayoutEntry {
                    binding: 2,
                    visibility: ShaderStages::FRAGMENT,
                    ty: BindingType::Sampler(SamplerBindingType::Filtering),
                    count: None,
                },
            ],
        });
        
        let uniform_bind_group = device.create_bind_group(&BindGroupDescriptor {
            label: Some("Text Bind Group"),
            layout: &bind_group_layout,
            entries: &[
                BindGroupEntry {
                    binding: 0,
                    resource: uniform_buffer.as_entire_binding(),
                },
                BindGroupEntry {
                    binding: 1,
                    resource: BindingResource::TextureView(&font_atlas.create_view(&TextureViewDescriptor::default())),
                },
                BindGroupEntry {
                    binding: 2,
                    resource: BindingResource::Sampler(&font_sampler),
                },
            ],
        });
        
        let shader = device.create_shader_module(ShaderModuleDescriptor {
            label: Some("Text Shader"),
            source: ShaderSource::Wgsl(include_str!("../shaders/text_3d.wgsl").into()),
        });
        
        let pipeline_layout = device.create_pipeline_layout(&PipelineLayoutDescriptor {
            label: Some("Text Pipeline Layout"),
            bind_group_layouts: &[&bind_group_layout],
            push_constant_ranges: &[],
        });
        
        let pipeline = device.create_render_pipeline(&RenderPipelineDescriptor {
            label: Some("Text Pipeline"),
            layout: Some(&pipeline_layout),
            vertex: VertexState {
                module: &shader,
                entry_point: "vs_main",
                buffers: &[VertexBufferLayout {
                    array_stride: std::mem::size_of::<TextVertex>() as BufferAddress,
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
                            format: VertexFormat::Float32x2,
                        },
                        VertexAttribute {
                            offset: (std::mem::size_of::<[f32; 3]>() + std::mem::size_of::<[f32; 2]>()) as BufferAddress,
                            shader_location: 2,
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
                topology: PrimitiveTopology::TriangleList,
                strip_index_format: None,
                front_face: FrontFace::Ccw,
                cull_mode: None, // Allow text to be visible from both sides
                unclipped_depth: false,
                polygon_mode: PolygonMode::Fill,
                conservative: false,
            },
            depth_stencil: Some(DepthStencilState {
                format: TextureFormat::Depth32Float,
                depth_write_enabled: false, // Text should not write depth
                depth_compare: CompareFunction::Less,
                stencil: StencilState::default(),
                bias: DepthBiasState::default(),
            }),
            multisample: MultisampleState::default(),
            multiview: None,
        });
        
        Self {
            pipeline,
            uniform_buffer,
            uniform_bind_group,
            font_atlas,
            font_sampler,
            char_width: 1.0 / 16.0,  // 16x16 character grid
            char_height: 1.0 / 16.0,
            chars_per_row: 16,
        }
    }
    
    /// Create a simple bitmap font texture
    fn create_bitmap_font(device: &Device, queue: &wgpu::Queue) -> Texture {
        // Create a simple 8x8 pixel font for each character in a 16x16 grid
        let font_size = 128; // 16 chars * 8 pixels each
        let mut font_data = vec![0u8; (font_size * font_size) as usize];
        
        // Generate simple pixel font data (just rectangles for now)
        for char_y in 0..16 {
            for char_x in 0..16 {
                let char_code = char_y * 16 + char_x;
                
                // Simple pattern for visible characters
                if char_code >= 32 && char_code < 127 {
                    for py in 1..7 {
                        for px in 1..7 {
                            let x = char_x * 8 + px;
                            let y = char_y * 8 + py;
                            font_data[(y * font_size + x) as usize] = 255;
                        }
                    }
                }
            }
        }
        
        let texture = device.create_texture(&TextureDescriptor {
            label: Some("Font Atlas"),
            size: Extent3d {
                width: font_size,
                height: font_size,
                depth_or_array_layers: 1,
            },
            mip_level_count: 1,
            sample_count: 1,
            dimension: TextureDimension::D2,
            format: TextureFormat::R8Unorm,
            usage: TextureUsages::TEXTURE_BINDING | TextureUsages::COPY_DST,
            view_formats: &[],
        });
        
        // Upload font data
        device.queue().write_texture(
            ImageCopyTexture {
                texture: &texture,
                mip_level: 0,
                origin: Origin3d::ZERO,
                aspect: TextureAspect::All,
            },
            &font_data,
            ImageDataLayout {
                offset: 0,
                bytes_per_row: Some(font_size),
                rows_per_image: Some(font_size),
            },
            Extent3d {
                width: font_size,
                height: font_size,
                depth_or_array_layers: 1,
            },
        );
        
        texture
    }
    
    /// Generate mesh for rendering text string
    pub fn create_text_mesh(&self, text: &str, transform: &Transform, color: [f32; 4]) -> (Vec<TextVertex>, Vec<u16>) {
        let mut vertices = Vec::new();
        let mut indices = Vec::new();
        
        let lines: Vec<&str> = text.split('\n').collect();
        let char_size = 0.1; // Size of each character in world units
        
        for (line_idx, line) in lines.iter().enumerate() {
            let line_y = -(line_idx as f32) * char_size * 1.2; // Line spacing
            
            for (char_idx, ch) in line.char_indices() {
                let char_x = char_idx as f32 * char_size;
                
                // Calculate UV coordinates for this character
                let char_code = ch as u8;
                let grid_x = (char_code % 16) as f32;
                let grid_y = (char_code / 16) as f32;
                
                let u0 = grid_x * self.char_width;
                let u1 = (grid_x + 1.0) * self.char_width;
                let v0 = grid_y * self.char_height;
                let v1 = (grid_y + 1.0) * self.char_height;
                
                // Create quad for this character
                let base_idx = vertices.len() as u16;
                
                // Transform positions
                let pos_tl = transform.position + Vec3::new(char_x, line_y + char_size, 0.0);
                let pos_tr = transform.position + Vec3::new(char_x + char_size, line_y + char_size, 0.0);
                let pos_bl = transform.position + Vec3::new(char_x, line_y, 0.0);
                let pos_br = transform.position + Vec3::new(char_x + char_size, line_y, 0.0);
                
                vertices.extend_from_slice(&[
                    TextVertex { position: pos_tl.into(), uv: [u0, v0], color },
                    TextVertex { position: pos_tr.into(), uv: [u1, v0], color },
                    TextVertex { position: pos_bl.into(), uv: [u0, v1], color },
                    TextVertex { position: pos_br.into(), uv: [u1, v1], color },
                ]);
                
                // Two triangles per character
                indices.extend_from_slice(&[
                    base_idx, base_idx + 1, base_idx + 2,
                    base_idx + 1, base_idx + 3, base_idx + 2,
                ]);
            }
        }
        
        (vertices, indices)
    }
}
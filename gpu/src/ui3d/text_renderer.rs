//! 3D Text rendering system for UI components with SDF support

use crate::math::*;
use wgpu::*;
use wgpu::util::DeviceExt;
use bytemuck::{Pod, Zeroable};
use rusttype::{Font, Scale, point};
use std::collections::HashMap;
use super::sdf_generator;
use super::debug_atlas;

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

/// Glyph information in the atlas
#[derive(Debug, Clone)]
struct GlyphInfo {
    tex_coords: (f32, f32, f32, f32), // min_u, min_v, max_u, max_v
    size: (f32, f32),
    offset: (f32, f32),
}

/// 3D Text renderer using rusttype with TTF fonts
pub struct TextRenderer3D {
    pub pipeline: RenderPipeline,
    pub uniform_buffer: Buffer,
    pub uniform_bind_group: BindGroup,
    pub font_atlas: Texture,
    pub font_sampler: Sampler,
    
    // Font and atlas info
    font_data: Vec<u8>, // Store current font data
    glyph_cache: HashMap<char, GlyphInfo>,
    atlas_size: u32,
    font_size: f32,
    available_fonts: HashMap<String, Vec<u8>>,
}

impl TextRenderer3D {
    pub fn new(device: &Device, queue: &Queue, config: &SurfaceConfiguration) -> Self {
        log::info!("Creating TextRenderer3D...");
        // Load available fonts - store as 'static references
        let mut available_fonts = HashMap::new();
        
        // Use static font data directly
        static HACK_REGULAR: &[u8] = include_bytes!("../../../assets/fonts/Hack-Regular.ttf");
        static HACK_BOLD: &[u8] = include_bytes!("../../../assets/fonts/Hack-Bold.ttf");
        static INTER: &[u8] = include_bytes!("../../../assets/fonts/Inter.ttf");
        static DROID_SANS: &[u8] = include_bytes!("../../../assets/fonts/DroidSans.ttf");
        static DROID_SANS_MONO: &[u8] = include_bytes!("../../../assets/fonts/DroidSansMono.ttf");
        
        available_fonts.insert("Hack-Regular".to_string(), HACK_REGULAR.to_vec());
        available_fonts.insert("Hack-Bold".to_string(), HACK_BOLD.to_vec());
        available_fonts.insert("Inter".to_string(), INTER.to_vec());
        available_fonts.insert("DroidSans".to_string(), DROID_SANS.to_vec());
        available_fonts.insert("DroidSansMono".to_string(), DROID_SANS_MONO.to_vec());
        
        // Load default font (Hack-Regular)
        let font_data = HACK_REGULAR.to_vec();
        let font = Font::try_from_bytes(&font_data)
            .expect("Failed to load default font");
        
        // Create font atlas with the real font - use larger size for better quality
        let font_size = 64.0;
        let (font_atlas, glyph_cache, atlas_size) = Self::create_font_atlas(device, queue, &font, font_size);
        
        log::info!("Font atlas created with {} glyphs", glyph_cache.len());
        
        let font_sampler = device.create_sampler(&SamplerDescriptor {
            label: Some("Font Sampler"),
            address_mode_u: AddressMode::ClampToEdge,
            address_mode_v: AddressMode::ClampToEdge,
            address_mode_w: AddressMode::ClampToEdge,
            mag_filter: FilterMode::Linear,
            min_filter: FilterMode::Linear,
            mipmap_filter: FilterMode::Linear,
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
        
        Self {
            pipeline,
            uniform_buffer,
            uniform_bind_group,
            font_atlas,
            font_sampler,
            font_data,
            glyph_cache,
            atlas_size,
            font_size,
            available_fonts,
        }
    }
    
    /// Create a font atlas texture with SDF for high-quality rendering
    fn create_font_atlas<'a>(
        device: &Device, 
        queue: &Queue, 
        font: &Font<'a>,
        font_size: f32
    ) -> (Texture, HashMap<char, GlyphInfo>, u32) {
        let scale = Scale::uniform(font_size);
        let mut glyph_cache = HashMap::new();
        
        // Calculate atlas size needed - make it larger for SDF
        let sdf_padding = 4; // Reduced padding for faster generation
        let padding = 2;
        let chars_per_row = 16;
        let cell_size = (font_size * 1.2) as u32 + (padding + sdf_padding) * 2;
        let atlas_size = cell_size * chars_per_row;
        
        log::info!("Creating font atlas: size={}, cell_size={}, font_size={}", 
                   atlas_size, cell_size, font_size);
        
        // Create texture data
        let mut atlas_data = vec![0u8; (atlas_size * atlas_size * 4) as usize];
        
        // Rasterize glyphs for printable ASCII characters using rusttype
        let mut rasterized_count = 0;
        log::info!("Starting glyph rasterization for {} characters", ('~' as u32 - ' ' as u32 + 1));
        for ch in ' '..='~' {
            let glyph = font.glyph(ch).scaled(scale);
            
            // Calculate position in atlas
            let index = (ch as u32) - 32;
            let row = index / chars_per_row;
            let col = index % chars_per_row;
            let x_offset = col * cell_size + padding;
            let y_offset = row * cell_size + padding;
            
            // Get glyph metrics and rasterize
            let glyph = glyph.positioned(point(0.0, 0.0));
            
            if let Some(bounding_box) = glyph.pixel_bounding_box() {
                // Calculate proper glyph dimensions
                let glyph_width = (bounding_box.max.x - bounding_box.min.x) as u32;
                let glyph_height = (bounding_box.max.y - bounding_box.min.y) as u32;
                
                // First rasterize to a temporary buffer
                let temp_width = (glyph_width + sdf_padding * 2) as usize;
                let temp_height = (glyph_height + sdf_padding * 2) as usize;
                let mut temp_buffer = vec![0u8; temp_width * temp_height * 4];
                
                let mut pixels_written = 0;
                glyph.draw(|gx, gy, v| {
                    let px = sdf_padding as usize + gx as usize;
                    let py = sdf_padding as usize + gy as usize;
                    
                    if px < temp_width && py < temp_height {
                        let idx = (py * temp_width + px) * 4;
                        let alpha = (v * 255.0) as u8;
                        if alpha > 0 {
                            temp_buffer[idx] = 255;
                            temp_buffer[idx + 1] = 255;
                            temp_buffer[idx + 2] = 255;
                            temp_buffer[idx + 3] = alpha;
                            pixels_written += 1;
                        }
                    }
                });
                
                // Generate SDF for this glyph
                if pixels_written > 0 {
                    if ch == 'A' || ch == ' ' || ch == '~' {
                        log::info!("Generating SDF for '{}' ({}x{} pixels)", ch, temp_width, temp_height);
                    }
                    let sdf_data = sdf_generator::generate_sdf_fast(
                        &temp_buffer,
                        temp_width,
                        temp_height,
                        sdf_padding as i32
                    );
                    
                    // Copy SDF data to atlas
                    for sy in 0..temp_height {
                        for sx in 0..temp_width {
                            let src_idx = sy * temp_width + sx;
                            let dst_x = x_offset as usize + sx;
                            let dst_y = y_offset as usize + sy;
                            
                            if dst_x < atlas_size as usize && dst_y < atlas_size as usize {
                                let dst_idx = (dst_y * atlas_size as usize + dst_x) * 4;
                                let sdf_value = sdf_data[src_idx];
                                atlas_data[dst_idx] = sdf_value;
                                atlas_data[dst_idx + 1] = sdf_value;
                                atlas_data[dst_idx + 2] = sdf_value;
                                atlas_data[dst_idx + 3] = sdf_value;
                            }
                        }
                    }
                }
                
                if pixels_written > 0 {
                    rasterized_count += 1;
                    if ch == 'A' || ch == 'H' || ch == 'T' {
                        log::debug!("Glyph '{}': {} pixels written at offset ({}, {})", 
                                  ch, pixels_written, x_offset, y_offset);
                    }
                }
                
                // Store glyph info with SDF-adjusted metrics
                let min_u = x_offset as f32 / atlas_size as f32;
                let min_v = y_offset as f32 / atlas_size as f32;
                let max_u = (x_offset + glyph_width + sdf_padding * 2) as f32 / atlas_size as f32;
                let max_v = (y_offset + glyph_height + sdf_padding * 2) as f32 / atlas_size as f32;
                
                glyph_cache.insert(ch, GlyphInfo {
                    tex_coords: (min_u, min_v, max_u, max_v),
                    size: (glyph_width as f32, glyph_height as f32),
                    offset: (bounding_box.min.x as f32, bounding_box.min.y as f32),
                });
            } else if ch == ' ' {
                // Handle space character specially
                let space_width = font_size * 0.3;
                glyph_cache.insert(ch, GlyphInfo {
                    tex_coords: (0.0, 0.0, 0.0, 0.0),
                    size: (space_width, font_size),
                    offset: (0.0, 0.0),
                });
            }
        }
        
        // Create texture
        let texture = device.create_texture(&TextureDescriptor {
            label: Some("Font Atlas"),
            size: Extent3d {
                width: atlas_size,
                height: atlas_size,
                depth_or_array_layers: 1,
            },
            mip_level_count: 1,
            sample_count: 1,
            dimension: TextureDimension::D2,
            format: TextureFormat::Rgba8UnormSrgb,
            usage: TextureUsages::TEXTURE_BINDING | TextureUsages::COPY_DST,
            view_formats: &[],
        });
        
        // Upload texture data
        queue.write_texture(
            ImageCopyTexture {
                texture: &texture,
                mip_level: 0,
                origin: Origin3d::ZERO,
                aspect: TextureAspect::All,
            },
            &atlas_data,
            ImageDataLayout {
                offset: 0,
                bytes_per_row: Some(atlas_size * 4),
                rows_per_image: Some(atlas_size),
            },
            Extent3d {
                width: atlas_size,
                height: atlas_size,
                depth_or_array_layers: 1,
            },
        );
        
        // Debug: Log rasterization results and save atlas
        log::info!("Generated SDF atlas with {} glyphs, atlas size: {}x{}", rasterized_count, atlas_size, atlas_size);
        
        // Skip saving debug atlas for performance during runtime
        // Uncomment to debug:
        // if let Err(e) = debug_atlas::save_atlas_as_ppm(&atlas_data, atlas_size, atlas_size, "font_atlas_sdf_debug.ppm") {
        //     log::warn!("Failed to save debug SDF atlas: {}", e);
        // }
        
        if let Some(info) = glyph_cache.get(&'A') {
            log::info!("Glyph 'A': size={:?}, tex_coords={:?}", info.size, info.tex_coords);
        }
        if let Some(info) = glyph_cache.get(&'H') {
            log::info!("Glyph 'H': size={:?}, tex_coords={:?}", info.size, info.tex_coords);
        }
        
        (texture, glyph_cache, atlas_size)
    }
    
    /// Switch to a different font by name
    pub fn switch_font(&mut self, font_name: &str, device: &Device, queue: &Queue) -> Result<(), String> {
        if let Some(new_font_data) = self.available_fonts.get(font_name) {
            self.font_data = new_font_data.clone();
            match Font::try_from_bytes(&self.font_data) {
                Some(font) => {
                    // Recreate font atlas with new font
                    let (font_atlas, glyph_cache, atlas_size) = 
                        Self::create_font_atlas(device, queue, &font, self.font_size);
                    self.font_atlas = font_atlas;
                    self.glyph_cache = glyph_cache;
                    self.atlas_size = atlas_size;
                    Ok(())
                }
                None => Err(format!("Failed to load font: {}", font_name))
            }
        } else {
            Err(format!("Font '{}' not found. Available fonts: {:?}", 
                font_name, self.available_fonts.keys().collect::<Vec<_>>()))
        }
    }
    
    /// Get list of available fonts
    pub fn get_available_fonts(&self) -> Vec<String> {
        self.available_fonts.keys().cloned().collect()
    }
    
    /// Generate mesh for rendering text string
    pub fn create_text_mesh(&self, text: &str, transform: &Transform, color: [f32; 4]) -> (Vec<TextVertex>, Vec<u16>) {
        let mut vertices = Vec::new();
        let mut indices = Vec::new();
        
        let scale = 0.01; // Scale factor for 3D world
        let line_height = self.font_size * scale * 1.2;
        
        let mut cursor_x = 0.0;
        let mut cursor_y = 0.0;
        
        for ch in text.chars() {
            if ch == '\n' {
                cursor_x = 0.0;
                cursor_y -= line_height;
                continue;
            }
            
            if let Some(glyph_info) = self.glyph_cache.get(&ch) {
                let base_idx = vertices.len() as u16;
                
                let width = glyph_info.size.0 * scale;
                let height = glyph_info.size.1 * scale;
                
                // Calculate vertex positions relative to transform
                let x = cursor_x + glyph_info.offset.0 * scale;
                let y = cursor_y + glyph_info.offset.1 * scale;
                
                let pos_tl = transform.position + Vec3::new(x, y + height, 0.0);
                let pos_tr = transform.position + Vec3::new(x + width, y + height, 0.0);
                let pos_bl = transform.position + Vec3::new(x, y, 0.0);
                let pos_br = transform.position + Vec3::new(x + width, y, 0.0);
                
                // Add vertices with texture coordinates
                vertices.extend_from_slice(&[
                    TextVertex {
                        position: pos_tl.into(),
                        uv: [glyph_info.tex_coords.0, glyph_info.tex_coords.1],
                        color,
                    },
                    TextVertex {
                        position: pos_tr.into(),
                        uv: [glyph_info.tex_coords.2, glyph_info.tex_coords.1],
                        color,
                    },
                    TextVertex {
                        position: pos_bl.into(),
                        uv: [glyph_info.tex_coords.0, glyph_info.tex_coords.3],
                        color,
                    },
                    TextVertex {
                        position: pos_br.into(),
                        uv: [glyph_info.tex_coords.2, glyph_info.tex_coords.3],
                        color,
                    },
                ]);
                
                // Add indices for two triangles
                indices.extend_from_slice(&[
                    base_idx, base_idx + 1, base_idx + 2,
                    base_idx + 1, base_idx + 3, base_idx + 2,
                ]);
                
                // Advance cursor
                cursor_x += width;
            } else if ch == ' ' {
                // Handle space character
                cursor_x += self.font_size * scale * 0.5;
            }
        }
        
        (vertices, indices)
    }
}
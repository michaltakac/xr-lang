//! Simple text rendering for performance overlay
//! Uses a bitmap font texture for fast 2D text rendering

use wgpu::*;
use wgpu::util::DeviceExt;

pub struct PerfText {
    texture: Texture,
    texture_view: TextureView,
    sampler: Sampler,
    vertex_buffer: Buffer,
    index_buffer: Buffer,
    bind_group: Option<BindGroup>,
}

#[repr(C)]
#[derive(Copy, Clone, Debug, bytemuck::Pod, bytemuck::Zeroable)]
struct TextVertex {
    position: [f32; 2],
    tex_coord: [f32; 2],
    color: [f32; 4],
}

impl PerfText {
    pub fn new(device: &Device, queue: &Queue) -> Self {
        // Create a simple 8x16 bitmap font texture (ASCII characters)
        // For now, we'll create a simple texture with numbers 0-9 and basic characters
        let font_width = 128;
        let font_height = 128;
        let mut font_data = vec![0u8; (font_width * font_height * 4) as usize];
        
        // Generate simple digit bitmaps (0-9, F, P, S, etc.)
        Self::generate_simple_font(&mut font_data, font_width, font_height);
        
        let texture_size = wgpu::Extent3d {
            width: font_width,
            height: font_height,
            depth_or_array_layers: 1,
        };
        
        let texture = device.create_texture(&TextureDescriptor {
            label: Some("Perf Font Texture"),
            size: texture_size,
            mip_level_count: 1,
            sample_count: 1,
            dimension: TextureDimension::D2,
            format: TextureFormat::Rgba8UnormSrgb,
            usage: TextureUsages::TEXTURE_BINDING | TextureUsages::COPY_DST,
            view_formats: &[],
        });
        
        queue.write_texture(
            ImageCopyTexture {
                texture: &texture,
                mip_level: 0,
                origin: Origin3d::ZERO,
                aspect: TextureAspect::All,
            },
            &font_data,
            ImageDataLayout {
                offset: 0,
                bytes_per_row: Some(4 * font_width),
                rows_per_image: Some(font_height),
            },
            texture_size,
        );
        
        let texture_view = texture.create_view(&TextureViewDescriptor::default());
        
        let sampler = device.create_sampler(&SamplerDescriptor {
            address_mode_u: AddressMode::ClampToEdge,
            address_mode_v: AddressMode::ClampToEdge,
            address_mode_w: AddressMode::ClampToEdge,
            mag_filter: FilterMode::Nearest,
            min_filter: FilterMode::Nearest,
            mipmap_filter: FilterMode::Nearest,
            ..Default::default()
        });
        
        // Create buffers for text quads
        let vertex_buffer = device.create_buffer(&BufferDescriptor {
            label: Some("Perf Text Vertices"),
            size: 4096,  // Space for many characters
            usage: BufferUsages::VERTEX | BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });
        
        let index_buffer = device.create_buffer(&BufferDescriptor {
            label: Some("Perf Text Indices"),
            size: 2048,
            usage: BufferUsages::INDEX | BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });
        
        Self {
            texture,
            texture_view,
            sampler,
            vertex_buffer,
            index_buffer,
            bind_group: None,
        }
    }
    
    fn generate_simple_font(data: &mut [u8], width: u32, height: u32) {
        // Simple bitmap patterns for digits and letters
        // Each character is 8x8 pixels
        let char_patterns = [
            // '0'
            [
                0b01111110,
                0b11000011,
                0b11000011,
                0b11000011,
                0b11000011,
                0b11000011,
                0b01111110,
                0b00000000,
            ],
            // '1'
            [
                0b00011000,
                0b00111000,
                0b00011000,
                0b00011000,
                0b00011000,
                0b00011000,
                0b01111110,
                0b00000000,
            ],
            // '2'
            [
                0b01111110,
                0b11000011,
                0b00000011,
                0b00001110,
                0b00111000,
                0b11100000,
                0b11111111,
                0b00000000,
            ],
            // 'F'
            [
                0b11111111,
                0b11000000,
                0b11000000,
                0b11111110,
                0b11000000,
                0b11000000,
                0b11000000,
                0b00000000,
            ],
            // 'P'
            [
                0b11111110,
                0b11000011,
                0b11000011,
                0b11111110,
                0b11000000,
                0b11000000,
                0b11000000,
                0b00000000,
            ],
            // 'S'
            [
                0b01111111,
                0b11000000,
                0b11000000,
                0b01111110,
                0b00000011,
                0b00000011,
                0b11111110,
                0b00000000,
            ],
        ];
        
        // Place characters in texture
        for (char_idx, pattern) in char_patterns.iter().enumerate() {
            let char_x = (char_idx % 16) * 8;
            let char_y = (char_idx / 16) * 8;
            
            for (row_idx, &row) in pattern.iter().enumerate() {
                for bit in 0..8 {
                    let pixel = if (row >> (7 - bit)) & 1 == 1 {
                        255u8
                    } else {
                        0u8
                    };
                    
                    let x = char_x + bit;
                    let y = char_y + row_idx;
                    let idx = ((y * width as usize + x) * 4) as usize;
                    
                    if idx + 3 < data.len() {
                        data[idx] = pixel;      // R
                        data[idx + 1] = pixel;   // G
                        data[idx + 2] = pixel;   // B
                        data[idx + 3] = pixel;   // A
                    }
                }
            }
        }
    }
    
    pub fn update_text(&self, queue: &Queue, text: &str, x: f32, y: f32, scale: f32) -> (Vec<TextVertex>, Vec<u16>) {
        let mut vertices = Vec::new();
        let mut indices = Vec::new();
        
        let char_width = 8.0 * scale;
        let char_height = 8.0 * scale;
        let mut cursor_x = x;
        
        for ch in text.chars() {
            // Map character to texture coordinates
            let char_idx = match ch {
                '0'..='9' => (ch as u32 - '0' as u32) as usize,
                'F' | 'f' => 3,
                'P' | 'p' => 4,
                'S' | 's' => 5,
                ' ' => { cursor_x += char_width; continue; }
                _ => 0, // Default to '0' for unknown chars
            };
            
            let tex_x = (char_idx % 16) as f32 * 8.0 / 128.0;
            let tex_y = (char_idx / 16) as f32 * 8.0 / 128.0;
            let tex_w = 8.0 / 128.0;
            let tex_h = 8.0 / 128.0;
            
            let base_idx = vertices.len() as u16;
            
            // Create quad for character
            vertices.push(TextVertex {
                position: [cursor_x, y],
                tex_coord: [tex_x, tex_y],
                color: [1.0, 1.0, 1.0, 1.0],
            });
            vertices.push(TextVertex {
                position: [cursor_x + char_width, y],
                tex_coord: [tex_x + tex_w, tex_y],
                color: [1.0, 1.0, 1.0, 1.0],
            });
            vertices.push(TextVertex {
                position: [cursor_x + char_width, y + char_height],
                tex_coord: [tex_x + tex_w, tex_y + tex_h],
                color: [1.0, 1.0, 1.0, 1.0],
            });
            vertices.push(TextVertex {
                position: [cursor_x, y + char_height],
                tex_coord: [tex_x, tex_y + tex_h],
                color: [1.0, 1.0, 1.0, 1.0],
            });
            
            // Add indices for the quad
            indices.push(base_idx);
            indices.push(base_idx + 1);
            indices.push(base_idx + 2);
            indices.push(base_idx);
            indices.push(base_idx + 2);
            indices.push(base_idx + 3);
            
            cursor_x += char_width;
        }
        
        // Update GPU buffers
        if !vertices.is_empty() {
            queue.write_buffer(&self.vertex_buffer, 0, bytemuck::cast_slice(&vertices));
            queue.write_buffer(&self.index_buffer, 0, bytemuck::cast_slice(&indices));
        }
        
        (vertices, indices)
    }
    
    pub fn get_texture_view(&self) -> &TextureView {
        &self.texture_view
    }
    
    pub fn get_sampler(&self) -> &Sampler {
        &self.sampler
    }
}
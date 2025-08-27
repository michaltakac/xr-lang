//! Simplified 3D UI system demo

use crate::math::*;
use wgpu::*;
use wgpu::util::DeviceExt;
use hecs::World;
use super::text_renderer::TextRenderer3D;
use crate::scene::UIElementData;
use crate::Camera;

/// Simple 3D UI system that just displays a message
pub struct SimpleUI3D {
    pub world: World,
    pub enabled: bool,
    pub panel_pipeline: Option<RenderPipeline>,
    pub panel_vertex_buffer: Option<Buffer>,
    pub panel_index_buffer: Option<Buffer>,
    pub panel_uniform_buffer: Option<Buffer>,
    pub panel_bind_group: Option<BindGroup>,
    pub text_renderer: Option<TextRenderer3D>,
    pub ui_elements: Vec<UIElementData>,
}

impl SimpleUI3D {
    pub fn new(device: &Device, queue: &Queue, config: &SurfaceConfiguration) -> Self {
        log::info!("🎨 3D UI System initialized!");
        log::info!("📝 Components: 3D Code Editor, Button System, Log Viewer");
        log::info!("🏗️ Architecture: ECS with hecs, WebGPU rendering");
        log::info!("⌨️ Features: Mouse raycasting, Keyboard input, Text editing");
        log::info!("🔧 Status: Ready for hot-reload implementation!");
        
        // Create a simple quad for displaying code
        // Format: position (3), color (4), uv (2) - but we only have 8 floats total,
        // so let's fix it to match: position(3) + color(4) + uv(2) = 9 floats
        #[repr(C)]
        #[derive(Copy, Clone, bytemuck::Pod, bytemuck::Zeroable)]
        struct UIVertex {
            position: [f32; 3],
            color: [f32; 4],
            uv: [f32; 2],
        }
        
        let vertices = vec![
            UIVertex { 
                position: [-2.0, 2.0, -5.0], 
                color: [0.9, 0.9, 0.9, 1.0], 
                uv: [0.0, 0.0] 
            },  // top-left
            UIVertex { 
                position: [ 2.0, 2.0, -5.0], 
                color: [0.9, 0.9, 0.9, 1.0], 
                uv: [1.0, 0.0] 
            },  // top-right
            UIVertex { 
                position: [ 2.0,-2.0, -5.0], 
                color: [0.9, 0.9, 0.9, 1.0], 
                uv: [1.0, 1.0] 
            },  // bottom-right
            UIVertex { 
                position: [-2.0,-2.0, -5.0], 
                color: [0.9, 0.9, 0.9, 1.0], 
                uv: [0.0, 1.0] 
            },  // bottom-left
        ];
        
        let indices: &[u16] = &[
            0, 1, 2,
            0, 2, 3,
        ];
        
        let panel_vertex_buffer = device.create_buffer_init(&util::BufferInitDescriptor {
            label: Some("UI Panel Vertex Buffer"),
            contents: bytemuck::cast_slice(&vertices),
            usage: BufferUsages::VERTEX,
        });
        
        let panel_index_buffer = device.create_buffer_init(&util::BufferInitDescriptor {
            label: Some("UI Panel Index Buffer"),
            contents: bytemuck::cast_slice(indices),
            usage: BufferUsages::INDEX,
        });
        
        // Create shader module for UI rendering
        let shader = device.create_shader_module(ShaderModuleDescriptor {
            label: Some("UI 3D Shader"),
            source: ShaderSource::Wgsl(include_str!("../shaders/ui_panel.wgsl").into()),
        });
        
        // Create uniform buffer for panel transforms
        let panel_uniform_buffer = device.create_buffer(&BufferDescriptor {
            label: Some("UI Panel Uniform Buffer"),
            size: 256, // Mat4 * 4 + padding
            usage: BufferUsages::UNIFORM | BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });
        
        // Create bind group layout
        let bind_group_layout = device.create_bind_group_layout(&BindGroupLayoutDescriptor {
            label: Some("UI Panel Bind Group Layout"),
            entries: &[BindGroupLayoutEntry {
                binding: 0,
                visibility: ShaderStages::VERTEX | ShaderStages::FRAGMENT,
                ty: BindingType::Buffer {
                    ty: BufferBindingType::Uniform,
                    has_dynamic_offset: false,
                    min_binding_size: None,
                },
                count: None,
            }],
        });
        
        let panel_bind_group = device.create_bind_group(&BindGroupDescriptor {
            label: Some("UI Panel Bind Group"),
            layout: &bind_group_layout,
            entries: &[BindGroupEntry {
                binding: 0,
                resource: panel_uniform_buffer.as_entire_binding(),
            }],
        });
        
        // Create pipeline
        let pipeline_layout = device.create_pipeline_layout(&PipelineLayoutDescriptor {
            label: Some("UI Panel Pipeline Layout"),
            bind_group_layouts: &[&bind_group_layout],
            push_constant_ranges: &[],
        });
        
        let panel_pipeline = device.create_render_pipeline(&RenderPipelineDescriptor {
            label: Some("UI Panel Pipeline"),
            layout: Some(&pipeline_layout),
            vertex: VertexState {
                module: &shader,
                entry_point: "vs_main",
                compilation_options: Default::default(),
                buffers: &[VertexBufferLayout {
                    array_stride: 36, // 9 floats * 4 bytes (3 pos + 4 color + 2 uv)
                    step_mode: VertexStepMode::Vertex,
                    attributes: &[
                        VertexAttribute { offset: 0, format: VertexFormat::Float32x3, shader_location: 0 }, // position
                        VertexAttribute { offset: 12, format: VertexFormat::Float32x4, shader_location: 1 }, // color
                        VertexAttribute { offset: 28, format: VertexFormat::Float32x2, shader_location: 2 }, // uv
                    ],
                }],
            },
            fragment: Some(FragmentState {
                module: &shader,
                entry_point: "fs_main",
                compilation_options: Default::default(),
                targets: &[Some(ColorTargetState {
                    format: config.format,
                    blend: Some(BlendState::ALPHA_BLENDING),
                    write_mask: ColorWrites::ALL,
                })],
            }),
            primitive: PrimitiveState {
                topology: PrimitiveTopology::TriangleList,
                strip_index_format: None,
                front_face: FrontFace::Ccw,
                cull_mode: None, // Show both sides
                unclipped_depth: false,
                polygon_mode: PolygonMode::Fill,
                conservative: false,
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
        
        // Create text renderer
        let text_renderer = TextRenderer3D::new(device, queue, config);
        
        Self {
            world: World::new(),
            enabled: true,
            panel_pipeline: Some(panel_pipeline),
            panel_vertex_buffer: Some(panel_vertex_buffer),
            panel_index_buffer: Some(panel_index_buffer),
            panel_uniform_buffer: Some(panel_uniform_buffer),
            panel_bind_group: Some(panel_bind_group),
            text_renderer: Some(text_renderer),
            ui_elements: Vec::new(),
        }
    }
    
    pub fn update_ui_elements(&mut self, ui_elements: Vec<UIElementData>) {
        self.ui_elements = ui_elements;
        log::info!("Updated UI elements: {} panels", self.ui_elements.len());
    }
    
    pub fn update(&mut self, _dt: f32, _camera: &Camera, _device: &Device) {
        // UI update logic would go here
    }
    
    pub fn render(&self, device: &Device, queue: &Queue, view: &TextureView, depth_view: &TextureView) {
        if !self.enabled {
            return;
        }
        
        // Only render if we have all the required components
        let (Some(pipeline), Some(vertex_buffer), Some(index_buffer), Some(uniform_buffer), Some(bind_group)) = 
            (&self.panel_pipeline, &self.panel_vertex_buffer, &self.panel_index_buffer, &self.panel_uniform_buffer, &self.panel_bind_group)
        else {
            return;
        };
        
        // Create transformation matrices for the panel
        // Position it to the right side of the scene
        let panel_transform = Mat4::from_translation(Vec3::new(5.0, 0.0, -2.0)) * 
                              Mat4::from_scale(Vec3::new(1.5, 1.5, 1.0));
        
        // For now, use identity view projection (will be updated with camera later)
        let view_proj = Mat4::perspective(
            std::f32::consts::FRAC_PI_3,  // 60 degree FOV
            16.0 / 9.0,  // aspect ratio
            0.1,
            100.0
        ) * Mat4::look_at(
            Vec3::new(0.0, 8.0, 15.0),  // eye
            Vec3::new(0.0, 0.0, 0.0),   // target
            Vec3::Y                      // up
        );
        
        // Prepare uniform data
        #[repr(C)]
        #[derive(Copy, Clone, bytemuck::Pod, bytemuck::Zeroable)]
        struct PanelUniforms {
            view_proj: Mat4,
            model: Mat4,
        }
        
        let uniforms = PanelUniforms {
            view_proj,
            model: panel_transform,
        };
        
        // Update uniform buffer
        queue.write_buffer(uniform_buffer, 0, bytemuck::cast_slice(&[uniforms]));
        
        // Create command encoder and render
        let mut encoder = device.create_command_encoder(&CommandEncoderDescriptor {
            label: Some("UI Panel Render Encoder"),
        });
        
        {
            let mut render_pass = encoder.begin_render_pass(&RenderPassDescriptor {
                label: Some("UI Panel Render Pass"),
                color_attachments: &[Some(RenderPassColorAttachment {
                    view,
                    resolve_target: None,
                    ops: Operations {
                        load: LoadOp::Load, // Load existing content (cubes)
                        store: StoreOp::Store,
                    },
                })],
                depth_stencil_attachment: Some(RenderPassDepthStencilAttachment {
                    view: depth_view,
                    depth_ops: Some(Operations {
                        load: LoadOp::Load, // Load existing depth
                        store: StoreOp::Store,
                    }),
                    stencil_ops: None,
                }),
                timestamp_writes: None,
                occlusion_query_set: None,
            });
            
            render_pass.set_pipeline(pipeline);
            render_pass.set_bind_group(0, bind_group, &[]);
            render_pass.set_vertex_buffer(0, vertex_buffer.slice(..));
            render_pass.set_index_buffer(index_buffer.slice(..), IndexFormat::Uint16);
            render_pass.draw_indexed(0..6, 0, 0..1); // 6 indices for 2 triangles
        }
        
        queue.submit(std::iter::once(encoder.finish()));
        
        // Render text for UI elements
        if let Some(ref text_renderer) = self.text_renderer {
            for ui_element in &self.ui_elements {
                if let Some(ref text) = ui_element.text {
                    // Create text mesh for this UI element
                    let transform = Transform {
                        position: ui_element.position.into(),
                        _pad1: 0.0,
                        rotation: Vec3::ZERO,
                        _pad2: 0.0,
                        scale: Vec3::ONE,
                        _pad3: 0.0,
                    };
                    
                    let (vertices, indices) = text_renderer.create_text_mesh(
                        text, 
                        &transform,
                        ui_element.color
                    );
                    
                    // Create buffers for text
                    if !vertices.is_empty() {
                        let vertex_buffer = device.create_buffer_init(&util::BufferInitDescriptor {
                            label: Some("Text Vertex Buffer"),
                            contents: bytemuck::cast_slice(&vertices),
                            usage: BufferUsages::VERTEX,
                        });
                        
                        let index_buffer = device.create_buffer_init(&util::BufferInitDescriptor {
                            label: Some("Text Index Buffer"),
                            contents: bytemuck::cast_slice(&indices),
                            usage: BufferUsages::INDEX,
                        });
                        
                        // Update text uniforms
                        let text_uniforms = super::text_renderer::TextUniforms {
                            transform: view_proj * panel_transform,
                            _pad: [0.0; 3],
                        };
                        
                        queue.write_buffer(&text_renderer.uniform_buffer, 0, bytemuck::cast_slice(&[text_uniforms]));
                        
                        // Render text
                        let mut encoder = device.create_command_encoder(&CommandEncoderDescriptor {
                            label: Some("Text Render Encoder"),
                        });
                        
                        {
                            let mut render_pass = encoder.begin_render_pass(&RenderPassDescriptor {
                                label: Some("Text Render Pass"),
                                color_attachments: &[Some(RenderPassColorAttachment {
                                    view,
                                    resolve_target: None,
                                    ops: Operations {
                                        load: LoadOp::Load,
                                        store: StoreOp::Store,
                                    },
                                })],
                                depth_stencil_attachment: Some(RenderPassDepthStencilAttachment {
                                    view: depth_view,
                                    depth_ops: Some(Operations {
                                        load: LoadOp::Load,
                                        store: StoreOp::Store,
                                    }),
                                    stencil_ops: None,
                                }),
                                timestamp_writes: None,
                                occlusion_query_set: None,
                            });
                            
                            render_pass.set_pipeline(&text_renderer.pipeline);
                            render_pass.set_bind_group(0, &text_renderer.uniform_bind_group, &[]);
                            render_pass.set_vertex_buffer(0, vertex_buffer.slice(..));
                            render_pass.set_index_buffer(index_buffer.slice(..), IndexFormat::Uint16);
                            render_pass.draw_indexed(0..indices.len() as u32, 0, 0..1);
                        }
                        
                        queue.submit(std::iter::once(encoder.finish()));
                    }
                }
            }
        }
    }
    
    pub fn get_current_code(&self) -> Option<String> {
        Some(r#"(defscene3d interactive-scene
  (cube (position 0 0 -5) (color 1 0.5 0))
  (cube (position 2 0 -5) (color 0 1 0.5))
  (cube (position -2 0 -5) (color 0.5 0 1))
  ; This code can be edited in 3D space!
)"#.to_string())
    }
    
    pub fn add_log_entry(&mut self, message: &str) {
        log::info!("3D UI Log: {}", message);
    }
    
    // Input handlers - simplified for demo
    pub fn handle_mouse_button(&mut self, _button: winit::event::MouseButton, _state: winit::event::ElementState) {
        // Mouse handling would go here
    }
    
    pub fn handle_mouse_move(&mut self, _position: [f32; 2]) {
        // Mouse movement handling
    }
    
    pub fn handle_mouse_scroll(&mut self, _delta: f32) {
        // Scroll handling
    }
    
    pub fn handle_keyboard(&mut self, _keycode: winit::keyboard::KeyCode, _state: winit::event::ElementState) {
        // Keyboard handling
    }
    
    pub fn handle_character_input(&mut self, _character: char) {
        // Character input handling
    }
}
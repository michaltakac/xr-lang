//! 2D Performance Overlay
//! Renders performance stats as a 2D overlay on top of the 3D scene

use wgpu::*;
use wgpu::util::DeviceExt;
use std::collections::VecDeque;
use std::time::{Duration, Instant};

const SAMPLE_COUNT: usize = 60;
const UPDATE_INTERVAL_MS: u64 = 100;

pub struct PerfOverlay {
    // Timing
    frame_times: VecDeque<Duration>,
    last_frame_time: Instant,
    last_update_time: Instant,
    
    // Stats
    current_fps: f32,
    avg_frame_time_ms: f32,
    min_frame_time_ms: f32,
    max_frame_time_ms: f32,
    draw_calls: u32,
    triangles_rendered: u32,
    
    // Rendering
    enabled: bool,
    pipeline: Option<RenderPipeline>,
    vertex_buffer: Buffer,
    uniform_buffer: Buffer,
    bind_group: Option<BindGroup>,
    screen_size: [f32; 2],
}

#[repr(C)]
#[derive(Copy, Clone, Debug, bytemuck::Pod, bytemuck::Zeroable)]
struct OverlayVertex {
    position: [f32; 2],
    tex_coords: [f32; 2],
}

#[repr(C)]
#[derive(Copy, Clone, Debug, bytemuck::Pod, bytemuck::Zeroable)]
struct OverlayUniforms {
    screen_size: [f32; 2],
    panel_pos: [f32; 2],
    panel_size: [f32; 2],
    _padding1: [f32; 2],  // Align to 16 bytes
    bg_color: [f32; 4],
    text_color: [f32; 4],
}

impl PerfOverlay {
    pub fn new(device: &Device, format: TextureFormat) -> Self {
        // Create vertices for a quad (two triangles)
        let vertices = [
            OverlayVertex { position: [0.0, 0.0], tex_coords: [0.0, 0.0] },
            OverlayVertex { position: [1.0, 0.0], tex_coords: [1.0, 0.0] },
            OverlayVertex { position: [1.0, 1.0], tex_coords: [1.0, 1.0] },
            OverlayVertex { position: [0.0, 0.0], tex_coords: [0.0, 0.0] },
            OverlayVertex { position: [1.0, 1.0], tex_coords: [1.0, 1.0] },
            OverlayVertex { position: [0.0, 1.0], tex_coords: [0.0, 1.0] },
        ];
        
        let vertex_buffer = device.create_buffer_init(&util::BufferInitDescriptor {
            label: Some("Perf Overlay Vertices"),
            contents: bytemuck::cast_slice(&vertices),
            usage: BufferUsages::VERTEX,
        });
        
        // Initial uniforms
        let uniforms = OverlayUniforms {
            screen_size: [800.0, 600.0],
            panel_pos: [10.0, 10.0],
            panel_size: [200.0, 100.0],
            _padding1: [0.0, 0.0],
            bg_color: [0.0, 0.0, 0.0, 0.85],
            text_color: [0.0, 1.0, 0.0, 1.0],
        };
        
        let uniform_buffer = device.create_buffer_init(&util::BufferInitDescriptor {
            label: Some("Perf Overlay Uniforms"),
            contents: bytemuck::cast_slice(&[uniforms]),
            usage: BufferUsages::UNIFORM | BufferUsages::COPY_DST,
        });
        
        // Create shader
        let shader = device.create_shader_module(ShaderModuleDescriptor {
            label: Some("Perf Overlay Shader"),
            source: ShaderSource::Wgsl(include_str!("shaders/perf_overlay.wgsl").into()),
        });
        
        // Create bind group layout
        let bind_group_layout = device.create_bind_group_layout(&BindGroupLayoutDescriptor {
            label: Some("Perf Overlay Bind Group Layout"),
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
        
        let bind_group = device.create_bind_group(&BindGroupDescriptor {
            label: Some("Perf Overlay Bind Group"),
            layout: &bind_group_layout,
            entries: &[
                BindGroupEntry {
                    binding: 0,
                    resource: uniform_buffer.as_entire_binding(),
                },
            ],
        });
        
        // Create pipeline
        let pipeline_layout = device.create_pipeline_layout(&PipelineLayoutDescriptor {
            label: Some("Perf Overlay Pipeline Layout"),
            bind_group_layouts: &[&bind_group_layout],
            push_constant_ranges: &[],
        });
        
        let pipeline = device.create_render_pipeline(&RenderPipelineDescriptor {
            label: Some("Perf Overlay Pipeline"),
            layout: Some(&pipeline_layout),
            vertex: VertexState {
                module: &shader,
                entry_point: "vs_main",
                compilation_options: Default::default(),
                buffers: &[VertexBufferLayout {
                    array_stride: std::mem::size_of::<OverlayVertex>() as BufferAddress,
                    step_mode: VertexStepMode::Vertex,
                    attributes: &[
                        VertexAttribute {
                            offset: 0,
                            shader_location: 0,
                            format: VertexFormat::Float32x2,
                        },
                        VertexAttribute {
                            offset: std::mem::size_of::<[f32; 2]>() as BufferAddress,
                            shader_location: 1,
                            format: VertexFormat::Float32x2,
                        },
                    ],
                }],
            },
            fragment: Some(FragmentState {
                module: &shader,
                entry_point: "fs_main",
                compilation_options: Default::default(),
                targets: &[Some(ColorTargetState {
                    format,
                    blend: Some(BlendState::ALPHA_BLENDING),
                    write_mask: ColorWrites::ALL,
                })],
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
            depth_stencil: None,
            multisample: MultisampleState::default(),
            multiview: None,
        });
        
        Self {
            frame_times: VecDeque::with_capacity(SAMPLE_COUNT),
            last_frame_time: Instant::now(),
            last_update_time: Instant::now(),
            
            current_fps: 0.0,
            avg_frame_time_ms: 0.0,
            min_frame_time_ms: f32::MAX,
            max_frame_time_ms: 0.0,
            draw_calls: 0,
            triangles_rendered: 0,
            
            enabled: true,
            pipeline: Some(pipeline),
            vertex_buffer,
            uniform_buffer,
            bind_group: Some(bind_group),
            screen_size: [800.0, 600.0],
        }
    }
    
    pub fn frame_start(&mut self) {
        let now = Instant::now();
        let frame_time = now.duration_since(self.last_frame_time);
        self.last_frame_time = now;
        
        // Add to history
        self.frame_times.push_back(frame_time);
        if self.frame_times.len() > SAMPLE_COUNT {
            self.frame_times.pop_front();
        }
        
        // Update stats if enough time has passed
        if now.duration_since(self.last_update_time).as_millis() >= UPDATE_INTERVAL_MS as u128 {
            self.update_stats();
            self.last_update_time = now;
        }
    }
    
    fn update_stats(&mut self) {
        if self.frame_times.is_empty() {
            return;
        }
        
        let mut total_time = Duration::ZERO;
        let mut min_time = Duration::from_secs(1);
        let mut max_time = Duration::ZERO;
        
        for &frame_time in &self.frame_times {
            total_time += frame_time;
            min_time = min_time.min(frame_time);
            max_time = max_time.max(frame_time);
        }
        
        let avg_time = total_time / self.frame_times.len() as u32;
        
        self.avg_frame_time_ms = avg_time.as_secs_f32() * 1000.0;
        self.min_frame_time_ms = min_time.as_secs_f32() * 1000.0;
        self.max_frame_time_ms = max_time.as_secs_f32() * 1000.0;
        self.current_fps = if avg_time.as_secs_f32() > 0.0 {
            1.0 / avg_time.as_secs_f32()
        } else {
            0.0
        };
    }
    
    pub fn record_draw_call(&mut self) {
        self.draw_calls += 1;
    }
    
    pub fn record_triangles(&mut self, count: u32) {
        self.triangles_rendered += count;
    }
    
    pub fn reset_frame_stats(&mut self) {
        self.draw_calls = 0;
        self.triangles_rendered = 0;
    }
    
    pub fn render(&self, device: &Device, queue: &Queue, view: &TextureView) {
        if !self.enabled {
            return;
        }
        
        let Some(pipeline) = &self.pipeline else { return };
        let Some(bind_group) = &self.bind_group else { return };
        
        // Print stats to console for now (text rendering coming next)
        if self.current_fps > 0.0 {
            println!("\r📊 FPS: {:.1} | Frame: {:.2}ms | Draws: {} | Tris: {:.1}k     ", 
                     self.current_fps, 
                     self.avg_frame_time_ms,
                     self.draw_calls,
                     self.triangles_rendered as f32 / 1000.0);
        }
        
        // Update uniforms with current screen size
        let uniforms = OverlayUniforms {
            screen_size: self.screen_size,
            panel_pos: [10.0, 10.0],
            panel_size: [200.0, 100.0],
            _padding1: [0.0, 0.0],
            bg_color: [0.1, 0.1, 0.1, 0.9],
            text_color: if self.current_fps >= 60.0 {
                [0.0, 1.0, 0.0, 1.0]  // Green
            } else if self.current_fps >= 30.0 {
                [1.0, 1.0, 0.0, 1.0]  // Yellow
            } else {
                [1.0, 0.0, 0.0, 1.0]  // Red
            },
        };
        
        queue.write_buffer(&self.uniform_buffer, 0, bytemuck::cast_slice(&[uniforms]));
        
        // Create render pass
        let mut encoder = device.create_command_encoder(&CommandEncoderDescriptor {
            label: Some("Perf Overlay Encoder"),
        });
        
        {
            let mut render_pass = encoder.begin_render_pass(&RenderPassDescriptor {
                label: Some("Perf Overlay Pass"),
                color_attachments: &[Some(RenderPassColorAttachment {
                    view,
                    resolve_target: None,
                    ops: Operations {
                        load: LoadOp::Load,  // Don't clear - we're overlaying
                        store: StoreOp::Store,
                    },
                })],
                depth_stencil_attachment: None,
                timestamp_writes: None,
                occlusion_query_set: None,
            });
            
            render_pass.set_pipeline(pipeline);
            render_pass.set_bind_group(0, bind_group, &[]);
            render_pass.set_vertex_buffer(0, self.vertex_buffer.slice(..));
            
            // Draw background panel
            render_pass.draw(0..6, 0..1);
            
            // Draw FPS graph bars (multiple instances)
            let bar_count = 20;  // Number of bars in the graph
            render_pass.draw(0..6, 1..(bar_count + 1));
        }
        
        queue.submit(std::iter::once(encoder.finish()));
    }
    
    pub fn resize(&mut self, width: u32, height: u32) {
        self.screen_size = [width as f32, height as f32];
    }
    
    pub fn toggle(&mut self) {
        self.enabled = !self.enabled;
        println!("📊 Performance overlay: {}", if self.enabled { "ON" } else { "OFF" });
    }
    
    pub fn is_enabled(&self) -> bool {
        self.enabled
    }
}
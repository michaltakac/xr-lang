//! Live Performance Overlay with real-time updates
//! Shows FPS graph, frame time chart, and GPU metrics

use wgpu::*;
use wgpu::util::DeviceExt;
use std::collections::VecDeque;
use std::time::Instant;

const SAMPLE_COUNT: usize = 60;  // 60 frames of history for graph
const UPDATE_INTERVAL_MS: u64 = 16;  // Update every frame (~60fps)

pub struct LivePerfOverlay {
    // Timing
    frame_times: VecDeque<f32>,  // Store as milliseconds for graph
    last_frame_time: Instant,
    frame_count: u32,
    
    // Current stats
    current_fps: f32,
    avg_frame_time_ms: f32,
    draw_calls: u32,
    triangles_rendered: u32,
    
    // Rendering
    enabled: bool,
    pipeline: Option<RenderPipeline>,
    vertex_buffer: Buffer,
    uniform_buffer: Buffer,
    bind_group: Option<BindGroup>,
    screen_size: [f32; 2],
    
    // Graph data buffer
    graph_buffer: Buffer,
}

#[repr(C)]
#[derive(Copy, Clone, Debug, bytemuck::Pod, bytemuck::Zeroable)]
struct OverlayVertex {
    position: [f32; 2],
    color: [f32; 4],
}

#[repr(C)]
#[derive(Copy, Clone, Debug, bytemuck::Pod, bytemuck::Zeroable)]
struct OverlayUniforms {
    screen_size: [f32; 2],
    panel_pos: [f32; 2],
    panel_size: [f32; 2],
    _padding1: [f32; 2],
    bg_color: [f32; 4],
    text_color: [f32; 4],
    current_fps: f32,
    avg_frame_ms: f32,
    draw_calls: f32,
    triangle_count: f32,
}

impl LivePerfOverlay {
    pub fn new(device: &Device, format: TextureFormat) -> Self {
        // Create vertex buffer for panel and graph
        let mut vertices = Vec::new();
        
        // Background panel (6 vertices for quad)
        vertices.extend_from_slice(&[
            OverlayVertex { position: [0.0, 0.0], color: [0.0, 0.0, 0.0, 0.9] },
            OverlayVertex { position: [1.0, 0.0], color: [0.0, 0.0, 0.0, 0.9] },
            OverlayVertex { position: [1.0, 1.0], color: [0.0, 0.0, 0.0, 0.9] },
            OverlayVertex { position: [0.0, 0.0], color: [0.0, 0.0, 0.0, 0.9] },
            OverlayVertex { position: [1.0, 1.0], color: [0.0, 0.0, 0.0, 0.9] },
            OverlayVertex { position: [0.0, 1.0], color: [0.0, 0.0, 0.0, 0.9] },
        ]);
        
        // Reserve space for graph bars (60 samples * 6 vertices each)
        for _ in 0..SAMPLE_COUNT {
            for _ in 0..6 {
                vertices.push(OverlayVertex { 
                    position: [0.0, 0.0], 
                    color: [0.0, 1.0, 0.0, 0.8] 
                });
            }
        }
        
        let vertex_buffer = device.create_buffer_init(&util::BufferInitDescriptor {
            label: Some("Live Perf Overlay Vertices"),
            contents: bytemuck::cast_slice(&vertices),
            usage: BufferUsages::VERTEX | BufferUsages::COPY_DST,
        });
        
        // Graph data buffer
        let graph_buffer = device.create_buffer(&BufferDescriptor {
            label: Some("Graph Data"),
            size: (SAMPLE_COUNT * 4) as u64,  // f32 per sample
            usage: BufferUsages::STORAGE | BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });
        
        // Initial uniforms
        let uniforms = OverlayUniforms {
            screen_size: [800.0, 600.0],
            panel_pos: [10.0, 10.0],
            panel_size: [250.0, 120.0],
            _padding1: [0.0, 0.0],
            bg_color: [0.05, 0.05, 0.05, 0.95],
            text_color: [0.0, 1.0, 0.0, 1.0],
            current_fps: 0.0,
            avg_frame_ms: 0.0,
            draw_calls: 0.0,
            triangle_count: 0.0,
        };
        
        let uniform_buffer = device.create_buffer_init(&util::BufferInitDescriptor {
            label: Some("Live Perf Overlay Uniforms"),
            contents: bytemuck::cast_slice(&[uniforms]),
            usage: BufferUsages::UNIFORM | BufferUsages::COPY_DST,
        });
        
        // Create shader
        let shader = device.create_shader_module(ShaderModuleDescriptor {
            label: Some("Live Perf Overlay Shader"),
            source: ShaderSource::Wgsl(include_str!("shaders/perf_overlay_live.wgsl").into()),
        });
        
        // Create bind group layout
        let bind_group_layout = device.create_bind_group_layout(&BindGroupLayoutDescriptor {
            label: Some("Live Perf Overlay Bind Group Layout"),
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
            label: Some("Live Perf Overlay Bind Group"),
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
            label: Some("Live Perf Overlay Pipeline Layout"),
            bind_group_layouts: &[&bind_group_layout],
            push_constant_ranges: &[],
        });
        
        let pipeline = device.create_render_pipeline(&RenderPipelineDescriptor {
            label: Some("Live Perf Overlay Pipeline"),
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
                            offset: 8,
                            shader_location: 1,
                            format: VertexFormat::Float32x4,
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
            frame_count: 0,
            
            current_fps: 0.0,
            avg_frame_time_ms: 0.0,
            draw_calls: 0,
            triangles_rendered: 0,
            
            enabled: true,
            pipeline: Some(pipeline),
            vertex_buffer,
            uniform_buffer,
            bind_group: Some(bind_group),
            screen_size: [800.0, 600.0],
            graph_buffer,
        }
    }
    
    pub fn frame_start(&mut self) {
        let now = Instant::now();
        let frame_time = now.duration_since(self.last_frame_time);
        self.last_frame_time = now;
        
        // Convert to milliseconds and store
        let frame_ms = frame_time.as_secs_f32() * 1000.0;
        self.frame_times.push_back(frame_ms);
        if self.frame_times.len() > SAMPLE_COUNT {
            self.frame_times.pop_front();
        }
        
        // Calculate current stats
        if !self.frame_times.is_empty() {
            let sum: f32 = self.frame_times.iter().sum();
            self.avg_frame_time_ms = sum / self.frame_times.len() as f32;
            self.current_fps = if self.avg_frame_time_ms > 0.0 {
                1000.0 / self.avg_frame_time_ms
            } else {
                0.0
            };
        }
        
        self.frame_count += 1;
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
    
    pub fn render(&mut self, device: &Device, queue: &Queue, view: &TextureView) {
        if !self.enabled {
            return;
        }
        
        // Update graph vertices first (while self is mutable)
        self.update_graph_vertices(queue);
        
        let Some(pipeline) = &self.pipeline else { return };
        let Some(bind_group) = &self.bind_group else { return };
        
        // Update uniforms with current stats
        let uniforms = OverlayUniforms {
            screen_size: self.screen_size,
            panel_pos: [10.0, 10.0],
            panel_size: [250.0, 120.0],
            _padding1: [0.0, 0.0],
            bg_color: [0.05, 0.05, 0.05, 0.95],
            text_color: if self.current_fps >= 60.0 {
                [0.0, 1.0, 0.0, 1.0]  // Green
            } else if self.current_fps >= 30.0 {
                [1.0, 0.8, 0.0, 1.0]  // Yellow
            } else {
                [1.0, 0.0, 0.0, 1.0]  // Red
            },
            current_fps: self.current_fps,
            avg_frame_ms: self.avg_frame_time_ms,
            draw_calls: self.draw_calls as f32,
            triangle_count: self.triangles_rendered as f32 / 1000.0,
        };
        
        queue.write_buffer(&self.uniform_buffer, 0, bytemuck::cast_slice(&[uniforms]));
        
        // Create render pass
        let mut encoder = device.create_command_encoder(&CommandEncoderDescriptor {
            label: Some("Live Perf Overlay Encoder"),
        });
        
        {
            let mut render_pass = encoder.begin_render_pass(&RenderPassDescriptor {
                label: Some("Live Perf Overlay Pass"),
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
            
            // Draw graph bars
            let bar_count = self.frame_times.len().min(SAMPLE_COUNT);
            if bar_count > 0 {
                let start_vertex = 6;  // After panel vertices
                let vertices_per_bar = 6;
                render_pass.draw(
                    start_vertex..(start_vertex + bar_count as u32 * vertices_per_bar),
                    0..1
                );
            }
        }
        
        queue.submit(std::iter::once(encoder.finish()));
    }
    
    fn update_graph_vertices(&mut self, queue: &Queue) {
        let mut vertices = Vec::new();
        
        // Keep background panel vertices
        vertices.extend_from_slice(&[
            OverlayVertex { position: [0.0, 0.0], color: [0.05, 0.05, 0.05, 0.95] },
            OverlayVertex { position: [250.0, 0.0], color: [0.05, 0.05, 0.05, 0.95] },
            OverlayVertex { position: [250.0, 120.0], color: [0.05, 0.05, 0.05, 0.95] },
            OverlayVertex { position: [0.0, 0.0], color: [0.05, 0.05, 0.05, 0.95] },
            OverlayVertex { position: [250.0, 120.0], color: [0.05, 0.05, 0.05, 0.95] },
            OverlayVertex { position: [0.0, 120.0], color: [0.05, 0.05, 0.05, 0.95] },
        ]);
        
        // Add graph bars
        let graph_x_start = 15.0;
        let graph_y_base = 100.0;
        let graph_width = 220.0;
        let graph_height = 50.0;
        let bar_width = graph_width / SAMPLE_COUNT as f32;
        
        for (i, &frame_ms) in self.frame_times.iter().enumerate() {
            let x = graph_x_start + i as f32 * bar_width;
            
            // Normalize frame time (0-33ms = full height)
            let normalized_height = (frame_ms / 33.0).min(1.0);
            let bar_height = normalized_height * graph_height;
            
            // Color based on frame time
            let color = if frame_ms <= 16.67 {
                [0.0, 1.0, 0.0, 0.8]  // Green (60+ fps)
            } else if frame_ms <= 33.33 {
                [1.0, 0.8, 0.0, 0.8]  // Yellow (30-60 fps)
            } else {
                [1.0, 0.0, 0.0, 0.8]  // Red (<30 fps)
            };
            
            // Create bar quad
            vertices.extend_from_slice(&[
                OverlayVertex { position: [x, graph_y_base], color },
                OverlayVertex { position: [x + bar_width * 0.8, graph_y_base], color },
                OverlayVertex { position: [x + bar_width * 0.8, graph_y_base - bar_height], color },
                OverlayVertex { position: [x, graph_y_base], color },
                OverlayVertex { position: [x + bar_width * 0.8, graph_y_base - bar_height], color },
                OverlayVertex { position: [x, graph_y_base - bar_height], color },
            ]);
        }
        
        // Pad remaining slots
        while vertices.len() < 6 + SAMPLE_COUNT * 6 {
            vertices.push(OverlayVertex { position: [0.0, 0.0], color: [0.0, 0.0, 0.0, 0.0] });
        }
        
        // Update vertex buffer
        queue.write_buffer(&self.vertex_buffer, 0, bytemuck::cast_slice(&vertices));
    }
    
    pub fn resize(&mut self, width: u32, height: u32) {
        self.screen_size = [width as f32, height as f32];
    }
    
    pub fn toggle(&mut self) {
        self.enabled = !self.enabled;
    }
    
    pub fn is_enabled(&self) -> bool {
        self.enabled
    }
}
//! Performance monitoring UI component
//! Displays FPS, frame time, and GPU metrics similar to Three.js stats

use std::collections::VecDeque;
use std::time::{Duration, Instant};
use wgpu::*;
use wgpu::util::DeviceExt;

const SAMPLE_COUNT: usize = 60;  // Keep 60 frames of history
const UPDATE_INTERVAL_MS: u64 = 100;  // Update display every 100ms

#[derive(Debug)]
pub struct PerfMonitor {
    // Timing data
    frame_times: VecDeque<Duration>,
    last_frame_time: Instant,
    last_update_time: Instant,
    
    // Calculated stats
    current_fps: f32,
    avg_frame_time_ms: f32,
    min_frame_time_ms: f32,
    max_frame_time_ms: f32,
    
    // GPU metrics (if available)
    gpu_memory_used: Option<u64>,
    draw_calls: u32,
    triangles_rendered: u32,
    
    // UI rendering
    enabled: bool,
    position: [f32; 2],  // Screen position (top-left corner)
    scale: f32,
    
    // GPU resources for rendering
    vertex_buffer: Option<Buffer>,
    uniform_buffer: Option<Buffer>,
    bind_group: Option<BindGroup>,
    pipeline: Option<RenderPipeline>,
}

impl PerfMonitor {
    pub fn new() -> Self {
        Self {
            frame_times: VecDeque::with_capacity(SAMPLE_COUNT),
            last_frame_time: Instant::now(),
            last_update_time: Instant::now(),
            
            current_fps: 0.0,
            avg_frame_time_ms: 0.0,
            min_frame_time_ms: f32::MAX,
            max_frame_time_ms: 0.0,
            
            gpu_memory_used: None,
            draw_calls: 0,
            triangles_rendered: 0,
            
            enabled: true,
            position: [10.0, 10.0],
            scale: 1.0,
            
            vertex_buffer: None,
            uniform_buffer: None,
            bind_group: None,
            pipeline: None,
        }
    }
    
    /// Initialize GPU resources for rendering the stats panel
    pub fn init_gpu_resources(&mut self, device: &Device, format: TextureFormat) {
        // Create shader for stats panel
        let shader = device.create_shader_module(ShaderModuleDescriptor {
            label: Some("PerfMonitor Shader"),
            source: ShaderSource::Wgsl(include_str!("shaders/perf_monitor.wgsl").into()),
        });
        
        // Create pipeline layout
        let bind_group_layout = device.create_bind_group_layout(&BindGroupLayoutDescriptor {
            label: Some("PerfMonitor Bind Group Layout"),
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
        
        let pipeline_layout = device.create_pipeline_layout(&PipelineLayoutDescriptor {
            label: Some("PerfMonitor Pipeline Layout"),
            bind_group_layouts: &[&bind_group_layout],
            push_constant_ranges: &[],
        });
        
        // Create render pipeline
        self.pipeline = Some(device.create_render_pipeline(&RenderPipelineDescriptor {
            label: Some("PerfMonitor Pipeline"),
            layout: Some(&pipeline_layout),
            vertex: VertexState {
                module: &shader,
                entry_point: "vs_main",
                compilation_options: Default::default(),
                buffers: &[VertexBufferLayout {
                    array_stride: 8 * 4,  // 2 floats for position, 4 floats for color, 2 for UV
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
                        VertexAttribute {
                            offset: 24,
                            shader_location: 2,
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
                ..Default::default()
            },
            depth_stencil: None,
            multisample: MultisampleState::default(),
            multiview: None,
        }));
        
        // Create uniform buffer
        let uniform_data = StatsUniforms {
            screen_size: [800.0, 600.0],  // Will be updated on resize
            panel_position: self.position,
            panel_size: [150.0, 80.0],
            background_color: [0.0, 0.0, 0.0, 0.8],
            text_color: [0.0, 1.0, 0.0, 1.0],
            _padding: [0.0; 2],
        };
        
        self.uniform_buffer = Some(device.create_buffer_init(&util::BufferInitDescriptor {
            label: Some("PerfMonitor Uniforms"),
            contents: bytemuck::cast_slice(&[uniform_data]),
            usage: BufferUsages::UNIFORM | BufferUsages::COPY_DST,
        }));
        
        // Create bind group
        self.bind_group = Some(device.create_bind_group(&BindGroupDescriptor {
            label: Some("PerfMonitor Bind Group"),
            layout: &bind_group_layout,
            entries: &[
                BindGroupEntry {
                    binding: 0,
                    resource: self.uniform_buffer.as_ref().unwrap().as_entire_binding(),
                },
            ],
        }));
        
        // Create vertex buffer for panel quad and text
        self.update_vertex_buffer(device);
    }
    
    /// Update timing statistics
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
    
    /// Calculate statistics from frame history
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
    
    /// Update vertex buffer with current stats text
    fn update_vertex_buffer(&mut self, device: &Device) {
        // Generate vertices for stats display
        let mut vertices = Vec::new();
        
        // Background panel (dark semi-transparent)
        let panel_width = 150.0;
        let panel_height = 80.0;
        
        // Panel quad vertices
        vertices.extend_from_slice(&[
            // Position (2), Color (4), UV (2)
            0.0, 0.0,           0.0, 0.0, 0.0, 0.8,  0.0, 0.0,
            panel_width, 0.0,   0.0, 0.0, 0.0, 0.8,  1.0, 0.0,
            panel_width, panel_height, 0.0, 0.0, 0.0, 0.8,  1.0, 1.0,
            
            0.0, 0.0,           0.0, 0.0, 0.0, 0.8,  0.0, 0.0,
            panel_width, panel_height, 0.0, 0.0, 0.0, 0.8,  1.0, 1.0,
            0.0, panel_height,  0.0, 0.0, 0.0, 0.8,  0.0, 1.0,
        ]);
        
        // Create or update vertex buffer
        self.vertex_buffer = Some(device.create_buffer_init(&util::BufferInitDescriptor {
            label: Some("PerfMonitor Vertices"),
            contents: bytemuck::cast_slice(&vertices),
            usage: BufferUsages::VERTEX | BufferUsages::COPY_DST,
        }));
    }
    
    /// Record performance metrics
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
    
    /// Render the performance monitor overlay
    pub fn render(&self, encoder: &mut CommandEncoder, view: &TextureView, queue: &Queue) {
        if !self.enabled {
            return;
        }
        
        let Some(pipeline) = &self.pipeline else { return };
        let Some(vertex_buffer) = &self.vertex_buffer else { return };
        let Some(bind_group) = &self.bind_group else { return };
        
        // Create text for display
        let fps_text = format!("FPS: {:.0}", self.current_fps);
        let frame_time_text = format!("Frame: {:.2}ms", self.avg_frame_time_ms);
        let draw_calls_text = format!("Draws: {}", self.draw_calls);
        let triangles_text = format!("Tris: {:.1}k", self.triangles_rendered as f32 / 1000.0);
        
        // Render pass for overlay
        let mut render_pass = encoder.begin_render_pass(&RenderPassDescriptor {
            label: Some("PerfMonitor Render Pass"),
            color_attachments: &[Some(RenderPassColorAttachment {
                view,
                resolve_target: None,
                ops: Operations {
                    load: LoadOp::Load,  // Don't clear, we're overlaying
                    store: StoreOp::Store,
                },
            })],
            depth_stencil_attachment: None,
            ..Default::default()
        });
        
        render_pass.set_pipeline(pipeline);
        render_pass.set_bind_group(0, bind_group, &[]);
        render_pass.set_vertex_buffer(0, vertex_buffer.slice(..));
        
        // Draw background panel
        render_pass.draw(0..6, 0..1);
        
        // TODO: Add text rendering for the actual stats
        // This would require integrating with the text rendering system
    }
    
    pub fn toggle(&mut self) {
        self.enabled = !self.enabled;
    }
    
    pub fn is_enabled(&self) -> bool {
        self.enabled
    }
}

#[repr(C)]
#[derive(Copy, Clone, Debug, bytemuck::Pod, bytemuck::Zeroable)]
struct StatsUniforms {
    screen_size: [f32; 2],
    panel_position: [f32; 2],
    panel_size: [f32; 2],
    background_color: [f32; 4],
    text_color: [f32; 4],
    _padding: [f32; 2],
}
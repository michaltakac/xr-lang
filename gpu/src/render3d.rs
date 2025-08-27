//! 3D rendering system for XR-DSL

use crate::math::*;
use crate::ui3d::UI3DSystem;
use crate::scene::*;
use wgpu::*;
use wgpu::util::DeviceExt;
use bytemuck::{Pod, Zeroable};

#[repr(C)]
#[derive(Debug, Clone, Copy, Pod, Zeroable)]
pub struct Vertex3D {
    pub position: [f32; 3],
    pub normal: [f32; 3],
    pub uv: [f32; 2],
}

impl Vertex3D {
    pub fn new(position: [f32; 3], normal: [f32; 3], uv: [f32; 2]) -> Self {
        Self { position, normal, uv }
    }
    
    pub fn desc<'a>() -> VertexBufferLayout<'a> {
        VertexBufferLayout {
            array_stride: std::mem::size_of::<Vertex3D>() as BufferAddress,
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
                    format: VertexFormat::Float32x3,
                },
                VertexAttribute {
                    offset: (std::mem::size_of::<[f32; 3]>() * 2) as BufferAddress,
                    shader_location: 2,
                    format: VertexFormat::Float32x2,
                },
            ],
        }
    }
}

#[repr(C)]
#[derive(Debug, Clone, Copy, Pod, Zeroable)]
pub struct Uniforms {
    pub view_proj: Mat4,
    pub model: Mat4,
    pub time: f32,
    pub _pad: [f32; 3],
}

pub struct Mesh {
    pub vertices: Vec<Vertex3D>,
    pub indices: Vec<u16>,
    pub vertex_buffer: Buffer,
    pub index_buffer: Buffer,
    pub transform: Transform,
}

impl Mesh {
    pub fn new(device: &Device, vertices: Vec<Vertex3D>, indices: Vec<u16>) -> Self {
        let vertex_buffer = device.create_buffer_init(&util::BufferInitDescriptor {
            label: Some("Vertex Buffer"),
            contents: bytemuck::cast_slice(&vertices),
            usage: BufferUsages::VERTEX,
        });
        
        let index_buffer = device.create_buffer_init(&util::BufferInitDescriptor {
            label: Some("Index Buffer"),
            contents: bytemuck::cast_slice(&indices),
            usage: BufferUsages::INDEX,
        });
        
        Self {
            vertices,
            indices,
            vertex_buffer,
            index_buffer,
            transform: Transform::IDENTITY,
        }
    }
    
    pub fn cube(device: &Device) -> Self {
        let vertices = vec![
            // Front face
            Vertex3D::new([-1.0, -1.0,  1.0], [ 0.0,  0.0,  1.0], [0.0, 0.0]),
            Vertex3D::new([ 1.0, -1.0,  1.0], [ 0.0,  0.0,  1.0], [1.0, 0.0]),
            Vertex3D::new([ 1.0,  1.0,  1.0], [ 0.0,  0.0,  1.0], [1.0, 1.0]),
            Vertex3D::new([-1.0,  1.0,  1.0], [ 0.0,  0.0,  1.0], [0.0, 1.0]),
            
            // Back face
            Vertex3D::new([-1.0, -1.0, -1.0], [ 0.0,  0.0, -1.0], [1.0, 0.0]),
            Vertex3D::new([-1.0,  1.0, -1.0], [ 0.0,  0.0, -1.0], [1.0, 1.0]),
            Vertex3D::new([ 1.0,  1.0, -1.0], [ 0.0,  0.0, -1.0], [0.0, 1.0]),
            Vertex3D::new([ 1.0, -1.0, -1.0], [ 0.0,  0.0, -1.0], [0.0, 0.0]),
            
            // Top face
            Vertex3D::new([-1.0,  1.0, -1.0], [ 0.0,  1.0,  0.0], [0.0, 1.0]),
            Vertex3D::new([-1.0,  1.0,  1.0], [ 0.0,  1.0,  0.0], [0.0, 0.0]),
            Vertex3D::new([ 1.0,  1.0,  1.0], [ 0.0,  1.0,  0.0], [1.0, 0.0]),
            Vertex3D::new([ 1.0,  1.0, -1.0], [ 0.0,  1.0,  0.0], [1.0, 1.0]),
            
            // Bottom face
            Vertex3D::new([-1.0, -1.0, -1.0], [ 0.0, -1.0,  0.0], [1.0, 1.0]),
            Vertex3D::new([ 1.0, -1.0, -1.0], [ 0.0, -1.0,  0.0], [0.0, 1.0]),
            Vertex3D::new([ 1.0, -1.0,  1.0], [ 0.0, -1.0,  0.0], [0.0, 0.0]),
            Vertex3D::new([-1.0, -1.0,  1.0], [ 0.0, -1.0,  0.0], [1.0, 0.0]),
            
            // Right face
            Vertex3D::new([ 1.0, -1.0, -1.0], [ 1.0,  0.0,  0.0], [1.0, 0.0]),
            Vertex3D::new([ 1.0,  1.0, -1.0], [ 1.0,  0.0,  0.0], [1.0, 1.0]),
            Vertex3D::new([ 1.0,  1.0,  1.0], [ 1.0,  0.0,  0.0], [0.0, 1.0]),
            Vertex3D::new([ 1.0, -1.0,  1.0], [ 1.0,  0.0,  0.0], [0.0, 0.0]),
            
            // Left face
            Vertex3D::new([-1.0, -1.0, -1.0], [-1.0,  0.0,  0.0], [0.0, 0.0]),
            Vertex3D::new([-1.0, -1.0,  1.0], [-1.0,  0.0,  0.0], [1.0, 0.0]),
            Vertex3D::new([-1.0,  1.0,  1.0], [-1.0,  0.0,  0.0], [1.0, 1.0]),
            Vertex3D::new([-1.0,  1.0, -1.0], [-1.0,  0.0,  0.0], [0.0, 1.0]),
        ];
        
        let indices = vec![
             0,  1,  2,   2,  3,  0,   // front
             4,  5,  6,   6,  7,  4,   // back
             8,  9, 10,  10, 11,  8,   // top
            12, 13, 14,  14, 15, 12,   // bottom
            16, 17, 18,  18, 19, 16,   // right
            20, 21, 22,  22, 23, 20,   // left
        ];
        
        Self::new(device, vertices, indices)
    }
    
    pub fn plane(device: &Device) -> Self {
        let vertices = vec![
            Vertex3D::new([-1.0, 0.0, -1.0], [0.0, 1.0, 0.0], [0.0, 1.0]),
            Vertex3D::new([ 1.0, 0.0, -1.0], [0.0, 1.0, 0.0], [1.0, 1.0]),
            Vertex3D::new([ 1.0, 0.0,  1.0], [0.0, 1.0, 0.0], [1.0, 0.0]),
            Vertex3D::new([-1.0, 0.0,  1.0], [0.0, 1.0, 0.0], [0.0, 0.0]),
        ];
        
        let indices = vec![0, 1, 2, 2, 3, 0];
        
        Self::new(device, vertices, indices)
    }
}

pub struct Renderer3D {
    pub render_pipeline: RenderPipeline,
    pub uniform_buffer: Buffer,
    pub uniform_bind_group: BindGroup,
    pub camera: Camera,
    pub meshes: Vec<Mesh>,
    pub time: f32,
    pub ui_system: UI3DSystem,
    pub scene_data: SceneData,
    pub aspect_ratio: f32,
}

impl Renderer3D {
    pub fn new(device: &Device, queue: &Queue, config: &SurfaceConfiguration) -> Self {
        let shader = device.create_shader_module(ShaderModuleDescriptor {
            label: Some("3D Shader"),
            source: ShaderSource::Wgsl(include_str!("../../examples/basic_3d.wgsl").into()),
        });
        
        let uniform_buffer = device.create_buffer(&BufferDescriptor {
            label: Some("Uniform Buffer"),
            size: std::mem::size_of::<Uniforms>() as u64,
            usage: BufferUsages::UNIFORM | BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });
        
        let uniform_bind_group_layout = device.create_bind_group_layout(&BindGroupLayoutDescriptor {
            label: Some("Uniform Bind Group Layout"),
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
        
        let uniform_bind_group = device.create_bind_group(&BindGroupDescriptor {
            label: Some("Uniform Bind Group"),
            layout: &uniform_bind_group_layout,
            entries: &[BindGroupEntry {
                binding: 0,
                resource: uniform_buffer.as_entire_binding(),
            }],
        });
        
        let render_pipeline_layout = device.create_pipeline_layout(&PipelineLayoutDescriptor {
            label: Some("Render Pipeline Layout"),
            bind_group_layouts: &[&uniform_bind_group_layout],
            push_constant_ranges: &[],
        });
        
        let render_pipeline = device.create_render_pipeline(&RenderPipelineDescriptor {
            label: Some("3D Render Pipeline"),
            layout: Some(&render_pipeline_layout),
            vertex: VertexState {
                module: &shader,
                entry_point: "vs_main",
                buffers: &[Vertex3D::desc()],
                compilation_options: Default::default(),
            },
            fragment: Some(FragmentState {
                module: &shader,
                entry_point: "fs_main",
                targets: &[Some(ColorTargetState {
                    format: config.format,
                    blend: Some(BlendState::REPLACE),
                    write_mask: ColorWrites::ALL,
                })],
                compilation_options: Default::default(),
            }),
            primitive: PrimitiveState {
                topology: PrimitiveTopology::TriangleList,
                strip_index_format: None,
                front_face: FrontFace::Ccw,
                cull_mode: Some(Face::Back),
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
            multisample: MultisampleState {
                count: 1,
                mask: !0,
                alpha_to_coverage_enabled: false,
            },
            multiview: None,
        });
        
        let camera = Camera::perspective(
            Vec3::new(0.0, 5.0, 10.0),
            Vec3::ZERO,
            Vec3::Y,
            std::f32::consts::FRAC_PI_4,
            config.width as f32 / config.height as f32,
            0.1,
            100.0,
        );
        
        let mut renderer = Self {
            render_pipeline,
            uniform_buffer,
            uniform_bind_group,
            camera,
            meshes: Vec::new(),
            time: 0.0,
            ui_system: UI3DSystem::new(device, queue, config),
            scene_data: SceneData::default(),
            aspect_ratio: config.width as f32 / config.height as f32,
        };
        
        // Initialize scene from default data
        renderer.rebuild_scene(device);
        renderer
    }
    
    pub fn add_mesh(&mut self, mesh: Mesh) {
        self.meshes.push(mesh);
    }
    
    pub fn load_scene(&mut self, scene_data: SceneData, device: &Device) {
        println!("🔄 Loading new scene with {} cubes, {} UI elements", 
            scene_data.cubes.len(), scene_data.ui_elements.len());
        self.scene_data = scene_data;
        self.rebuild_scene(device);
        
        // Update UI elements
        self.ui_system.update_ui_elements(self.scene_data.ui_elements.clone());
        
        self.ui_system.add_log_entry(&format!("Scene reloaded with {} objects and {} UI elements", 
            self.scene_data.cubes.len(), self.scene_data.ui_elements.len()));
    }
    
    fn rebuild_scene(&mut self, device: &Device) {
        // Clear existing meshes
        self.meshes.clear();
        
        // Create meshes from scene data
        for cube_data in &self.scene_data.cubes {
            let mut mesh = Mesh::cube(device);
            mesh.transform.position = cube_data.position;
            mesh.transform.scale = cube_data.scale;
            
            // Reset rotation to avoid accumulating rotations on reload
            mesh.transform.rotation = Vec3::ZERO;
            
            self.meshes.push(mesh);
        }
        
        // Apply camera from scene data if available
        if let Some(ref camera_data) = self.scene_data.camera {
            // Update camera with DSL settings
            self.camera = Camera::perspective(
                camera_data.position,
                camera_data.target,
                Vec3::Y,
                camera_data.fov, // FOV already in radians from DSL parsing
                self.aspect_ratio,
                0.1,
                100.0,
            );
            
            println!("📹 Applied DSL Camera: pos({:.1}, {:.1}, {:.1}), target({:.1}, {:.1}, {:.1}), fov: {:.1}°", 
                camera_data.position.x, camera_data.position.y, camera_data.position.z,
                camera_data.target.x, camera_data.target.y, camera_data.target.z,
                camera_data.fov * 180.0 / std::f32::consts::PI);
        }
        
        // Log behavior information for debugging
        println!("✅ Rebuilt scene with {} meshes", self.meshes.len());
        for (name, behavior) in &self.scene_data.behaviors {
            if let Some(speed) = behavior.state.get("speed") {
                println!("🎯 Behavior '{}': speed = {}", name, speed);
            }
        }
    }
    
    
    pub fn update(&mut self, dt: f32, device: &Device) {
        self.time += dt;
        
        // Update camera based on scene data or use orbital fallback
        if let Some(ref camera_data) = self.scene_data.camera {
            // Use DSL camera settings
            self.camera.update(camera_data.position, camera_data.target, Vec3::Y);
        } else {
            // Fallback to orbital camera animation
            let radius = 10.0;
            let speed = 0.5;
            let x = (self.time * speed).cos() * radius;
            let z = (self.time * speed).sin() * radius;
            let y = 5.0;
            
            self.camera.update(Vec3::new(x, y, z), Vec3::ZERO, Vec3::Y);
        }
        
        // Update mesh rotations based on scene data and behaviors
        for (i, mesh) in self.meshes.iter_mut().enumerate() {
            if i < self.scene_data.cubes.len() {
                let cube_data = &self.scene_data.cubes[i];
                
                // Get rotation speed from behavior
                let rotation_speed = if let Some(ref behavior_name) = cube_data.behavior {
                    if let Some(behavior) = self.scene_data.behaviors.get(behavior_name) {
                        behavior.state.get("speed").unwrap_or(&1.0) * dt
                    } else {
                        1.0 * dt
                    }
                } else {
                    0.0 // No rotation if no behavior
                };
                
                mesh.transform.rotation.y += rotation_speed;
            } else {
                // Fallback for any extra meshes
                mesh.transform.rotation.y = self.time + i as f32;
            }
        }
        
        // Update UI system
        self.ui_system.update(dt, &self.camera, device);
    }

    pub fn render(&self, device: &Device, queue: &Queue, view: &TextureView, depth_view: &TextureView) {
        // Clear first
        if self.meshes.is_empty() {
            // Just clear if no meshes
            let mut encoder = device.create_command_encoder(&CommandEncoderDescriptor {
                label: Some("Clear Encoder"),
            });
            {
                let _render_pass = encoder.begin_render_pass(&RenderPassDescriptor {
                    label: Some("Clear Pass"),
                    color_attachments: &[Some(RenderPassColorAttachment {
                        view,
                        resolve_target: None,
                        ops: Operations {
                            load: LoadOp::Clear(Color { r: 0.1, g: 0.2, b: 0.3, a: 1.0 }),
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
                    timestamp_writes: None,
                    occlusion_query_set: None,
                });
            }
            queue.submit(std::iter::once(encoder.finish()));
            return;
        }
        
        // Render each mesh with proper uniform updates
        for (i, mesh) in self.meshes.iter().enumerate() {
            let uniforms = Uniforms {
                view_proj: self.camera.view_projection,
                model: mesh.transform.to_matrix(),
                time: self.time,
                _pad: [0.0; 3],
            };
            
            // Update uniforms for this specific mesh BEFORE creating the command encoder
            queue.write_buffer(&self.uniform_buffer, 0, bytemuck::cast_slice(&[uniforms]));
            
            let mut encoder = device.create_command_encoder(&CommandEncoderDescriptor {
                label: Some(&format!("3D Render Encoder for Mesh {}", i)),
            });
            
            {
                let mut render_pass = encoder.begin_render_pass(&RenderPassDescriptor {
                    label: Some(&format!("3D Scene Render Pass for Mesh {}", i)),
                    color_attachments: &[Some(RenderPassColorAttachment {
                        view,
                        resolve_target: None,
                        ops: Operations {
                            load: if i == 0 { 
                                LoadOp::Clear(Color { r: 0.1, g: 0.2, b: 0.3, a: 1.0 })
                            } else {
                                LoadOp::Load
                            },
                            store: StoreOp::Store,
                        },
                    })],
                    depth_stencil_attachment: Some(RenderPassDepthStencilAttachment {
                        view: depth_view,
                        depth_ops: Some(Operations {
                            load: if i == 0 { LoadOp::Clear(1.0) } else { LoadOp::Load },
                            store: StoreOp::Store,
                        }),
                        stencil_ops: None,
                    }),
                    timestamp_writes: None,
                    occlusion_query_set: None,
                });
                
                render_pass.set_pipeline(&self.render_pipeline);
                render_pass.set_bind_group(0, &self.uniform_bind_group, &[]);
                render_pass.set_vertex_buffer(0, mesh.vertex_buffer.slice(..));
                render_pass.set_index_buffer(mesh.index_buffer.slice(..), IndexFormat::Uint16);
                render_pass.draw_indexed(0..mesh.indices.len() as u32, 0, 0..1);
            }
            
            queue.submit(std::iter::once(encoder.finish()));
        }
        
        // Render UI on top
        self.ui_system.render(device, queue, view, depth_view);
    }
}
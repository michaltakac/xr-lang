//! 3D rendering system for XR-DSL

use crate::math::*;
use crate::ui3d::UI3DSystem;
use crate::scene::*;
use crate::runtime_state::RuntimeState;
use crate::behavior_system::BehaviorSystem;
use crate::code_sync::CodeSync;
use crate::perf_overlay_live::LivePerfOverlay;
use crate::materials::{MeshBasicMaterial, Side};
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
    pub material: Option<MeshBasicMaterial>,
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
            material: None,
        }
    }
    
    pub fn from_primitive(device: &Device, primitive: &crate::entity::PrimitiveType) -> Self {
        // Generate mesh data using the mesh_gen module
        let mesh_data = crate::mesh_gen::MeshData::from_primitive(primitive);
        
        // Convert mesh_gen vertices to Vertex3D
        let vertices: Vec<Vertex3D> = mesh_data.vertices.iter().map(|v| {
            Vertex3D::new(v.position, v.normal, v.tex_coords)
        }).collect();
        
        // Convert u32 indices to u16 (with bounds checking)
        let indices: Vec<u16> = mesh_data.indices.iter().map(|&i| {
            if i > u16::MAX as u32 {
                panic!("Index {} exceeds u16 max value", i);
            }
            i as u16
        }).collect();
        
        Self::new(device, vertices, indices)
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
    pub uniform_bind_group_layout: BindGroupLayout,
    pub camera: Camera,
    pub camera_target: Vec3,
    pub meshes: Vec<Mesh>,
    pub time: f32,
    pub ui_system: UI3DSystem,
    pub scene_data: SceneData,
    pub aspect_ratio: f32,
    pub keyboard_state: KeyboardState,
    pub mouse_state: MouseState,
    pub orbit_state: OrbitState,
    pub runtime_state: RuntimeState,
    pub behavior_system: BehaviorSystem,
    pub code_sync: CodeSync,
    pub perf_overlay: LivePerfOverlay,
    pub device: *const Device,
    pub surface_format: TextureFormat,
}

pub struct OrbitState {
    pub spherical_coords: (f32, f32, f32), // (radius, theta, phi)
}

pub struct KeyboardState {
    pub keys_pressed: std::collections::HashSet<String>,
}

pub struct MouseState {
    pub is_dragging: bool,
    pub last_position: Option<(f32, f32)>,
    pub delta: (f32, f32),
    pub scroll_delta: f32,
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
        
        let camera_target = Vec3::ZERO;
        let camera = Camera::perspective(
            Vec3::new(0.0, 5.0, 10.0),
            camera_target,
            Vec3::Y,
            std::f32::consts::FRAC_PI_4,
            config.width as f32 / config.height as f32,
            0.1,
            100.0,
        );
        
        // Calculate initial spherical coords from camera position
        let offset = Vec3::new(0.0, 5.0, 10.0) - camera_target;
        let radius = offset.length();
        let theta = offset.z.atan2(offset.x);
        let phi = (offset.y / radius).acos();
        
        let mut renderer = Self {
            render_pipeline,
            uniform_buffer,
            uniform_bind_group,
            uniform_bind_group_layout,
            camera,
            camera_target,
            meshes: Vec::new(),
            time: 0.0,
            ui_system: UI3DSystem::new(device, queue, config),
            scene_data: SceneData::default(),
            aspect_ratio: config.width as f32 / config.height as f32,
            keyboard_state: KeyboardState {
                keys_pressed: std::collections::HashSet::new(),
            },
            mouse_state: MouseState {
                is_dragging: false,
                last_position: None,
                delta: (0.0, 0.0),
                scroll_delta: 0.0,
            },
            orbit_state: OrbitState {
                spherical_coords: (radius, theta, phi),
            },
            runtime_state: RuntimeState::new(),
            behavior_system: BehaviorSystem::new(),
            code_sync: CodeSync::new(),
            perf_overlay: LivePerfOverlay::new(device, config.format),
            device: device as *const Device,
            surface_format: config.format,
        };
        
        // Initialize scene from default data
        renderer.rebuild_scene(device);
        renderer
    }
    
    pub fn add_mesh(&mut self, mesh: Mesh) {
        self.meshes.push(mesh);
    }
    
    pub fn set_source_path(&mut self, path: impl AsRef<std::path::Path>) {
        self.code_sync.set_source_path(path);
    }
    
    pub fn load_scene(&mut self, mut scene_data: SceneData, device: &Device) {
        println!("🔄 Loading new scene with {} entities, {} UI elements", 
            scene_data.entities.len(), scene_data.ui_elements.len());
        
        // Hot-swap behaviors from AST
        if !scene_data.ast.is_empty() {
            if let Err(e) = self.behavior_system.hot_swap_behaviors(&scene_data.ast) {
                println!("⚠️ Failed to hot-swap behaviors: {}", e);
                self.ui_system.add_log_entry(&format!("⚠️ Behavior hot-swap failed: {}", e));
            } else {
                self.ui_system.add_log_entry("✨ Behaviors hot-swapped!");
            }
        }
        
        // Extract current runtime state before loading new scene
        if self.runtime_state.should_preserve_camera() {
            self.runtime_state.preserve_camera(self.camera.position, self.camera_target, 
                std::f32::consts::FRAC_PI_4); // TODO: Store FOV properly
            
            // Queue camera update for code sync if in Live mode
            if self.runtime_state.authoring_mode == crate::runtime_state::AuthoringMode::Live {
                if let Some(camera_state) = &self.runtime_state.camera_overrides {
                    self.code_sync.queue_camera_update(camera_state);
                }
            }
        }
        
        // Apply preserved state to new scene data
        self.runtime_state.apply_to_scene(&mut scene_data);
        
        self.scene_data = scene_data;
        self.rebuild_scene(device);
        
        // Update UI elements
        self.ui_system.update_ui_elements(self.scene_data.ui_elements.clone());
        
        self.ui_system.add_log_entry(&format!("Scene reloaded with {} entities, {} UI elements, {} behaviors", 
            self.scene_data.entities.len(), self.scene_data.ui_elements.len(), 
            self.behavior_system.behavior_ast.len()));
    }
    
    pub fn apply_scene_changes(&mut self, mut scene_data: SceneData, changes: Vec<crate::reconciliation::SceneChange>, device: &Device) {
        println!("⚡ Applying {} incremental changes", changes.len());
        
        let mut needs_full_rebuild = false;
        let mut model_reloads = Vec::new();
        
        // Process changes to determine what needs updating
        for change in &changes {
            match change {
                crate::reconciliation::SceneChange::EntityAdded { entity } => {
                    println!("  + Adding entity: {}", entity.name);
                    // For now, trigger full rebuild for entity additions
                    needs_full_rebuild = true;
                }
                crate::reconciliation::SceneChange::EntityRemoved { id } => {
                    println!("  - Removing entity: {}", id);
                    needs_full_rebuild = true;
                }
                crate::reconciliation::SceneChange::EntityModified { id, changes } => {
                    println!("  ~ Modifying entity: {}", id);
                    
                    // Find the mesh index for this entity
                    if let Some((mesh_idx, _entity)) = self.scene_data.entities.iter().enumerate()
                        .find(|(_, e)| e.id == *id) {
                        
                        if mesh_idx < self.meshes.len() {
                            // Apply transform changes directly without rebuilding
                            if let Some(ref transform) = changes.transform {
                                self.meshes[mesh_idx].transform.position = transform.position;
                                self.meshes[mesh_idx].transform.rotation = transform.rotation;
                                self.meshes[mesh_idx].transform.scale = transform.scale;
                                println!("    Updated transform");
                            }
                            
                            // Material changes require shader updates (future optimization)
                            if changes.material.is_some() {
                                println!("    Material changed (full rebuild needed)");
                                needs_full_rebuild = true;
                            }
                            
                            // Mesh source changes require model reload
                            if let Some(ref new_mesh) = changes.mesh {
                                if let crate::entity::MeshSource::Model(model) = new_mesh {
                                    model_reloads.push((mesh_idx, model.clone()));
                                } else {
                                    needs_full_rebuild = true;
                                }
                            }
                        }
                    }
                }
                crate::reconciliation::SceneChange::BehaviorAdded { .. } |
                crate::reconciliation::SceneChange::BehaviorModified { .. } |
                crate::reconciliation::SceneChange::BehaviorRemoved { .. } => {
                    // These are handled by the hot-swap below
                }
                crate::reconciliation::SceneChange::CameraChanged { .. } => {
                    // Camera changes can be applied directly
                    if let Some(ref camera_data) = scene_data.camera {
                        if !self.runtime_state.should_preserve_camera() {
                            self.camera_target = camera_data.target;
                            self.camera = Camera::perspective(
                                camera_data.position,
                                camera_data.target,
                                Vec3::Y,
                                camera_data.fov,
                                self.aspect_ratio,
                                0.1,
                                100.0,
                            );
                            println!("  📷 Camera updated");
                        }
                    }
                }
                crate::reconciliation::SceneChange::LightingChanged { .. } => {
                    // Lighting changes will be handled when we have a lighting system
                    println!("  💡 Lighting changed (not yet implemented)");
                }
                _ => {}
            }
        }
        
        // Hot-swap behaviors if we have AST (always hot-swap for incremental updates)
        // This ensures behavior changes are always applied, even for state-only changes
        if !scene_data.ast.is_empty() {
            if let Err(e) = self.behavior_system.hot_swap_behaviors(&scene_data.ast) {
                println!("⚠️ Failed to hot-swap behaviors: {}", e);
                self.ui_system.add_log_entry(&format!("⚠️ Behavior hot-swap failed: {}", e));
            } else {
                self.ui_system.add_log_entry("✨ Behaviors hot-swapped!");
                println!("  ✨ Behaviors hot-swapped (incremental)");
            }
        }
        
        // Reload specific models without full rebuild
        for (mesh_idx, model_source) in model_reloads {
            match crate::model_loader::load_model(&model_source) {
                Ok(mesh_data) => {
                    let vertices: Vec<Vertex3D> = mesh_data.vertices.iter().map(|v| {
                        Vertex3D::new(v.position, v.normal, v.tex_coords)
                    }).collect();
                    
                    let indices: Vec<u16> = mesh_data.indices.iter().map(|&i| {
                        if i > u16::MAX as u32 {
                            u16::MAX
                        } else {
                            i as u16
                        }
                    }).collect();
                    
                    let old_transform = self.meshes[mesh_idx].transform.clone();
                    self.meshes[mesh_idx] = Mesh::new(device, vertices, indices);
                    self.meshes[mesh_idx].transform = old_transform;
                    
                    // Get path from ModelSource for logging
                    let path_str = match model_source {
                        crate::entity::ModelSource::GLTF { path } |
                        crate::entity::ModelSource::GLB { path } |
                        crate::entity::ModelSource::OBJ { path } |
                        crate::entity::ModelSource::STL { path } |
                        crate::entity::ModelSource::PLY { path } => path.display().to_string(),
                        _ => "unknown".to_string(),
                    };
                    println!("    🔄 Reloaded model: {}", path_str);
                }
                Err(e) => {
                    log::error!("Failed to reload model: {}", e);
                }
            }
        }
        
        // Apply preserved state
        self.runtime_state.apply_to_scene(&mut scene_data);
        
        // Update scene data
        self.scene_data = scene_data;
        
        // Only rebuild if necessary
        if needs_full_rebuild {
            println!("  🔨 Full scene rebuild required");
            self.rebuild_scene(device);
        }
        
        // Update UI elements
        self.ui_system.update_ui_elements(self.scene_data.ui_elements.clone());
        
        self.ui_system.add_log_entry(&format!("⚡ Applied {} changes (rebuild: {})", 
            changes.len(), needs_full_rebuild));
    }
    
    fn rebuild_scene(&mut self, device: &Device) {
        // Clear existing meshes
        self.meshes.clear();
        
        // Create meshes from scene data
        for entity in &self.scene_data.entities {
            // Create mesh based on entity's mesh source
            let mut mesh = match &entity.mesh {
                crate::entity::MeshSource::Primitive(primitive) => {
                    // Generate proper mesh for each primitive type
                    Mesh::from_primitive(device, primitive)
                }
                crate::entity::MeshSource::Model(model_source) => {
                    // Load model from file
                    match crate::model_loader::load_model(model_source) {
                        Ok(mesh_data) => {
                            // Convert mesh_gen vertices to Vertex3D
                            let vertices: Vec<Vertex3D> = mesh_data.vertices.iter().map(|v| {
                                Vertex3D::new(v.position, v.normal, v.tex_coords)
                            }).collect();
                            
                            // Convert u32 indices to u16 (with bounds checking)
                            let indices: Vec<u16> = mesh_data.indices.iter().map(|&i| {
                                if i > u16::MAX as u32 {
                                    log::warn!("Index {} exceeds u16 max, clamping", i);
                                    u16::MAX
                                } else {
                                    i as u16
                                }
                            }).collect();
                            
                            Mesh::new(device, vertices, indices)
                        }
                        Err(e) => {
                            log::error!("Failed to load model: {}", e);
                            Mesh::cube(device) // Fallback to cube on error
                        }
                    }
                }
                _ => Mesh::cube(device), // Fallback for procedural
            };
            
            mesh.transform.position = entity.transform.position;
            mesh.transform.scale = entity.transform.scale;
            mesh.transform.rotation = entity.transform.rotation;
            
            // Apply material if specified
            if let Some(ref material_def) = entity.material {
                let device = unsafe { &*self.device };
                
                match material_def {
                    dsl::ast::MaterialDef::MeshBasic { color, opacity, transparent, side, wireframe } => {
                        let mut mat = MeshBasicMaterial::new()
                            .with_color(color[0], color[1], color[2])
                            .with_opacity(*opacity);
                        
                        mat.transparent = *transparent;
                        mat.wireframe = *wireframe;
                        mat.side = match side.as_str() {
                            "double" => Side::Double,
                            "back" => Side::Back,
                            _ => Side::Front,
                        };
                        
                        mat.init(device, self.surface_format, &self.uniform_bind_group_layout);
                        mesh.material = Some(mat);
                    }
                    _ => {} // Standard material not yet implemented
                }
            }
            
            self.meshes.push(mesh);
        }
        
        // Apply camera from scene data if available
        if let Some(ref camera_data) = self.scene_data.camera {
            // Update camera with DSL settings
            self.camera_target = camera_data.target;
            self.camera = Camera::perspective(
                camera_data.position,
                camera_data.target,
                Vec3::Y,
                camera_data.fov, // FOV already in radians from DSL parsing
                self.aspect_ratio,
                0.1,
                100.0,
            );
            
            // Update spherical coordinates for orbit controls
            let offset = camera_data.position - camera_data.target;
            let radius = offset.length();
            let theta = offset.z.atan2(offset.x);
            let phi = (offset.y / radius).acos();
            self.orbit_state.spherical_coords = (radius, theta, phi);
            
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
        // Track frame timing for performance overlay
        self.perf_overlay.frame_start();
        
        self.time += dt;
        
        // Try to sync code changes if in Live mode
        if self.runtime_state.authoring_mode == crate::runtime_state::AuthoringMode::Live {
            // Sync periodically (e.g., every 60 frames)
            static mut SYNC_COUNTER: u32 = 0;
            unsafe {
                SYNC_COUNTER += 1;
                if SYNC_COUNTER >= 60 {
                    SYNC_COUNTER = 0;
                    let _ = self.code_sync.sync_to_file(&self.runtime_state);
                }
            }
        }
        
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
        
        // Update mesh rotations using the behavior system
        for (i, mesh) in self.meshes.iter_mut().enumerate() {
            if i < self.scene_data.entities.len() {
                let entity = &self.scene_data.entities[i];
                
                // Execute behavior if assigned
                if let Some(ref behavior_name) = entity.behavior {
                    let object_id = &entity.id;
                    
                    // Run the behavior through the interpreter
                    match self.behavior_system.update_behavior(object_id, behavior_name, dt) {
                        Ok(update) => {
                        // Apply generic property updates
                        let mut rotation_x = None;
                        let mut rotation_y = None;
                        let mut rotation_z = None;
                        let mut position_x = None;
                        let mut position_y = None;
                        let mut position_z = None;
                        let mut scale_x = None;
                        let mut scale_y = None;
                        let mut scale_z = None;
                        
                        // Extract all properties
                        for (key, value) in &update.properties {
                            if let vm::Value::F32(v) = value {
                                match key.as_str() {
                                    "rotation.x" => rotation_x = Some(*v),
                                    "rotation.y" => rotation_y = Some(*v),
                                    "rotation.z" => rotation_z = Some(*v),
                                    "position.x" => position_x = Some(*v),
                                    "position.y" => position_y = Some(*v),
                                    "position.z" => position_z = Some(*v),
                                    "scale.x" => scale_x = Some(*v),
                                    "scale.y" => scale_y = Some(*v),
                                    "scale.z" => scale_z = Some(*v),
                                    _ => {}
                                }
                            }
                        }
                        
                        // Apply rotations (convert Euler angles to quaternion)
                        if rotation_x.is_some() || rotation_y.is_some() || rotation_z.is_some() {
                            let rx = rotation_x.unwrap_or(0.0);  // pitch (rotation around X)
                            let ry = rotation_y.unwrap_or(0.0);  // yaw (rotation around Y)
                            let rz = rotation_z.unwrap_or(0.0);  // roll (rotation around Z)
                            mesh.transform.rotation = Quat::from_euler(rx, ry, rz);
                        }
                        
                        // Apply position if any component changed
                        if position_x.is_some() || position_y.is_some() || position_z.is_some() {
                            mesh.transform.position = Vec3::new(
                                position_x.unwrap_or(mesh.transform.position.x),
                                position_y.unwrap_or(mesh.transform.position.y),
                                position_z.unwrap_or(mesh.transform.position.z),
                            );
                        }
                        
                        // Apply scale if any component changed
                        if scale_x.is_some() || scale_y.is_some() || scale_z.is_some() {
                            mesh.transform.scale = Vec3::new(
                                scale_x.unwrap_or(mesh.transform.scale.x),
                                scale_y.unwrap_or(mesh.transform.scale.y),
                                scale_z.unwrap_or(mesh.transform.scale.z),
                            );
                        }
                        }
                        Err(_e) => {
                            // println!("      ERROR: Behavior update failed for '{}': {}", behavior_name, _e);
                            // Fallback to simple rotation if behavior fails
                            let rotation_speed = 1.0 * dt;
                            let rotation_delta = Quat::from_axis_angle(Vec3::Y, rotation_speed);
                            mesh.transform.rotation = rotation_delta * mesh.transform.rotation;
                        }
                    }
                } else {
                    // No behavior - no rotation
                }
            } else {
                // Fallback for any extra meshes - rotate around Y
                mesh.transform.rotation = Quat::from_axis_angle(Vec3::Y, self.time + i as f32);
            }
        }
        
        // Update camera based on keyboard input
        self.update_camera_from_input(dt);
        
        // Update UI system
        self.ui_system.update(dt, &self.camera, device);
    }

    pub fn render(&mut self, device: &Device, queue: &Queue, view: &TextureView, depth_view: &TextureView) {
        // Reset frame stats for performance overlay
        self.perf_overlay.reset_frame_stats();
        
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
            
            // Update material uniforms if material exists
            if let Some(ref material) = mesh.material {
                material.update(queue);
            }
            
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
                
                // Use material pipeline if available, otherwise use default
                if let Some(ref material) = mesh.material {
                    if let Some(pipeline) = material.get_pipeline() {
                        render_pass.set_pipeline(pipeline);
                        render_pass.set_bind_group(0, &self.uniform_bind_group, &[]);
                        if let Some(bind_group) = material.get_bind_group() {
                            render_pass.set_bind_group(1, bind_group, &[]);
                        }
                    } else {
                        // Fallback to default pipeline if material is not initialized
                        render_pass.set_pipeline(&self.render_pipeline);
                        render_pass.set_bind_group(0, &self.uniform_bind_group, &[]);
                    }
                } else {
                    // Use default pipeline
                    render_pass.set_pipeline(&self.render_pipeline);
                    render_pass.set_bind_group(0, &self.uniform_bind_group, &[]);
                }
                
                render_pass.set_vertex_buffer(0, mesh.vertex_buffer.slice(..));
                render_pass.set_index_buffer(mesh.index_buffer.slice(..), IndexFormat::Uint16);
                
                // Track performance metrics
                self.perf_overlay.record_draw_call();
                self.perf_overlay.record_triangles(mesh.indices.len() as u32 / 3);
                
                render_pass.draw_indexed(0..mesh.indices.len() as u32, 0, 0..1);
            }
            
            queue.submit(std::iter::once(encoder.finish()));
        }
        
        // Render UI on top
        self.ui_system.render(device, queue, view, depth_view);
        
        // Render performance overlay on top of everything
        self.perf_overlay.render(device, queue, view);
    }
    
    pub fn resize(&mut self, new_width: u32, new_height: u32) {
        // Update aspect ratio
        self.aspect_ratio = new_width as f32 / new_height as f32;
        
        // Update performance overlay with new screen size
        self.perf_overlay.resize(new_width, new_height);
        
        // Update camera projection matrix with new aspect ratio
        self.camera.set_aspect_ratio(self.aspect_ratio);
        
        // If we have a scene with camera data, recreate the camera with the proper FOV and new aspect
        if let Some(ref camera_data) = self.scene_data.camera {
            self.camera = Camera::perspective(
                self.camera.position,  // Keep current position (may be runtime-modified)
                self.camera_target,     // Keep current target
                Vec3::Y,
                camera_data.fov,        // Use FOV from DSL
                self.aspect_ratio,      // New aspect ratio
                0.1,
                100.0,
            );
        }
        
        // Update UI system with new dimensions
        self.ui_system.resize(new_width, new_height);
    }
    
    pub fn handle_keyboard_input(&mut self, event: &winit::event::KeyEvent, _device: &Device) {
        use winit::keyboard::{KeyCode, PhysicalKey};
        
        // Handle special preservation toggle key
        if matches!(event.physical_key, PhysicalKey::Code(KeyCode::KeyP)) 
            && matches!(event.state, winit::event::ElementState::Pressed) {
            self.runtime_state.toggle_preservation_mode();
            self.ui_system.add_log_entry(&format!("📌 Runtime preservation mode: {:?}", 
                self.runtime_state.authoring_mode));
            return;
        }
        
        // Toggle performance overlay with F1
        if matches!(event.physical_key, PhysicalKey::Code(KeyCode::F1)) 
            && matches!(event.state, winit::event::ElementState::Pressed) {
            self.perf_overlay.toggle();
            let status = if self.perf_overlay.is_enabled() { "ON" } else { "OFF" };
            self.ui_system.add_log_entry(&format!("📊 Performance overlay: {}", status));
            return;
        }
        
        // Convert key event to string for matching with DSL configuration
        let key_str = match event.physical_key {
            PhysicalKey::Code(KeyCode::KeyW) => "W",
            PhysicalKey::Code(KeyCode::KeyS) => "S",
            PhysicalKey::Code(KeyCode::KeyA) => "A",
            PhysicalKey::Code(KeyCode::KeyD) => "D",
            PhysicalKey::Code(KeyCode::Space) => "Space",
            PhysicalKey::Code(KeyCode::ShiftLeft) | PhysicalKey::Code(KeyCode::ShiftRight) => "Shift",
            PhysicalKey::Code(KeyCode::ArrowUp) => "Up",
            PhysicalKey::Code(KeyCode::ArrowDown) => "Down",
            PhysicalKey::Code(KeyCode::ArrowLeft) => "Left",
            PhysicalKey::Code(KeyCode::ArrowRight) => "Right",
            _ => return,
        };
        
        match event.state {
            winit::event::ElementState::Pressed => {
                self.keyboard_state.keys_pressed.insert(key_str.to_string());
            }
            winit::event::ElementState::Released => {
                self.keyboard_state.keys_pressed.remove(key_str);
            }
        }
    }
    
    pub fn handle_mouse_button(&mut self, button: winit::event::MouseButton, state: winit::event::ElementState) {
        use winit::event::{MouseButton, ElementState};
        
        if button == MouseButton::Left {
            match state {
                ElementState::Pressed => {
                    self.mouse_state.is_dragging = true;
                }
                ElementState::Released => {
                    self.mouse_state.is_dragging = false;
                    self.mouse_state.last_position = None;
                }
            }
        }
    }
    
    pub fn handle_mouse_motion(&mut self, position: (f32, f32)) {
        if self.mouse_state.is_dragging {
            if let Some(last_pos) = self.mouse_state.last_position {
                self.mouse_state.delta = (
                    position.0 - last_pos.0,
                    position.1 - last_pos.1,
                );
            }
            self.mouse_state.last_position = Some(position);
        } else {
            self.mouse_state.delta = (0.0, 0.0);
        }
    }
    
    pub fn handle_mouse_wheel(&mut self, delta: f32) {
        self.mouse_state.scroll_delta = delta;
    }
    
    pub fn update_camera_from_input(&mut self, dt: f32) {
        if let Some(input_data) = &self.scene_data.input {
            if let Some(controls) = &input_data.camera_controls {
                // Handle orbit controls with mouse
                if let Some(orbit) = &controls.orbit_controls {
                    if orbit.enabled && self.mouse_state.is_dragging {
                        // Update spherical coordinates based on mouse delta
                        let sensitivity = orbit.sensitivity * 0.01;
                        self.orbit_state.spherical_coords.1 -= self.mouse_state.delta.0 * sensitivity;
                        self.orbit_state.spherical_coords.2 += self.mouse_state.delta.1 * sensitivity;
                        
                        // Clamp phi (vertical angle)
                        self.orbit_state.spherical_coords.2 = self.orbit_state.spherical_coords.2
                            .max(orbit.min_polar_angle)
                            .min(orbit.max_polar_angle);
                        
                        // Clear delta after using it
                        self.mouse_state.delta = (0.0, 0.0);
                    }
                    
                    // Handle zoom with mouse wheel
                    if orbit.enable_zoom && self.mouse_state.scroll_delta != 0.0 {
                        let zoom_factor = 1.0 - self.mouse_state.scroll_delta * orbit.zoom_speed * 0.1;
                        self.orbit_state.spherical_coords.0 *= zoom_factor;
                        self.orbit_state.spherical_coords.0 = self.orbit_state.spherical_coords.0
                            .max(orbit.min_distance)
                            .min(orbit.max_distance);
                        self.mouse_state.scroll_delta = 0.0;
                    }
                    
                    // Convert spherical to Cartesian
                    let (r, theta, phi) = self.orbit_state.spherical_coords;
                    let x = r * phi.sin() * theta.cos();
                    let y = r * phi.cos();
                    let z = r * phi.sin() * theta.sin();
                    self.camera.position = self.camera_target + Vec3::new(x, y, z);
                }
                
                let move_speed = controls.move_speed * dt;
                let rotate_speed = controls.rotate_speed * dt;
                
                // Get camera forward and right vectors
                let forward = (self.camera_target - self.camera.position).normalize();
                let right = forward.cross(Vec3::Y).normalize();
                let up = Vec3::Y;
                
                // Handle movement
                if self.keyboard_state.keys_pressed.contains(&controls.movement_keys.forward) {
                    self.camera.position = self.camera.position + forward * move_speed;
                    self.camera_target = self.camera_target + forward * move_speed;
                }
                if self.keyboard_state.keys_pressed.contains(&controls.movement_keys.backward) {
                    self.camera.position = self.camera.position - forward * move_speed;
                    self.camera_target = self.camera_target - forward * move_speed;
                }
                if self.keyboard_state.keys_pressed.contains(&controls.movement_keys.left) {
                    self.camera.position = self.camera.position - right * move_speed;
                    self.camera_target = self.camera_target - right * move_speed;
                }
                if self.keyboard_state.keys_pressed.contains(&controls.movement_keys.right) {
                    self.camera.position = self.camera.position + right * move_speed;
                    self.camera_target = self.camera_target + right * move_speed;
                }
                if self.keyboard_state.keys_pressed.contains(&controls.movement_keys.up) {
                    self.camera.position = self.camera.position + up * move_speed;
                    self.camera_target = self.camera_target + up * move_speed;
                }
                if self.keyboard_state.keys_pressed.contains(&controls.movement_keys.down) {
                    self.camera.position = self.camera.position - up * move_speed;
                    self.camera_target = self.camera_target - up * move_speed;
                }
                
                // Handle rotation (orbit around target)
                if self.keyboard_state.keys_pressed.contains(&controls.rotation_keys.yaw_left) {
                    let offset = self.camera.position - self.camera_target;
                    let angle = rotate_speed;
                    let cos = angle.cos();
                    let sin = angle.sin();
                    let new_x = offset.x * cos - offset.z * sin;
                    let new_z = offset.x * sin + offset.z * cos;
                    self.camera.position = self.camera_target + Vec3::new(new_x, offset.y, new_z);
                }
                if self.keyboard_state.keys_pressed.contains(&controls.rotation_keys.yaw_right) {
                    let offset = self.camera.position - self.camera_target;
                    let angle = -rotate_speed;
                    let cos = angle.cos();
                    let sin = angle.sin();
                    let new_x = offset.x * cos - offset.z * sin;
                    let new_z = offset.x * sin + offset.z * cos;
                    self.camera.position = self.camera_target + Vec3::new(new_x, offset.y, new_z);
                }
                if self.keyboard_state.keys_pressed.contains(&controls.rotation_keys.pitch_up) {
                    let offset = self.camera.position - self.camera_target;
                    // Simple pitch by adjusting y position
                    let distance = offset.length();
                    self.camera.position.y = self.camera.position.y + rotate_speed * distance * 0.5;
                }
                if self.keyboard_state.keys_pressed.contains(&controls.rotation_keys.pitch_down) {
                    let offset = self.camera.position - self.camera_target;
                    // Simple pitch by adjusting y position
                    let distance = offset.length();
                    self.camera.position.y = self.camera.position.y - rotate_speed * distance * 0.5;
                }
                
                // Update the camera view matrix after movement
                self.camera = Camera::perspective(
                    self.camera.position,
                    self.camera_target,
                    Vec3::Y,
                    std::f32::consts::FRAC_PI_4, // TODO: Store FOV properly
                    self.aspect_ratio,
                    0.1,
                    100.0,
                );
                
                // Track camera state for preservation and sync
                if self.runtime_state.should_preserve_camera() {
                    self.runtime_state.preserve_camera(
                        self.camera.position, 
                        self.camera_target,
                        std::f32::consts::FRAC_PI_4  // TODO: Store FOV properly
                    );
                    
                    // Queue for code sync if in Live mode
                    if self.runtime_state.authoring_mode == crate::runtime_state::AuthoringMode::Live {
                        if let Some(camera_state) = &self.runtime_state.camera_overrides {
                            self.code_sync.queue_camera_update(camera_state);
                        }
                    }
                }
            }
        }
    }
}
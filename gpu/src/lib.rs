//! GPU integration via wgpu/WebGPU

pub mod device;
pub mod shader;
pub mod cache;
pub mod render;
pub mod compute;
pub mod math;
pub mod render3d;
pub mod ui3d;
pub mod entity;
pub mod mesh_gen;
pub mod model_loader;
pub mod reconciliation;
pub mod scene;
pub mod gizmo;
pub mod runtime_state;
pub mod behavior_system;
pub mod code_sync;
pub mod perf_monitor;
pub mod perf_monitor_simple;
pub mod perf_overlay;
pub mod perf_overlay_live;
pub mod perf_text;
pub mod debug_config;
pub mod materials;
pub mod instanced_renderer;
pub mod frustum;
pub mod benchmark;
pub mod gpu_driven_renderer;
pub mod gpu_memory_pool;

pub use device::*;
pub use shader::*;
pub use cache::*;
pub use render::*;
pub use compute::*;
pub use math::*;
pub use render3d::*;
pub use ui3d::*;
pub use scene::*;
pub use gizmo::*;

use wgpu::*;

#[derive(Debug)]
pub struct GpuCtx<'window> {
    pub device: Device,
    pub queue: Queue,
    pub surface: Surface<'window>,
    pub config: SurfaceConfiguration,
}

pub async fn init<'window>(window: &'window winit::window::Window) -> anyhow::Result<GpuCtx<'window>> {
    let instance = Instance::new(InstanceDescriptor {
        backends: Backends::PRIMARY,
        flags: InstanceFlags::default(),
        dx12_shader_compiler: Dx12Compiler::default(),
        gles_minor_version: Gles3MinorVersion::default(),
    });
    let surface = instance.create_surface(window)?;
    
    let adapter = instance
        .request_adapter(&RequestAdapterOptions {
            power_preference: PowerPreference::HighPerformance,
            compatible_surface: Some(&surface),
            force_fallback_adapter: false,
        })
        .await
        .ok_or_else(|| anyhow::anyhow!("Failed to find adapter"))?;
    
    let (device, queue) = adapter
        .request_device(
            &DeviceDescriptor {
                label: Some("XR-DSL Device"),
                required_features: Features::empty(),
                required_limits: Limits::default(),
            },
            None,
        )
        .await?;
    
    let size = window.inner_size();
    let caps = surface.get_capabilities(&adapter);
    let format = caps.formats[0];
    
    let config = SurfaceConfiguration {
        usage: TextureUsages::RENDER_ATTACHMENT,
        format,
        width: size.width,
        height: size.height,
        present_mode: caps.present_modes[0],
        alpha_mode: caps.alpha_modes[0],
        view_formats: vec![],
        desired_maximum_frame_latency: 2,
    };
    
    surface.configure(&device, &config);
    
    Ok(GpuCtx {
        device,
        queue,
        surface,
        config,
    })
}
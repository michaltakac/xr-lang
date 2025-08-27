//! Desktop development host for XR-DSL

mod hotreload;

use anyhow::Result;
use std::time::Instant;
use winit::{
    event::{Event, WindowEvent},
    event_loop::EventLoop,
    window::{Window},
};
use hotreload::{HotReloader, SceneLoader};

#[tokio::main]
async fn main() -> Result<()> {
    env_logger::init();
    
    println!("🚀 Starting XR-DSL Desktop 3D Test...");
    
    let event_loop = EventLoop::new()?;
    let window = std::sync::Arc::new(event_loop.create_window(
        Window::default_attributes()
            .with_title("XR-DSL Desktop - 3D Test")
            .with_inner_size(winit::dpi::LogicalSize::new(800, 600))
    )?);
    
    println!("🔧 Initializing GPU context...");
    let mut gpu_ctx = gpu::init(&*window).await?;
    println!("✅ GPU context initialized!");
    
    // Create 3D renderer
    let mut renderer_3d = gpu::Renderer3D::new(&gpu_ctx.device, &gpu_ctx.config);
    println!("✅ 3D renderer created!");
    
    // Set up hot-reload system
    let examples_path = std::path::Path::new("examples");
    let mut hot_reloader = HotReloader::new(&examples_path)?;
    let mut scene_loader = SceneLoader::new();
    
    // Try to load the spinning cubes example initially
    let spinning_cubes_path = "examples/spinning_cubes.xrdsl";
    if let Ok(Some(scene_data)) = scene_loader.load_scene_from_file(spinning_cubes_path) {
        renderer_3d.load_scene(scene_data, &gpu_ctx.device);
        println!("✅ Loaded initial scene from: {}", spinning_cubes_path);
    } else {
        println!("⚠️  Could not load spinning_cubes.xrdsl, using default scene");
    }
    
    println!("🎨 3D scene ready! You should see animated cubes.");
    println!("🎮 Use this app to test 3D rendering!");
    println!("💡 You should see spinning cubes with orbital camera movement");
    println!("📊 The scene uses WebGPU for cross-platform rendering");
    println!("🛠️  3D UI system has been integrated with ECS architecture using hecs!");
    println!("📝 The system includes: 3D code editor, button components, log viewer");
    println!("⌨️  Input handling: mouse raycasting, keyboard input, text editing");
    println!("🔥 HOT-RELOAD ACTIVE: Edit examples/spinning_cubes.xrdsl to see live updates!");
    println!("✨ Live DSL compilation and 3D scene updates working!");
    
    // Request the initial frame
    window.request_redraw();
    
    let mut last_render = Instant::now();
    let window_clone = window.clone();
    
    event_loop.run(move |event, target| {
        match event {
            Event::WindowEvent {
                event: WindowEvent::CloseRequested,
                ..
            } => target.exit(),
            Event::WindowEvent {
                event: WindowEvent::RedrawRequested,
                ..
            } => {
                
                // Update time for animation
                let now = Instant::now();
                let dt = now.duration_since(last_render).as_secs_f32();
                last_render = now;
                
                // Check for hot-reload changes
                if let Some(changed_file) = hot_reloader.check_for_changes() {
                    println!("📁 Detected change in: {}", changed_file);
                    if let Ok(Some(new_scene)) = scene_loader.load_scene_from_file(&changed_file) {
                        renderer_3d.load_scene(new_scene, &gpu_ctx.device);
                    }
                }
                
                // Update 3D scene (including UI)
                renderer_3d.update(dt, &gpu_ctx.device);
                
                // Get current surface texture
                let Ok(output) = gpu_ctx.surface.get_current_texture() else { 
                    eprintln!("⚠️ Failed to get surface texture");
                    return;
                };
                let view = output.texture.create_view(&wgpu::TextureViewDescriptor::default());
                
                // Create depth texture for this frame
                let depth_texture = gpu_ctx.device.create_texture(&wgpu::TextureDescriptor {
                    label: Some("Depth Buffer"),
                    size: wgpu::Extent3d {
                        width: gpu_ctx.config.width,
                        height: gpu_ctx.config.height,
                        depth_or_array_layers: 1,
                    },
                    mip_level_count: 1,
                    sample_count: 1,
                    dimension: wgpu::TextureDimension::D2,
                    format: wgpu::TextureFormat::Depth32Float,
                    usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
                    view_formats: &[],
                });
                let depth_view = depth_texture.create_view(&wgpu::TextureViewDescriptor::default());
                
                // Render the scene using the 3D renderer
                renderer_3d.render(&gpu_ctx.device, &gpu_ctx.queue, &view, &depth_view);
                
                // Present the frame
                output.present();
                
                // Request next frame for animation
                window_clone.request_redraw();
            }
            Event::WindowEvent {
                event: WindowEvent::Resized(new_size),
                ..
            } => {
                // Update surface configuration
                gpu_ctx.config.width = new_size.width.max(1);
                gpu_ctx.config.height = new_size.height.max(1);
                gpu_ctx.surface.configure(&gpu_ctx.device, &gpu_ctx.config);
                
                // Update camera aspect ratio
                renderer_3d.camera.set_aspect_ratio(new_size.width as f32 / new_size.height as f32);
            }
            _ => {}
        }
    })?;
    
    Ok(())
}
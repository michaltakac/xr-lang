//! Desktop development host for XR-DSL

mod hotreload;

use anyhow::Result;
use std::time::Instant;
use winit::{
    event::{Event, WindowEvent, KeyEvent, ElementState, MouseButton, MouseScrollDelta},
    event_loop::EventLoop,
    window::{Window},
    keyboard::{KeyCode, PhysicalKey},
    dpi::PhysicalPosition,
};
use hotreload::{HotReloader, SceneLoader, SceneUpdate};

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
    let mut renderer_3d = gpu::Renderer3D::new(&gpu_ctx.device, &gpu_ctx.queue, &gpu_ctx.config);
    println!("✅ 3D renderer created!");
    
    // Load scene from command-line argument or default to spinning_cubes.xrdsl
    let args: Vec<String> = std::env::args().collect();
    let initial_scene = if args.len() > 1 {
        // Use provided scene file - convert to absolute path if relative
        let path = std::path::PathBuf::from(&args[1]);
        if path.is_absolute() {
            path
        } else {
            std::env::current_dir().unwrap_or_else(|_| std::path::PathBuf::from(".")).join(path)
        }
    } else {
        // Default to examples directory
        let examples_path = if std::path::Path::new("examples").exists() {
            std::path::Path::new("examples")
        } else if std::path::Path::new("../../examples").exists() {
            std::path::Path::new("../../examples")
        } else {
            std::path::Path::new(".")
        };
        examples_path.join("spinning_cubes.xrdsl")
    };
    
    // Set up hot-reload system to watch the directory containing the target file
    let watch_path = if let Some(parent) = initial_scene.parent() {
        parent
    } else {
        std::path::Path::new(".")
    };
    let mut hot_reloader = HotReloader::new(&watch_path)?;
    let mut scene_loader = SceneLoader::new();
    
    if initial_scene.exists() {
        if let Ok(Some(update)) = scene_loader.load_scene_from_file(initial_scene.to_str().unwrap()) {
            // Set the source path for code sync
            renderer_3d.set_source_path(&initial_scene);
            
            match update {
                SceneUpdate::Full(scene_data) => {
                    renderer_3d.load_scene(scene_data, &gpu_ctx.device);
                }
                SceneUpdate::Incremental { scene_data, .. } => {
                    // For initial load, treat as full update
                    renderer_3d.load_scene(scene_data, &gpu_ctx.device);
                }
            }
            println!("✅ Loaded initial scene from: {}", initial_scene.display());
        } else {
            println!("⚠️  Could not parse {}, using default scene", initial_scene.display());
        }
    } else {
        println!("⚠️  Scene file {} not found, using default scene", initial_scene.display());
    }
    
    println!("🎨 3D scene ready! You should see animated cubes.");
    println!("🎮 Use this app to test 3D rendering!");
    println!("💡 You should see spinning cubes with orbital camera movement");
    println!("📊 The scene uses WebGPU for cross-platform rendering");
    println!("🛠️  3D UI system has been integrated with ECS architecture using hecs!");
    println!("📝 The system includes: 3D code editor, button components, log viewer");
    println!("⌨️  Input handling: mouse raycasting, keyboard input, text editing");
    println!("🔥 HOT-RELOAD ACTIVE: Edit any .xrdsl file in examples/ to see live updates!");
    println!("✨ Live DSL compilation and 3D scene updates working!");
    println!("📌 TIP: Run with a scene file: cargo run -p desktop -- examples/rotation_test.xrdsl");
    println!("");
    println!("🆕 RUNTIME STATE PRESERVATION:");
    println!("   Press [P] to toggle preservation modes: Design -> Play -> Live");
    println!("   • Design: All changes reset on reload (default)");
    println!("   • Play: Runtime changes preserved, not saved");
    println!("   • Live: Runtime changes sync to code (future feature)");
    println!("   Try: cargo run -p desktop -- examples/preserve_test.xrdsl");
    
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
                event: WindowEvent::KeyboardInput { event, .. },
                ..
            } => {
                renderer_3d.handle_keyboard_input(&event, &gpu_ctx.device);
            }
            Event::WindowEvent {
                event: WindowEvent::MouseInput { button, state, .. },
                ..
            } => {
                renderer_3d.handle_mouse_button(button, state);
            }
            Event::WindowEvent {
                event: WindowEvent::CursorMoved { position, .. },
                ..
            } => {
                let PhysicalPosition { x, y } = position;
                renderer_3d.handle_mouse_motion((x as f32, y as f32));
            }
            Event::WindowEvent {
                event: WindowEvent::MouseWheel { delta, .. },
                ..
            } => {
                let scroll_delta = match delta {
                    MouseScrollDelta::LineDelta(_, y) => y,
                    MouseScrollDelta::PixelDelta(PhysicalPosition { y, .. }) => y as f32 * 0.01,
                };
                renderer_3d.handle_mouse_wheel(scroll_delta);
            }
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
                    if let Ok(Some(update)) = scene_loader.load_scene_from_file(&changed_file) {
                        match update {
                            SceneUpdate::Full(scene_data) => {
                                println!("🔄 Full scene reload");
                                renderer_3d.load_scene(scene_data, &gpu_ctx.device);
                            }
                            SceneUpdate::Incremental { scene_data, changes } => {
                                println!("⚡ Incremental update with {} changes", changes.len());
                                // Apply incremental changes
                                renderer_3d.apply_scene_changes(scene_data, changes, &gpu_ctx.device);
                            }
                        }
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
                
                // Update renderer with new dimensions
                renderer_3d.resize(new_size.width, new_size.height);
            }
            _ => {}
        }
    })?;
    
    Ok(())
}
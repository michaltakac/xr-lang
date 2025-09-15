//! Quest 3 XR application host with hot-reload support

mod vulkan_integration;
mod scene_renderer;
mod hot_reload;

// Use ndk-glue for Android entry point
#[cfg(target_os = "android")]
#[ndk_glue::main(backtrace = "on")]
fn android_main() {
    android::android_main();
}

// Android implementation
#[cfg(target_os = "android")]
mod android {
    use anyhow::{Context, Result};
    use openxr as xr;
    use std::sync::Arc;
    use std::time::Duration;
    use std::collections::HashMap;
    
    use vm::{
        capability::{CapabilityTable, PlatformInfo, DeviceType, DisplayInfo, InputCapability},
        image::{XRLangImage, Platform},
        VM,
    };

    // Internal android_main function called from ndk-glue
    pub fn android_main() {
        // Simple initialization to test if the function is even called
        use std::io::Write;
        
        // Try to write to Android log directly
        let _ = std::panic::catch_unwind(|| {
            android_logger::init_once(
                android_logger::Config::default()
                    .with_tag("XR-Lang")
                    .with_max_level(log::LevelFilter::Trace),
            );
        });
        
        // Use Android's __android_log_print directly for debugging
        extern "C" {
            fn __android_log_print(prio: i32, tag: *const i8, fmt: *const i8, ...) -> i32;
        }
        
        unsafe {
            let tag = b"XR-Lang\0".as_ptr() as *const i8;
            let msg = b"=== XR-Lang android_main ENTRY POINT REACHED ===\0".as_ptr() as *const i8;
            __android_log_print(4, tag, msg); // 4 = INFO level
        }
        
        log::error!("XR-Lang: android_main called!");
        log::info!("XR-Lang: Starting Quest 3 app");
        
        // Try to run the main function
        match quest_main() {
            Ok(_) => {
                log::info!("XR-Lang: quest_main completed successfully");
            }
            Err(e) => {
                log::error!("XR-Lang: quest_main failed: {}", e);
                unsafe {
                    let tag = b"XR-Lang\0".as_ptr() as *const i8;
                    let error_msg = format!("ERROR: {}\0", e);
                    __android_log_print(6, tag, error_msg.as_ptr() as *const i8);
                }
            }
        }
        
        unsafe {
            let tag = b"XR-Lang\0".as_ptr() as *const i8;
            let msg = b"=== XR-Lang android_main EXITING ===\0".as_ptr() as *const i8;
            __android_log_print(4, tag, msg);
        }
    }

    use crate::vulkan_integration::{VulkanContext, XrSwapchain, StereoRenderer};
    use crate::hot_reload::{HotReloadServer, apply_hot_reload};
    
    fn quest_main() -> Result<()> {
        log::info!("quest_main: Initializing OpenXR...");

        // Initialize OpenXR
        log::debug!("Loading OpenXR entry...");
        let entry = unsafe { 
            xr::Entry::load()
                .context("Failed to load OpenXR runtime - is the Quest OpenXR loader available?")?
        };
        log::info!("OpenXR entry loaded successfully");
        let available_extensions = entry.enumerate_extensions()?;
        log::info!("Available OpenXR extensions: {:?}", available_extensions);

        // Create OpenXR instance with Android loader
        let mut enabled_extensions = xr::ExtensionSet::default();
        enabled_extensions.khr_android_create_instance = true;
        enabled_extensions.khr_vulkan_enable2 = true;
        
        // Only enable extensions if available
        if available_extensions.fb_passthrough {
            enabled_extensions.fb_passthrough = true;
            log::info!("Enabling FB passthrough extension");
        }
        if available_extensions.fb_hand_tracking_mesh {
            enabled_extensions.fb_hand_tracking_mesh = true;
            log::info!("Enabling FB hand tracking extension");
        }
        
        log::info!("Creating OpenXR instance...");
        let xr_instance = entry.create_instance(
            &xr::ApplicationInfo {
                application_name: "XR-Lang",
                application_version: 1,
                engine_name: "XR-Lang VM",
                engine_version: 1,
            },
            &enabled_extensions,
            &[],
        ).context("Failed to create OpenXR instance")?;
        log::info!("OpenXR instance created successfully");

        // Get system for Quest 3
        log::info!("Getting HMD system...");
        let system = xr_instance
            .system(xr::FormFactor::HEAD_MOUNTED_DISPLAY)
            .context("Failed to get HMD system - is the app running in VR mode?")?;
        log::info!("Got HMD system successfully");

        // Create Vulkan context that can be shared with OpenXR
        log::info!("Creating shared Vulkan context...");
        let vulkan_context = Arc::new(VulkanContext::new_for_openxr(&xr_instance, system)?);
        
        // Create OpenXR session with Vulkan
        let session_info = vulkan_context.create_openxr_session_info();
        let (session, mut frame_wait, mut frame_stream) = unsafe {
            xr_instance.create_session::<xr::Vulkan>(
                system,
                &session_info,
            )?
        };
        
        log::info!("OpenXR session created successfully!");

        // Create reference space
        let reference_space = session
            .create_reference_space(xr::ReferenceSpaceType::STAGE, xr::Posef::IDENTITY)
            .or_else(|_| {
                session.create_reference_space(xr::ReferenceSpaceType::LOCAL, xr::Posef::IDENTITY)
            })?;

        // Get view configuration for stereo rendering
        let view_config_type = xr::ViewConfigurationType::PRIMARY_STEREO;
        let views = xr_instance.enumerate_view_configuration_views(system, view_config_type)?;
        
        // Get recommended resolution for each eye
        let resolution = (
            views[0].recommended_image_rect_width,
            views[0].recommended_image_rect_height,
        );
        log::info!("Swapchain resolution per eye: {:?}", resolution);
        
        // Create stereo renderer with swapchains
        let mut stereo_renderer = StereoRenderer::new(
            &session,
            Arc::clone(&vulkan_context),
            resolution,
        )?;

        // Set up hot-reload server
        let (hot_reload_server, hot_reload_receiver) = HotReloadServer::new(9090)?;

        // Load initial XR-Lang image
        let image_data = load_initial_image()?;
        let image = if !image_data.is_empty() {
            XRLangImage::deserialize(&image_data)
                .map_err(|e| anyhow::anyhow!("Failed to deserialize image: {}", e))?
        } else {
            create_test_image()
        };

        // Create capability table for Quest 3
        let capabilities = create_quest_capabilities();

        // Initialize VM with Quest capabilities
        let mut vm = VM::with_capabilities(std::rc::Rc::new(capabilities));
        
        // Boot VM with initial image if available
        if !image_data.is_empty() && !image.bytecode.is_empty() {
            // TODO: Execute bytecode when VM supports it
            log::info!("Would boot VM with {} bytes of bytecode", image.bytecode.len());
        }

        // Main render loop
        let mut event_storage = xr::EventDataBuffer::new();
        let mut session_running = false;
        
        log::info!("Starting OpenXR render loop");
        
        'main: loop {
            // Poll OpenXR events
            while let Some(event) = xr_instance.poll_event(&mut event_storage)? {
                match event {
                    xr::Event::SessionStateChanged(e) => {
                        log::info!("Session state changed to {:?}", e.state());
                        match e.state() {
                            xr::SessionState::READY => {
                                session.begin(view_config_type)?;
                                session_running = true;
                            }
                            xr::SessionState::STOPPING => {
                                session.end()?;
                                session_running = false;
                            }
                            xr::SessionState::EXITING | xr::SessionState::LOSS_PENDING => {
                                break 'main;
                            }
                            _ => {}
                        }
                    }
                    xr::Event::InstanceLossPending(_) => {
                        log::info!("Instance loss pending");
                        break 'main;
                    }
                    _ => {}
                }
            }

            if !session_running {
                std::thread::sleep(Duration::from_millis(100));
                continue;
            }

            // Poll hot-reload server for connections
            let _ = hot_reload_server.poll();
            
            // Check for hot-reload updates
            if let Ok(new_image_data) = hot_reload_receiver.try_recv() {
                log::info!("Applying hot-reload update");
                if let Err(e) = apply_hot_reload(&mut vm, &new_image_data) {
                    log::error!("Hot-reload failed: {}", e);
                }
            }

            // Wait for next frame
            let frame_state = frame_wait.wait()?;
            frame_stream.begin()?;

            if !frame_state.should_render {
                frame_stream.end(
                    frame_state.predicted_display_time,
                    xr::EnvironmentBlendMode::OPAQUE,
                    &[],
                )?;
                continue;
            }

            // Locate views for both eyes
            let (_, views) = session.locate_views(
                view_config_type,
                frame_state.predicted_display_time,
                &reference_space,
            )?;

            // Tick VM with frame time
            let _dt = 1.0 / 90.0; // Quest 3 runs at 90Hz
            // TODO: Update VM simulation when API supports it
            // vm.update(dt);

            // Render to both eyes (acquires, renders, but doesn't release yet)
            stereo_renderer.acquire_and_render(&views)?;
            
            // Build projection views for submission
            let projection_views = stereo_renderer.build_projection_views(&views);
            
            // Submit rendered frame to OpenXR
            let projection = xr::CompositionLayerProjection::new()
                .space(&reference_space)
                .views(&projection_views);
                
            frame_stream.end(
                frame_state.predicted_display_time,
                xr::EnvironmentBlendMode::OPAQUE,
                &[&projection],
            )?;
            
            // Release swapchain images after submission
            stereo_renderer.release_images()?;
        }

        Ok(())
    }


    fn load_initial_image() -> Result<Vec<u8>> {
        // Try to load from APK assets first
        // TODO: Implement APK asset loading
        // For now, return a minimal test image
        Ok(vec![])
    }

    fn create_test_image() -> XRLangImage {
        XRLangImage {
            version: "1.0".to_string(),
            platform: Platform::MetaQuest,
            created_at: chrono::Utc::now().timestamp() as u64,
            globals: std::collections::HashMap::new(),
            metadata: vm::image::ImageMetadata {
                name: "quest-demo".to_string(),
                author: "XR-Lang".to_string(),
                description: "Quest 3 demo app".to_string(),
                xr_lang_version: "1.0".to_string(),
                dependencies: vec![],
                entry_point: "main".to_string(),
                checksum: "".to_string(),
            },
            capabilities_required: vec![],
            snapshot_data: None,
            bytecode: vec![], // Add empty bytecode
        }
    }
    
    fn create_quest_capabilities() -> CapabilityTable {
        // Create platform info for Quest 3
        let platform_info = PlatformInfo {
            name: "Meta Quest 3".to_string(),
            version: "1.0".to_string(),
            vendor: "Meta".to_string(),
            device_type: DeviceType::HeadMountedDisplay,
            display_info: DisplayInfo {
                resolution: (2064 * 2, 2208), // Combined resolution for both eyes
                refresh_rate: 90.0,
                fov: Some(110.0),
                ipd: Some(63.0), // Average IPD in mm
            },
            input_capabilities: vec![
                InputCapability::Controllers,
                InputCapability::HandTracking,
                InputCapability::VoiceInput,
            ],
        };
        
        // Create capability table with empty capabilities for now
        CapabilityTable::new(platform_info)
    }
}

// Non-Android stub to allow workspace builds/tests on desktop
#[cfg(not(target_os = "android"))]
mod stub {
    #[no_mangle]
    pub extern "C" fn android_main_stub() {}
}

//! Vulkan integration for OpenXR and wgpu sharing

use anyhow::{Context, Result};
use ash::{vk, Entry, Instance, Device};
use vk::Handle;
use openxr as xr;
use std::ffi::{CStr, CString};
use std::sync::Arc;
use std::mem::ManuallyDrop;

/// Creates a Vulkan instance that can be shared between OpenXR and wgpu
pub struct VulkanContext {
    pub entry: Entry,
    pub instance: Instance,
    pub physical_device: vk::PhysicalDevice,
    pub device: ash::Device,
    pub queue_family_index: u32,
    pub queue: vk::Queue,
    pub command_pool: vk::CommandPool,
    pub render_pass: vk::RenderPass,
    pub pipeline_layout: vk::PipelineLayout,
    pub pipeline: vk::Pipeline,
}

impl VulkanContext {
    /// Create a new Vulkan context for OpenXR
    pub fn new_for_openxr(
        xr_instance: &xr::Instance,
        system: xr::SystemId,
    ) -> Result<Self> {
        // Load Vulkan entry point
        let entry = unsafe { Entry::load()? };
        
        // Get OpenXR's Vulkan requirements
        let vk_requirements = xr_instance
            .graphics_requirements::<xr::Vulkan>(system)?;
        
        log::info!("OpenXR Vulkan requirements: {:?}", vk_requirements);
        
        // Application info
        let app_name = CString::new("XR-Lang")?;
        let engine_name = CString::new("XR-Lang Engine")?;
        let app_info = vk::ApplicationInfo::default()
            .application_name(app_name.as_c_str())
            .application_version(vk::make_api_version(0, 1, 0, 0))
            .engine_name(engine_name.as_c_str())
            .engine_version(vk::make_api_version(0, 1, 0, 0))
            .api_version(vk::make_api_version(1, 2, 0, 0)); // Use Vulkan 1.2
        
        // Get required extensions from OpenXR
        // Note: openxr 0.18 doesn't have vulkan_instance_extensions method
        // We need to manually specify the required extensions
        let mut required_extensions = vec![
            CString::new("VK_KHR_surface")?,
            CString::new("VK_KHR_android_surface")?,
            CString::new("VK_KHR_get_physical_device_properties2")?,
        ];
        
        // Add any platform-specific extensions if needed
        #[cfg(target_os = "android")]
        required_extensions.push(CString::new("VK_KHR_android_surface")?);
        
        let extension_pointers: Vec<*const std::ffi::c_char> = required_extensions
            .iter()
            .map(|s| s.as_ptr())
            .collect();
        
        // Validation layers for debug builds
        #[cfg(debug_assertions)]
        let layer_names = vec![CString::new("VK_LAYER_KHRONOS_validation")?];
        #[cfg(not(debug_assertions))]
        let layer_names = vec![];
        
        let layer_pointers: Vec<*const std::ffi::c_char> = layer_names
            .iter()
            .map(|s| s.as_ptr())
            .collect();
        
        // Create Vulkan instance
        let create_info = vk::InstanceCreateInfo::default()
            .application_info(&app_info)
            .enabled_extension_names(&extension_pointers)
            .enabled_layer_names(&layer_pointers);
        
        let instance = unsafe { 
            entry.create_instance(&create_info, None)?
        };
        
        // Get physical device from OpenXR
        let physical_device = unsafe {
            // OpenXR expects a raw pointer to the Vulkan instance
            let vk_instance_raw = instance.handle().as_raw() as *mut _;
            let physical_device_raw = xr_instance
                .vulkan_graphics_device(system, vk_instance_raw)?;
            // Cast the raw handle back to a PhysicalDevice
            std::mem::transmute::<u64, vk::PhysicalDevice>(physical_device_raw as u64)
        };
        
        // Find queue family for graphics
        let queue_family_props = unsafe {
            instance.get_physical_device_queue_family_properties(physical_device)
        };
        
        let queue_family_index = queue_family_props
            .iter()
            .enumerate()
            .find(|(_, props)| props.queue_flags.contains(vk::QueueFlags::GRAPHICS))
            .map(|(i, _)| i as u32)
            .context("No graphics queue family found")?;
        
        // Get required device extensions
        // Note: openxr 0.18 doesn't have vulkan_device_extensions method
        // We need to manually specify the required extensions
        let device_extensions = vec![
            CString::new("VK_KHR_swapchain")?,
        ];
        
        let device_extension_pointers: Vec<*const std::ffi::c_char> = device_extensions
            .iter()
            .map(|s| s.as_ptr())
            .collect();
        
        // Create logical device
        let queue_priorities = [1.0];
        let queue_create_info = vk::DeviceQueueCreateInfo::default()
            .queue_family_index(queue_family_index)
            .queue_priorities(&queue_priorities);
        
        let queue_create_infos = [queue_create_info];
        let device_create_info = vk::DeviceCreateInfo::default()
            .queue_create_infos(&queue_create_infos)
            .enabled_extension_names(&device_extension_pointers);
        
        let device = unsafe {
            instance.create_device(physical_device, &device_create_info, None)?
        };
        
        // Get queue
        let queue = unsafe { device.get_device_queue(queue_family_index, 0) };
        
        // Create command pool
        let command_pool = unsafe {
            let pool_info = vk::CommandPoolCreateInfo::default()
                .flags(vk::CommandPoolCreateFlags::RESET_COMMAND_BUFFER)
                .queue_family_index(queue_family_index);
            device.create_command_pool(&pool_info, None)?
        };
        
        // Create render pass for VR rendering
        let render_pass = unsafe {
            let color_attachment = vk::AttachmentDescription::default()
                .format(vk::Format::R8G8B8A8_SRGB) // Will be set per swapchain
                .samples(vk::SampleCountFlags::TYPE_1)
                .load_op(vk::AttachmentLoadOp::CLEAR)
                .store_op(vk::AttachmentStoreOp::STORE)
                .initial_layout(vk::ImageLayout::UNDEFINED)
                .final_layout(vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL);
            
            let color_attachment_ref = vk::AttachmentReference::default()
                .attachment(0)
                .layout(vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL);
            
            let subpass = vk::SubpassDescription::default()
                .pipeline_bind_point(vk::PipelineBindPoint::GRAPHICS)
                .color_attachments(std::slice::from_ref(&color_attachment_ref));
            
            let render_pass_info = vk::RenderPassCreateInfo::default()
                .attachments(std::slice::from_ref(&color_attachment))
                .subpasses(std::slice::from_ref(&subpass));
            
            device.create_render_pass(&render_pass_info, None)?
        };
        
        // Create pipeline layout (empty for now)
        let pipeline_layout = unsafe {
            let layout_info = vk::PipelineLayoutCreateInfo::default();
            device.create_pipeline_layout(&layout_info, None)?
        };
        
        // Create a simple pipeline that clears to a color
        let pipeline = unsafe {
            create_test_pipeline(&device, render_pass, pipeline_layout)?
        };
        
        Ok(VulkanContext {
            entry,
            instance,
            physical_device,
            device,
            queue_family_index,
            queue,
            command_pool,
            render_pass,
            pipeline_layout,
            pipeline,
        })
    }
    
    /// Create OpenXR session info for this Vulkan context
    pub fn create_openxr_session_info(&self) -> xr::vulkan::SessionCreateInfo {
        xr::vulkan::SessionCreateInfo {
            instance: self.instance.handle().as_raw() as *mut _,
            physical_device: self.physical_device.as_raw() as *mut _,
            device: self.device.handle().as_raw() as *mut _,
            queue_family_index: self.queue_family_index,
            queue_index: 0,
        }
    }
    
    /// Create wgpu instance from existing Vulkan context
    pub unsafe fn create_wgpu_instance(&self) -> Result<wgpu::Instance> {
        // Create wgpu instance with Vulkan backend
        let instance = wgpu::Instance::new(wgpu::InstanceDescriptor {
            backends: wgpu::Backends::VULKAN,
            ..Default::default()
        });
        
        // Note: wgpu doesn't directly support using an existing Vulkan instance
        // This is a limitation we need to work around
        // For now, we'll create a separate wgpu instance
        // TODO: Investigate wgpu-hal for lower-level Vulkan access
        
        Ok(instance)
    }
}

/// Manages OpenXR swapchain for rendering
pub struct XrSwapchain {
    pub swapchain: xr::Swapchain<xr::Vulkan>,
    pub resolution: vk::Extent2D,
    pub format: vk::Format,
    pub images: Vec<vk::Image>,
    pub image_views: Vec<vk::ImageView>,
    pub framebuffers: Vec<vk::Framebuffer>,
    pub current_image_index: Option<u32>,
}

impl XrSwapchain {
    pub fn new(
        session: &xr::Session<xr::Vulkan>,
        device: &ash::Device,
        render_pass: vk::RenderPass,
        resolution: (u32, u32),
    ) -> Result<Self> {
        // Get supported swapchain formats
        let formats = session.enumerate_swapchain_formats()?;
        
        // Prefer SRGB format for better color
        let format = formats
            .iter()
            .find(|&&f| f == vk::Format::R8G8B8A8_SRGB.as_raw() as u32)
            .or_else(|| formats.iter().find(|&&f| f == vk::Format::B8G8R8A8_SRGB.as_raw() as u32))
            .or_else(|| formats.first())
            .copied()
            .context("No swapchain formats available")?;
        
        // Create swapchain
        let swapchain = session.create_swapchain(&xr::SwapchainCreateInfo {
            create_flags: xr::SwapchainCreateFlags::EMPTY,
            usage_flags: xr::SwapchainUsageFlags::COLOR_ATTACHMENT
                | xr::SwapchainUsageFlags::SAMPLED,
            format: format as _,
            sample_count: 1,
            width: resolution.0,
            height: resolution.1,
            face_count: 1,
            array_size: 1,
            mip_count: 1,
        })?;
        
        // Get swapchain images from OpenXR
        let images = swapchain.enumerate_images()?
            .into_iter()
            .map(|img| vk::Image::from_raw(img as _))
            .collect::<Vec<_>>();
        
        // Create image views for each swapchain image
        let image_views = images
            .iter()
            .map(|&image| {
                let view_info = vk::ImageViewCreateInfo::default()
                    .image(image)
                    .view_type(vk::ImageViewType::TYPE_2D)
                    .format(vk::Format::from_raw(format as _))
                    .subresource_range(vk::ImageSubresourceRange {
                        aspect_mask: vk::ImageAspectFlags::COLOR,
                        base_mip_level: 0,
                        level_count: 1,
                        base_array_layer: 0,
                        layer_count: 1,
                    });
                unsafe { device.create_image_view(&view_info, None) }
            })
            .collect::<Result<Vec<_>, _>>()?;
        
        // Create framebuffers for each image view
        let framebuffers = image_views
            .iter()
            .map(|&image_view| {
                let attachments = [image_view];
                let framebuffer_info = vk::FramebufferCreateInfo::default()
                    .render_pass(render_pass)
                    .attachments(&attachments)
                    .width(resolution.0)
                    .height(resolution.1)
                    .layers(1);
                unsafe { device.create_framebuffer(&framebuffer_info, None) }
            })
            .collect::<Result<Vec<_>, _>>()?;
        
        Ok(XrSwapchain {
            swapchain,
            resolution: vk::Extent2D {
                width: resolution.0,
                height: resolution.1,
            },
            format: vk::Format::from_raw(format as _),
            images,
            image_views,
            framebuffers,
            current_image_index: None,
        })
    }
    
    /// Get the current swapchain image for rendering
    pub fn acquire(&mut self) -> Result<u32> {
        let image_index = self.swapchain.acquire_image()?;
        self.swapchain.wait_image(xr::Duration::INFINITE)?;
        self.current_image_index = Some(image_index);
        Ok(image_index)
    }
    
    /// Release the swapchain image after rendering
    pub fn release(&mut self) -> Result<()> {
        self.swapchain.release_image()?;
        self.current_image_index = None;
        Ok(())
    }
    
    /// Get current framebuffer for rendering
    pub fn current_framebuffer(&self) -> Option<vk::Framebuffer> {
        self.current_image_index
            .and_then(|idx| self.framebuffers.get(idx as usize))
            .copied()
    }
}

/// Render state for stereoscopic VR rendering
pub struct StereoRenderer {
    pub left_swapchain: XrSwapchain,
    pub right_swapchain: XrSwapchain,
    pub vulkan_context: Arc<VulkanContext>,
    pub command_buffers: Vec<vk::CommandBuffer>,
}

impl StereoRenderer {
    pub fn new(
        session: &xr::Session<xr::Vulkan>,
        vulkan_context: Arc<VulkanContext>,
        resolution: (u32, u32),
    ) -> Result<Self> {
        let left_swapchain = XrSwapchain::new(
            session,
            &vulkan_context.device,
            vulkan_context.render_pass,
            resolution,
        )?;
        let right_swapchain = XrSwapchain::new(
            session,
            &vulkan_context.device,
            vulkan_context.render_pass,
            resolution,
        )?;
        
        // Allocate command buffers for rendering
        let command_buffers = unsafe {
            let alloc_info = vk::CommandBufferAllocateInfo::default()
                .command_pool(vulkan_context.command_pool)
                .level(vk::CommandBufferLevel::PRIMARY)
                .command_buffer_count(2); // One for each eye
            vulkan_context.device.allocate_command_buffers(&alloc_info)?
        };
        
        Ok(StereoRenderer {
            left_swapchain,
            right_swapchain,
            vulkan_context,
            command_buffers,
        })
    }
    
    /// Acquire swapchain images and render to them
    pub fn acquire_and_render(&mut self, views: &[xr::View]) -> Result<()> {
        // Acquire images
        let left_idx = self.left_swapchain.acquire()?;
        let right_idx = self.right_swapchain.acquire()?;
        
        // Render to left eye
        if let Some(framebuffer) = self.left_swapchain.current_framebuffer() {
            self.render_eye(
                0, // command buffer index
                framebuffer,
                &self.left_swapchain,
                &views[0],
                [0.1, 0.2, 0.3, 1.0], // Dark blue for left eye
            )?;
        }
        
        // Render to right eye
        if let Some(framebuffer) = self.right_swapchain.current_framebuffer() {
            self.render_eye(
                1, // command buffer index
                framebuffer,
                &self.right_swapchain,
                &views[1],
                [0.3, 0.2, 0.1, 1.0], // Dark red for right eye
            )?;
        }
        
        // Submit command buffers
        unsafe {
            let submit_info = vk::SubmitInfo::default()
                .command_buffers(&self.command_buffers);
            self.vulkan_context.device
                .queue_submit(self.vulkan_context.queue, &[submit_info], vk::Fence::null())?;
            self.vulkan_context.device.queue_wait_idle(self.vulkan_context.queue)?;
        }
        
        Ok(())
    }
    
    /// Render to a single eye
    fn render_eye(
        &self,
        cmd_buffer_idx: usize,
        framebuffer: vk::Framebuffer,
        swapchain: &XrSwapchain,
        view: &xr::View,
        clear_color: [f32; 4],
    ) -> Result<()> {
        let cmd_buffer = self.command_buffers[cmd_buffer_idx];
        let device = &self.vulkan_context.device;
        
        unsafe {
            // Begin command buffer
            let begin_info = vk::CommandBufferBeginInfo::default()
                .flags(vk::CommandBufferUsageFlags::ONE_TIME_SUBMIT);
            device.begin_command_buffer(cmd_buffer, &begin_info)?;
            
            // Begin render pass
            let clear_values = [vk::ClearValue {
                color: vk::ClearColorValue {
                    float32: clear_color,
                },
            }];
            
            let render_pass_begin = vk::RenderPassBeginInfo::default()
                .render_pass(self.vulkan_context.render_pass)
                .framebuffer(framebuffer)
                .render_area(vk::Rect2D {
                    offset: vk::Offset2D { x: 0, y: 0 },
                    extent: swapchain.resolution,
                })
                .clear_values(&clear_values);
            
            device.cmd_begin_render_pass(
                cmd_buffer,
                &render_pass_begin,
                vk::SubpassContents::INLINE,
            );
            
            // Bind pipeline
            device.cmd_bind_pipeline(
                cmd_buffer,
                vk::PipelineBindPoint::GRAPHICS,
                self.vulkan_context.pipeline,
            );
            
            // Set viewport
            let viewport = vk::Viewport {
                x: 0.0,
                y: 0.0,
                width: swapchain.resolution.width as f32,
                height: swapchain.resolution.height as f32,
                min_depth: 0.0,
                max_depth: 1.0,
            };
            device.cmd_set_viewport(cmd_buffer, 0, &[viewport]);
            
            // Set scissor
            let scissor = vk::Rect2D {
                offset: vk::Offset2D { x: 0, y: 0 },
                extent: swapchain.resolution,
            };
            device.cmd_set_scissor(cmd_buffer, 0, &[scissor]);
            
            // TODO: Draw actual scene here
            // For now, just the clear color shows
            
            // End render pass
            device.cmd_end_render_pass(cmd_buffer);
            
            // End command buffer
            device.end_command_buffer(cmd_buffer)?;
        }
        
        Ok(())
    }
    
    /// Build projection views for submission
    pub fn build_projection_views(
        &self,
        views: &[xr::View],
    ) -> Vec<xr::CompositionLayerProjectionView<xr::Vulkan>> {
        vec![
            xr::CompositionLayerProjectionView::new()
                .pose(views[0].pose)
                .fov(views[0].fov)
                .sub_image(xr::SwapchainSubImage::new()
                    .swapchain(&self.left_swapchain.swapchain)
                    .image_array_index(0)
                    .image_rect(xr::Rect2Di {
                        offset: xr::Offset2Di { x: 0, y: 0 },
                        extent: xr::Extent2Di {
                            width: self.left_swapchain.resolution.width as _,
                            height: self.left_swapchain.resolution.height as _,
                        },
                    })),
            xr::CompositionLayerProjectionView::new()
                .pose(views[1].pose)
                .fov(views[1].fov)
                .sub_image(xr::SwapchainSubImage::new()
                    .swapchain(&self.right_swapchain.swapchain)
                    .image_array_index(0)
                    .image_rect(xr::Rect2Di {
                        offset: xr::Offset2Di { x: 0, y: 0 },
                        extent: xr::Extent2Di {
                            width: self.right_swapchain.resolution.width as _,
                            height: self.right_swapchain.resolution.height as _,
                        },
                    })),
        ]
    }
    
    /// Release swapchain images after rendering
    pub fn release_images(&mut self) -> Result<()> {
        self.left_swapchain.release()?;
        self.right_swapchain.release()?;
        Ok(())
    }
}

/// Create a simple test pipeline for clearing/rendering
unsafe fn create_test_pipeline(
    device: &ash::Device,
    render_pass: vk::RenderPass,
    layout: vk::PipelineLayout,
) -> Result<vk::Pipeline> {
    // For now, create a minimal pipeline that just uses the render pass
    // We'll use placeholder shaders until we have real ones compiled
    
    // Create shader modules with minimal SPIR-V
    // These are just placeholder shaders that will be replaced
    let shader_code = &[0x07230203, 0x00010000, 0x00080001, 0x00000006];
    
    let vert_module = {
        let create_info = vk::ShaderModuleCreateInfo::default()
            .code(shader_code);
        device.create_shader_module(&create_info, None).unwrap_or(vk::ShaderModule::null())
    };
    
    let frag_module = {
        let create_info = vk::ShaderModuleCreateInfo::default()
            .code(shader_code);
        device.create_shader_module(&create_info, None).unwrap_or(vk::ShaderModule::null())
    };
    
    // If shader modules are null, just return a null pipeline for now
    if vert_module == vk::ShaderModule::null() || frag_module == vk::ShaderModule::null() {
        return Ok(vk::Pipeline::null());
    }
    
    let shader_stages = [
        vk::PipelineShaderStageCreateInfo::default()
            .stage(vk::ShaderStageFlags::VERTEX)
            .module(vert_module)
            .name(CStr::from_bytes_with_nul_unchecked(b"main\0")),
        vk::PipelineShaderStageCreateInfo::default()
            .stage(vk::ShaderStageFlags::FRAGMENT)
            .module(frag_module)
            .name(CStr::from_bytes_with_nul_unchecked(b"main\0")),
    ];
    
    // Empty vertex input
    let vertex_input = vk::PipelineVertexInputStateCreateInfo::default();
    
    // Triangle list
    let input_assembly = vk::PipelineInputAssemblyStateCreateInfo::default()
        .topology(vk::PrimitiveTopology::TRIANGLE_LIST);
    
    // Dynamic viewport and scissor
    let viewport_state = vk::PipelineViewportStateCreateInfo::default()
        .viewport_count(1)
        .scissor_count(1);
    
    // Rasterization state
    let rasterization = vk::PipelineRasterizationStateCreateInfo::default()
        .polygon_mode(vk::PolygonMode::FILL)
        .cull_mode(vk::CullModeFlags::NONE)
        .front_face(vk::FrontFace::COUNTER_CLOCKWISE)
        .line_width(1.0);
    
    // Multisample state
    let multisample = vk::PipelineMultisampleStateCreateInfo::default()
        .rasterization_samples(vk::SampleCountFlags::TYPE_1);
    
    // Color blend attachment
    let color_blend_attachment = vk::PipelineColorBlendAttachmentState::default()
        .color_write_mask(
            vk::ColorComponentFlags::R
                | vk::ColorComponentFlags::G
                | vk::ColorComponentFlags::B
                | vk::ColorComponentFlags::A,
        );
    
    // Color blend state
    let color_blend = vk::PipelineColorBlendStateCreateInfo::default()
        .attachments(std::slice::from_ref(&color_blend_attachment));
    
    // Dynamic states
    let dynamic_states = [vk::DynamicState::VIEWPORT, vk::DynamicState::SCISSOR];
    let dynamic_state = vk::PipelineDynamicStateCreateInfo::default()
        .dynamic_states(&dynamic_states);
    
    // Create pipeline
    let pipeline_info = vk::GraphicsPipelineCreateInfo::default()
        .stages(&shader_stages)
        .vertex_input_state(&vertex_input)
        .input_assembly_state(&input_assembly)
        .viewport_state(&viewport_state)
        .rasterization_state(&rasterization)
        .multisample_state(&multisample)
        .color_blend_state(&color_blend)
        .dynamic_state(&dynamic_state)
        .layout(layout)
        .render_pass(render_pass)
        .subpass(0);
    
    let pipelines = device
        .create_graphics_pipelines(vk::PipelineCache::null(), &[pipeline_info], None)
        .map_err(|e| anyhow::anyhow!("Failed to create pipeline: {:?}", e))?;
    
    // Clean up shader modules
    device.destroy_shader_module(vert_module, None);
    device.destroy_shader_module(frag_module, None);
    
    Ok(pipelines[0])
}
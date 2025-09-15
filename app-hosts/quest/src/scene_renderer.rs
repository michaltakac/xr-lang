//! Scene rendering integration with XR-Lang VM

use anyhow::Result;
use ash::{vk, Device};
use glam::{Mat4, Vec3};
use std::sync::Arc;

/// Manages rendering of VM scene graph to Vulkan
pub struct SceneRenderer {
    device: Arc<Device>,
    
    // Vertex buffer for primitives
    vertex_buffer: vk::Buffer,
    vertex_memory: vk::DeviceMemory,
    
    // Uniform buffer for transforms
    uniform_buffer: vk::Buffer,
    uniform_memory: vk::DeviceMemory,
    
    // Descriptor set for binding uniforms
    descriptor_pool: vk::DescriptorPool,
    descriptor_set_layout: vk::DescriptorSetLayout,
    descriptor_sets: Vec<vk::DescriptorSet>,
}

/// Per-frame uniform data
#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct FrameUniforms {
    pub view_proj: Mat4,
    pub eye_pos: Vec3,
    pub time: f32,
}

impl SceneRenderer {
    pub fn new(
        device: Arc<Device>,
        physical_device: vk::PhysicalDevice,
        instance: &ash::Instance,
    ) -> Result<Self> {
        unsafe {
            // Get memory properties
            let mem_props = instance.get_physical_device_memory_properties(physical_device);
            
            // Create vertex buffer for a cube (for testing)
            let vertices = create_cube_vertices();
            let vertex_size = (vertices.len() * std::mem::size_of::<f32>()) as u64;
            
            let vertex_buffer_info = vk::BufferCreateInfo::default()
                .size(vertex_size)
                .usage(vk::BufferUsageFlags::VERTEX_BUFFER)
                .sharing_mode(vk::SharingMode::EXCLUSIVE);
            
            let vertex_buffer = device.create_buffer(&vertex_buffer_info, None)?;
            
            // Allocate memory for vertex buffer
            let vertex_mem_reqs = device.get_buffer_memory_requirements(vertex_buffer);
            let vertex_memory = allocate_memory(
                &device,
                &mem_props,
                vertex_mem_reqs,
                vk::MemoryPropertyFlags::HOST_VISIBLE | vk::MemoryPropertyFlags::HOST_COHERENT,
            )?;
            
            device.bind_buffer_memory(vertex_buffer, vertex_memory, 0)?;
            
            // Upload vertex data
            let data_ptr = device.map_memory(
                vertex_memory,
                0,
                vertex_size,
                vk::MemoryMapFlags::empty(),
            )?;
            std::ptr::copy_nonoverlapping(
                vertices.as_ptr() as *const u8,
                data_ptr as *mut u8,
                vertex_size as usize,
            );
            device.unmap_memory(vertex_memory);
            
            // Create uniform buffer
            let uniform_size = std::mem::size_of::<FrameUniforms>() as u64;
            let uniform_buffer_info = vk::BufferCreateInfo::default()
                .size(uniform_size)
                .usage(vk::BufferUsageFlags::UNIFORM_BUFFER)
                .sharing_mode(vk::SharingMode::EXCLUSIVE);
            
            let uniform_buffer = device.create_buffer(&uniform_buffer_info, None)?;
            
            let uniform_mem_reqs = device.get_buffer_memory_requirements(uniform_buffer);
            let uniform_memory = allocate_memory(
                &device,
                &mem_props,
                uniform_mem_reqs,
                vk::MemoryPropertyFlags::HOST_VISIBLE | vk::MemoryPropertyFlags::HOST_COHERENT,
            )?;
            
            device.bind_buffer_memory(uniform_buffer, uniform_memory, 0)?;
            
            // Create descriptor set layout
            let binding = vk::DescriptorSetLayoutBinding::default()
                .binding(0)
                .descriptor_type(vk::DescriptorType::UNIFORM_BUFFER)
                .descriptor_count(1)
                .stage_flags(vk::ShaderStageFlags::VERTEX | vk::ShaderStageFlags::FRAGMENT);
            
            let layout_info = vk::DescriptorSetLayoutCreateInfo::default()
                .bindings(std::slice::from_ref(&binding));
            
            let descriptor_set_layout = device.create_descriptor_set_layout(&layout_info, None)?;
            
            // Create descriptor pool
            let pool_size = vk::DescriptorPoolSize {
                ty: vk::DescriptorType::UNIFORM_BUFFER,
                descriptor_count: 2, // For both eyes
            };
            
            let pool_info = vk::DescriptorPoolCreateInfo::default()
                .max_sets(2)
                .pool_sizes(std::slice::from_ref(&pool_size));
            
            let descriptor_pool = device.create_descriptor_pool(&pool_info, None)?;
            
            // Allocate descriptor sets
            let layouts = vec![descriptor_set_layout; 2];
            let alloc_info = vk::DescriptorSetAllocateInfo::default()
                .descriptor_pool(descriptor_pool)
                .set_layouts(&layouts);
            
            let descriptor_sets = device.allocate_descriptor_sets(&alloc_info)?;
            
            // Update descriptor sets
            for &set in &descriptor_sets {
                let buffer_info = vk::DescriptorBufferInfo {
                    buffer: uniform_buffer,
                    offset: 0,
                    range: uniform_size,
                };
                
                let write = vk::WriteDescriptorSet::default()
                    .dst_set(set)
                    .dst_binding(0)
                    .descriptor_type(vk::DescriptorType::UNIFORM_BUFFER)
                    .buffer_info(std::slice::from_ref(&buffer_info));
                
                device.update_descriptor_sets(&[write], &[]);
            }
            
            Ok(SceneRenderer {
                device,
                vertex_buffer,
                vertex_memory,
                uniform_buffer,
                uniform_memory,
                descriptor_pool,
                descriptor_set_layout,
                descriptor_sets,
            })
        }
    }
    
    /// Update uniforms for a frame
    pub fn update_uniforms(&self, view: &Mat4, proj: &Mat4, eye_pos: Vec3, time: f32) -> Result<()> {
        unsafe {
            let uniforms = FrameUniforms {
                view_proj: *proj * *view,
                eye_pos,
                time,
            };
            
            let data_ptr = self.device.map_memory(
                self.uniform_memory,
                0,
                std::mem::size_of::<FrameUniforms>() as u64,
                vk::MemoryMapFlags::empty(),
            )?;
            
            std::ptr::copy_nonoverlapping(
                &uniforms as *const _ as *const u8,
                data_ptr as *mut u8,
                std::mem::size_of::<FrameUniforms>(),
            );
            
            self.device.unmap_memory(self.uniform_memory);
        }
        Ok(())
    }
    
    /// Render test scene (for now just draws a cube)
    pub fn render_test_scene(
        &self,
        cmd_buffer: vk::CommandBuffer,
        _eye_index: usize,
    ) -> Result<()> {
        unsafe {
            // Bind vertex buffer
            self.device.cmd_bind_vertex_buffers(
                cmd_buffer,
                0,
                &[self.vertex_buffer],
                &[0],
            );
            
            // Draw a test cube (36 vertices for 12 triangles)
            self.device.cmd_draw(cmd_buffer, 36, 1, 0, 0);
        }
        
        Ok(())
    }
}

/// Create vertices for a unit cube
fn create_cube_vertices() -> Vec<f32> {
    vec![
        // Front face
        -0.5, -0.5,  0.5,  0.0, 0.0, 1.0,
         0.5, -0.5,  0.5,  0.0, 0.0, 1.0,
         0.5,  0.5,  0.5,  0.0, 0.0, 1.0,
        -0.5, -0.5,  0.5,  0.0, 0.0, 1.0,
         0.5,  0.5,  0.5,  0.0, 0.0, 1.0,
        -0.5,  0.5,  0.5,  0.0, 0.0, 1.0,
        
        // Back face
        -0.5, -0.5, -0.5,  0.0, 0.0, -1.0,
        -0.5,  0.5, -0.5,  0.0, 0.0, -1.0,
         0.5,  0.5, -0.5,  0.0, 0.0, -1.0,
        -0.5, -0.5, -0.5,  0.0, 0.0, -1.0,
         0.5,  0.5, -0.5,  0.0, 0.0, -1.0,
         0.5, -0.5, -0.5,  0.0, 0.0, -1.0,
        
        // Top face
        -0.5,  0.5,  0.5,  0.0, 1.0, 0.0,
         0.5,  0.5,  0.5,  0.0, 1.0, 0.0,
         0.5,  0.5, -0.5,  0.0, 1.0, 0.0,
        -0.5,  0.5,  0.5,  0.0, 1.0, 0.0,
         0.5,  0.5, -0.5,  0.0, 1.0, 0.0,
        -0.5,  0.5, -0.5,  0.0, 1.0, 0.0,
        
        // Bottom face
        -0.5, -0.5,  0.5,  0.0, -1.0, 0.0,
        -0.5, -0.5, -0.5,  0.0, -1.0, 0.0,
         0.5, -0.5, -0.5,  0.0, -1.0, 0.0,
        -0.5, -0.5,  0.5,  0.0, -1.0, 0.0,
         0.5, -0.5, -0.5,  0.0, -1.0, 0.0,
         0.5, -0.5,  0.5,  0.0, -1.0, 0.0,
        
        // Right face
         0.5, -0.5,  0.5,  1.0, 0.0, 0.0,
         0.5, -0.5, -0.5,  1.0, 0.0, 0.0,
         0.5,  0.5, -0.5,  1.0, 0.0, 0.0,
         0.5, -0.5,  0.5,  1.0, 0.0, 0.0,
         0.5,  0.5, -0.5,  1.0, 0.0, 0.0,
         0.5,  0.5,  0.5,  1.0, 0.0, 0.0,
        
        // Left face
        -0.5, -0.5,  0.5,  -1.0, 0.0, 0.0,
        -0.5,  0.5,  0.5,  -1.0, 0.0, 0.0,
        -0.5,  0.5, -0.5,  -1.0, 0.0, 0.0,
        -0.5, -0.5,  0.5,  -1.0, 0.0, 0.0,
        -0.5,  0.5, -0.5,  -1.0, 0.0, 0.0,
        -0.5, -0.5, -0.5,  -1.0, 0.0, 0.0,
    ]
}

/// Find and allocate memory
unsafe fn allocate_memory(
    device: &Device,
    mem_props: &vk::PhysicalDeviceMemoryProperties,
    requirements: vk::MemoryRequirements,
    flags: vk::MemoryPropertyFlags,
) -> Result<vk::DeviceMemory> {
    let memory_type_index = find_memory_type_index(mem_props, requirements.memory_type_bits, flags)
        .ok_or_else(|| anyhow::anyhow!("Failed to find suitable memory type"))?;
    
    let alloc_info = vk::MemoryAllocateInfo::default()
        .allocation_size(requirements.size)
        .memory_type_index(memory_type_index);
    
    Ok(device.allocate_memory(&alloc_info, None)?)
}

/// Find memory type index
fn find_memory_type_index(
    mem_props: &vk::PhysicalDeviceMemoryProperties,
    type_filter: u32,
    properties: vk::MemoryPropertyFlags,
) -> Option<u32> {
    for i in 0..mem_props.memory_type_count {
        if (type_filter & (1 << i)) != 0
            && mem_props.memory_types[i as usize]
                .property_flags
                .contains(properties)
        {
            return Some(i);
        }
    }
    None
}
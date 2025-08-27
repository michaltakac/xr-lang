//! GPU device management

use wgpu::*;

pub struct DeviceManager {
    pub device: Device,
    pub queue: Queue,
}

impl DeviceManager {
    pub fn new(device: Device, queue: Queue) -> Self {
        Self { device, queue }
    }
}

pub fn compile_module(device: &Device, wgsl: &str) -> ShaderModule {
    device.create_shader_module(ShaderModuleDescriptor {
        label: Some("dsl-wgsl"),
        source: ShaderSource::Wgsl(std::borrow::Cow::Borrowed(wgsl)),
    })
}
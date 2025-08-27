//! GPU resource caching for hot-reload performance

use std::collections::HashMap;
use wgpu::*;
use twox_hash::XxHash64;
use std::hash::{Hash, Hasher};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ShaderId(u64);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct PipelineId(u64);

pub struct ShaderCache {
    pub modules: HashMap<ShaderId, ShaderModule>,
}

pub struct PipelineCache {
    pub pipelines: HashMap<PipelineId, RenderPipeline>,
}

pub fn hash_wgsl(src: &str) -> ShaderId {
    let mut h = XxHash64::default();
    src.hash(&mut h);
    ShaderId(h.finish())
}

impl ShaderCache {
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
        }
    }
    
    pub fn get_or_compile(
        &mut self,
        device: &Device,
        wgsl: &str,
    ) -> (ShaderId, &ShaderModule) {
        let id = hash_wgsl(wgsl);
        if !self.modules.contains_key(&id) {
            let module = device.create_shader_module(ShaderModuleDescriptor {
                label: Some("dsl-wgsl"),
                source: ShaderSource::Wgsl(std::borrow::Cow::Borrowed(wgsl)),
            });
            self.modules.insert(id, module);
        }
        (id, self.modules.get(&id).unwrap())
    }
}

impl PipelineCache {
    pub fn new() -> Self {
        Self {
            pipelines: HashMap::new(),
        }
    }
}
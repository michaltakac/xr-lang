//! Rendering utilities

// use wgpu::*;

pub struct Renderer {
    // TODO: Implement rendering pipeline
}

impl Renderer {
    pub fn new() -> Self {
        Self {}
    }
    
    pub fn render_frame(&mut self, _ctx: &crate::GpuCtx) -> anyhow::Result<()> {
        // TODO: Implement frame rendering
        Ok(())
    }
}
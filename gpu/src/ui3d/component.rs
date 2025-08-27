//! Base trait for 3D UI components

use crate::math::*;
use crate::ui3d::{TextRenderer3D, InputSystem3D};
use wgpu::RenderPass;

/// Base trait for all 3D UI components
pub trait UI3DComponent {
    /// Update component logic
    fn update(&mut self, dt: f32);
    
    /// Render the component
    fn render<'a>(&'a self, render_pass: &mut RenderPass<'a>, text_renderer: &'a TextRenderer3D);
    
    /// Handle input events
    fn handle_input(&mut self, input: &InputSystem3D);
    
    /// Check if a world position is inside this component
    fn contains_point(&self, world_pos: Vec3) -> bool;
    
    /// Get the transform of this component
    fn get_transform(&self) -> &Transform;
    
    /// Get mutable transform
    fn get_transform_mut(&mut self) -> &mut Transform;
    
    /// Set whether this component is focused/active
    fn set_focused(&mut self, focused: bool);
    
    /// Check if component is focused
    fn is_focused(&self) -> bool;
}

/// Base properties shared by all UI components
#[derive(Debug, Clone)]
pub struct UIComponentBase {
    pub transform: Transform,
    pub size: Vec3,
    pub focused: bool,
    pub visible: bool,
    pub background_color: [f32; 4],
    pub border_color: [f32; 4],
    pub border_width: f32,
}

impl UIComponentBase {
    pub fn new(position: Vec3, size: Vec3) -> Self {
        Self {
            transform: Transform::with_position(position),
            size,
            focused: false,
            visible: true,
            background_color: [0.1, 0.1, 0.1, 0.9], // Dark semi-transparent
            border_color: [0.3, 0.3, 0.3, 1.0],     // Light gray border
            border_width: 0.02,
        }
    }
    
    pub fn contains_point(&self, world_pos: Vec3) -> bool {
        if !self.visible {
            return false;
        }
        
        // Transform world position to local coordinates
        let local_pos = world_pos - self.transform.position;
        
        // Check if within bounds
        local_pos.x >= -self.size.x / 2.0 && local_pos.x <= self.size.x / 2.0 &&
        local_pos.y >= -self.size.y / 2.0 && local_pos.y <= self.size.y / 2.0 &&
        local_pos.z >= -self.size.z / 2.0 && local_pos.z <= self.size.z / 2.0
    }
}
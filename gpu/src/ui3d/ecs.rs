//! ECS components and systems for 3D UI using hecs

use crate::math::*;
use crate::ui3d::TextRenderer3D;
use hecs::*;
use wgpu::{Device, RenderPass};

/// ECS component for 3D positioning and transformation
#[derive(Debug, Clone, Copy)]
pub struct Transform3D {
    pub position: Vec3,
    pub rotation: Vec3,
    pub scale: Vec3,
}

impl Transform3D {
    pub fn new(position: Vec3) -> Self {
        Self {
            position,
            rotation: Vec3::ZERO,
            scale: Vec3::ONE,
        }
    }
    
    pub fn with_scale(mut self, scale: Vec3) -> Self {
        self.scale = scale;
        self
    }
    
    pub fn to_transform(&self) -> Transform {
        Transform::with_position(self.position)
    }
}

/// ECS component for UI layout and visual properties
#[derive(Debug, Clone, Copy)]
pub struct UILayout {
    pub size: Vec3,
    pub background_color: [f32; 4],
    pub border_color: [f32; 4],
    pub border_width: f32,
    pub visible: bool,
    pub focused: bool,
}

impl UILayout {
    pub fn new(size: Vec3) -> Self {
        Self {
            size,
            background_color: [0.1, 0.1, 0.1, 0.9],
            border_color: [0.3, 0.3, 0.3, 1.0],
            border_width: 0.02,
            visible: true,
            focused: false,
        }
    }
    
    pub fn with_dark_editor_style(mut self) -> Self {
        self.background_color = [0.05, 0.05, 0.05, 0.95];
        self.border_color = [0.2, 0.4, 0.8, 1.0];
        self
    }
    
    pub fn contains_point(&self, transform: &Transform3D, world_pos: Vec3) -> bool {
        if !self.visible {
            return false;
        }
        
        let local_pos = world_pos - transform.position;
        
        local_pos.x >= -self.size.x / 2.0 && local_pos.x <= self.size.x / 2.0 &&
        local_pos.y >= -self.size.y / 2.0 && local_pos.y <= self.size.y / 2.0 &&
        local_pos.z >= -self.size.z / 2.0 && local_pos.z <= self.size.z / 2.0
    }
}

/// ECS component for text content and properties
#[derive(Debug, Clone)]
pub struct TextContent {
    pub content: String,
    pub font_size: f32,
    pub color: [f32; 4],
    pub line_height: f32,
    pub max_visible_lines: usize,
    pub max_line_length: usize,
}

impl TextContent {
    pub fn new(content: String) -> Self {
        Self {
            content,
            font_size: 0.03,
            color: [0.9, 0.9, 0.9, 1.0],
            line_height: 1.2,
            max_visible_lines: 20,
            max_line_length: 80,
        }
    }
    
    pub fn get_line_count(&self) -> usize {
        self.content.lines().count().max(1)
    }
    
    pub fn get_lines(&self) -> Vec<&str> {
        self.content.lines().collect()
    }
}

/// ECS component for text editor functionality
#[derive(Debug, Clone)]
pub struct TextEditor {
    pub cursor_line: usize,
    pub cursor_col: usize,
    pub scroll_offset: usize,
    pub syntax_colors: SyntaxColors,
    pub show_line_numbers: bool,
    pub is_editable: bool,
}

impl TextEditor {
    pub fn new() -> Self {
        Self {
            cursor_line: 0,
            cursor_col: 0,
            scroll_offset: 0,
            syntax_colors: SyntaxColors::default(),
            show_line_numbers: true,
            is_editable: true,
        }
    }
    
    pub fn readonly() -> Self {
        let mut editor = Self::new();
        editor.is_editable = false;
        editor.show_line_numbers = false;
        editor
    }
}

#[derive(Debug, Clone)]
pub struct SyntaxColors {
    pub text: [f32; 4],
    pub keyword: [f32; 4],
    pub string: [f32; 4],
    pub comment: [f32; 4],
    pub number: [f32; 4],
    pub symbol: [f32; 4],
}

impl Default for SyntaxColors {
    fn default() -> Self {
        Self {
            text: [0.9, 0.9, 0.9, 1.0],
            keyword: [0.5, 0.8, 1.0, 1.0],
            string: [0.8, 1.0, 0.5, 1.0],
            comment: [0.6, 0.6, 0.6, 1.0],
            number: [1.0, 0.8, 0.5, 1.0],
            symbol: [1.0, 0.5, 0.8, 1.0],
        }
    }
}

/// ECS component for button behavior
#[derive(Debug, Clone)]
pub struct Button {
    pub label: String,
    pub is_pressed: bool,
    pub is_hovered: bool,
    pub action: ButtonAction,
}

impl Button {
    pub fn new(label: String, action: ButtonAction) -> Self {
        Self {
            label,
            is_pressed: false,
            is_hovered: false,
            action,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ButtonAction {
    SaveCode,
    LoadFile,
    ClearLog,
    Custom(String),
}

/// ECS component for scroll behavior
#[derive(Debug, Clone)]
pub struct Scrollable {
    pub scroll_offset: f32,
    pub content_height: f32,
    pub viewport_height: f32,
    pub scroll_speed: f32,
}

impl Scrollable {
    pub fn new(viewport_height: f32) -> Self {
        Self {
            scroll_offset: 0.0,
            content_height: 0.0,
            viewport_height,
            scroll_speed: 3.0,
        }
    }
    
    pub fn scroll(&mut self, delta: f32) {
        let max_scroll = (self.content_height - self.viewport_height).max(0.0);
        self.scroll_offset = (self.scroll_offset + delta * self.scroll_speed)
            .clamp(0.0, max_scroll);
    }
}

/// ECS component for rendering state
#[derive(Debug)]
pub struct RenderData {
    pub vertex_buffer: Option<wgpu::Buffer>,
    pub index_buffer: Option<wgpu::Buffer>,
    pub index_count: u32,
    pub needs_update: bool,
}

impl RenderData {
    pub fn new() -> Self {
        Self {
            vertex_buffer: None,
            index_buffer: None,
            index_count: 0,
            needs_update: true,
        }
    }
    
    pub fn mark_dirty(&mut self) {
        self.needs_update = true;
    }
}

/// Helper function to create a 3D code editor entity
pub fn spawn_code_editor(world: &mut World, position: Vec3, size: Vec3, initial_content: &str) -> Entity {
    world.spawn((
        Transform3D::new(position),
        UILayout::new(size).with_dark_editor_style(),
        TextContent::new(initial_content.to_string()),
        TextEditor::new(),
        RenderData::new(),
    ))
}

/// Helper function to create a 3D button entity
pub fn spawn_button(world: &mut World, position: Vec3, size: Vec3, label: &str, action: ButtonAction) -> Entity {
    world.spawn((
        Transform3D::new(position),
        UILayout::new(size),
        Button::new(label.to_string(), action),
        TextContent::new(label.to_string()),
        RenderData::new(),
    ))
}

/// Helper function to create a 3D log viewer entity
pub fn spawn_log_viewer(world: &mut World, position: Vec3, size: Vec3) -> Entity {
    world.spawn((
        Transform3D::new(position),
        UILayout::new(size),
        TextContent::new("Log output will appear here...\n".to_string()),
        TextEditor::readonly(),
        Scrollable::new(size.y),
        RenderData::new(),
    ))
}
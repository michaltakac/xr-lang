//! 3D Code Editor component for live DSL editing

use crate::math::*;
use crate::ui3d::{UI3DComponent, UIComponentBase, TextRenderer3D, InputSystem3D, TextVertex};
use wgpu::{RenderPass, Buffer, Device, BufferDescriptor, BufferUsages, util::DeviceExt};
use winit::keyboard::KeyCode;

/// 3D floating code editor with syntax highlighting and cursor
pub struct CodeEditor3D {
    pub base: UIComponentBase,
    pub content: String,
    pub cursor_line: usize,
    pub cursor_col: usize,
    pub scroll_offset: usize,
    pub max_visible_lines: usize,
    pub max_line_length: usize,
    pub syntax_colors: SyntaxColors,
    pub show_line_numbers: bool,
    
    // Rendering data
    vertex_buffer: Option<Buffer>,
    index_buffer: Option<Buffer>,
    index_count: u32,
    needs_update: bool,
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
            text: [0.9, 0.9, 0.9, 1.0],        // Light gray
            keyword: [0.5, 0.8, 1.0, 1.0],     // Light blue
            string: [0.8, 1.0, 0.5, 1.0],      // Light green
            comment: [0.6, 0.6, 0.6, 1.0],     // Gray
            number: [1.0, 0.8, 0.5, 1.0],      // Orange
            symbol: [1.0, 0.5, 0.8, 1.0],      // Pink
        }
    }
}

impl CodeEditor3D {
    pub fn new(position: Vec3, size: Vec3, initial_content: &str) -> Self {
        let mut base = UIComponentBase::new(position, size);
        base.background_color = [0.05, 0.05, 0.05, 0.95]; // Very dark background
        base.border_color = [0.2, 0.4, 0.8, 1.0]; // Blue border when focused
        
        Self {
            base,
            content: initial_content.to_string(),
            cursor_line: 0,
            cursor_col: 0,
            scroll_offset: 0,
            max_visible_lines: (size.y * 10.0) as usize, // Rough estimate
            max_line_length: (size.x * 10.0) as usize,
            syntax_colors: SyntaxColors::default(),
            show_line_numbers: true,
            vertex_buffer: None,
            index_buffer: None,
            index_count: 0,
            needs_update: true,
        }
    }
    
    pub fn get_content(&self) -> &str {
        &self.content
    }
    
    pub fn set_content(&mut self, content: String) {
        self.content = content;
        self.cursor_line = self.cursor_line.min(self.get_line_count().saturating_sub(1));
        self.cursor_col = 0;
        self.needs_update = true;
    }
    
    fn get_line_count(&self) -> usize {
        self.content.lines().count().max(1)
    }
    
    fn get_current_line(&self) -> &str {
        self.content.lines().nth(self.cursor_line).unwrap_or("")
    }
    
    fn get_visible_lines(&self) -> impl Iterator<Item = (usize, &str)> {
        self.content.lines()
            .enumerate()
            .skip(self.scroll_offset)
            .take(self.max_visible_lines)
    }
    
    fn insert_char(&mut self, ch: char) {
        if ch.is_control() && ch != '\n' && ch != '\t' {
            return;
        }
        
        let lines: Vec<&str> = self.content.lines().collect();
        let mut new_lines = lines;
        
        // Ensure we have enough lines
        while new_lines.len() <= self.cursor_line {
            new_lines.push("");
        }
        
        let current_line = new_lines[self.cursor_line].to_string();
        let mut chars: Vec<char> = current_line.chars().collect();
        
        // Insert character at cursor position
        chars.insert(self.cursor_col.min(chars.len()), ch);
        new_lines[self.cursor_line] = &chars.into_iter().collect::<String>();
        
        // Update content
        self.content = new_lines.join("\n");
        
        if ch == '\n' {
            self.cursor_line += 1;
            self.cursor_col = 0;
        } else {
            self.cursor_col += 1;
        }
        
        self.needs_update = true;
    }
    
    fn delete_char(&mut self) {
        if self.cursor_col == 0 {
            if self.cursor_line > 0 {
                // Join with previous line
                let lines: Vec<String> = self.content.lines().map(|s| s.to_string()).collect();
                let mut new_lines = lines;
                
                if self.cursor_line < new_lines.len() {
                    let current = new_lines.remove(self.cursor_line);
                    self.cursor_line -= 1;
                    self.cursor_col = new_lines[self.cursor_line].len();
                    new_lines[self.cursor_line].push_str(&current);
                    
                    self.content = new_lines.join("\n");
                    self.needs_update = true;
                }
            }
        } else {
            // Delete character before cursor
            let lines: Vec<&str> = self.content.lines().collect();
            let mut new_lines: Vec<String> = lines.iter().map(|s| s.to_string()).collect();
            
            if self.cursor_line < new_lines.len() {
                let mut chars: Vec<char> = new_lines[self.cursor_line].chars().collect();
                if self.cursor_col > 0 && self.cursor_col <= chars.len() {
                    chars.remove(self.cursor_col - 1);
                    new_lines[self.cursor_line] = chars.into_iter().collect();
                    self.cursor_col -= 1;
                    
                    self.content = new_lines.join("\n");
                    self.needs_update = true;
                }
            }
        }
    }
    
    fn move_cursor(&mut self, direction: CursorDirection) {
        match direction {
            CursorDirection::Up => {
                if self.cursor_line > 0 {
                    self.cursor_line -= 1;
                    let line_len = self.get_current_line().chars().count();
                    self.cursor_col = self.cursor_col.min(line_len);
                    
                    // Scroll up if needed
                    if self.cursor_line < self.scroll_offset {
                        self.scroll_offset = self.cursor_line;
                    }
                }
            }
            CursorDirection::Down => {
                if self.cursor_line + 1 < self.get_line_count() {
                    self.cursor_line += 1;
                    let line_len = self.get_current_line().chars().count();
                    self.cursor_col = self.cursor_col.min(line_len);
                    
                    // Scroll down if needed
                    if self.cursor_line >= self.scroll_offset + self.max_visible_lines {
                        self.scroll_offset = self.cursor_line + 1 - self.max_visible_lines;
                    }
                }
            }
            CursorDirection::Left => {
                if self.cursor_col > 0 {
                    self.cursor_col -= 1;
                } else if self.cursor_line > 0 {
                    self.cursor_line -= 1;
                    self.cursor_col = self.get_current_line().chars().count();
                }
            }
            CursorDirection::Right => {
                let line_len = self.get_current_line().chars().count();
                if self.cursor_col < line_len {
                    self.cursor_col += 1;
                } else if self.cursor_line + 1 < self.get_line_count() {
                    self.cursor_line += 1;
                    self.cursor_col = 0;
                }
            }
        }
    }
    
    fn update_mesh(&mut self, device: &Device, text_renderer: &TextRenderer3D) {
        if !self.needs_update {
            return;
        }
        
        let mut all_vertices = Vec::new();
        let mut all_indices = Vec::new();
        
        let char_size = 0.03;
        let line_height = char_size * 1.2;
        let line_number_width = if self.show_line_numbers { 0.2 } else { 0.0 };
        
        // Render visible lines
        for (line_idx, (actual_line, line_text)) in self.get_visible_lines().enumerate() {
            let y_pos = -line_idx as f32 * line_height;
            
            // Line numbers
            if self.show_line_numbers {
                let line_num_text = format!("{:3}", actual_line + 1);
                let line_num_transform = Transform::with_position(
                    self.base.transform.position + Vec3::new(-self.base.size.x / 2.0 + 0.05, y_pos, 0.01)
                );
                
                let (vertices, indices) = text_renderer.create_text_mesh(
                    &line_num_text,
                    &line_num_transform,
                    [0.4, 0.4, 0.4, 1.0] // Gray line numbers
                );
                
                let base_idx = all_vertices.len() as u16;
                all_vertices.extend(vertices);
                all_indices.extend(indices.iter().map(|i| i + base_idx));
            }
            
            // Line text with basic syntax highlighting
            if !line_text.is_empty() {
                let text_transform = Transform::with_position(
                    self.base.transform.position + Vec3::new(-self.base.size.x / 2.0 + line_number_width, y_pos, 0.01)
                );
                
                let color = self.get_syntax_color(line_text);
                let (vertices, indices) = text_renderer.create_text_mesh(
                    line_text,
                    &text_transform,
                    color
                );
                
                let base_idx = all_vertices.len() as u16;
                all_vertices.extend(vertices);
                all_indices.extend(indices.iter().map(|i| i + base_idx));
            }
        }
        
        // Render cursor
        if self.base.focused && self.cursor_line >= self.scroll_offset && self.cursor_line < self.scroll_offset + self.max_visible_lines {
            let cursor_y = -(self.cursor_line - self.scroll_offset) as f32 * line_height;
            let cursor_x = self.cursor_col as f32 * char_size;
            
            let cursor_transform = Transform::with_position(
                self.base.transform.position + Vec3::new(-self.base.size.x / 2.0 + line_number_width + cursor_x, cursor_y, 0.02)
            );
            
            let (vertices, indices) = text_renderer.create_text_mesh(
                "|",
                &cursor_transform,
                [1.0, 1.0, 1.0, 1.0] // White cursor
            );
            
            let base_idx = all_vertices.len() as u16;
            all_vertices.extend(vertices);
            all_indices.extend(indices.iter().map(|i| i + base_idx));
        }
        
        // Update buffers
        if !all_vertices.is_empty() {
            self.vertex_buffer = Some(device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
                label: Some("Code Editor Vertex Buffer"),
                contents: bytemuck::cast_slice(&all_vertices),
                usage: BufferUsages::VERTEX,
            }));
            
            self.index_buffer = Some(device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
                label: Some("Code Editor Index Buffer"),
                contents: bytemuck::cast_slice(&all_indices),
                usage: BufferUsages::INDEX,
            }));
            
            self.index_count = all_indices.len() as u32;
        }
        
        self.needs_update = false;
    }
    
    fn get_syntax_color(&self, line: &str) -> [f32; 4] {
        // Basic syntax highlighting for Lisp-like syntax
        let trimmed = line.trim();
        if trimmed.starts_with(';') {
            self.syntax_colors.comment
        } else if trimmed.starts_with('(') {
            if trimmed.contains("defbehavior") || trimmed.contains("defscene3d") || trimmed.contains("let") {
                self.syntax_colors.keyword
            } else {
                self.syntax_colors.symbol
            }
        } else if trimmed.starts_with('"') {
            self.syntax_colors.string
        } else if trimmed.chars().all(|c| c.is_ascii_digit() || c == '.' || c == '-') {
            self.syntax_colors.number
        } else {
            self.syntax_colors.text
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum CursorDirection {
    Up,
    Down,
    Left,
    Right,
}

impl UI3DComponent for CodeEditor3D {
    fn update(&mut self, _dt: f32) {
        // Animation or time-based updates could go here
    }
    
    fn render<'a>(&'a self, render_pass: &mut RenderPass<'a>, _text_renderer: &'a TextRenderer3D) {
        if let (Some(vertex_buffer), Some(index_buffer)) = (&self.vertex_buffer, &self.index_buffer) {
            if self.index_count > 0 {
                render_pass.set_vertex_buffer(0, vertex_buffer.slice(..));
                render_pass.set_index_buffer(index_buffer.slice(..), wgpu::IndexFormat::Uint16);
                render_pass.draw_indexed(0..self.index_count, 0, 0..1);
            }
        }
    }
    
    fn handle_input(&mut self, input: &InputSystem3D) {
        // Handle keyboard input for text editing
        for &key in &input.keyboard.just_pressed_keys {
            match key {
                VirtualKeyCode::Up => self.move_cursor(CursorDirection::Up),
                VirtualKeyCode::Down => self.move_cursor(CursorDirection::Down),
                VirtualKeyCode::Left => self.move_cursor(CursorDirection::Left),
                VirtualKeyCode::Right => self.move_cursor(CursorDirection::Right),
                VirtualKeyCode::Back => self.delete_char(),
                VirtualKeyCode::Return => self.insert_char('\n'),
                VirtualKeyCode::Tab => {
                    self.insert_char(' ');
                    self.insert_char(' ');
                }
                _ => {}
            }
        }
        
        // Handle text input
        for ch in input.get_text_input().chars() {
            self.insert_char(ch);
        }
        
        // Handle scrolling
        if input.mouse.scroll_delta != 0.0 {
            let scroll_lines = (input.mouse.scroll_delta * 3.0) as i32;
            if scroll_lines > 0 {
                self.scroll_offset = self.scroll_offset.saturating_sub(scroll_lines as usize);
            } else {
                let max_scroll = self.get_line_count().saturating_sub(self.max_visible_lines);
                self.scroll_offset = (self.scroll_offset + (-scroll_lines) as usize).min(max_scroll);
            }
        }
    }
    
    fn contains_point(&self, world_pos: Vec3) -> bool {
        self.base.contains_point(world_pos)
    }
    
    fn get_transform(&self) -> &Transform {
        &self.base.transform
    }
    
    fn get_transform_mut(&mut self) -> &mut Transform {
        &mut self.base.transform
    }
    
    fn set_focused(&mut self, focused: bool) {
        self.base.focused = focused;
        if focused {
            self.base.border_color = [0.2, 0.6, 1.0, 1.0]; // Bright blue when focused
        } else {
            self.base.border_color = [0.3, 0.3, 0.3, 1.0]; // Gray when not focused
        }
    }
    
    fn is_focused(&self) -> bool {
        self.base.focused
    }
}
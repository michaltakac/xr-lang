//! ECS systems for 3D UI components

use crate::math::*;
use crate::ui3d::{InputSystem3D, TextRenderer3D, TextVertex, ecs::*};
use hecs::*;
use wgpu::{Device, RenderPass, BufferDescriptor, BufferUsages, util::DeviceExt};
use winit::keyboard::KeyCode;

/// System for updating text editor logic
pub fn update_text_editor_system(world: &mut World, input: &InputSystem3D) {
    for (entity, (transform, layout, content, editor)) in 
        world.query_mut::<(&Transform3D, &mut UILayout, &mut TextContent, &mut TextEditor)>() 
    {
        if !layout.focused || !editor.is_editable {
            continue;
        }
        
        // Handle keyboard input for text editing
        for &key in &input.keyboard.just_pressed_keys {
            match key {
                VirtualKeyCode::Up => move_cursor_up(content, editor),
                VirtualKeyCode::Down => move_cursor_down(content, editor),
                VirtualKeyCode::Left => move_cursor_left(content, editor),
                VirtualKeyCode::Right => move_cursor_right(content, editor),
                VirtualKeyCode::Back => delete_char(content, editor),
                VirtualKeyCode::Return => insert_char(content, editor, '\n'),
                VirtualKeyCode::Tab => {
                    insert_char(content, editor, ' ');
                    insert_char(content, editor, ' ');
                }
                _ => {}
            }
        }
        
        // Handle text input
        for ch in input.get_text_input().chars() {
            insert_char(content, editor, ch);
        }
        
        // Handle scrolling
        if input.mouse.scroll_delta != 0.0 {
            let scroll_lines = (input.mouse.scroll_delta * 3.0) as i32;
            if scroll_lines > 0 {
                editor.scroll_offset = editor.scroll_offset.saturating_sub(scroll_lines as usize);
            } else {
                let max_scroll = content.get_line_count().saturating_sub(content.max_visible_lines);
                editor.scroll_offset = (editor.scroll_offset + (-scroll_lines) as usize).min(max_scroll);
            }
        }
        
        // Mark render data as needing update if content changed
        if let Ok(mut render_data) = world.query_one::<&mut RenderData>(entity) {
            render_data.mark_dirty();
        }
    }
}

/// System for updating button interactions
pub fn update_button_system(world: &mut World, input: &InputSystem3D) {
    for (_entity, (transform, layout, button)) in 
        world.query_mut::<(&Transform3D, &UILayout, &mut Button)>() 
    {
        let was_hovered = button.is_hovered;
        button.is_hovered = layout.contains_point(transform, input.mouse.world_position);
        
        if button.is_hovered && input.mouse.left_just_pressed {
            button.is_pressed = true;
            // Handle button action
            match &button.action {
                ButtonAction::SaveCode => {
                    log::info!("Save code button pressed");
                    // TODO: Implement save functionality
                }
                ButtonAction::LoadFile => {
                    log::info!("Load file button pressed");
                    // TODO: Implement load functionality
                }
                ButtonAction::ClearLog => {
                    log::info!("Clear log button pressed");
                    // TODO: Clear log viewer
                }
                ButtonAction::Custom(action) => {
                    log::info!("Custom button action: {}", action);
                }
            }
        } else {
            button.is_pressed = false;
        }
        
        // Update button appearance based on state
        if let Ok(mut layout) = world.query_one::<&mut UILayout>(_entity) {
            if button.is_pressed {
                layout.background_color = [0.3, 0.6, 1.0, 1.0]; // Blue when pressed
            } else if button.is_hovered {
                layout.background_color = [0.2, 0.2, 0.4, 1.0]; // Dark blue when hovered
            } else {
                layout.background_color = [0.15, 0.15, 0.15, 1.0]; // Default gray
            }
        }
    }
}

/// System for updating scrollable components
pub fn update_scrollable_system(world: &mut World, input: &InputSystem3D) {
    for (entity, (transform, layout, scrollable)) in 
        world.query_mut::<(&Transform3D, &UILayout, &mut Scrollable)>() 
    {
        if layout.contains_point(transform, input.mouse.world_position) && input.mouse.scroll_delta != 0.0 {
            scrollable.scroll(-input.mouse.scroll_delta);
            
            // Mark render data as needing update
            if let Ok(mut render_data) = world.query_one::<&mut RenderData>(entity) {
                render_data.mark_dirty();
            }
        }
    }
}

/// System for handling focus management
pub fn update_focus_system(world: &mut World, input: &InputSystem3D) {
    let mut clicked_entity = None;
    
    // Find what was clicked
    if input.mouse.left_just_pressed {
        for (entity, (transform, layout)) in world.query::<(&Transform3D, &UILayout)>().iter() {
            if layout.contains_point(transform, input.mouse.world_position) {
                clicked_entity = Some(entity);
                break;
            }
        }
    }
    
    // Update focus states
    for (_entity, layout) in world.query_mut::<&mut UILayout>() {
        let should_focus = clicked_entity.map_or(false, |e| e == _entity);
        if layout.focused != should_focus {
            layout.focused = should_focus;
            
            // Update border color based on focus
            if layout.focused {
                layout.border_color = [0.2, 0.6, 1.0, 1.0]; // Bright blue when focused
            } else {
                layout.border_color = [0.3, 0.3, 0.3, 1.0]; // Gray when not focused
            }
        }
    }
}

/// System for updating render meshes
pub fn update_render_system(world: &mut World, device: &Device, text_renderer: &TextRenderer3D) {
    for (entity, (transform, layout, content, render_data)) in 
        world.query_mut::<(&Transform3D, &UILayout, &TextContent, &mut RenderData)>() 
    {
        if !render_data.needs_update {
            continue;
        }
        
        let mut all_vertices = Vec::new();
        let mut all_indices = Vec::new();
        
        // Handle text editor rendering
        if let Ok(editor) = world.query_one::<&TextEditor>(entity) {
            render_text_editor(
                &mut all_vertices, 
                &mut all_indices, 
                transform, 
                layout, 
                content, 
                editor, 
                text_renderer
            );
        } 
        // Handle button rendering
        else if let Ok(button) = world.query_one::<&Button>(entity) {
            render_button(
                &mut all_vertices, 
                &mut all_indices, 
                transform, 
                layout, 
                content, 
                button, 
                text_renderer
            );
        }
        // Handle scrollable content
        else if let Ok(scrollable) = world.query_one::<&Scrollable>(entity) {
            render_scrollable_content(
                &mut all_vertices, 
                &mut all_indices, 
                transform, 
                layout, 
                content, 
                scrollable, 
                text_renderer
            );
        }
        
        // Update GPU buffers
        if !all_vertices.is_empty() {
            render_data.vertex_buffer = Some(device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
                label: Some("UI Component Vertex Buffer"),
                contents: bytemuck::cast_slice(&all_vertices),
                usage: BufferUsages::VERTEX,
            }));
            
            render_data.index_buffer = Some(device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
                label: Some("UI Component Index Buffer"),
                contents: bytemuck::cast_slice(&all_indices),
                usage: BufferUsages::INDEX,
            }));
            
            render_data.index_count = all_indices.len() as u32;
        }
        
        render_data.needs_update = false;
    }
}

/// System for rendering all UI components
pub fn render_system<'a>(world: &'a World, render_pass: &mut RenderPass<'a>) {
    for (_entity, (layout, render_data)) in world.query::<(&UILayout, &RenderData)>().iter() {
        if !layout.visible {
            continue;
        }
        
        if let (Some(vertex_buffer), Some(index_buffer)) = (&render_data.vertex_buffer, &render_data.index_buffer) {
            if render_data.index_count > 0 {
                render_pass.set_vertex_buffer(0, vertex_buffer.slice(..));
                render_pass.set_index_buffer(index_buffer.slice(..), wgpu::IndexFormat::Uint16);
                render_pass.draw_indexed(0..render_data.index_count, 0, 0..1);
            }
        }
    }
}

// Helper functions for text editor operations
fn move_cursor_up(content: &mut TextContent, editor: &mut TextEditor) {
    if editor.cursor_line > 0 {
        editor.cursor_line -= 1;
        let lines = content.get_lines();
        let line_len = lines.get(editor.cursor_line).map_or(0, |l| l.chars().count());
        editor.cursor_col = editor.cursor_col.min(line_len);
        
        if editor.cursor_line < editor.scroll_offset {
            editor.scroll_offset = editor.cursor_line;
        }
    }
}

fn move_cursor_down(content: &mut TextContent, editor: &mut TextEditor) {
    let line_count = content.get_line_count();
    if editor.cursor_line + 1 < line_count {
        editor.cursor_line += 1;
        let lines = content.get_lines();
        let line_len = lines.get(editor.cursor_line).map_or(0, |l| l.chars().count());
        editor.cursor_col = editor.cursor_col.min(line_len);
        
        if editor.cursor_line >= editor.scroll_offset + content.max_visible_lines {
            editor.scroll_offset = editor.cursor_line + 1 - content.max_visible_lines;
        }
    }
}

fn move_cursor_left(content: &TextContent, editor: &mut TextEditor) {
    if editor.cursor_col > 0 {
        editor.cursor_col -= 1;
    } else if editor.cursor_line > 0 {
        editor.cursor_line -= 1;
        let lines = content.get_lines();
        editor.cursor_col = lines.get(editor.cursor_line).map_or(0, |l| l.chars().count());
    }
}

fn move_cursor_right(content: &TextContent, editor: &mut TextEditor) {
    let lines = content.get_lines();
    let line_len = lines.get(editor.cursor_line).map_or(0, |l| l.chars().count());
    if editor.cursor_col < line_len {
        editor.cursor_col += 1;
    } else if editor.cursor_line + 1 < content.get_line_count() {
        editor.cursor_line += 1;
        editor.cursor_col = 0;
    }
}

fn insert_char(content: &mut TextContent, editor: &mut TextEditor, ch: char) {
    if ch.is_control() && ch != '\n' && ch != '\t' {
        return;
    }
    
    let mut lines: Vec<String> = content.get_lines().iter().map(|s| s.to_string()).collect();
    
    // Ensure we have enough lines
    while lines.len() <= editor.cursor_line {
        lines.push(String::new());
    }
    
    let mut chars: Vec<char> = lines[editor.cursor_line].chars().collect();
    chars.insert(editor.cursor_col.min(chars.len()), ch);
    lines[editor.cursor_line] = chars.into_iter().collect();
    
    content.content = lines.join("\n");
    
    if ch == '\n' {
        editor.cursor_line += 1;
        editor.cursor_col = 0;
    } else {
        editor.cursor_col += 1;
    }
}

fn delete_char(content: &mut TextContent, editor: &mut TextEditor) {
    if editor.cursor_col == 0 {
        if editor.cursor_line > 0 {
            let mut lines: Vec<String> = content.get_lines().iter().map(|s| s.to_string()).collect();
            if editor.cursor_line < lines.len() {
                let current = lines.remove(editor.cursor_line);
                editor.cursor_line -= 1;
                editor.cursor_col = lines[editor.cursor_line].len();
                lines[editor.cursor_line].push_str(&current);
                content.content = lines.join("\n");
            }
        }
    } else {
        let mut lines: Vec<String> = content.get_lines().iter().map(|s| s.to_string()).collect();
        if editor.cursor_line < lines.len() {
            let mut chars: Vec<char> = lines[editor.cursor_line].chars().collect();
            if editor.cursor_col > 0 && editor.cursor_col <= chars.len() {
                chars.remove(editor.cursor_col - 1);
                lines[editor.cursor_line] = chars.into_iter().collect();
                editor.cursor_col -= 1;
                content.content = lines.join("\n");
            }
        }
    }
}

fn render_text_editor(
    all_vertices: &mut Vec<TextVertex>,
    all_indices: &mut Vec<u16>,
    transform: &Transform3D,
    layout: &UILayout,
    content: &TextContent,
    editor: &TextEditor,
    text_renderer: &TextRenderer3D,
) {
    let char_size = content.font_size;
    let line_height = char_size * content.line_height;
    let line_number_width = if editor.show_line_numbers { 0.2 } else { 0.0 };
    let lines = content.get_lines();
    
    // Render visible lines
    for (line_idx, actual_line) in lines.iter().enumerate().skip(editor.scroll_offset).take(content.max_visible_lines) {
        let y_pos = -(line_idx - editor.scroll_offset) as f32 * line_height;
        
        // Line numbers
        if editor.show_line_numbers {
            let line_num_text = format!("{:3}", actual_line + 1);
            let line_num_transform = Transform::with_position(
                transform.position + Vec3::new(-layout.size.x / 2.0 + 0.05, y_pos, 0.01)
            );
            
            let (vertices, indices) = text_renderer.create_text_mesh(
                &line_num_text,
                &line_num_transform,
                [0.4, 0.4, 0.4, 1.0]
            );
            
            let base_idx = all_vertices.len() as u16;
            all_vertices.extend(vertices);
            all_indices.extend(indices.iter().map(|i| i + base_idx));
        }
        
        // Line text with syntax highlighting
        if !lines[actual_line].is_empty() {
            let text_transform = Transform::with_position(
                transform.position + Vec3::new(-layout.size.x / 2.0 + line_number_width, y_pos, 0.01)
            );
            
            let color = get_syntax_color(lines[actual_line], &editor.syntax_colors);
            let (vertices, indices) = text_renderer.create_text_mesh(
                lines[actual_line],
                &text_transform,
                color
            );
            
            let base_idx = all_vertices.len() as u16;
            all_vertices.extend(vertices);
            all_indices.extend(indices.iter().map(|i| i + base_idx));
        }
    }
    
    // Render cursor if focused
    if layout.focused && editor.cursor_line >= editor.scroll_offset && 
       editor.cursor_line < editor.scroll_offset + content.max_visible_lines {
        let cursor_y = -(editor.cursor_line - editor.scroll_offset) as f32 * line_height;
        let cursor_x = editor.cursor_col as f32 * char_size;
        
        let cursor_transform = Transform::with_position(
            transform.position + Vec3::new(-layout.size.x / 2.0 + line_number_width + cursor_x, cursor_y, 0.02)
        );
        
        let (vertices, indices) = text_renderer.create_text_mesh(
            "|",
            &cursor_transform,
            [1.0, 1.0, 1.0, 1.0]
        );
        
        let base_idx = all_vertices.len() as u16;
        all_vertices.extend(vertices);
        all_indices.extend(indices.iter().map(|i| i + base_idx));
    }
}

fn render_button(
    all_vertices: &mut Vec<TextVertex>,
    all_indices: &mut Vec<u16>,
    transform: &Transform3D,
    _layout: &UILayout,
    content: &TextContent,
    _button: &Button,
    text_renderer: &TextRenderer3D,
) {
    let text_transform = transform.to_transform();
    let (vertices, indices) = text_renderer.create_text_mesh(
        &content.content,
        &text_transform,
        content.color
    );
    
    let base_idx = all_vertices.len() as u16;
    all_vertices.extend(vertices);
    all_indices.extend(indices.iter().map(|i| i + base_idx));
}

fn render_scrollable_content(
    all_vertices: &mut Vec<TextVertex>,
    all_indices: &mut Vec<u16>,
    transform: &Transform3D,
    layout: &UILayout,
    content: &TextContent,
    scrollable: &Scrollable,
    text_renderer: &TextRenderer3D,
) {
    let lines = content.get_lines();
    let line_height = content.font_size * content.line_height;
    let scroll_lines = (scrollable.scroll_offset / line_height) as usize;
    
    for (line_idx, line_text) in lines.iter().enumerate().skip(scroll_lines).take(content.max_visible_lines) {
        let y_pos = -(line_idx - scroll_lines) as f32 * line_height + scrollable.scroll_offset % line_height;
        
        if y_pos > -layout.size.y / 2.0 && y_pos < layout.size.y / 2.0 {
            let text_transform = Transform::with_position(
                transform.position + Vec3::new(-layout.size.x / 2.0 + 0.05, y_pos, 0.01)
            );
            
            let (vertices, indices) = text_renderer.create_text_mesh(
                line_text,
                &text_transform,
                content.color
            );
            
            let base_idx = all_vertices.len() as u16;
            all_vertices.extend(vertices);
            all_indices.extend(indices.iter().map(|i| i + base_idx));
        }
    }
}

fn get_syntax_color(line: &str, colors: &SyntaxColors) -> [f32; 4] {
    let trimmed = line.trim();
    if trimmed.starts_with(';') {
        colors.comment
    } else if trimmed.starts_with('(') {
        if trimmed.contains("defbehavior") || trimmed.contains("defscene3d") || trimmed.contains("let") {
            colors.keyword
        } else {
            colors.symbol
        }
    } else if trimmed.starts_with('"') {
        colors.string
    } else if trimmed.chars().all(|c| c.is_ascii_digit() || c == '.' || c == '-') {
        colors.number
    } else {
        colors.text
    }
}
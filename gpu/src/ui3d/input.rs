//! 3D Input system for mouse and keyboard interaction

use crate::math::*;
use std::collections::HashSet;
use winit::event::{ElementState, MouseButton};
use winit::keyboard::KeyCode;

#[derive(Debug, Clone)]
pub struct MouseState {
    pub position: [f32; 2],        // Screen coordinates
    pub world_position: Vec3,       // 3D world position from raycast
    pub world_direction: Vec3,      // Ray direction
    pub left_pressed: bool,
    pub left_just_pressed: bool,
    pub right_pressed: bool,
    pub right_just_pressed: bool,
    pub scroll_delta: f32,
}

#[derive(Debug, Clone)]
pub struct KeyboardState {
    pub pressed_keys: HashSet<KeyCode>,
    pub just_pressed_keys: HashSet<KeyCode>,
    pub just_released_keys: HashSet<KeyCode>,
    pub text_input: String,  // Characters typed this frame
}

/// 3D Input system that handles mouse raycasting and keyboard input
pub struct InputSystem3D {
    pub mouse: MouseState,
    pub keyboard: KeyboardState,
    prev_mouse_left: bool,
    prev_mouse_right: bool,
    prev_keys: HashSet<KeyCode>,
}

impl InputSystem3D {
    pub fn new() -> Self {
        Self {
            mouse: MouseState {
                position: [0.0, 0.0],
                world_position: Vec3::ZERO,
                world_direction: Vec3::new(0.0, 0.0, -1.0),
                left_pressed: false,
                left_just_pressed: false,
                right_pressed: false,
                right_just_pressed: false,
                scroll_delta: 0.0,
            },
            keyboard: KeyboardState {
                pressed_keys: HashSet::new(),
                just_pressed_keys: HashSet::new(),
                just_released_keys: HashSet::new(),
                text_input: String::new(),
            },
            prev_mouse_left: false,
            prev_mouse_right: false,
            prev_keys: HashSet::new(),
        }
    }
    
    pub fn update(&mut self, camera: &Camera) {
        // Update just_pressed states
        self.mouse.left_just_pressed = self.mouse.left_pressed && !self.prev_mouse_left;
        self.mouse.right_just_pressed = self.mouse.right_pressed && !self.prev_mouse_right;
        
        self.prev_mouse_left = self.mouse.left_pressed;
        self.prev_mouse_right = self.mouse.right_pressed;
        
        // Update keyboard just pressed/released
        self.keyboard.just_pressed_keys.clear();
        self.keyboard.just_released_keys.clear();
        
        for key in &self.keyboard.pressed_keys {
            if !self.prev_keys.contains(key) {
                self.keyboard.just_pressed_keys.insert(*key);
            }
        }
        
        for key in &self.prev_keys {
            if !self.keyboard.pressed_keys.contains(key) {
                self.keyboard.just_released_keys.insert(*key);
            }
        }
        
        self.prev_keys = self.keyboard.pressed_keys.clone();
        
        // Calculate world ray from mouse position
        self.update_mouse_ray(camera);
        
        // Clear per-frame data
        self.mouse.scroll_delta = 0.0;
        self.keyboard.text_input.clear();
    }
    
    fn update_mouse_ray(&mut self, camera: &Camera) {
        // Convert screen coordinates to normalized device coordinates
        let ndc_x = (self.mouse.position[0] / 800.0) * 2.0 - 1.0;  // Assume 800px width for now
        let ndc_y = -((self.mouse.position[1] / 600.0) * 2.0 - 1.0); // Assume 600px height, flip Y
        
        // Create ray from camera through mouse position
        let ray_origin = camera.position;
        
        // This is a simplified ray calculation - in practice you'd use the inverse view-projection matrix
        let forward = Vec3::new(0.0, 0.0, -1.0); // Simplified
        let right = Vec3::new(1.0, 0.0, 0.0);
        let up = Vec3::new(0.0, 1.0, 0.0);
        
        let ray_direction = (forward + right * ndc_x * 0.5 + up * ndc_y * 0.5).normalize();
        
        self.mouse.world_direction = ray_direction;
        
        // Cast ray to find intersection with UI plane (z = 0 for now)
        let t = -ray_origin.z / ray_direction.z;
        if t > 0.0 {
            self.mouse.world_position = ray_origin + ray_direction * t;
        }
    }
    
    // Event handlers
    pub fn handle_mouse_button(&mut self, button: MouseButton, state: ElementState) {
        let pressed = state == ElementState::Pressed;
        match button {
            MouseButton::Left => self.mouse.left_pressed = pressed,
            MouseButton::Right => self.mouse.right_pressed = pressed,
            _ => {}
        }
    }
    
    pub fn handle_mouse_move(&mut self, position: [f32; 2]) {
        self.mouse.position = position;
    }
    
    pub fn handle_mouse_scroll(&mut self, delta: f32) {
        self.mouse.scroll_delta = delta;
    }
    
    pub fn handle_keyboard(&mut self, keycode: KeyCode, state: ElementState) {
        match state {
            ElementState::Pressed => {
                self.keyboard.pressed_keys.insert(keycode);
            }
            ElementState::Released => {
                self.keyboard.pressed_keys.remove(&keycode);
            }
        }
    }
    
    pub fn handle_character_input(&mut self, character: char) {
        self.keyboard.text_input.push(character);
    }
    
    // Helper methods
    pub fn is_key_pressed(&self, key: KeyCode) -> bool {
        self.keyboard.pressed_keys.contains(&key)
    }
    
    pub fn is_key_just_pressed(&self, key: KeyCode) -> bool {
        self.keyboard.just_pressed_keys.contains(&key)
    }
    
    pub fn is_key_just_released(&self, key: KeyCode) -> bool {
        self.keyboard.just_released_keys.contains(&key)
    }
    
    /// Get text that should be added to input this frame
    pub fn get_text_input(&self) -> &str {
        &self.keyboard.text_input
    }
    
    /// Check for special key combinations
    pub fn is_ctrl_pressed(&self) -> bool {
        self.is_key_pressed(KeyCode::ControlLeft) || self.is_key_pressed(KeyCode::ControlRight)
    }
    
    pub fn is_shift_pressed(&self) -> bool {
        self.is_key_pressed(KeyCode::ShiftLeft) || self.is_key_pressed(KeyCode::ShiftRight)
    }
}
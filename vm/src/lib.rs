//! Virtual Machine for XR-DSL
//!
//! Provides object space, heap management, and hot-swapping capabilities.

use std::collections::HashMap;

#[repr(C)]
pub struct World {
    pub panel_color: [f32; 4],
    pub state: HashMap<u32, Vec<f32>>,
}

impl World {
    pub fn new() -> Self {
        Self {
            panel_color: [0.2, 0.8, 1.0, 1.0],
            state: HashMap::new(),
        }
    }
}

impl Default for World {
    fn default() -> Self {
        Self::new()
    }
}

pub mod hotswap;
pub mod ffi;

//! JIT Compilation for XR-DSL using Cranelift

pub mod cranelift_backend;
pub mod loader;
pub mod symbols;
pub mod vtable;

pub use cranelift_backend::*;
pub use loader::*;
pub use symbols::*;
pub use vtable::*;

use std::collections::HashMap;
use std::sync::{RwLock, LazyLock};

// Global symbol registry for extern calls
static SYMBOLS: LazyLock<RwLock<HashMap<String, usize>>> = LazyLock::new(|| RwLock::new(HashMap::new()));

/// Register a symbol for JIT code to call
pub fn register_symbol(name: &str, ptr: *const u8) {
    SYMBOLS.write().unwrap().insert(name.to_string(), ptr as usize);
}

/// Resolve a symbol by name
pub fn resolve_symbol(name: &str) -> Option<*const u8> {
    SYMBOLS.read().unwrap().get(name).copied().map(|addr| addr as *const u8)
}

/// Initialize the JIT with common symbols
pub fn init_jit() {
    register_symbol("state_get_f32", vm::ffi::state_get_f32 as *const u8);
    register_symbol("state_set_f32", vm::ffi::state_set_f32 as *const u8);
    register_symbol("gpu_panel_set_color", vm::ffi::gpu_panel_set_color as *const u8);
    register_symbol("fast_sin_f32", vm::ffi::fast_sin_f32 as *const u8);
    register_symbol("fast_cos_f32", vm::ffi::fast_cos_f32 as *const u8);
    register_symbol("fast_sqrt_f32", vm::ffi::fast_sqrt_f32 as *const u8);
}
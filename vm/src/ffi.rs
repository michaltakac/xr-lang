//! FFI interface for JIT-compiled code to interact with the VM

use crate::World;
use std::sync::atomic::{AtomicPtr, Ordering};

#[repr(C)]
pub struct BehaviorVTable {
    pub init: extern "C" fn(entity: u32, world: *mut World) -> bool,
    pub update: extern "C" fn(entity: u32, world: *mut World, dt: f32),
    pub on_select: extern "C" fn(entity: u32, world: *mut World),
    pub drop_fn: extern "C" fn(entity: u32, world: *mut World),
}

unsafe impl Send for BehaviorVTable {}
unsafe impl Sync for BehaviorVTable {}

static CURRENT_VTABLE: AtomicPtr<BehaviorVTable> = AtomicPtr::new(std::ptr::null_mut());

pub fn swap_vtable(vt: *mut BehaviorVTable) {
    CURRENT_VTABLE.store(vt, Ordering::SeqCst);
}

pub fn get_vtable() -> *mut BehaviorVTable {
    CURRENT_VTABLE.load(Ordering::Relaxed)
}

// Default implementations
extern "C" fn default_init(_entity: u32, _world: *mut World) -> bool {
    true
}

extern "C" fn default_update(_entity: u32, _world: *mut World, _dt: f32) {}

extern "C" fn default_on_select(_entity: u32, _world: *mut World) {}

extern "C" fn default_drop(_entity: u32, _world: *mut World) {}

pub fn create_default_vtable() -> Box<BehaviorVTable> {
    Box::new(BehaviorVTable {
        init: default_init,
        update: default_update,
        on_select: default_on_select,
        drop_fn: default_drop,
    })
}

// Host-provided functions that JIT code can call
#[no_mangle]
pub extern "C" fn state_get_f32(world: *mut World, entity: u32, slot: i32) -> f32 {
    let w = unsafe { &mut *world };
    w.state
        .entry(entity)
        .or_default()
        .get(slot as usize)
        .cloned()
        .unwrap_or(0.0)
}

#[no_mangle]
pub extern "C" fn state_set_f32(world: *mut World, entity: u32, slot: i32, value: f32) {
    let w = unsafe { &mut *world };
    let v = w.state.entry(entity).or_default();
    if v.len() <= slot as usize {
        v.resize(slot as usize + 1, 0.0);
    }
    v[slot as usize] = value;
}

#[no_mangle]
pub extern "C" fn gpu_panel_set_color(world: *mut World, r: f32, g: f32, b: f32, a: f32) {
    unsafe {
        (*world).panel_color = [r, g, b, a];
    }
}

#[no_mangle]
pub extern "C" fn fast_sin_f32(x: f32) -> f32 {
    x.sin()
}

#[no_mangle]
pub extern "C" fn fast_cos_f32(x: f32) -> f32 {
    x.cos()
}

#[no_mangle]
pub extern "C" fn fast_sqrt_f32(x: f32) -> f32 {
    x.sqrt()
}
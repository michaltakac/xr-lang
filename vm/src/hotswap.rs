//! Hot-swapping support for live code updates

use crate::{World, ffi::BehaviorVTable};

pub struct CompiledUnit {
    pub vtable_ptr: *mut BehaviorVTable,
}

unsafe impl Send for CompiledUnit {}
unsafe impl Sync for CompiledUnit {}

pub fn apply_unit(unit: CompiledUnit, _world: &mut World) -> anyhow::Result<()> {
    // TODO: Implement atomic swap at safe point
    // For now, just update the vtable
    crate::ffi::swap_vtable(unit.vtable_ptr);
    Ok(())
}
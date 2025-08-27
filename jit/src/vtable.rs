//! Behavior VTable for hot-swapping

pub use vm::ffi::BehaviorVTable;
use std::sync::atomic::{AtomicPtr, Ordering};

static CURRENT_VTABLE: AtomicPtr<BehaviorVTable> = AtomicPtr::new(std::ptr::null_mut());

pub fn swap(vt: *mut BehaviorVTable) {
    CURRENT_VTABLE.store(vt, Ordering::SeqCst);
}

pub fn get() -> *mut BehaviorVTable {
    CURRENT_VTABLE.load(Ordering::Relaxed)
}
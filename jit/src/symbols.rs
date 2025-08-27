//! Symbol resolution for JIT

pub fn register(name: &str, ptr: *const u8) {
    crate::register_symbol(name, ptr);
}

pub fn resolve(name: &str) -> Option<*const u8> {
    crate::resolve_symbol(name)
}
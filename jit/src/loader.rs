//! Executable memory loader

pub trait Loader {
    fn allocate_executable(&mut self, size: usize, align: usize) -> anyhow::Result<*mut u8>;
    fn publish(&mut self, ptr: *mut u8, code: &[u8]) -> anyhow::Result<()>;
    fn make_executable(&mut self, ptr: *mut u8, size: usize) -> anyhow::Result<()>;
    fn resolve(&self, symbol: &str) -> Option<*const u8>;
}

pub struct SimpleLoader {
    // TODO: Implement memory management
}

impl SimpleLoader {
    pub fn new() -> Self {
        Self {}
    }
}

impl Loader for SimpleLoader {
    fn allocate_executable(&mut self, _size: usize, _align: usize) -> anyhow::Result<*mut u8> {
        // TODO: Allocate RWX memory
        anyhow::bail!("not implemented")
    }
    
    fn publish(&mut self, _ptr: *mut u8, _code: &[u8]) -> anyhow::Result<()> {
        // TODO: Copy code to executable memory
        Ok(())
    }
    
    fn make_executable(&mut self, _ptr: *mut u8, _size: usize) -> anyhow::Result<()> {
        // TODO: Set memory permissions to RX
        Ok(())
    }
    
    fn resolve(&self, symbol: &str) -> Option<*const u8> {
        crate::resolve_symbol(symbol)
    }
}
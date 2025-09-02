//! High-performance GPU memory pool inspired by UE5's memory management
//! Minimizes allocations and provides persistent mapped buffers

use wgpu::*;
use std::collections::VecDeque;

/// Ring buffer for efficient GPU memory streaming
pub struct RingBuffer {
    buffer: Buffer,
    size: u64,
    write_offset: u64,
    frame_offsets: VecDeque<(u64, u64)>,  // (start, end) per frame
    current_frame: u64,
}

impl RingBuffer {
    pub fn new(device: &Device, size: u64, usage: BufferUsages) -> Self {
        let buffer = device.create_buffer(&BufferDescriptor {
            label: Some("Ring Buffer"),
            size,
            usage: usage | BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });
        
        Self {
            buffer,
            size,
            write_offset: 0,
            frame_offsets: VecDeque::with_capacity(4),  // Track last 4 frames
            current_frame: 0,
        }
    }
    
    /// Allocate space in the ring buffer
    pub fn allocate(&mut self, size: u64, alignment: u64) -> Option<u64> {
        // Align offset
        let aligned_offset = (self.write_offset + alignment - 1) & !(alignment - 1);
        
        // Check if we need to wrap
        if aligned_offset + size > self.size {
            // Wrap to beginning
            self.write_offset = 0;
            let aligned_offset = 0;
            
            // Check if we have space at the beginning
            if let Some((oldest_start, _)) = self.frame_offsets.front() {
                if aligned_offset + size > *oldest_start {
                    return None;  // Buffer full, need to wait
                }
            }
        }
        
        let offset = aligned_offset;
        self.write_offset = aligned_offset + size;
        
        Some(offset)
    }
    
    /// Mark end of frame
    pub fn end_frame(&mut self) {
        self.frame_offsets.push_back((0, self.write_offset));
        if self.frame_offsets.len() > 4 {
            self.frame_offsets.pop_front();
        }
        self.current_frame += 1;
    }
    
    pub fn buffer(&self) -> &Buffer {
        &self.buffer
    }
}

/// Memory pool for different buffer types
pub struct GPUMemoryPool {
    // Pools for different usage patterns
    vertex_pool: BufferPool,
    index_pool: BufferPool,
    uniform_pool: BufferPool,
    storage_pool: BufferPool,
    staging_pool: BufferPool,
    
    // Ring buffers for streaming data
    uniform_ring: RingBuffer,
    vertex_ring: RingBuffer,
}

struct BufferPool {
    buffers: Vec<PooledBuffer>,
    free_list: Vec<usize>,
    block_size: u64,
    usage: BufferUsages,
}

struct PooledBuffer {
    buffer: Buffer,
    in_use: bool,
    last_used_frame: u64,
}

impl BufferPool {
    fn new(block_size: u64, usage: BufferUsages) -> Self {
        Self {
            buffers: Vec::new(),
            free_list: Vec::new(),
            block_size,
            usage,
        }
    }
    
    fn allocate(&mut self, device: &Device) -> BufferHandle {
        if let Some(index) = self.free_list.pop() {
            self.buffers[index].in_use = true;
            BufferHandle {
                pool_index: index,
            }
        } else {
            // Allocate new buffer
            let buffer = device.create_buffer(&BufferDescriptor {
                label: Some("Pooled Buffer"),
                size: self.block_size,
                usage: self.usage,
                mapped_at_creation: false,
            });
            
            let index = self.buffers.len();
            self.buffers.push(PooledBuffer {
                buffer,
                in_use: true,
                last_used_frame: 0,
            });
            
            BufferHandle {
                pool_index: index,
            }
        }
    }
    
    fn get_buffer(&self, handle: &BufferHandle) -> Option<&Buffer> {
        self.buffers.get(handle.pool_index).map(|b| &b.buffer)
    }
    
    fn release(&mut self, handle: BufferHandle) {
        if handle.pool_index < self.buffers.len() {
            self.buffers[handle.pool_index].in_use = false;
            self.free_list.push(handle.pool_index);
        }
    }
    
    fn garbage_collect(&mut self, current_frame: u64) {
        // Free buffers not used for 100+ frames
        let threshold = current_frame.saturating_sub(100);
        
        self.buffers.retain(|buffer| {
            buffer.in_use || buffer.last_used_frame > threshold
        });
    }
}

pub struct BufferHandle {
    pool_index: usize,
}

impl GPUMemoryPool {
    pub fn new(device: &Device) -> Self {
        let vertex_pool = BufferPool::new(
            16 * 1024 * 1024,  // 16MB blocks
            BufferUsages::VERTEX | BufferUsages::COPY_DST,
        );
        
        let index_pool = BufferPool::new(
            8 * 1024 * 1024,  // 8MB blocks
            BufferUsages::INDEX | BufferUsages::COPY_DST,
        );
        
        let uniform_pool = BufferPool::new(
            256 * 1024,  // 256KB blocks
            BufferUsages::UNIFORM | BufferUsages::COPY_DST,
        );
        
        let storage_pool = BufferPool::new(
            64 * 1024 * 1024,  // 64MB blocks
            BufferUsages::STORAGE | BufferUsages::COPY_DST,
        );
        
        let staging_pool = BufferPool::new(
            16 * 1024 * 1024,  // 16MB blocks
            BufferUsages::MAP_WRITE | BufferUsages::COPY_SRC,
        );
        
        // Ring buffers for frequently updated data
        let uniform_ring = RingBuffer::new(
            device,
            4 * 1024 * 1024,  // 4MB ring buffer
            BufferUsages::UNIFORM | BufferUsages::COPY_DST,
        );
        
        let vertex_ring = RingBuffer::new(
            device,
            32 * 1024 * 1024,  // 32MB ring buffer
            BufferUsages::VERTEX | BufferUsages::COPY_DST,
        );
        
        Self {
            vertex_pool,
            index_pool,
            uniform_pool,
            storage_pool,
            staging_pool,
            uniform_ring,
            vertex_ring,
        }
    }
    
    pub fn allocate_vertex_buffer(&mut self, device: &Device) -> BufferHandle {
        self.vertex_pool.allocate(device)
    }
    
    pub fn allocate_index_buffer(&mut self, device: &Device) -> BufferHandle {
        self.index_pool.allocate(device)
    }
    
    pub fn allocate_uniform_buffer(&mut self, device: &Device) -> BufferHandle {
        self.uniform_pool.allocate(device)
    }
    
    pub fn allocate_storage_buffer(&mut self, device: &Device) -> BufferHandle {
        self.storage_pool.allocate(device)
    }
    
    pub fn allocate_staging_buffer(&mut self, device: &Device) -> BufferHandle {
        self.staging_pool.allocate(device)
    }
    
    pub fn get_vertex_buffer(&self, handle: &BufferHandle) -> Option<&Buffer> {
        self.vertex_pool.get_buffer(handle)
    }
    
    pub fn get_index_buffer(&self, handle: &BufferHandle) -> Option<&Buffer> {
        self.index_pool.get_buffer(handle)
    }
    
    pub fn get_uniform_buffer(&self, handle: &BufferHandle) -> Option<&Buffer> {
        self.uniform_pool.get_buffer(handle)
    }
    
    pub fn get_storage_buffer(&self, handle: &BufferHandle) -> Option<&Buffer> {
        self.storage_pool.get_buffer(handle)
    }
    
    pub fn get_staging_buffer(&self, handle: &BufferHandle) -> Option<&Buffer> {
        self.staging_pool.get_buffer(handle)
    }
    
    /// Allocate space in uniform ring buffer for per-frame data
    pub fn allocate_uniform_ring(&mut self, size: u64) -> Option<(u64, &Buffer)> {
        let offset = self.uniform_ring.allocate(size, 256)?;  // 256-byte alignment for uniforms
        Some((offset, self.uniform_ring.buffer()))
    }
    
    /// Allocate space in vertex ring buffer for dynamic vertices
    pub fn allocate_vertex_ring(&mut self, size: u64) -> Option<(u64, &Buffer)> {
        let offset = self.vertex_ring.allocate(size, 16)?;  // 16-byte alignment for vertices
        Some((offset, self.vertex_ring.buffer()))
    }
    
    pub fn end_frame(&mut self, frame_number: u64) {
        self.uniform_ring.end_frame();
        self.vertex_ring.end_frame();
        
        // Periodic garbage collection
        if frame_number % 100 == 0 {
            self.vertex_pool.garbage_collect(frame_number);
            self.index_pool.garbage_collect(frame_number);
            self.uniform_pool.garbage_collect(frame_number);
            self.storage_pool.garbage_collect(frame_number);
            self.staging_pool.garbage_collect(frame_number);
        }
    }
}
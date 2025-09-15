//! Hot-reload system for XR-Lang bytecode compilation and network transfer

use anyhow::{Context, Result};
use std::sync::mpsc::{Sender, Receiver};
use std::path::Path;
use std::net::{TcpListener, TcpStream};
use std::io::{Read, Write};

use vm::{
    image::{XRLangImage, Platform},
    VM,
};

/// Hot-reload server that listens for code updates
pub struct HotReloadServer {
    listener: TcpListener,
    tx: Sender<Vec<u8>>,
}

impl HotReloadServer {
    /// Create a new hot-reload server on the specified port
    pub fn new(port: u16) -> Result<(Self, Receiver<Vec<u8>>)> {
        let (tx, rx) = std::sync::mpsc::channel();
        let listener = TcpListener::bind(format!("0.0.0.0:{}", port))?;
        listener.set_nonblocking(true)?;
        
        log::info!("Hot-reload server listening on port {}", port);
        
        Ok((HotReloadServer { listener, tx }, rx))
    }
    
    /// Check for incoming connections and handle them
    pub fn poll(&self) -> Result<()> {
        match self.listener.accept() {
            Ok((mut stream, addr)) => {
                log::info!("Hot-reload connection from {}", addr);
                self.handle_connection(stream)?;
            }
            Err(e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                // No connections available, this is normal
            }
            Err(e) => {
                log::error!("Failed to accept connection: {}", e);
            }
        }
        Ok(())
    }
    
    /// Handle a hot-reload connection
    fn handle_connection(&self, mut stream: TcpStream) -> Result<()> {
        // Read the incoming data
        let mut buffer = Vec::new();
        stream.read_to_end(&mut buffer)?;
        
        // Check if this is XR-Lang source code or compiled bytecode
        if buffer.starts_with(b"// XR-Lang") || buffer.starts_with(b"(") {
            // This is source code, compile it
            log::info!("Received {} bytes of XR-Lang source", buffer.len());
            let compiled = compile_xrlang(&buffer)?;
            let _ = self.tx.send(compiled);
        } else {
            // This is already compiled bytecode/image
            log::info!("Received {} bytes of compiled bytecode", buffer.len());
            let _ = self.tx.send(buffer);
        }
        
        // Send acknowledgment
        stream.write_all(b"OK")?;
        Ok(())
    }
}

/// Compile XR-Lang source code to bytecode
pub fn compile_xrlang(source: &[u8]) -> Result<Vec<u8>> {
    let source_str = std::str::from_utf8(source)
        .context("Invalid UTF-8 in source code")?;
    
    // TODO: Integrate with actual DSL compiler when available
    // For now, create a minimal test image
    log::info!("Compiling {} bytes of XR-Lang source", source_str.len());
    
    // Create a test image with minimal bytecode
    let image = XRLangImage {
        version: "1.0".to_string(),
        platform: Platform::MetaQuest,
        created_at: std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs(),
        globals: std::collections::HashMap::new(),
        metadata: vm::image::ImageMetadata {
            name: "hot-reload".to_string(),
            author: "XR-Lang".to_string(),
            description: "Hot-reloaded code".to_string(),
            xr_lang_version: "1.0".to_string(),
            dependencies: vec![],
            entry_point: "main".to_string(),
            checksum: "".to_string(),
        },
        capabilities_required: vec![],
        snapshot_data: None,
        bytecode: vec![], // Empty bytecode for now
    };
    
    // Serialize the image
    image.serialize()
        .map_err(|e| anyhow::anyhow!("Failed to serialize image: {}", e))
}

/// Apply hot-reloaded code to the VM
pub fn apply_hot_reload(vm: &mut VM, image_data: &[u8]) -> Result<()> {
    let image = XRLangImage::deserialize(image_data)
        .map_err(|e| anyhow::anyhow!("Failed to deserialize image: {}", e))?;
    
    // TODO: Execute bytecode when VM supports it
    // For now, just log that we received the update
    log::info!("Received hot-reload image: {}", image.metadata.name);
    log::info!("Bytecode size: {} bytes", image.bytecode.len());
    
    // Update VM state if there's bytecode
    if !image.bytecode.is_empty() {
        // TODO: vm.load_bytecode(&image.bytecode)?;
        log::info!("Would execute {} bytes of bytecode", image.bytecode.len());
    }
    
    log::info!("Hot-reload processed successfully");
    Ok(())
}

/// Hot-reload client for sending code from development machine
pub struct HotReloadClient {
    target_addr: String,
}

impl HotReloadClient {
    pub fn new(target_addr: String) -> Self {
        HotReloadClient { target_addr }
    }
    
    /// Send a file to the hot-reload server
    pub fn send_file(&self, path: &Path) -> Result<()> {
        let content = std::fs::read(path)
            .context("Failed to read file")?;
        
        self.send_data(&content)
    }
    
    /// Send raw data to the hot-reload server
    pub fn send_data(&self, data: &[u8]) -> Result<()> {
        let mut stream = TcpStream::connect(&self.target_addr)
            .context("Failed to connect to hot-reload server")?;
        
        stream.write_all(data)
            .context("Failed to send data")?;
        
        // Read acknowledgment
        let mut response = [0u8; 2];
        stream.read_exact(&mut response)
            .context("Failed to read acknowledgment")?;
        
        if &response == b"OK" {
            log::info!("Hot-reload successful");
            Ok(())
        } else {
            anyhow::bail!("Hot-reload failed: unexpected response");
        }
    }
}
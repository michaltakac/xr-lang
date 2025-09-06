//! Virtual Machine for XR-Lang
//!
//! Core execution engine providing:
//! - Homoiconic value system (code as data)
//! - Stack-based bytecode VM
//! - Event-sourced persistence with time-travel
//! - Hot-swapping capabilities

use std::collections::HashMap;

// Legacy World structure (to be refactored)
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

// Core modules for XR-Lang
pub mod value;        // Homoiconic value system
pub mod bytecode;     // Stack-based bytecode VM
pub mod persistence;  // Journal & snapshot store
pub mod parser;       // EDN-like S-expression parser
pub mod intrinsics;   // Scene primitives as native functions

// Cross-platform support modules
pub mod image;        // Portable image structure for deployment
pub mod capability;   // Platform capability abstraction

// Simplified hot-reload system (replaces complex reconciliation)
pub mod hotreload;            // Hot-reload manager with explicit policies
pub mod scene_differ;         // Minimal scene change detection
pub mod preservation_manager; // State preservation across reloads
pub mod hotswap_coordinator;  // Main hot-swap coordination

// Legacy modules (to be refactored)
pub mod hotswap;
pub mod ffi;
pub mod interpreter;

// Re-export key types
pub use value::{Value, Symbol, Environment};
pub use bytecode::{OpCode, ByteCode, VM};
pub use persistence::{Journal, State, PersistenceLayer};
pub use interpreter::*;

// Re-export cross-platform types
pub use image::{XRLangImage, ImageBuilder, Platform, ImageMetadata};
pub use capability::{CapabilityTable, Capability, PlatformInfo, DeviceType};

// Re-export hot-reload types
pub use hotreload::{HotReloadManager, ReloadPolicy, PreservationMeta};
pub use scene_differ::{SceneDiffer, DiffResult, SceneChange};
pub use preservation_manager::{PreservationManager, PreservedState, PreservationHeuristics};
pub use hotswap_coordinator::{HotSwapCoordinator, AuthoringMode, HotSwapConfig, HotSwapEvent, HotSwapStats};

//! 3D UI system for immersive development environment

pub mod simple;
pub use simple::*;

// Full ECS implementation (disabled for compilation issues)
// pub mod component;
// pub mod text_renderer;  
// pub mod input;
// pub mod editor;
// pub mod ecs;
// pub mod systems;

// pub use component::*;
// pub use text_renderer::*;
// pub use input::*;
// pub use editor::*;
// pub use ecs::*;

// Re-export the simple UI system as UI3DSystem for compatibility
pub use SimpleUI3D as UI3DSystem;
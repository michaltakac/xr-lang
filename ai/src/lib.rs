//! AI integration for voice and code generation

pub mod stt;
pub mod ollama;
pub mod claude;
pub mod tools;

pub use stt::*;
pub use ollama::*;
pub use claude::*;
pub use tools::*;
//! XR-DSL Language Frontend
//!
//! Provides parsing, AST, macro expansion, and lowering to IR.

pub mod ast;
pub mod parser;
pub mod macros;
pub mod lower;

pub use ast::*;
pub use parser::parse;
pub use lower::lower_file;

// Convenience function for simple DSL evaluation
pub fn eval_simple(src: &str) -> anyhow::Result<ir::Module> {
    let tops = parse(src)?;
    lower_file(&tops)
}
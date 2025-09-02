//! XR-DSL Language Frontend
//!
//! Provides parsing, AST, macro expansion, and lowering to IR.

pub mod ast;
pub mod parser;
pub mod parser_helpers;
pub mod macros;
pub mod lower;
pub mod color;
pub mod scene_macros;
pub mod error;
pub mod position_tracker;
pub mod repl_formatter;
pub mod logger;

pub use ast::*;
pub use parser::parse;
pub use lower::lower_file;

// Convenience function for simple DSL evaluation
pub fn eval_simple(src: &str) -> anyhow::Result<ir::Module> {
    let tops = parse(src).map_err(|e| anyhow::anyhow!("{}", e))?;
    lower_file(&tops)
}
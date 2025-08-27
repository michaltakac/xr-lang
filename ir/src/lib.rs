//! Intermediate Representation for XR-DSL
//!
//! This crate provides a typed SSA (Static Single Assignment) intermediate representation
//! that serves as the target for DSL compilation and input to the JIT compiler.

pub mod ssa;
pub mod ty;

pub use ssa::*;
pub use ty::*;

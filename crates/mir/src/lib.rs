//! MIR - Mid-level Intermediate Representation for SkyLow
//!
//! This crate provides a register-based IR for lowering from BaseLang AST.

pub mod ir;
pub mod lower;

pub use ir::{BinOp, CmpOp, Inst, MirFunction, MirProgram, Reg};
pub use lower::lower_program;

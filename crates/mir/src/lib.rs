//! MIR - Mid-level Intermediate Representation for SkyLow
//!
//! This crate provides a register-based IR for lowering from BaseLang AST.

pub mod ir;
pub mod lower;

pub use ir::{
    AssertInfo, BinOp, CmpOp, FunctionDebugInfo, FunctionKind, Inst, InstKind, MirFunction,
    MirParam, MirProgram, Reg, SourceSpan,
};
pub use lower::lower_program;

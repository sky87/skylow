//! Codegen - Shared code generation infrastructure for SkyLow
//!
//! This crate provides common AArch64 code generation utilities shared
//! between the JIT compiler and ELF binary generator.

pub mod aarch64;

pub use aarch64::emit::{ArmReg, Cond, Emitter};
pub use aarch64::lower::{lower_instructions, lower_instructions_with_debug, Backend, LowerResult};
pub use aarch64::regalloc::SimpleRegAlloc;

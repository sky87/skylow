//! JIT - Just-In-Time Compiler for SkyLow
//!
//! This crate compiles MIR to native machine code and executes it.

mod aarch64;
mod exec;

pub use aarch64::codegen::{compile_function, CompiledTest};
pub use exec::ExecutableMemory;

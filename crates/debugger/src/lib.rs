//! SkyDbg debugger library
//!
//! This crate provides the ptrace-based debugger backend and REPL for SkyLow.
//! Core debugger logic (commands, session, script) lives in the `debug` crate.

pub mod repl;
pub mod target;

pub use target::{PtraceTarget, Registers};

// Re-export commonly used items from `debug` for convenience
pub use debug::commands::parse;
pub use debug::{Command, Session};

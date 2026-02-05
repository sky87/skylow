//! Debugger crate for SkyLow
//!
//! This crate provides debugging support for JIT-compiled and ELF code,
//! including breakpoint management, stepping, and variable inspection.
//!
//! The main types are:
//! - [`Debugger`] - Main debugger instance
//! - [`Breakpoint`] - Breakpoint representation
//! - [`DebugState`] - Current debugging state (running, stopped, etc.)

pub mod arch;
pub mod breakpoints;
pub mod commands;
pub mod inspection;
pub mod session;
pub mod script;
pub mod source_map;
pub mod stepping;
pub mod target;
pub mod types;

pub use arch::ArchInfo;
pub use breakpoints::BreakpointManager;
pub use commands::Command;
pub use inspection::Inspector;
pub use session::Session;
pub use source_map::SourceMapper;
pub use stepping::Stepper;
pub use target::Target;
pub use types::{
    Breakpoint, BreakpointId, BreakpointKind, DebugState, Debugger, RegisterValue, StackFrame,
    StopReason,
};

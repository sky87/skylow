//! Debugger crate for SkyLow
//!
//! This crate provides debugging support for JIT-compiled and ELF code,
//! including breakpoint management, stepping, and variable inspection.
//!
//! The main types are:
//! - [`Debugger`] - Main debugger instance
//! - [`Breakpoint`] - Breakpoint representation
//! - [`DebugState`] - Current debugging state (running, stopped, etc.)

pub mod breakpoints;
pub mod inspection;
pub mod source_map;
pub mod stepping;
pub mod types;

pub use breakpoints::BreakpointManager;
pub use inspection::Inspector;
pub use source_map::SourceMapper;
pub use stepping::Stepper;
pub use types::{
    Breakpoint, BreakpointId, BreakpointKind, DebugState, Debugger, RegisterValue, StackFrame,
    StopReason,
};

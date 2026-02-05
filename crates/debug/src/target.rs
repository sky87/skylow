//! Target abstraction for process control
//!
//! The [`Target`] trait abstracts over platform-specific process control mechanisms
//! (e.g., ptrace on Linux), allowing the debugger session logic to be platform-independent.

use crate::arch::ArchInfo;
use crate::types::StopReason;

/// Abstract interface for controlling a debug target process.
///
/// Implementors provide platform-specific mechanisms for starting, stopping,
/// stepping, and inspecting a target process.
pub trait Target {
    /// Start the target process and wait for it to stop.
    fn start(&mut self) -> Result<StopReason, String>;

    /// Continue execution until the next stop event.
    fn cont(&mut self) -> Result<StopReason, String>;

    /// Single-step one instruction.
    fn step(&mut self) -> Result<StopReason, String>;

    /// Continue past a breakpoint at the given address.
    ///
    /// Restores the original instruction, steps past it, re-installs the breakpoint,
    /// then continues execution.
    fn continue_past_breakpoint(&mut self, addr: u64) -> Result<StopReason, String>;

    /// Step past a breakpoint at the given address.
    ///
    /// Restores the original instruction, steps past it, then re-installs the breakpoint.
    fn step_past_breakpoint(&mut self, addr: u64) -> Result<StopReason, String>;

    /// Read the program counter.
    fn pc(&self) -> Result<u64, String>;

    /// Read a general-purpose register by index.
    fn gpr(&self, index: usize) -> Result<u64, String>;

    /// Read the stack pointer.
    fn sp(&self) -> Result<u64, String>;

    /// Read the processor status register (e.g. pstate on AArch64, eflags on x86_64).
    fn status_reg(&self) -> Result<u64, String>;

    /// Read all general-purpose registers. Returns (index, value) pairs.
    fn all_gprs(&self) -> Result<Vec<(usize, u64)>, String>;

    /// Set a breakpoint at the given address.
    fn set_breakpoint(&mut self, addr: u64) -> Result<(), String>;

    /// Remove a breakpoint at the given address. Returns true if one was removed.
    fn remove_breakpoint(&mut self, addr: u64) -> Result<bool, String>;

    /// Check if the target process is currently running (started and not exited).
    fn is_running(&self) -> bool;

    /// Unwind the stack, returning (frame_pointer, address) pairs.
    ///
    /// The first entry is (current_fp, current_pc). Subsequent entries are
    /// (saved_fp, return_address). At most `max_frames` frames are returned.
    fn unwind_stack(&self, max_frames: usize) -> Result<Vec<(u64, u64)>, String>;

    /// Kill the target process.
    fn kill(&mut self) -> Result<(), String>;

    /// Get the architecture info for this target.
    fn arch(&self) -> &'static ArchInfo;
}

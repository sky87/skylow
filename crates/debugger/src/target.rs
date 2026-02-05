//! Process target control via ptrace
//!
//! This module provides low-level process control for debugging ELF binaries.

use std::ffi::CString;
use std::io;
use std::path::Path;

/// Reason the target stopped
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StopReason {
    /// Hit a breakpoint
    Breakpoint(u64),
    /// Single step completed
    Step,
    /// Program exited with status code
    Exited(i32),
    /// Program was killed by signal
    Signaled(i32),
    /// Stopped by other signal
    Signal(i32),
}

/// AArch64 registers
#[derive(Debug, Clone, Default)]
pub struct Registers {
    /// General purpose registers X0-X30
    pub x: [u64; 31],
    /// Stack pointer
    pub sp: u64,
    /// Program counter
    pub pc: u64,
    /// Processor state
    pub pstate: u64,
}

/// A debugger target process
pub struct Target {
    /// Process ID
    pid: i32,
    /// Whether the process has been started
    running: bool,
    /// Path to the binary
    binary: String,
    /// Arguments to the binary
    args: Vec<String>,
    /// Saved instructions at breakpoint locations (addr -> original instruction)
    breakpoint_instructions: std::collections::HashMap<u64, u32>,
}

impl Target {
    /// Create a new target for a binary
    pub fn new(binary: impl AsRef<Path>, args: Vec<String>) -> Self {
        Self {
            pid: 0,
            running: false,
            binary: binary.as_ref().to_string_lossy().to_string(),
            args,
            breakpoint_instructions: std::collections::HashMap::new(),
        }
    }

    /// Start the target process
    pub fn start(&mut self) -> io::Result<StopReason> {
        // Fork and exec the target
        let pid = unsafe { libc::fork() };

        if pid < 0 {
            return Err(io::Error::last_os_error());
        }

        if pid == 0 {
            // Child process
            self.child_exec();
        }

        // Parent process
        self.pid = pid;
        self.running = true;

        // Wait for the child to stop (after PTRACE_TRACEME + exec)
        self.wait()
    }

    /// Execute the target binary in the child process
    fn child_exec(&self) -> ! {
        // Request to be traced
        unsafe {
            libc::ptrace(libc::PTRACE_TRACEME, 0, 0, 0);
        }

        // Build argv
        let binary_cstr = CString::new(self.binary.as_str()).unwrap();
        let mut argv_cstrings: Vec<CString> = vec![binary_cstr.clone()];
        for arg in &self.args {
            argv_cstrings.push(CString::new(arg.as_str()).unwrap());
        }

        let mut argv: Vec<*const libc::c_char> = argv_cstrings.iter().map(|s| s.as_ptr()).collect();
        argv.push(std::ptr::null());

        // Exec
        unsafe {
            libc::execv(binary_cstr.as_ptr(), argv.as_ptr());
        }

        // If we get here, exec failed
        eprintln!("exec failed: {}", io::Error::last_os_error());
        unsafe { libc::_exit(127) };
    }

    /// Wait for the target to stop
    pub fn wait(&self) -> io::Result<StopReason> {
        let mut status: libc::c_int = 0;
        let result = unsafe { libc::waitpid(self.pid, &mut status, 0) };

        if result < 0 {
            return Err(io::Error::last_os_error());
        }

        // Decode wait status
        if libc::WIFEXITED(status) {
            Ok(StopReason::Exited(libc::WEXITSTATUS(status)))
        } else if libc::WIFSIGNALED(status) {
            Ok(StopReason::Signaled(libc::WTERMSIG(status)))
        } else if libc::WIFSTOPPED(status) {
            let sig = libc::WSTOPSIG(status);
            if sig == libc::SIGTRAP {
                // Could be breakpoint or step
                let regs = self.get_registers()?;
                // On AArch64, BRK sets PC to the BRK instruction address
                if self.breakpoint_instructions.contains_key(&regs.pc) {
                    Ok(StopReason::Breakpoint(regs.pc))
                } else {
                    Ok(StopReason::Step)
                }
            } else {
                Ok(StopReason::Signal(sig))
            }
        } else {
            Ok(StopReason::Signal(0))
        }
    }

    /// Continue execution
    pub fn cont(&self) -> io::Result<StopReason> {
        let result = unsafe { libc::ptrace(libc::PTRACE_CONT, self.pid, 0, 0) };
        if result < 0 {
            return Err(io::Error::last_os_error());
        }
        self.wait()
    }

    /// Single step one instruction
    pub fn step(&self) -> io::Result<StopReason> {
        let result = unsafe { libc::ptrace(libc::PTRACE_SINGLESTEP, self.pid, 0, 0) };
        if result < 0 {
            return Err(io::Error::last_os_error());
        }
        self.wait()
    }

    /// Get register values
    pub fn get_registers(&self) -> io::Result<Registers> {
        let mut regs = Registers::default();

        // Use PTRACE_GETREGSET with NT_PRSTATUS for AArch64
        let mut iovec = libc::iovec {
            iov_base: &mut regs as *mut Registers as *mut libc::c_void,
            iov_len: std::mem::size_of::<Registers>(),
        };

        let result = unsafe {
            libc::ptrace(
                libc::PTRACE_GETREGSET,
                self.pid,
                libc::NT_PRSTATUS,
                &mut iovec as *mut libc::iovec,
            )
        };

        if result < 0 {
            return Err(io::Error::last_os_error());
        }

        Ok(regs)
    }

    /// Set register values
    pub fn set_registers(&self, regs: &Registers) -> io::Result<()> {
        let iovec = libc::iovec {
            iov_base: regs as *const Registers as *mut libc::c_void,
            iov_len: std::mem::size_of::<Registers>(),
        };

        let result = unsafe {
            libc::ptrace(
                libc::PTRACE_SETREGSET,
                self.pid,
                libc::NT_PRSTATUS,
                &iovec as *const libc::iovec,
            )
        };

        if result < 0 {
            return Err(io::Error::last_os_error());
        }

        Ok(())
    }

    /// Read memory from the target
    pub fn read_memory(&self, addr: u64, size: usize) -> io::Result<Vec<u8>> {
        let mut data = vec![0u8; size];
        let mut offset = 0;

        while offset < size {
            let word = unsafe {
                libc::ptrace(
                    libc::PTRACE_PEEKDATA,
                    self.pid,
                    (addr + offset as u64) as *mut libc::c_void,
                    0,
                )
            };

            if word == -1 {
                let err = io::Error::last_os_error();
                if err.raw_os_error() != Some(0) {
                    return Err(err);
                }
            }

            let bytes = word.to_ne_bytes();
            let copy_len = std::cmp::min(8, size - offset);
            data[offset..offset + copy_len].copy_from_slice(&bytes[..copy_len]);
            offset += 8;
        }

        data.truncate(size);
        Ok(data)
    }

    /// Write memory to the target
    pub fn write_memory(&self, addr: u64, data: &[u8]) -> io::Result<()> {
        let mut offset = 0;

        while offset < data.len() {
            // Read existing word if we're not writing a full word
            let remaining = data.len() - offset;
            let mut word_bytes = if remaining < 8 {
                let existing = self.read_memory(addr + offset as u64, 8)?;
                let mut bytes = [0u8; 8];
                bytes.copy_from_slice(&existing);
                bytes
            } else {
                [0u8; 8]
            };

            // Copy new data into word
            let copy_len = std::cmp::min(8, remaining);
            word_bytes[..copy_len].copy_from_slice(&data[offset..offset + copy_len]);

            let word = i64::from_ne_bytes(word_bytes);
            let result = unsafe {
                libc::ptrace(
                    libc::PTRACE_POKEDATA,
                    self.pid,
                    (addr + offset as u64) as *mut libc::c_void,
                    word,
                )
            };

            if result < 0 {
                return Err(io::Error::last_os_error());
            }

            offset += 8;
        }

        Ok(())
    }

    /// Set a breakpoint at an address
    pub fn set_breakpoint(&mut self, addr: u64) -> io::Result<()> {
        // Read the original instruction
        let orig_bytes = self.read_memory(addr, 4)?;
        let orig_inst = u32::from_le_bytes([orig_bytes[0], orig_bytes[1], orig_bytes[2], orig_bytes[3]]);

        // Save original instruction
        self.breakpoint_instructions.insert(addr, orig_inst);

        // Write BRK #0 instruction (0xD4200000)
        let brk_inst: u32 = 0xD4200000;
        self.write_memory(addr, &brk_inst.to_le_bytes())?;

        Ok(())
    }

    /// Remove a breakpoint at an address
    pub fn remove_breakpoint(&mut self, addr: u64) -> io::Result<bool> {
        if let Some(orig_inst) = self.breakpoint_instructions.remove(&addr) {
            self.write_memory(addr, &orig_inst.to_le_bytes())?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    /// Continue past a breakpoint
    ///
    /// This restores the original instruction, single steps, then reinstalls the breakpoint.
    pub fn continue_past_breakpoint(&mut self, bp_addr: u64) -> io::Result<StopReason> {
        // Restore original instruction
        if let Some(&orig_inst) = self.breakpoint_instructions.get(&bp_addr) {
            self.write_memory(bp_addr, &orig_inst.to_le_bytes())?;

            // Set PC back to the breakpoint address
            let mut regs = self.get_registers()?;
            regs.pc = bp_addr;
            self.set_registers(&regs)?;

            // Single step
            let result = self.step()?;

            // Reinstall breakpoint
            let brk_inst: u32 = 0xD4200000;
            self.write_memory(bp_addr, &brk_inst.to_le_bytes())?;

            // If we hit another breakpoint or finished stepping, handle it
            match result {
                StopReason::Step => self.cont(),
                other => Ok(other),
            }
        } else {
            self.cont()
        }
    }

    /// Step past a breakpoint: restore original instruction, step, re-install breakpoint
    pub fn step_past_breakpoint(&mut self, bp_addr: u64) -> io::Result<StopReason> {
        if let Some(orig_inst) = self.breakpoint_instructions.remove(&bp_addr) {
            // Restore original instruction in memory
            self.write_memory(bp_addr, &orig_inst.to_le_bytes())?;

            // Set PC back to the breakpoint address
            let mut regs = self.get_registers()?;
            regs.pc = bp_addr;
            self.set_registers(&regs)?;

            // Single step (executes the original instruction)
            // Note: breakpoint_instructions entry is removed so wait() won't
            // misidentify the step as a breakpoint hit
            let result = self.step()?;

            // Re-insert breakpoint into tracking map and memory
            self.breakpoint_instructions.insert(bp_addr, orig_inst);
            let brk_inst: u32 = 0xD4200000;
            self.write_memory(bp_addr, &brk_inst.to_le_bytes())?;

            Ok(result)
        } else {
            // No breakpoint here, just step normally
            self.step()
        }
    }

    /// Get the process ID
    #[allow(dead_code)]
    pub fn pid(&self) -> i32 {
        self.pid
    }

    /// Check if the target is running
    pub fn is_running(&self) -> bool {
        self.running
    }

    /// Kill the target process
    pub fn kill(&mut self) -> io::Result<()> {
        if self.pid > 0 && self.running {
            unsafe { libc::kill(self.pid, libc::SIGKILL) };
            self.running = false;
        }
        Ok(())
    }

    /// Unwind the stack and return a list of (frame_pointer, return_address) pairs.
    ///
    /// On AArch64, the frame chain is:
    /// - X29 (FP) points to the current frame
    /// - [FP] contains the previous FP (saved by callee)
    /// - [FP+8] contains the return address (saved LR)
    ///
    /// Returns frames from innermost (current) to outermost (main/entry).
    /// The first entry is (current_fp, current_pc), subsequent entries are
    /// (saved_fp, return_address).
    pub fn unwind_stack(&self, max_frames: usize) -> io::Result<Vec<(u64, u64)>> {
        let regs = self.get_registers()?;
        let mut frames = Vec::new();

        // First frame: current PC and FP
        frames.push((regs.x[29], regs.pc));

        // Walk the frame pointer chain
        let mut fp = regs.x[29]; // X29 is the frame pointer on AArch64

        for _ in 1..max_frames {
            // Stop if FP is null or invalid
            if fp == 0 || fp < 0x1000 {
                break;
            }

            // Read saved FP and return address from the stack
            // On AArch64: [FP] = saved FP, [FP+8] = saved LR (return address)
            let frame_data = match self.read_memory(fp, 16) {
                Ok(data) => data,
                Err(_) => break, // Stop on memory read errors
            };

            if frame_data.len() < 16 {
                break;
            }

            let saved_fp = u64::from_le_bytes([
                frame_data[0],
                frame_data[1],
                frame_data[2],
                frame_data[3],
                frame_data[4],
                frame_data[5],
                frame_data[6],
                frame_data[7],
            ]);
            let return_addr = u64::from_le_bytes([
                frame_data[8],
                frame_data[9],
                frame_data[10],
                frame_data[11],
                frame_data[12],
                frame_data[13],
                frame_data[14],
                frame_data[15],
            ]);

            // Stop if return address is null (reached the bottom of the stack)
            if return_addr == 0 {
                break;
            }

            frames.push((saved_fp, return_addr));

            // Move to the previous frame
            // Stop if we're not making progress (stuck in a loop)
            if saved_fp == 0 || saved_fp == fp {
                break;
            }
            fp = saved_fp;
        }

        Ok(frames)
    }
}

impl Drop for Target {
    fn drop(&mut self) {
        let _ = self.kill();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_stop_reason_equality() {
        assert_eq!(StopReason::Exited(0), StopReason::Exited(0));
        assert_ne!(StopReason::Exited(0), StopReason::Exited(1));
        assert_eq!(StopReason::Breakpoint(0x1000), StopReason::Breakpoint(0x1000));
        assert_eq!(StopReason::Step, StopReason::Step);
    }

    #[test]
    fn test_stop_reason_debug() {
        // Test Debug trait for StopReason variants
        let _ = format!("{:?}", StopReason::Breakpoint(0x1000));
        let _ = format!("{:?}", StopReason::Step);
        let _ = format!("{:?}", StopReason::Exited(0));
        let _ = format!("{:?}", StopReason::Signaled(9));
        let _ = format!("{:?}", StopReason::Signal(11));
    }

    #[test]
    fn test_stop_reason_clone() {
        let reason = StopReason::Breakpoint(0x2000);
        let cloned = reason.clone();
        assert_eq!(reason, cloned);
    }

    #[test]
    fn test_registers_default() {
        let regs = Registers::default();
        assert_eq!(regs.pc, 0);
        assert_eq!(regs.sp, 0);
        assert_eq!(regs.x[0], 0);
        assert_eq!(regs.pstate, 0);
    }

    #[test]
    fn test_registers_clone() {
        let mut regs = Registers::default();
        regs.pc = 0x400000;
        regs.x[0] = 42;
        regs.x[29] = 0x7fff0000;

        let cloned = regs.clone();
        assert_eq!(cloned.pc, 0x400000);
        assert_eq!(cloned.x[0], 42);
        assert_eq!(cloned.x[29], 0x7fff0000);
    }

    #[test]
    fn test_registers_debug() {
        let regs = Registers::default();
        let _ = format!("{:?}", regs);
    }

    #[test]
    fn test_target_new() {
        let target = Target::new("/bin/true", vec![]);
        assert_eq!(target.pid(), 0);
        assert!(!target.is_running());
    }

    #[test]
    fn test_target_new_with_args() {
        let target = Target::new("/bin/echo", vec!["hello".to_string(), "world".to_string()]);
        assert_eq!(target.pid(), 0);
        assert!(!target.is_running());
    }

    #[test]
    fn test_target_new_path_buf() {
        use std::path::PathBuf;
        let path = PathBuf::from("/usr/bin/test");
        let target = Target::new(&path, vec![]);
        assert_eq!(target.pid(), 0);
    }
}

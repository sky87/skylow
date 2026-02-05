//! Process target control via ptrace
//!
//! This module provides low-level process control for debugging ELF binaries.

use debug::types::StopReason;
use std::ffi::CString;
use std::io;
use std::path::Path;

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

/// A debugger target process using ptrace
pub struct PtraceTarget {
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

impl PtraceTarget {
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
    fn ptrace_start(&mut self) -> io::Result<StopReason> {
        let pid = unsafe { libc::fork() };

        if pid < 0 {
            return Err(io::Error::last_os_error());
        }

        if pid == 0 {
            self.child_exec();
        }

        self.pid = pid;
        self.running = true;

        self.wait()
    }

    /// Execute the target binary in the child process
    fn child_exec(&self) -> ! {
        unsafe {
            libc::ptrace(libc::PTRACE_TRACEME, 0, 0, 0);
        }

        let binary_cstr = CString::new(self.binary.as_str()).unwrap();
        let mut argv_cstrings: Vec<CString> = vec![binary_cstr.clone()];
        for arg in &self.args {
            argv_cstrings.push(CString::new(arg.as_str()).unwrap());
        }

        let mut argv: Vec<*const libc::c_char> = argv_cstrings.iter().map(|s| s.as_ptr()).collect();
        argv.push(std::ptr::null());

        unsafe {
            libc::execv(binary_cstr.as_ptr(), argv.as_ptr());
        }

        eprintln!("exec failed: {}", io::Error::last_os_error());
        unsafe { libc::_exit(127) };
    }

    /// Wait for the target to stop
    fn wait(&self) -> io::Result<StopReason> {
        let mut status: libc::c_int = 0;
        let result = unsafe { libc::waitpid(self.pid, &mut status, 0) };

        if result < 0 {
            return Err(io::Error::last_os_error());
        }

        if libc::WIFEXITED(status) {
            Ok(StopReason::Exited(libc::WEXITSTATUS(status)))
        } else if libc::WIFSIGNALED(status) {
            Ok(StopReason::Signaled(libc::WTERMSIG(status)))
        } else if libc::WIFSTOPPED(status) {
            let sig = libc::WSTOPSIG(status);
            if sig == libc::SIGTRAP {
                let regs = self.get_registers()?;
                if self.breakpoint_instructions.contains_key(&regs.pc) {
                    Ok(StopReason::BreakpointHit(regs.pc))
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
    fn ptrace_cont(&self) -> io::Result<StopReason> {
        let result = unsafe { libc::ptrace(libc::PTRACE_CONT, self.pid, 0, 0) };
        if result < 0 {
            return Err(io::Error::last_os_error());
        }
        self.wait()
    }

    /// Single step one instruction
    fn ptrace_step(&self) -> io::Result<StopReason> {
        let result = unsafe { libc::ptrace(libc::PTRACE_SINGLESTEP, self.pid, 0, 0) };
        if result < 0 {
            return Err(io::Error::last_os_error());
        }
        self.wait()
    }

    /// Get register values
    pub fn get_registers(&self) -> io::Result<Registers> {
        let mut regs = Registers::default();

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
            let remaining = data.len() - offset;
            let mut word_bytes = if remaining < 8 {
                let existing = self.read_memory(addr + offset as u64, 8)?;
                let mut bytes = [0u8; 8];
                bytes.copy_from_slice(&existing);
                bytes
            } else {
                [0u8; 8]
            };

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

    /// Set a breakpoint at an address (internal)
    fn ptrace_set_breakpoint(&mut self, addr: u64) -> io::Result<()> {
        let orig_bytes = self.read_memory(addr, 4)?;
        let orig_inst = u32::from_le_bytes([orig_bytes[0], orig_bytes[1], orig_bytes[2], orig_bytes[3]]);

        self.breakpoint_instructions.insert(addr, orig_inst);

        let brk_inst: u32 = 0xD4200000;
        self.write_memory(addr, &brk_inst.to_le_bytes())?;

        Ok(())
    }

    /// Remove a breakpoint at an address (internal)
    fn ptrace_remove_breakpoint(&mut self, addr: u64) -> io::Result<bool> {
        if let Some(orig_inst) = self.breakpoint_instructions.remove(&addr) {
            self.write_memory(addr, &orig_inst.to_le_bytes())?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    /// Continue past a breakpoint (internal)
    fn ptrace_continue_past_breakpoint(&mut self, bp_addr: u64) -> io::Result<StopReason> {
        if let Some(&orig_inst) = self.breakpoint_instructions.get(&bp_addr) {
            self.write_memory(bp_addr, &orig_inst.to_le_bytes())?;

            let mut regs = self.get_registers()?;
            regs.pc = bp_addr;
            self.set_registers(&regs)?;

            let result = self.ptrace_step()?;

            let brk_inst: u32 = 0xD4200000;
            self.write_memory(bp_addr, &brk_inst.to_le_bytes())?;

            match result {
                StopReason::Step => self.ptrace_cont(),
                other => Ok(other),
            }
        } else {
            self.ptrace_cont()
        }
    }

    /// Step past a breakpoint (internal)
    fn ptrace_step_past_breakpoint(&mut self, bp_addr: u64) -> io::Result<StopReason> {
        if let Some(orig_inst) = self.breakpoint_instructions.remove(&bp_addr) {
            self.write_memory(bp_addr, &orig_inst.to_le_bytes())?;

            let mut regs = self.get_registers()?;
            regs.pc = bp_addr;
            self.set_registers(&regs)?;

            let result = self.ptrace_step()?;

            self.breakpoint_instructions.insert(bp_addr, orig_inst);
            let brk_inst: u32 = 0xD4200000;
            self.write_memory(bp_addr, &brk_inst.to_le_bytes())?;

            Ok(result)
        } else {
            self.ptrace_step()
        }
    }

    /// Get the process ID
    #[allow(dead_code)]
    pub fn pid(&self) -> i32 {
        self.pid
    }

    /// Kill the target process
    fn ptrace_kill(&mut self) -> io::Result<()> {
        if self.pid > 0 && self.running {
            unsafe { libc::kill(self.pid, libc::SIGKILL) };
            self.running = false;
        }
        Ok(())
    }

    /// Unwind the stack (internal)
    fn ptrace_unwind_stack(&self, max_frames: usize) -> io::Result<Vec<(u64, u64)>> {
        let regs = self.get_registers()?;
        let mut frames = Vec::new();

        frames.push((regs.x[29], regs.pc));

        let mut fp = regs.x[29];

        for _ in 1..max_frames {
            if fp == 0 || fp < 0x1000 {
                break;
            }

            let frame_data = match self.read_memory(fp, 16) {
                Ok(data) => data,
                Err(_) => break,
            };

            if frame_data.len() < 16 {
                break;
            }

            let saved_fp = u64::from_le_bytes([
                frame_data[0], frame_data[1], frame_data[2], frame_data[3],
                frame_data[4], frame_data[5], frame_data[6], frame_data[7],
            ]);
            let return_addr = u64::from_le_bytes([
                frame_data[8], frame_data[9], frame_data[10], frame_data[11],
                frame_data[12], frame_data[13], frame_data[14], frame_data[15],
            ]);

            if return_addr == 0 {
                break;
            }

            frames.push((saved_fp, return_addr));

            if saved_fp == 0 || saved_fp == fp {
                break;
            }
            fp = saved_fp;
        }

        Ok(frames)
    }
}

impl debug::Target for PtraceTarget {
    fn start(&mut self) -> Result<StopReason, String> {
        self.ptrace_start().map_err(|e| e.to_string())
    }

    fn cont(&mut self) -> Result<StopReason, String> {
        self.ptrace_cont().map_err(|e| e.to_string())
    }

    fn step(&mut self) -> Result<StopReason, String> {
        self.ptrace_step().map_err(|e| e.to_string())
    }

    fn continue_past_breakpoint(&mut self, addr: u64) -> Result<StopReason, String> {
        self.ptrace_continue_past_breakpoint(addr).map_err(|e| e.to_string())
    }

    fn step_past_breakpoint(&mut self, addr: u64) -> Result<StopReason, String> {
        self.ptrace_step_past_breakpoint(addr).map_err(|e| e.to_string())
    }

    fn pc(&self) -> Result<u64, String> {
        let regs = self.get_registers().map_err(|e| e.to_string())?;
        Ok(regs.pc)
    }

    fn gpr(&self, index: usize) -> Result<u64, String> {
        if index >= 31 {
            return Err(format!("Invalid GPR index: {}", index));
        }
        let regs = self.get_registers().map_err(|e| e.to_string())?;
        Ok(regs.x[index])
    }

    fn sp(&self) -> Result<u64, String> {
        let regs = self.get_registers().map_err(|e| e.to_string())?;
        Ok(regs.sp)
    }

    fn pstate(&self) -> Result<u64, String> {
        let regs = self.get_registers().map_err(|e| e.to_string())?;
        Ok(regs.pstate)
    }

    fn all_gprs(&self) -> Result<Vec<(usize, u64)>, String> {
        let regs = self.get_registers().map_err(|e| e.to_string())?;
        Ok((0..31).map(|i| (i, regs.x[i])).collect())
    }

    fn set_breakpoint(&mut self, addr: u64) -> Result<(), String> {
        self.ptrace_set_breakpoint(addr).map_err(|e| e.to_string())
    }

    fn remove_breakpoint(&mut self, addr: u64) -> Result<bool, String> {
        self.ptrace_remove_breakpoint(addr).map_err(|e| e.to_string())
    }

    fn is_running(&self) -> bool {
        self.running
    }

    fn unwind_stack(&self, max_frames: usize) -> Result<Vec<(u64, u64)>, String> {
        self.ptrace_unwind_stack(max_frames).map_err(|e| e.to_string())
    }

    fn kill(&mut self) -> Result<(), String> {
        self.ptrace_kill().map_err(|e| e.to_string())
    }
}

impl Drop for PtraceTarget {
    fn drop(&mut self) {
        let _ = self.ptrace_kill();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use debug::Target;

    #[test]
    fn test_stop_reason_equality() {
        assert_eq!(StopReason::Exited(0), StopReason::Exited(0));
        assert_ne!(StopReason::Exited(0), StopReason::Exited(1));
        assert_eq!(StopReason::BreakpointHit(0x1000), StopReason::BreakpointHit(0x1000));
        assert_eq!(StopReason::Step, StopReason::Step);
    }

    #[test]
    fn test_stop_reason_debug() {
        let _ = format!("{:?}", StopReason::BreakpointHit(0x1000));
        let _ = format!("{:?}", StopReason::Step);
        let _ = format!("{:?}", StopReason::Exited(0));
        let _ = format!("{:?}", StopReason::Signaled(9));
        let _ = format!("{:?}", StopReason::Signal(11));
    }

    #[test]
    fn test_stop_reason_clone() {
        let reason = StopReason::BreakpointHit(0x2000);
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
        let target = PtraceTarget::new("/bin/true", vec![]);
        assert_eq!(target.pid(), 0);
        assert!(!target.is_running());
    }

    #[test]
    fn test_target_new_with_args() {
        let target = PtraceTarget::new("/bin/echo", vec!["hello".to_string(), "world".to_string()]);
        assert_eq!(target.pid(), 0);
        assert!(!target.is_running());
    }

    #[test]
    fn test_target_new_path_buf() {
        use std::path::PathBuf;
        let path = PathBuf::from("/usr/bin/test");
        let target = PtraceTarget::new(&path, vec![]);
        assert_eq!(target.pid(), 0);
    }
}

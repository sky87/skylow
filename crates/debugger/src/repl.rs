//! REPL (Read-Eval-Print-Loop) for the debugger

use crate::commands::{self, BreakLocation, Command, InfoKind};
use crate::target::{StopReason, Target};
use debug::source_map::SourceMapper;
use debug::types::{Breakpoint, BreakpointId, DebugState, Debugger};
use debug::BreakpointManager;
use debuginfo::{read_skydbg, DebugInfo};
use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead, Write};
use std::path::PathBuf;

/// A debugging session
pub struct Session {
    /// The target process
    target: Target,
    /// Debug information
    debug_info: Option<DebugInfo>,
    /// Debug library state
    debugger: Debugger,
    /// Breakpoint ID to address mapping
    breakpoint_addrs: HashMap<BreakpointId, u64>,
    /// Base address where code is loaded
    code_base: u64,
    /// Last command (for repeat on empty line)
    last_command: Option<Command>,
    /// Binary path
    binary_path: PathBuf,
    /// Captured output lines (for testing)
    output: Vec<String>,
}

impl Session {
    /// Create a new debugging session
    pub fn new(binary_path: PathBuf, args: Vec<String>) -> io::Result<Self> {
        let target = Target::new(&binary_path, args);
        let mut debugger = Debugger::new();

        // Try to load debug info from sidecar file
        let debug_path = binary_path.with_extension("skydbg");
        let debug_info = if debug_path.exists() {
            match File::open(&debug_path) {
                Ok(mut file) => match read_skydbg(&mut file) {
                    Ok(info) => {
                        debugger.load_debug_info(info.clone());
                        Some(info)
                    }
                    Err(e) => {
                        eprintln!("Warning: failed to read debug info: {}", e);
                        None
                    }
                },
                Err(e) => {
                    eprintln!("Warning: failed to open debug file: {}", e);
                    None
                }
            }
        } else {
            None
        };

        // Code base is ELF load address + header size (64-byte ELF header + 56-byte program header)
        // This is where the code section actually starts in memory
        let code_base = 0x400000 + 120; // BASE_ADDR + header_size(1)

        Ok(Self {
            target,
            debug_info,
            debugger,
            breakpoint_addrs: HashMap::new(),
            code_base,
            last_command: None,
            binary_path,
            output: Vec::new(),
        })
    }

    /// Print a line (captures to output buffer for testing)
    fn println(&mut self, msg: impl AsRef<str>) {
        let s = msg.as_ref().to_string();
        println!("{}", s);
        self.output.push(s);
    }

    /// Get captured output (for testing)
    #[allow(dead_code)]
    pub fn get_output(&self) -> &[String] {
        &self.output
    }

    /// Clear captured output (for testing)
    #[allow(dead_code)]
    pub fn clear_output(&mut self) {
        self.output.clear();
    }

    /// Execute a command
    pub fn execute(&mut self, cmd: Command) -> Result<bool, String> {
        // Store command for repeat (except Empty)
        if cmd != Command::Empty {
            self.last_command = Some(cmd.clone());
        }

        match cmd {
            Command::Break(location) => self.cmd_break(location),
            Command::Delete(n) => self.cmd_delete(n),
            Command::Run(args) => self.cmd_run(args),
            Command::Continue => self.cmd_continue(),
            Command::Step => self.cmd_step(),
            Command::StepInstruction => self.cmd_stepi(),
            Command::Next => self.cmd_next(),
            Command::Finish => self.cmd_finish(),
            Command::Print(expr) => self.cmd_print(&expr),
            Command::Backtrace => self.cmd_backtrace(),
            Command::Info(kind) => self.cmd_info(kind),
            Command::List(loc) => self.cmd_list(loc),
            Command::Quit => return Ok(true),
            Command::Assert(expr) => self.cmd_assert(&expr),
            Command::ExpectStop(loc) => self.cmd_expect_stop(loc),
            Command::Empty => {
                // Repeat last command
                if let Some(last) = self.last_command.clone() {
                    return self.execute(last);
                }
                Ok(false)
            }
            Command::Unknown(s) => {
                self.println(format!("Unknown command: {}", s));
                Ok(false)
            }
        }
    }

    fn cmd_break(&mut self, location: BreakLocation) -> Result<bool, String> {
        let addr = match &location {
            BreakLocation::Source { file, line } => {
                // Resolve source location to address
                let offsets = SourceMapper::source_to_offsets(&self.debugger, file, *line);
                if offsets.is_empty() {
                    return Err(format!("No code at {}:{}", file, line));
                }
                self.code_base + offsets[0] as u64
            }
            BreakLocation::Function(name) => {
                // Resolve function name to address
                match SourceMapper::function_code_range(&self.debugger, name) {
                    Some((offset, _)) => self.code_base + offset as u64,
                    None => return Err(format!("Function not found: {}", name)),
                }
            }
            BreakLocation::Address(addr) => *addr,
        };

        // Add to debug library
        let bp = match &location {
            BreakLocation::Source { file, line } => {
                Breakpoint::at_source(BreakpointId(0), file.clone(), *line, None)
            }
            BreakLocation::Function(name) => {
                Breakpoint::at_function(BreakpointId(0), name.clone())
            }
            BreakLocation::Address(addr) => {
                Breakpoint::at_address(BreakpointId(0), *addr as u32)
            }
        };
        let id = self.debugger.add_breakpoint(bp);
        self.breakpoint_addrs.insert(id, addr);

        self.println(format!("Breakpoint {} at 0x{:x}", id.0, addr));
        Ok(false)
    }

    fn cmd_delete(&mut self, n: u32) -> Result<bool, String> {
        let id = BreakpointId(n);
        if let Some(addr) = self.breakpoint_addrs.remove(&id) {
            self.debugger.remove_breakpoint(id);
            if self.target.is_running() {
                let _ = self.target.remove_breakpoint(addr);
            }
            self.println(format!("Deleted breakpoint {}", n));
        } else {
            self.println(format!("No breakpoint number {}", n));
        }
        Ok(false)
    }

    fn cmd_run(&mut self, _args: Vec<String>) -> Result<bool, String> {
        self.println(format!("Starting program: {}", self.binary_path.display()));

        match self.target.start() {
            Ok(reason) => {
                self.debugger.set_state(DebugState::Running);

                // Install all breakpoints
                for (&id, &addr) in &self.breakpoint_addrs {
                    if let Err(e) = self.target.set_breakpoint(addr) {
                        eprintln!("Warning: failed to set breakpoint {}: {}", id.0, e);
                    }
                }

                // After initial stop from exec (reported as Step), continue to run the program
                match reason {
                    StopReason::Step => {
                        // Initial SIGTRAP from exec - continue to first breakpoint or end
                        match self.target.cont() {
                            Ok(stop_reason) => self.handle_stop(stop_reason),
                            Err(e) => Err(format!("Failed to continue: {}", e)),
                        }
                    }
                    _ => self.handle_stop(reason),
                }
            }
            Err(e) => Err(format!("Failed to start: {}", e)),
        }
    }

    fn cmd_continue(&mut self) -> Result<bool, String> {
        if !self.target.is_running() {
            return Err("Program is not running".to_string());
        }

        // Check if we're at a breakpoint and need to step past it
        let regs = self.target.get_registers().map_err(|e| e.to_string())?;
        let pc = regs.pc;

        // Check if PC-4 is a breakpoint (we stopped after BRK)
        let bp_addr = pc.wrapping_sub(4);
        let reason = if self.breakpoint_addrs.values().any(|&a| a == bp_addr) {
            self.target.continue_past_breakpoint(bp_addr).map_err(|e| e.to_string())?
        } else {
            self.target.cont().map_err(|e| e.to_string())?
        };

        self.handle_stop(reason)
    }

    fn cmd_step(&mut self) -> Result<bool, String> {
        if !self.target.is_running() {
            return Err("Program is not running".to_string());
        }

        // Step one instruction for now (source-level stepping would need line info)
        let reason = self.target.step().map_err(|e| e.to_string())?;
        self.handle_stop(reason)
    }

    fn cmd_stepi(&mut self) -> Result<bool, String> {
        if !self.target.is_running() {
            return Err("Program is not running".to_string());
        }

        let reason = self.target.step().map_err(|e| e.to_string())?;
        self.handle_stop(reason)
    }

    fn cmd_next(&mut self) -> Result<bool, String> {
        // For now, same as step (would need call detection for step-over)
        self.cmd_step()
    }

    fn cmd_finish(&mut self) -> Result<bool, String> {
        // For now, just continue
        self.cmd_continue()
    }

    fn cmd_print(&mut self, expr: &str) -> Result<bool, String> {
        if !self.target.is_running() {
            return Err("Program is not running".to_string());
        }

        // Simple: try to parse as register name (x0-x30, sp, pc)
        let regs = self.target.get_registers().map_err(|e| e.to_string())?;

        if expr.starts_with('x') || expr.starts_with('X') {
            if let Ok(n) = expr[1..].parse::<usize>() {
                if n < 31 {
                    self.println(format!("{} = {} (0x{:x})", expr, regs.x[n] as i64, regs.x[n]));
                    return Ok(false);
                }
            }
        }

        match expr.to_lowercase().as_str() {
            "sp" => self.println(format!("sp = 0x{:x}", regs.sp)),
            "pc" => self.println(format!("pc = 0x{:x}", regs.pc)),
            "pstate" => self.println(format!("pstate = 0x{:x}", regs.pstate)),
            _ => self.println(format!("Cannot evaluate: {}", expr)),
        }

        Ok(false)
    }

    fn cmd_backtrace(&mut self) -> Result<bool, String> {
        if !self.target.is_running() {
            return Err("Program is not running".to_string());
        }

        // Unwind the stack to get all frames
        let frames = self.target.unwind_stack(50).map_err(|e| e.to_string())?;

        for (index, (fp, addr)) in frames.iter().enumerate() {
            // For frame 0, addr is the current PC
            // For other frames, addr is the return address (points to instruction after call)
            // Subtract 4 to point to the call instruction for better source mapping
            let lookup_addr = if index == 0 { *addr } else { addr.saturating_sub(4) };

            // Convert to code offset
            let offset = lookup_addr.saturating_sub(self.code_base) as u32;

            // Get source location if available
            if let Some(loc) = SourceMapper::offset_to_source(&self.debugger, offset) {
                let func_name = loc.function.as_deref().unwrap_or("??");
                self.println(format!(
                    "#{:<2} 0x{:016x} in {} at {}:{}:{}",
                    index, addr, func_name, loc.file, loc.line, loc.col
                ));
            } else {
                // No debug info, just show address
                let func_name = SourceMapper::function_at_offset(&self.debugger, offset)
                    .unwrap_or_else(|| "??".to_string());
                if func_name == "??" {
                    self.println(format!("#{:<2} 0x{:016x}", index, addr));
                } else {
                    self.println(format!("#{:<2} 0x{:016x} in {}", index, addr, func_name));
                }
            }

            // Stop if we've hit a null frame pointer (bottom of stack)
            if *fp == 0 {
                break;
            }
        }

        Ok(false)
    }

    fn cmd_info(&mut self, kind: InfoKind) -> Result<bool, String> {
        match kind {
            InfoKind::Locals => {
                if !self.target.is_running() {
                    return Err("Program is not running".to_string());
                }
                self.println("(local variable inspection not yet implemented)");
            }
            InfoKind::Registers => {
                if !self.target.is_running() {
                    return Err("Program is not running".to_string());
                }
                let regs = self.target.get_registers().map_err(|e| e.to_string())?;
                let mut line = String::new();
                for i in 0..31 {
                    if i % 4 == 0 && i > 0 {
                        self.println(&line);
                        line.clear();
                    }
                    line.push_str(&format!("x{:<2} = {:016x}  ", i, regs.x[i]));
                }
                if !line.is_empty() {
                    self.println(&line);
                }
                self.println(format!("sp  = {:016x}", regs.sp));
                self.println(format!("pc  = {:016x}", regs.pc));
            }
            InfoKind::Breakpoints => {
                if self.breakpoint_addrs.is_empty() {
                    self.println("No breakpoints.");
                } else {
                    // Collect output lines first to avoid borrow issues
                    let lines: Vec<String> = self.debugger.breakpoints()
                        .filter_map(|bp| {
                            self.breakpoint_addrs.get(&bp.id).map(|&addr| {
                                let what = match &bp.kind {
                                    debug::types::BreakpointKind::Source { file, line, .. } => {
                                        format!("{}:{}", file, line)
                                    }
                                    debug::types::BreakpointKind::Function { name } => {
                                        format!("in {}", name)
                                    }
                                    debug::types::BreakpointKind::Address { .. } => {
                                        String::new()
                                    }
                                };
                                let enabled = if bp.enabled { "y" } else { "n" };
                                format!("{:<4} 0x{:016x} {} [{}]", bp.id.0, addr, what, enabled)
                            })
                        })
                        .collect();

                    self.println("Num  Address            What");
                    for line in lines {
                        self.println(line);
                    }
                }
            }
        }
        Ok(false)
    }

    fn cmd_list(&mut self, loc: Option<commands::SourceLocation>) -> Result<bool, String> {
        let (file, line) = if let Some(l) = loc {
            (l.file, l.line)
        } else if self.target.is_running() {
            // Use current location
            let regs = self.target.get_registers().map_err(|e| e.to_string())?;
            let offset = (regs.pc - self.code_base) as u32;
            if let Some(loc) = SourceMapper::offset_to_source(&self.debugger, offset) {
                (loc.file, loc.line)
            } else {
                return Err("No source location available".to_string());
            }
        } else {
            return Err("No location specified and program not running".to_string());
        };

        // Get source text
        if let Some(text) = SourceMapper::get_source_text(&self.debugger, &file) {
            let lines: Vec<&str> = text.lines().collect();
            let start = line.saturating_sub(5) as usize;
            let end = std::cmp::min(line as usize + 5, lines.len());

            for (i, line_text) in lines[start..end].iter().enumerate() {
                let line_num = start + i + 1;
                let marker = if line_num == line as usize { ">" } else { " " };
                self.println(format!("{} {:4} {}", marker, line_num, line_text));
            }
        } else {
            self.println(format!("No source available for {}", file));
        }

        Ok(false)
    }

    fn cmd_assert(&mut self, expr: &str) -> Result<bool, String> {
        // Simple assertion: check if expression evaluates to non-zero
        // For now, just support register comparisons like "x0 == 42"
        if let Some(eq_pos) = expr.find("==") {
            let left = expr[..eq_pos].trim();
            let right = expr[eq_pos + 2..].trim();

            let left_val = self.eval_simple(left)?;
            let right_val = self.eval_simple(right)?;

            if left_val != right_val {
                return Err(format!(
                    "Assertion failed: {} ({}) != {} ({})",
                    left, left_val, right, right_val
                ));
            }
            self.println(format!("Assertion passed: {} == {}", left, right));
        } else {
            return Err(format!("Cannot evaluate assertion: {}", expr));
        }
        Ok(false)
    }

    fn cmd_expect_stop(&mut self, loc: commands::SourceLocation) -> Result<bool, String> {
        if !self.target.is_running() {
            return Err("Program is not running".to_string());
        }

        let regs = self.target.get_registers().map_err(|e| e.to_string())?;
        let offset = (regs.pc - self.code_base) as u32;

        if let Some(current_loc) = SourceMapper::offset_to_source(&self.debugger, offset) {
            if current_loc.file.ends_with(&loc.file) && current_loc.line == loc.line {
                self.println(format!("Stop location verified: {}:{}", loc.file, loc.line));
                return Ok(false);
            }
            return Err(format!(
                "Expected stop at {}:{}, but stopped at {}:{}",
                loc.file, loc.line, current_loc.file, current_loc.line
            ));
        }

        Err(format!("Expected stop at {}:{}, but no source location available", loc.file, loc.line))
    }

    fn eval_simple(&self, expr: &str) -> Result<i64, String> {
        let expr = expr.trim();

        // Try as number
        if let Ok(n) = expr.parse::<i64>() {
            return Ok(n);
        }

        // Try as hex
        if expr.starts_with("0x") || expr.starts_with("0X") {
            if let Ok(n) = i64::from_str_radix(&expr[2..], 16) {
                return Ok(n);
            }
        }

        // Try as register
        if !self.target.is_running() {
            return Err("Program is not running".to_string());
        }

        let regs = self.target.get_registers().map_err(|e| e.to_string())?;

        if expr.starts_with('x') || expr.starts_with('X') {
            if let Ok(n) = expr[1..].parse::<usize>() {
                if n < 31 {
                    return Ok(regs.x[n] as i64);
                }
            }
        }

        match expr.to_lowercase().as_str() {
            "sp" => Ok(regs.sp as i64),
            "pc" => Ok(regs.pc as i64),
            _ => Err(format!("Cannot evaluate: {}", expr)),
        }
    }

    fn handle_stop(&mut self, reason: StopReason) -> Result<bool, String> {
        match reason {
            StopReason::Exited(code) => {
                self.println(format!("Program exited with code {}", code));
                self.debugger.set_state(DebugState::Stopped(debug::types::StopReason::Exited(code)));
                Ok(false)
            }
            StopReason::Signaled(sig) => {
                self.println(format!("Program terminated with signal {}", sig));
                self.debugger.set_state(DebugState::Stopped(debug::types::StopReason::Error(
                    format!("signal {}", sig)
                )));
                Ok(false)
            }
            StopReason::Breakpoint(addr) => {
                // Find which breakpoint
                let bp_id = self.breakpoint_addrs.iter()
                    .find(|(_, &a)| a == addr)
                    .map(|(id, _)| *id);

                let mut msg = if let Some(id) = bp_id {
                    BreakpointManager::record_hit(&mut self.debugger, id);
                    format!("Breakpoint {} hit", id.0)
                } else {
                    "Stopped at breakpoint".to_string()
                };

                // Show location
                let offset = (addr - self.code_base) as u32;
                if let Some(loc) = SourceMapper::offset_to_source(&self.debugger, offset) {
                    msg.push_str(&format!(", {} at {}:{}:{}",
                        loc.function.unwrap_or_default(),
                        loc.file, loc.line, loc.col));
                } else {
                    msg.push_str(&format!(" at 0x{:x}", addr));
                }
                self.println(&msg);

                self.debugger.set_state(DebugState::Stopped(
                    debug::types::StopReason::Breakpoint(bp_id.unwrap_or(BreakpointId(0)))
                ));
                Ok(false)
            }
            StopReason::Step => {
                let regs = self.target.get_registers().map_err(|e| e.to_string())?;
                let offset = (regs.pc - self.code_base) as u32;
                if let Some(loc) = SourceMapper::offset_to_source(&self.debugger, offset) {
                    self.println(format!("{}:{}: {}", loc.file, loc.line,
                        SourceMapper::get_source_line(&self.debugger, &loc.file, loc.line)
                            .unwrap_or_default()));
                } else {
                    self.println(format!("0x{:x}", regs.pc));
                }

                self.debugger.set_state(DebugState::Stopped(debug::types::StopReason::Step));
                Ok(false)
            }
            StopReason::Signal(sig) => {
                self.println(format!("Program received signal {}", sig));
                Ok(false)
            }
        }
    }

    /// Get the debug info
    pub fn debug_info(&self) -> Option<&DebugInfo> {
        self.debug_info.as_ref()
    }
}

/// Run the REPL
pub fn run(session: &mut Session) {
    println!("SkyDbg debugger");
    println!("Type 'help' for commands, 'quit' to exit.");
    println!();

    if session.debug_info().is_some() {
        println!("Debug info loaded.");
    } else {
        println!("Warning: No debug info available.");
    }
    println!();

    let stdin = io::stdin();
    let mut stdout = io::stdout();

    loop {
        print!("(skydbg) ");
        stdout.flush().unwrap();

        let mut line = String::new();
        match stdin.lock().read_line(&mut line) {
            Ok(0) => {
                println!();
                break;
            }
            Ok(_) => {}
            Err(e) => {
                eprintln!("Error reading input: {}", e);
                break;
            }
        }

        let cmd = commands::parse(&line);
        match session.execute(cmd) {
            Ok(true) => break, // Quit
            Ok(false) => {}
            Err(e) => eprintln!("Error: {}", e),
        }
    }

    println!("Goodbye.");
}

#[cfg(test)]
mod tests {
    use super::*;

    // Note: Most tests would require actually running processes,
    // which is complex in a test environment. We test what we can.

    #[test]
    fn test_session_creation_nonexistent() {
        // This should succeed (we don't check file existence in new())
        let session = Session::new(PathBuf::from("/nonexistent"), vec![]);
        assert!(session.is_ok());
    }

    #[test]
    fn test_output_capture() {
        let mut session = Session::new(PathBuf::from("/nonexistent"), vec![]).unwrap();

        // Initially empty
        assert!(session.get_output().is_empty());

        // Execute unknown command - should produce output
        let _ = session.execute(Command::Unknown("foo".to_string()));
        assert_eq!(session.get_output().len(), 1);
        assert_eq!(session.get_output()[0], "Unknown command: foo");

        // Execute another unknown command
        let _ = session.execute(Command::Unknown("bar".to_string()));
        assert_eq!(session.get_output().len(), 2);
        assert_eq!(session.get_output()[1], "Unknown command: bar");

        // Clear output
        session.clear_output();
        assert!(session.get_output().is_empty());
    }

    #[test]
    fn test_info_breakpoints_empty() {
        let mut session = Session::new(PathBuf::from("/nonexistent"), vec![]).unwrap();
        session.clear_output();

        let _ = session.execute(Command::Info(InfoKind::Breakpoints));
        assert_eq!(session.get_output(), &["No breakpoints."]);
    }
}

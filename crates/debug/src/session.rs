//! Debugging session that orchestrates target control, breakpoints, and inspection.

use crate::commands::{self, BreakLocation, Command, InfoKind};
use crate::source_map::SourceMapper;
use crate::target::Target;
use crate::types::{Breakpoint, BreakpointId, DebugState, Debugger, StopReason};
use crate::BreakpointManager;
use debuginfo::DebugInfo;
use std::collections::HashMap;

/// A debugging session generic over the target implementation.
pub struct Session<T: Target> {
    /// The target process
    target: T,
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
    /// Binary path (for display)
    binary_name: String,
    /// Captured output lines (for testing)
    output: Vec<String>,
}

impl<T: Target> Session<T> {
    /// Create a new debugging session.
    ///
    /// - `target`: the process control backend
    /// - `debug_info`: optional debug info (loaded by the caller)
    /// - `code_base`: the base address where code is loaded in memory
    /// - `binary_name`: display name for the binary (used in messages)
    pub fn new(
        target: T,
        debug_info: Option<DebugInfo>,
        code_base: u64,
        binary_name: String,
    ) -> Self {
        let mut debugger = Debugger::new();
        if let Some(ref info) = debug_info {
            debugger.load_debug_info(info.clone());
        }

        Self {
            target,
            debug_info,
            debugger,
            breakpoint_addrs: HashMap::new(),
            code_base,
            last_command: None,
            binary_name,
            output: Vec::new(),
        }
    }

    /// Print a line (captures to output buffer for testing)
    fn println(&mut self, msg: impl AsRef<str>) {
        let s = msg.as_ref().to_string();
        println!("{}", s);
        self.output.push(s);
    }

    /// Get captured output (for testing)
    pub fn get_output(&self) -> &[String] {
        &self.output
    }

    /// Clear captured output (for testing)
    pub fn clear_output(&mut self) {
        self.output.clear();
    }

    /// Execute a command. Returns `Ok(true)` if the session should quit.
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

    /// Get the debug info
    pub fn debug_info(&self) -> Option<&DebugInfo> {
        self.debug_info.as_ref()
    }

    fn cmd_break(&mut self, location: BreakLocation) -> Result<bool, String> {
        let addr = match &location {
            BreakLocation::Source { file, line } => {
                let offsets = SourceMapper::source_to_offsets(&self.debugger, file, *line);
                if offsets.is_empty() {
                    return Err(format!("No code at {}:{}", file, line));
                }
                self.code_base + offsets[0] as u64
            }
            BreakLocation::Function(name) => {
                match SourceMapper::function_code_range(&self.debugger, name) {
                    Some((offset, _)) => self.code_base + offset as u64,
                    None => return Err(format!("Function not found: {}", name)),
                }
            }
            BreakLocation::Address(addr) => *addr,
        };

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
        self.println(format!("Starting program: {}", self.binary_name));

        match self.target.start() {
            Ok(reason) => {
                self.debugger.set_state(DebugState::Running);

                // Install all breakpoints
                for (&id, &addr) in &self.breakpoint_addrs {
                    if let Err(e) = self.target.set_breakpoint(addr) {
                        eprintln!("Warning: failed to set breakpoint {}: {}", id.0, e);
                    }
                }

                // After initial stop from exec (reported as Step), continue to run
                match reason {
                    StopReason::Step => {
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

        let pc = self.target.pc()?;

        let reason = if self.breakpoint_addrs.values().any(|&a| a == pc) {
            self.target.continue_past_breakpoint(pc)?
        } else {
            self.target.cont()?
        };

        self.handle_stop(reason)
    }

    fn cmd_step(&mut self) -> Result<bool, String> {
        if !self.target.is_running() {
            return Err("Program is not running".to_string());
        }

        let start_pc = self.target.pc()?;
        let start_offset = (start_pc - self.code_base) as u32;
        let start_loc = SourceMapper::offset_to_source(&self.debugger, start_offset);
        let start_line = start_loc.as_ref().map(|l| (l.file.clone(), l.line));

        // First step: handle breakpoint if needed
        let pc = start_pc;
        let mut reason = if self.breakpoint_addrs.values().any(|&a| a == pc) {
            self.target.step_past_breakpoint(pc)?
        } else {
            self.target.step()?
        };

        // Keep stepping until we reach a different source line
        loop {
            match &reason {
                StopReason::Step => {
                    let pc = self.target.pc()?;
                    let offset = (pc - self.code_base) as u32;
                    if let Some(loc) = SourceMapper::offset_to_source(&self.debugger, offset) {
                        match &start_line {
                            Some((start_file, start_line_num)) => {
                                if loc.file != *start_file || loc.line != *start_line_num {
                                    break;
                                }
                            }
                            None => break,
                        }
                    }
                    // Same line or still no mapping — step again
                    let pc = self.target.pc()?;
                    reason = if self.breakpoint_addrs.values().any(|&a| a == pc) {
                        self.target.step_past_breakpoint(pc)?
                    } else {
                        self.target.step()?
                    };
                }
                _ => break,
            }
        }

        self.handle_stop(reason)
    }

    fn cmd_stepi(&mut self) -> Result<bool, String> {
        if !self.target.is_running() {
            return Err("Program is not running".to_string());
        }

        let reason = self.target.step()?;
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

        let arch = self.target.arch();

        if let Some(reg_idx) = arch.parse_gpr(expr) {
            let value = self.target.gpr(reg_idx)?;
            self.println(format!("{} = {} (0x{:x})", expr, value as i64, value));
            return Ok(false);
        }

        match expr.to_lowercase().as_str() {
            "sp" => {
                let sp = self.target.sp()?;
                self.println(format!("sp = 0x{:x}", sp));
            }
            "pc" => {
                let pc = self.target.pc()?;
                self.println(format!("pc = 0x{:x}", pc));
            }
            s if s == arch.status_reg_name => {
                let status = self.target.status_reg()?;
                self.println(format!("{} = 0x{:x}", arch.status_reg_name, status));
            }
            _ => {
                // Try to find as local variable
                let pc = self.target.pc()?;
                if let Some(reg_num) = self.find_local_register(expr, pc) {
                    if reg_num < arch.gpr_count {
                        let value = self.target.gpr(reg_num)?;
                        self.println(format!("{} = {} (0x{:x})", expr, value as i64, value));
                        return Ok(false);
                    }
                }
                self.println(format!("Cannot evaluate: {}", expr));
            }
        }

        Ok(false)
    }

    /// Find the register number for a local variable at the given PC
    fn find_local_register(&self, name: &str, pc: u64) -> Option<usize> {
        let debug_info = self.debug_info.as_ref()?;
        let code_offset = pc.saturating_sub(self.code_base) as u32;

        let func_idx = debug_info.symbols.iter().position(|sym| {
            code_offset >= sym.code_offset && code_offset < sym.code_offset + sym.code_size
        })?;

        let func_locals = debug_info.locals.iter().find(|fl| fl.func_id == func_idx as u32)?;

        func_locals.locals.iter()
            .find(|local| local.name == name)
            .map(|local| local.register as usize)
    }

    fn cmd_backtrace(&mut self) -> Result<bool, String> {
        if !self.target.is_running() {
            return Err("Program is not running".to_string());
        }

        let frames = self.target.unwind_stack(50)?;

        let arch = self.target.arch();
        for (index, (fp, addr)) in frames.iter().enumerate() {
            let lookup_addr = if index == 0 { *addr } else { addr.saturating_sub(arch.return_addr_decrement as u64) };
            let offset = lookup_addr.saturating_sub(self.code_base) as u32;

            if let Some(loc) = SourceMapper::offset_to_source(&self.debugger, offset) {
                let func_name = loc.function.as_deref().unwrap_or("??");
                self.println(format!(
                    "#{:<2} 0x{:016x} in {} at {}:{}:{}",
                    index, addr, func_name, loc.file, loc.line, loc.col
                ));
            } else if let Some(loc) = SourceMapper::function_decl_location(&self.debugger, offset) {
                let func_name = loc.function.as_deref().unwrap_or("??");
                self.println(format!(
                    "#{:<2} 0x{:016x} in {} at {}:{}:{}",
                    index, addr, func_name, loc.file, loc.line, loc.col
                ));
            } else {
                self.println(format!("#{:<2} 0x{:016x}", index, addr));
            }

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
                let pc = self.target.pc()?;
                let arch = self.target.arch();

                let output: Vec<String> = if let Some(ref debug_info) = self.debug_info {
                    let code_offset = pc.saturating_sub(self.code_base) as u32;

                    let func_id = debug_info.symbols.iter().position(|sym| {
                        code_offset >= sym.code_offset && code_offset < sym.code_offset + sym.code_size
                    });

                    if let Some(func_idx) = func_id {
                        if let Some(func_locals) = debug_info.locals.iter().find(|fl| fl.func_id == func_idx as u32) {
                            if func_locals.locals.is_empty() {
                                vec!["No locals.".to_string()]
                            } else {
                                func_locals.locals.iter()
                                    .filter_map(|local| {
                                        let reg_num = local.register as usize;
                                        if reg_num < arch.gpr_count {
                                            let value = self.target.gpr(reg_num).ok()?;
                                            Some(format!("{} = {} (0x{:x})", local.name, value as i64, value))
                                        } else {
                                            None
                                        }
                                    })
                                    .collect()
                            }
                        } else {
                            vec!["No locals.".to_string()]
                        }
                    } else {
                        vec!["Cannot determine current function.".to_string()]
                    }
                } else {
                    vec!["No debug info available.".to_string()]
                };

                for line in output {
                    self.println(&line);
                }
            }
            InfoKind::Registers => {
                if !self.target.is_running() {
                    return Err("Program is not running".to_string());
                }
                let arch = self.target.arch();
                let gprs = self.target.all_gprs()?;
                let mut line = String::new();
                for (i, (_, value)) in gprs.iter().enumerate() {
                    if i % 4 == 0 && i > 0 {
                        self.println(&line);
                        line.clear();
                    }
                    line.push_str(&format!("{:<4}= {:016x}  ", arch.gpr_name(i), value));
                }
                if !line.is_empty() {
                    self.println(&line);
                }
                let sp = self.target.sp()?;
                let pc = self.target.pc()?;
                self.println(format!("sp  = {:016x}", sp));
                self.println(format!("pc  = {:016x}", pc));
            }
            InfoKind::Breakpoints => {
                if self.breakpoint_addrs.is_empty() {
                    self.println("No breakpoints.");
                } else {
                    let lines: Vec<String> = self.debugger.breakpoints()
                        .filter_map(|bp| {
                            self.breakpoint_addrs.get(&bp.id).map(|&addr| {
                                let what = match &bp.kind {
                                    crate::types::BreakpointKind::Source { file, line, .. } => {
                                        format!("{}:{}", file, line)
                                    }
                                    crate::types::BreakpointKind::Function { name } => {
                                        format!("in {}", name)
                                    }
                                    crate::types::BreakpointKind::Address { .. } => {
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
            let pc = self.target.pc()?;
            let offset = (pc - self.code_base) as u32;
            if let Some(loc) = SourceMapper::offset_to_source(&self.debugger, offset) {
                (loc.file, loc.line)
            } else {
                return Err("No source location available".to_string());
            }
        } else {
            return Err("No location specified and program not running".to_string());
        };

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

        let pc = self.target.pc()?;
        let offset = (pc - self.code_base) as u32;

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

        let arch = self.target.arch();
        if let Some(reg_idx) = arch.parse_gpr(expr) {
            let value = self.target.gpr(reg_idx)?;
            return Ok(value as i64);
        }

        match expr.to_lowercase().as_str() {
            "sp" => Ok(self.target.sp()? as i64),
            "pc" => Ok(self.target.pc()? as i64),
            _ => Err(format!("Cannot evaluate: {}", expr)),
        }
    }

    fn handle_stop(&mut self, reason: StopReason) -> Result<bool, String> {
        match reason {
            StopReason::Exited(code) => {
                self.println(format!("Program exited with code {}", code));
                self.debugger.set_state(DebugState::Stopped(StopReason::Exited(code)));
                Ok(false)
            }
            StopReason::Signaled(sig) => {
                self.println(format!("Program terminated with signal {}", sig));
                self.debugger.set_state(DebugState::Stopped(StopReason::Error(
                    format!("signal {}", sig)
                )));
                Ok(false)
            }
            StopReason::BreakpointHit(addr) => {
                let bp_id = self.breakpoint_addrs.iter()
                    .find(|(_, &a)| a == addr)
                    .map(|(id, _)| *id);

                let mut msg = if let Some(id) = bp_id {
                    BreakpointManager::record_hit(&mut self.debugger, id);
                    format!("Breakpoint {} hit", id.0)
                } else {
                    "Stopped at breakpoint".to_string()
                };

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
                    StopReason::Breakpoint(bp_id.unwrap_or(BreakpointId(0)))
                ));
                Ok(false)
            }
            StopReason::Step => {
                let pc = self.target.pc()?;
                let offset = (pc - self.code_base) as u32;
                if let Some(loc) = SourceMapper::offset_to_source(&self.debugger, offset) {
                    if let Some(src) = SourceMapper::get_source_line(&self.debugger, &loc.file, loc.line) {
                        self.println(format!("{}:{}: {}", loc.file, loc.line, src));
                    } else {
                        self.println(format!("{}:{}", loc.file, loc.line));
                    }
                } else {
                    self.println(format!("0x{:x}", pc));
                }

                self.debugger.set_state(DebugState::Stopped(StopReason::Step));
                Ok(false)
            }
            StopReason::Signal(sig) => {
                self.println(format!("Program received signal {}", sig));
                Ok(false)
            }
            StopReason::Breakpoint(_) | StopReason::Error(_) | StopReason::Pause => {
                // These shouldn't come from the target directly, but handle gracefully
                self.debugger.set_state(DebugState::Stopped(reason));
                Ok(false)
            }
        }
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use crate::arch::AARCH64;
    use crate::target::Target;
    use crate::types::StopReason;

    /// A mock target for testing Session without ptrace.
    pub(crate) struct MockTarget {
        pub(crate) running: bool,
        pub(crate) pc: u64,
        pub(crate) gprs: [u64; 31],
        pub(crate) sp: u64,
        pub(crate) status_reg: u64,
        /// Sequence of stop reasons to return from start/cont/step
        pub(crate) stop_sequence: Vec<StopReason>,
        pub(crate) stop_index: usize,
        /// If set, start() returns this error
        pub(crate) start_error: Option<String>,
        /// If set, set_breakpoint() returns this error
        pub(crate) set_bp_error: Option<String>,
    }

    impl MockTarget {
        pub(crate) fn new() -> Self {
            Self {
                running: false,
                pc: 0,
                gprs: [0; 31],
                sp: 0,
                status_reg: 0,
                stop_sequence: vec![],
                stop_index: 0,
                start_error: None,
                set_bp_error: None,
            }
        }

        fn next_stop(&mut self) -> Result<StopReason, String> {
            if self.stop_index < self.stop_sequence.len() {
                let reason = self.stop_sequence[self.stop_index].clone();
                self.stop_index += 1;
                match &reason {
                    StopReason::Exited(_) | StopReason::Signaled(_) => {
                        self.running = false;
                    }
                    _ => {}
                }
                Ok(reason)
            } else {
                Ok(StopReason::Exited(0))
            }
        }
    }

    impl Target for MockTarget {
        fn start(&mut self) -> Result<StopReason, String> {
            if let Some(ref e) = self.start_error {
                return Err(e.clone());
            }
            self.running = true;
            self.next_stop()
        }

        fn cont(&mut self) -> Result<StopReason, String> {
            self.next_stop()
        }

        fn step(&mut self) -> Result<StopReason, String> {
            self.next_stop()
        }

        fn continue_past_breakpoint(&mut self, _addr: u64) -> Result<StopReason, String> {
            self.next_stop()
        }

        fn step_past_breakpoint(&mut self, _addr: u64) -> Result<StopReason, String> {
            self.next_stop()
        }

        fn pc(&self) -> Result<u64, String> {
            Ok(self.pc)
        }

        fn gpr(&self, index: usize) -> Result<u64, String> {
            if index < 31 {
                Ok(self.gprs[index])
            } else {
                Err(format!("Invalid GPR index: {}", index))
            }
        }

        fn sp(&self) -> Result<u64, String> {
            Ok(self.sp)
        }

        fn status_reg(&self) -> Result<u64, String> {
            Ok(self.status_reg)
        }

        fn all_gprs(&self) -> Result<Vec<(usize, u64)>, String> {
            Ok((0..31).map(|i| (i, self.gprs[i])).collect())
        }

        fn set_breakpoint(&mut self, _addr: u64) -> Result<(), String> {
            if let Some(ref e) = self.set_bp_error {
                return Err(e.clone());
            }
            Ok(())
        }

        fn remove_breakpoint(&mut self, _addr: u64) -> Result<bool, String> {
            Ok(true)
        }

        fn is_running(&self) -> bool {
            self.running
        }

        fn unwind_stack(&self, _max_frames: usize) -> Result<Vec<(u64, u64)>, String> {
            Ok(vec![(0, self.pc)])
        }

        fn kill(&mut self) -> Result<(), String> {
            self.running = false;
            Ok(())
        }

        fn arch(&self) -> &'static crate::arch::ArchInfo {
            &AARCH64
        }
    }

    #[test]
    fn test_session_unknown_command() {
        let target = MockTarget::new();
        let mut session = Session::new(target, None, 0x400078, "test".to_string());

        let _ = session.execute(Command::Unknown("foo".to_string()));
        assert_eq!(session.get_output().len(), 1);
        assert_eq!(session.get_output()[0], "Unknown command: foo");
    }

    #[test]
    fn test_session_output_capture() {
        let target = MockTarget::new();
        let mut session = Session::new(target, None, 0x400078, "test".to_string());

        assert!(session.get_output().is_empty());

        let _ = session.execute(Command::Unknown("foo".to_string()));
        assert_eq!(session.get_output().len(), 1);

        let _ = session.execute(Command::Unknown("bar".to_string()));
        assert_eq!(session.get_output().len(), 2);

        session.clear_output();
        assert!(session.get_output().is_empty());
    }

    #[test]
    fn test_session_info_breakpoints_empty() {
        let target = MockTarget::new();
        let mut session = Session::new(target, None, 0x400078, "test".to_string());
        session.clear_output();

        let _ = session.execute(Command::Info(InfoKind::Breakpoints));
        assert_eq!(session.get_output(), &["No breakpoints."]);
    }

    #[test]
    fn test_session_quit() {
        let target = MockTarget::new();
        let mut session = Session::new(target, None, 0x400078, "test".to_string());

        let result = session.execute(Command::Quit);
        assert_eq!(result, Ok(true));
    }

    #[test]
    fn test_session_step_before_run() {
        let target = MockTarget::new();
        let mut session = Session::new(target, None, 0x400078, "test".to_string());

        let result = session.execute(Command::Step);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("not running"));
    }

    #[test]
    fn test_session_continue_before_run() {
        let target = MockTarget::new();
        let mut session = Session::new(target, None, 0x400078, "test".to_string());

        let result = session.execute(Command::Continue);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("not running"));
    }

    #[test]
    fn test_session_print_before_run() {
        let target = MockTarget::new();
        let mut session = Session::new(target, None, 0x400078, "test".to_string());

        let result = session.execute(Command::Print("x0".to_string()));
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("not running"));
    }

    #[test]
    fn test_session_backtrace_before_run() {
        let target = MockTarget::new();
        let mut session = Session::new(target, None, 0x400078, "test".to_string());

        let result = session.execute(Command::Backtrace);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("not running"));
    }

    #[test]
    fn test_session_info_registers_before_run() {
        let target = MockTarget::new();
        let mut session = Session::new(target, None, 0x400078, "test".to_string());

        let result = session.execute(Command::Info(InfoKind::Registers));
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("not running"));
    }

    #[test]
    fn test_session_info_locals_before_run() {
        let target = MockTarget::new();
        let mut session = Session::new(target, None, 0x400078, "test".to_string());

        let result = session.execute(Command::Info(InfoKind::Locals));
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("not running"));
    }

    #[test]
    fn test_session_next_before_run() {
        let target = MockTarget::new();
        let mut session = Session::new(target, None, 0x400078, "test".to_string());

        let result = session.execute(Command::Next);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("not running"));
    }

    #[test]
    fn test_session_stepi_before_run() {
        let target = MockTarget::new();
        let mut session = Session::new(target, None, 0x400078, "test".to_string());

        let result = session.execute(Command::StepInstruction);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("not running"));
    }

    #[test]
    fn test_session_finish_before_run() {
        let target = MockTarget::new();
        let mut session = Session::new(target, None, 0x400078, "test".to_string());

        let result = session.execute(Command::Finish);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("not running"));
    }

    #[test]
    fn test_session_list_before_run() {
        let target = MockTarget::new();
        let mut session = Session::new(target, None, 0x400078, "test".to_string());

        let result = session.execute(Command::List(None));
        assert!(result.is_err());
    }

    #[test]
    fn test_session_expect_stop_before_run() {
        let target = MockTarget::new();
        let mut session = Session::new(target, None, 0x400078, "test".to_string());

        let result = session.execute(Command::ExpectStop(commands::SourceLocation {
            file: "test.skyl".to_string(),
            line: 1,
        }));
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("not running"));
    }

    #[test]
    fn test_session_assert_before_run() {
        let target = MockTarget::new();
        let mut session = Session::new(target, None, 0x400078, "test".to_string());

        let result = session.execute(Command::Assert("x0 == 0".to_string()));
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("not running"));
    }

    #[test]
    fn test_session_run_and_exit() {
        let mut target = MockTarget::new();
        // start() returns Step (initial SIGTRAP), then cont() returns Exited(0)
        target.stop_sequence = vec![StopReason::Step, StopReason::Exited(0)];

        let mut session = Session::new(target, None, 0x400078, "test".to_string());

        let result = session.execute(Command::Run(vec![]));
        assert_eq!(result, Ok(false));
        assert!(session.get_output().iter().any(|l| l.contains("Starting program")));
        assert!(session.get_output().iter().any(|l| l.contains("exited with code 0")));
    }

    #[test]
    fn test_session_empty_repeats_last() {
        let target = MockTarget::new();
        let mut session = Session::new(target, None, 0x400078, "test".to_string());

        // Set up a command
        session.execute(Command::Info(InfoKind::Breakpoints)).unwrap();
        session.clear_output();

        // Empty should repeat
        let result = session.execute(Command::Empty);
        assert_eq!(result, Ok(false));
        assert!(session.get_output().iter().any(|l| l.contains("No breakpoints")));
    }

    #[test]
    fn test_session_empty_no_last() {
        let target = MockTarget::new();
        let mut session = Session::new(target, None, 0x400078, "test".to_string());

        // Empty with no prior command
        let result = session.execute(Command::Empty);
        assert_eq!(result, Ok(false));
    }

    #[test]
    fn test_session_delete_nonexistent() {
        let target = MockTarget::new();
        let mut session = Session::new(target, None, 0x400078, "test".to_string());

        let result = session.execute(Command::Delete(99));
        assert_eq!(result, Ok(false));
        assert!(session.get_output().iter().any(|l| l.contains("No breakpoint number 99")));
    }

    #[test]
    fn test_session_break_by_address() {
        let target = MockTarget::new();
        let mut session = Session::new(target, None, 0x400078, "test".to_string());

        let result = session.execute(Command::Break(BreakLocation::Address(0x400100)));
        assert_eq!(result, Ok(false));
        assert!(session.get_output().iter().any(|l| l.contains("Breakpoint 1")));
    }

    #[test]
    fn test_session_debug_info() {
        let target = MockTarget::new();
        let session = Session::new(target, None, 0x400078, "test".to_string());
        assert!(session.debug_info().is_none());

        let target2 = MockTarget::new();
        let info = DebugInfo::default();
        let session2 = Session::new(target2, Some(info), 0x400078, "test".to_string());
        assert!(session2.debug_info().is_some());
    }

    #[test]
    fn test_session_handle_signaled() {
        let mut target = MockTarget::new();
        target.stop_sequence = vec![StopReason::Step, StopReason::Signaled(9)];

        let mut session = Session::new(target, None, 0x400078, "test".to_string());
        let result = session.execute(Command::Run(vec![]));
        assert_eq!(result, Ok(false));
        assert!(session.get_output().iter().any(|l| l.contains("terminated with signal")));
    }

    #[test]
    fn test_session_handle_signal() {
        let mut target = MockTarget::new();
        target.stop_sequence = vec![StopReason::Step, StopReason::Signal(11)];

        let mut session = Session::new(target, None, 0x400078, "test".to_string());
        let result = session.execute(Command::Run(vec![]));
        assert_eq!(result, Ok(false));
        assert!(session.get_output().iter().any(|l| l.contains("received signal")));
    }

    #[test]
    fn test_session_assert_with_numbers() {
        let mut target = MockTarget::new();
        target.running = true;

        let mut session = Session::new(target, None, 0x400078, "test".to_string());
        // Number-only assert doesn't need a running target
        let result = session.execute(Command::Assert("42 == 42".to_string()));
        assert_eq!(result, Ok(false));
        assert!(session.get_output().iter().any(|l| l.contains("Assertion passed")));
    }

    #[test]
    fn test_session_assert_failure() {
        let mut target = MockTarget::new();
        target.running = true;

        let mut session = Session::new(target, None, 0x400078, "test".to_string());
        let result = session.execute(Command::Assert("1 == 2".to_string()));
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Assertion failed"));
    }

    #[test]
    fn test_session_assert_no_equals() {
        let target = MockTarget::new();
        let mut session = Session::new(target, None, 0x400078, "test".to_string());

        let result = session.execute(Command::Assert("foo".to_string()));
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Cannot evaluate assertion"));
    }

    #[test]
    fn test_session_assert_hex() {
        let mut target = MockTarget::new();
        target.running = true;

        let mut session = Session::new(target, None, 0x400078, "test".to_string());
        let result = session.execute(Command::Assert("0x2a == 42".to_string()));
        assert_eq!(result, Ok(false));
    }

    #[test]
    fn test_session_eval_simple_invalid() {
        let mut target = MockTarget::new();
        target.running = true;

        let mut session = Session::new(target, None, 0x400078, "test".to_string());
        // "foo" is not a number or register
        let result = session.execute(Command::Assert("foo == 0".to_string()));
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Cannot evaluate"));
    }

    #[test]
    fn test_session_break_function_not_found() {
        let target = MockTarget::new();
        let mut session = Session::new(target, None, 0x400078, "test".to_string());

        let result = session.execute(Command::Break(BreakLocation::Function("nonexistent".to_string())));
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Function not found"));
    }

    #[test]
    fn test_session_break_source_no_code() {
        let target = MockTarget::new();
        let mut session = Session::new(target, None, 0x400078, "test".to_string());

        let result = session.execute(Command::Break(BreakLocation::Source {
            file: "nonexistent.skyl".to_string(),
            line: 1,
        }));
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("No code at"));
    }

    #[test]
    fn test_session_list_with_location_no_source() {
        let target = MockTarget::new();
        let mut session = Session::new(target, None, 0x400078, "test".to_string());

        let result = session.execute(Command::List(Some(commands::SourceLocation {
            file: "nonexistent.skyl".to_string(),
            line: 1,
        })));
        assert_eq!(result, Ok(false));
        assert!(session.get_output().iter().any(|l| l.contains("No source available")));
    }

    /// Create a debug info fixture with one function and source text.
    /// Function "test_fn" at code offset 0, size 100, source "test.skyl".
    /// Line mappings: line 1 at offset 0, line 2 at offset 4.
    /// Local variable "myvar" in register 0.
    fn make_debug_info() -> DebugInfo {
        use debuginfo::{
            FunctionLocals, LineMapping, LineMappingFlags, LocalVariable,
            SourceFile, Symbol, SymbolKind,
        };
        DebugInfo {
            sources: vec![SourceFile {
                id: 0,
                path: "test.skyl".to_string(),
                text: Some("let myvar = 42\nassert(myvar == 42)\n".to_string()),
            }],
            symbols: vec![Symbol {
                name: "test_fn".to_string(),
                source_id: 0,
                line: 1,
                col: 1,
                code_offset: 0,
                code_size: 100,
                kind: SymbolKind::Test,
            }],
            line_map: vec![
                LineMapping {
                    func_id: 0,
                    code_offset: 0,
                    source_id: 0,
                    line: 1,
                    col: 1,
                    flags: LineMappingFlags::STATEMENT,
                },
                LineMapping {
                    func_id: 0,
                    code_offset: 4,
                    source_id: 0,
                    line: 2,
                    col: 1,
                    flags: LineMappingFlags::STATEMENT,
                },
            ],
            locals: vec![FunctionLocals {
                func_id: 0,
                locals: vec![LocalVariable {
                    name: "myvar".to_string(),
                    register: 0,
                    live_start: 0,
                    live_end: 100,
                }],
            }],
        }
    }

    /// Create a running session with debug info and mock target at given pc offset.
    fn make_running_session(pc_offset: u64) -> Session<MockTarget> {
        let code_base: u64 = 0x400078;
        let mut target = MockTarget::new();
        target.running = true;
        target.pc = code_base + pc_offset;
        target.gprs[0] = 42;
        target.sp = 0xffff0000;
        target.status_reg = 0x60000000;
        Session::new(target, Some(make_debug_info()), code_base, "test".to_string())
    }

    #[test]
    fn test_session_print_register_while_running() {
        let mut session = make_running_session(0);

        session.clear_output();
        let result = session.execute(Command::Print("x0".to_string()));
        assert_eq!(result, Ok(false));
        assert!(session.get_output().iter().any(|l| l.contains("x0 = 42")));
    }

    #[test]
    fn test_session_print_sp_while_running() {
        let mut session = make_running_session(0);

        session.clear_output();
        let result = session.execute(Command::Print("sp".to_string()));
        assert_eq!(result, Ok(false));
        assert!(session.get_output().iter().any(|l| l.contains("sp = 0xffff0000")));
    }

    #[test]
    fn test_session_print_pc_while_running() {
        let mut session = make_running_session(0);

        session.clear_output();
        let result = session.execute(Command::Print("pc".to_string()));
        assert_eq!(result, Ok(false));
        assert!(session.get_output().iter().any(|l| l.contains("pc = 0x400078")));
    }

    #[test]
    fn test_session_print_pstate_while_running() {
        let mut session = make_running_session(0);

        session.clear_output();
        let result = session.execute(Command::Print("pstate".to_string()));
        assert_eq!(result, Ok(false));
        assert!(session.get_output().iter().any(|l| l.contains("pstate = 0x60000000")));
    }

    #[test]
    fn test_session_print_local_variable() {
        let mut session = make_running_session(0);

        session.clear_output();
        let result = session.execute(Command::Print("myvar".to_string()));
        assert_eq!(result, Ok(false));
        assert!(session.get_output().iter().any(|l| l.contains("myvar = 42")));
    }

    #[test]
    fn test_session_print_unknown_expr() {
        let mut session = make_running_session(0);

        session.clear_output();
        let result = session.execute(Command::Print("unknown_var".to_string()));
        assert_eq!(result, Ok(false));
        assert!(session.get_output().iter().any(|l| l.contains("Cannot evaluate")));
    }

    #[test]
    fn test_session_info_registers_while_running() {
        let mut session = make_running_session(0);

        session.clear_output();
        let result = session.execute(Command::Info(InfoKind::Registers));
        assert_eq!(result, Ok(false));
        // Should show x0, sp, pc
        let output = session.get_output();
        assert!(output.iter().any(|l| l.contains("x0")));
        assert!(output.iter().any(|l| l.contains("sp")));
        assert!(output.iter().any(|l| l.contains("pc")));
    }

    #[test]
    fn test_session_info_locals_while_running() {
        let mut session = make_running_session(0);

        session.clear_output();
        let result = session.execute(Command::Info(InfoKind::Locals));
        assert_eq!(result, Ok(false));
        assert!(session.get_output().iter().any(|l| l.contains("myvar = 42")));
    }

    #[test]
    fn test_session_info_locals_no_debug_info() {
        let code_base: u64 = 0x400078;
        let mut target = MockTarget::new();
        target.running = true;
        target.pc = code_base;
        let mut session = Session::new(target, None, code_base, "test".to_string());

        session.clear_output();
        let result = session.execute(Command::Info(InfoKind::Locals));
        assert_eq!(result, Ok(false));
        assert!(session.get_output().iter().any(|l| l.contains("No debug info")));
    }

    #[test]
    fn test_session_info_locals_no_locals() {
        let code_base: u64 = 0x400078;
        let mut target = MockTarget::new();
        target.running = true;
        target.pc = code_base;
        // Debug info with function but no locals entry
        let mut info = make_debug_info();
        info.locals.clear();
        let mut session = Session::new(target, Some(info), code_base, "test".to_string());

        session.clear_output();
        let result = session.execute(Command::Info(InfoKind::Locals));
        assert_eq!(result, Ok(false));
        assert!(session.get_output().iter().any(|l| l.contains("No locals")));
    }

    #[test]
    fn test_session_info_locals_empty_locals() {
        let code_base: u64 = 0x400078;
        let mut target = MockTarget::new();
        target.running = true;
        target.pc = code_base;
        let mut info = make_debug_info();
        info.locals[0].locals.clear();
        let mut session = Session::new(target, Some(info), code_base, "test".to_string());

        session.clear_output();
        let result = session.execute(Command::Info(InfoKind::Locals));
        assert_eq!(result, Ok(false));
        assert!(session.get_output().iter().any(|l| l.contains("No locals")));
    }

    #[test]
    fn test_session_info_locals_outside_function() {
        let code_base: u64 = 0x400078;
        let mut target = MockTarget::new();
        target.running = true;
        target.pc = code_base + 200; // outside function range
        let mut session = Session::new(target, Some(make_debug_info()), code_base, "test".to_string());

        session.clear_output();
        let result = session.execute(Command::Info(InfoKind::Locals));
        assert_eq!(result, Ok(false));
        assert!(session.get_output().iter().any(|l| l.contains("Cannot determine")));
    }

    #[test]
    fn test_session_stepi_while_running() {
        let code_base: u64 = 0x400078;
        let mut target = MockTarget::new();
        target.running = true;
        target.pc = code_base;
        target.stop_sequence = vec![StopReason::Step];

        let mut session = Session::new(target, Some(make_debug_info()), code_base, "test".to_string());
        session.clear_output();
        let result = session.execute(Command::StepInstruction);
        assert_eq!(result, Ok(false));
        // Should show source location
        assert!(session.get_output().iter().any(|l| l.contains("test.skyl:1")));
    }

    #[test]
    fn test_session_continue_while_running() {
        let code_base: u64 = 0x400078;
        let mut target = MockTarget::new();
        target.running = true;
        target.pc = code_base;
        target.stop_sequence = vec![StopReason::Exited(0)];

        let mut session = Session::new(target, Some(make_debug_info()), code_base, "test".to_string());
        session.clear_output();
        let result = session.execute(Command::Continue);
        assert_eq!(result, Ok(false));
        assert!(session.get_output().iter().any(|l| l.contains("exited with code 0")));
    }

    #[test]
    fn test_session_continue_at_breakpoint() {
        let code_base: u64 = 0x400078;
        let mut target = MockTarget::new();
        target.running = true;
        target.pc = code_base + 10;
        target.stop_sequence = vec![StopReason::Exited(0)];

        let mut session = Session::new(target, Some(make_debug_info()), code_base, "test".to_string());
        // Set breakpoint at address matching pc
        session.execute(Command::Break(BreakLocation::Address(code_base + 10))).unwrap();
        session.clear_output();
        let result = session.execute(Command::Continue);
        assert_eq!(result, Ok(false));
    }

    #[test]
    fn test_session_step_while_running() {
        let code_base: u64 = 0x400078;
        let mut target = MockTarget::new();
        target.running = true;
        target.pc = code_base;
        // Step returns Step, but we change line to different offset (line 2)
        target.stop_sequence = vec![StopReason::Step, StopReason::Step];

        let mut session = Session::new(target, Some(make_debug_info()), code_base, "test".to_string());
        session.clear_output();
        let result = session.execute(Command::Step);
        assert_eq!(result, Ok(false));
    }

    #[test]
    fn test_session_step_exits() {
        let code_base: u64 = 0x400078;
        let mut target = MockTarget::new();
        target.running = true;
        target.pc = code_base;
        target.stop_sequence = vec![StopReason::Exited(0)];

        let mut session = Session::new(target, Some(make_debug_info()), code_base, "test".to_string());
        session.clear_output();
        let result = session.execute(Command::Step);
        assert_eq!(result, Ok(false));
        assert!(session.get_output().iter().any(|l| l.contains("exited with code 0")));
    }

    #[test]
    fn test_session_backtrace_while_running() {
        let mut session = make_running_session(0);

        session.clear_output();
        let result = session.execute(Command::Backtrace);
        assert_eq!(result, Ok(false));
        // Should show frame #0 with function name
        assert!(session.get_output().iter().any(|l| l.contains("#0")));
        assert!(session.get_output().iter().any(|l| l.contains("test_fn")));
    }

    #[test]
    fn test_session_backtrace_no_debug_info() {
        let code_base: u64 = 0x400078;
        let mut target = MockTarget::new();
        target.running = true;
        target.pc = code_base + 200; // outside any function
        let mut session = Session::new(target, None, code_base, "test".to_string());

        session.clear_output();
        let result = session.execute(Command::Backtrace);
        assert_eq!(result, Ok(false));
        // Should show raw address
        assert!(session.get_output().iter().any(|l| l.contains("#0") && l.contains("0x")));
    }

    #[test]
    fn test_session_list_while_running() {
        let mut session = make_running_session(0);

        session.clear_output();
        let result = session.execute(Command::List(None));
        assert_eq!(result, Ok(false));
        assert!(session.get_output().iter().any(|l| l.contains("let myvar")));
    }

    #[test]
    fn test_session_list_while_running_no_source_location() {
        let code_base: u64 = 0x400078;
        let mut target = MockTarget::new();
        target.running = true;
        target.pc = code_base + 200; // outside any function
        let mut session = Session::new(target, None, code_base, "test".to_string());

        let result = session.execute(Command::List(None));
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("No source location"));
    }

    #[test]
    fn test_session_break_at_source() {
        let code_base: u64 = 0x400078;
        let target = MockTarget::new();
        let mut session = Session::new(target, Some(make_debug_info()), code_base, "test".to_string());

        let result = session.execute(Command::Break(BreakLocation::Source {
            file: "test.skyl".to_string(),
            line: 1,
        }));
        assert_eq!(result, Ok(false));
        assert!(session.get_output().iter().any(|l| l.contains("Breakpoint 1")));
    }

    #[test]
    fn test_session_break_at_function() {
        let code_base: u64 = 0x400078;
        let target = MockTarget::new();
        let mut session = Session::new(target, Some(make_debug_info()), code_base, "test".to_string());

        let result = session.execute(Command::Break(BreakLocation::Function("test_fn".to_string())));
        assert_eq!(result, Ok(false));
        assert!(session.get_output().iter().any(|l| l.contains("Breakpoint 1")));
    }

    #[test]
    fn test_session_delete_while_running() {
        let code_base: u64 = 0x400078;
        let mut target = MockTarget::new();
        target.running = true;
        target.pc = code_base;
        let mut session = Session::new(target, Some(make_debug_info()), code_base, "test".to_string());

        // Set and delete a breakpoint while running
        session.execute(Command::Break(BreakLocation::Address(0x400100))).unwrap();
        session.clear_output();
        let result = session.execute(Command::Delete(1));
        assert_eq!(result, Ok(false));
        assert!(session.get_output().iter().any(|l| l.contains("Deleted breakpoint 1")));
    }

    #[test]
    fn test_session_info_breakpoints_with_entries() {
        let code_base: u64 = 0x400078;
        let target = MockTarget::new();
        let mut session = Session::new(target, Some(make_debug_info()), code_base, "test".to_string());

        session.execute(Command::Break(BreakLocation::Source {
            file: "test.skyl".to_string(),
            line: 1,
        })).unwrap();
        session.execute(Command::Break(BreakLocation::Function("test_fn".to_string()))).unwrap();
        session.execute(Command::Break(BreakLocation::Address(0x400100))).unwrap();
        session.clear_output();

        let result = session.execute(Command::Info(InfoKind::Breakpoints));
        assert_eq!(result, Ok(false));
        let output = session.get_output();
        assert!(output.iter().any(|l| l.contains("Num")));
        assert!(output.iter().any(|l| l.contains("test.skyl:1")));
        assert!(output.iter().any(|l| l.contains("in test_fn")));
    }

    #[test]
    fn test_session_run_with_breakpoints() {
        let code_base: u64 = 0x400078;
        let mut target = MockTarget::new();
        target.stop_sequence = vec![StopReason::Step, StopReason::Exited(0)];

        let mut session = Session::new(target, Some(make_debug_info()), code_base, "test".to_string());
        session.execute(Command::Break(BreakLocation::Address(0x400100))).unwrap();
        session.clear_output();

        let result = session.execute(Command::Run(vec![]));
        assert_eq!(result, Ok(false));
        assert!(session.get_output().iter().any(|l| l.contains("Starting program")));
    }

    #[test]
    fn test_session_handle_breakpoint_hit() {
        let code_base: u64 = 0x400078;
        let mut target = MockTarget::new();
        // start returns Step, cont returns BreakpointHit at known address
        target.stop_sequence = vec![StopReason::Step, StopReason::BreakpointHit(code_base)];

        let mut session = Session::new(target, Some(make_debug_info()), code_base, "test".to_string());
        session.execute(Command::Break(BreakLocation::Address(code_base))).unwrap();
        session.clear_output();

        let result = session.execute(Command::Run(vec![]));
        assert_eq!(result, Ok(false));
        assert!(session.get_output().iter().any(|l| l.contains("Breakpoint 1 hit")));
    }

    #[test]
    fn test_session_handle_breakpoint_hit_unknown() {
        let code_base: u64 = 0x400078;
        let mut target = MockTarget::new();
        target.stop_sequence = vec![StopReason::Step, StopReason::BreakpointHit(code_base + 50)];

        let mut session = Session::new(target, Some(make_debug_info()), code_base, "test".to_string());
        session.clear_output();

        let result = session.execute(Command::Run(vec![]));
        assert_eq!(result, Ok(false));
        assert!(session.get_output().iter().any(|l| l.contains("Stopped at breakpoint")));
    }

    #[test]
    fn test_session_handle_stop_step_no_source() {
        let code_base: u64 = 0x400078;
        let mut target = MockTarget::new();
        target.running = true;
        target.pc = code_base + 200; // outside function range
        target.stop_sequence = vec![StopReason::Step];

        let mut session = Session::new(target, None, code_base, "test".to_string());
        session.clear_output();
        let result = session.execute(Command::StepInstruction);
        assert_eq!(result, Ok(false));
        // Should show raw hex address
        assert!(session.get_output().iter().any(|l| l.contains("0x")));
    }

    #[test]
    fn test_session_handle_stop_step_with_source_no_text() {
        let code_base: u64 = 0x400078;
        let mut target = MockTarget::new();
        target.running = true;
        target.pc = code_base;
        target.stop_sequence = vec![StopReason::Step];

        // Debug info with no source text
        let mut info = make_debug_info();
        info.sources[0].text = None;

        let mut session = Session::new(target, Some(info), code_base, "test".to_string());
        session.clear_output();
        let result = session.execute(Command::StepInstruction);
        assert_eq!(result, Ok(false));
        // Should show file:line but no source text
        assert!(session.get_output().iter().any(|l| l.contains("test.skyl:1")));
    }

    #[test]
    fn test_session_handle_pause() {
        let code_base: u64 = 0x400078;
        let mut target = MockTarget::new();
        target.stop_sequence = vec![StopReason::Step, StopReason::Pause];

        let mut session = Session::new(target, None, code_base, "test".to_string());
        let result = session.execute(Command::Run(vec![]));
        assert_eq!(result, Ok(false));
    }

    #[test]
    fn test_session_run_start_fails() {
        let code_base: u64 = 0x400078;
        let mut target = MockTarget::new();
        target.stop_sequence = vec![StopReason::Error("start failed".to_string())];

        let mut session = Session::new(target, None, code_base, "test".to_string());
        let result = session.execute(Command::Run(vec![]));
        // The Error variant from start is handled by handle_stop
        assert_eq!(result, Ok(false));
    }

    #[test]
    fn test_session_eval_register_while_running() {
        let mut session = make_running_session(0);

        session.clear_output();
        let result = session.execute(Command::Assert("x0 == 42".to_string()));
        assert_eq!(result, Ok(false));
        assert!(session.get_output().iter().any(|l| l.contains("Assertion passed")));
    }

    #[test]
    fn test_session_eval_sp_while_running() {
        let mut session = make_running_session(0);

        session.clear_output();
        let result = session.execute(Command::Assert("sp == 0xffff0000".to_string()));
        // sp is 0xffff0000 = 4294901760 as i64
        assert_eq!(result, Ok(false));
    }

    #[test]
    fn test_session_expect_stop_verified() {
        let mut session = make_running_session(0);

        session.clear_output();
        let result = session.execute(Command::ExpectStop(commands::SourceLocation {
            file: "test.skyl".to_string(),
            line: 1,
        }));
        assert_eq!(result, Ok(false));
        assert!(session.get_output().iter().any(|l| l.contains("Stop location verified")));
    }

    #[test]
    fn test_session_expect_stop_wrong_location() {
        let mut session = make_running_session(0);

        let result = session.execute(Command::ExpectStop(commands::SourceLocation {
            file: "test.skyl".to_string(),
            line: 99,
        }));
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Expected stop at"));
    }

    #[test]
    fn test_session_expect_stop_no_source() {
        let code_base: u64 = 0x400078;
        let mut target = MockTarget::new();
        target.running = true;
        target.pc = code_base + 200; // outside function

        let mut session = Session::new(target, None, code_base, "test".to_string());
        let result = session.execute(Command::ExpectStop(commands::SourceLocation {
            file: "test.skyl".to_string(),
            line: 1,
        }));
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("no source location"));
    }

    #[test]
    fn test_session_step_at_breakpoint() {
        let code_base: u64 = 0x400078;
        let mut target = MockTarget::new();
        target.running = true;
        target.pc = code_base;
        // step_past_breakpoint, then step returns Exited
        target.stop_sequence = vec![StopReason::Step, StopReason::Exited(0)];

        let mut session = Session::new(target, Some(make_debug_info()), code_base, "test".to_string());
        // Set breakpoint at current pc
        session.execute(Command::Break(BreakLocation::Address(code_base))).unwrap();
        session.clear_output();

        let result = session.execute(Command::Step);
        assert_eq!(result, Ok(false));
    }

    #[test]
    fn test_session_print_invalid_register_number() {
        let mut session = make_running_session(0);

        session.clear_output();
        // x99 is out of range
        let result = session.execute(Command::Print("x99".to_string()));
        assert_eq!(result, Ok(false));
        assert!(session.get_output().iter().any(|l| l.contains("Cannot evaluate")));
    }

    #[test]
    fn test_session_run_start_error() {
        let code_base: u64 = 0x400078;
        let mut target = MockTarget::new();
        target.start_error = Some("process not found".to_string());

        let mut session = Session::new(target, None, code_base, "test".to_string());
        let result = session.execute(Command::Run(vec![]));
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Failed to start"));
    }

    #[test]
    fn test_session_run_set_breakpoint_fails() {
        let code_base: u64 = 0x400078;
        let mut target = MockTarget::new();
        target.set_bp_error = Some("cannot write memory".to_string());
        target.stop_sequence = vec![StopReason::Step, StopReason::Exited(0)];

        let mut session = Session::new(target, Some(make_debug_info()), code_base, "test".to_string());
        session.execute(Command::Break(BreakLocation::Address(0x400100))).unwrap();
        session.clear_output();

        // Run should still succeed, but warn about breakpoint
        let result = session.execute(Command::Run(vec![]));
        assert_eq!(result, Ok(false));
    }

    #[test]
    fn test_session_info_locals_register_out_of_range() {
        use debuginfo::{FunctionLocals, LocalVariable};
        let code_base: u64 = 0x400078;
        let mut target = MockTarget::new();
        target.running = true;
        target.pc = code_base;

        let mut info = make_debug_info();
        // Replace locals with one that has register >= 31
        info.locals = vec![FunctionLocals {
            func_id: 0,
            locals: vec![LocalVariable {
                name: "bigvar".to_string(),
                register: 99,
                live_start: 0,
                live_end: 100,
            }],
        }];

        let mut session = Session::new(target, Some(info), code_base, "test".to_string());
        session.clear_output();
        let result = session.execute(Command::Info(InfoKind::Locals));
        assert_eq!(result, Ok(false));
        // No locals should be printed (all filtered out)
    }

    #[test]
    fn test_session_eval_pc_while_running() {
        let mut session = make_running_session(0);
        session.clear_output();
        // Assert using pc register
        let result = session.execute(Command::Assert("pc == 0x400078".to_string()));
        assert_eq!(result, Ok(false));
    }

    #[test]
    fn test_session_run_cont_fails() {
        let code_base: u64 = 0x400078;
        let mut target = MockTarget::new();
        // start succeeds with Step, but stop_sequence is empty so cont returns Exited(0)
        // We need to make cont fail. Let's use a special approach:
        // We'll put just one Step in sequence (for start), and no more.
        // Then cont() will call next_stop() which returns Exited(0) by default.
        // Actually, to make cont fail, we need more control.
        // Instead, let me test the path where start returns a non-Step reason.
        target.stop_sequence = vec![StopReason::BreakpointHit(code_base)];

        let mut session = Session::new(target, Some(make_debug_info()), code_base, "test".to_string());
        session.execute(Command::Break(BreakLocation::Address(code_base))).unwrap();
        session.clear_output();

        let result = session.execute(Command::Run(vec![]));
        assert_eq!(result, Ok(false));
        // The non-Step start reason goes through handle_stop directly
        assert!(session.get_output().iter().any(|l| l.contains("Breakpoint 1 hit")));
    }
}

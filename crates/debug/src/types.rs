//! Core debugger types

use debuginfo::DebugInfo;
use std::collections::HashMap;

/// Unique identifier for a breakpoint
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BreakpointId(pub u32);

/// Kind of breakpoint
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BreakpointKind {
    /// Breakpoint at a specific source location
    Source {
        /// Source file path
        file: String,
        /// Line number (1-indexed)
        line: u32,
        /// Optional column
        col: Option<u32>,
    },
    /// Breakpoint at a function entry
    Function {
        /// Function name
        name: String,
    },
    /// Breakpoint at a specific code address
    Address {
        /// Code offset within the function
        offset: u32,
    },
}

/// A breakpoint in the debugger
#[derive(Debug, Clone)]
pub struct Breakpoint {
    /// Unique identifier
    pub id: BreakpointId,
    /// Kind of breakpoint
    pub kind: BreakpointKind,
    /// Whether the breakpoint is enabled
    pub enabled: bool,
    /// Number of times this breakpoint has been hit
    pub hit_count: u32,
    /// Resolved code offset (if resolved)
    pub resolved_offset: Option<u32>,
    /// Original instruction at this location (for restoration)
    pub original_instruction: Option<u32>,
}

impl Breakpoint {
    /// Create a new source breakpoint
    pub fn at_source(id: BreakpointId, file: String, line: u32, col: Option<u32>) -> Self {
        Self {
            id,
            kind: BreakpointKind::Source { file, line, col },
            enabled: true,
            hit_count: 0,
            resolved_offset: None,
            original_instruction: None,
        }
    }

    /// Create a new function breakpoint
    pub fn at_function(id: BreakpointId, name: String) -> Self {
        Self {
            id,
            kind: BreakpointKind::Function { name },
            enabled: true,
            hit_count: 0,
            resolved_offset: None,
            original_instruction: None,
        }
    }

    /// Create a new address breakpoint
    pub fn at_address(id: BreakpointId, offset: u32) -> Self {
        Self {
            id,
            kind: BreakpointKind::Address { offset },
            enabled: true,
            hit_count: 0,
            resolved_offset: Some(offset),
            original_instruction: None,
        }
    }

    /// Check if the breakpoint is resolved to a code offset
    pub fn is_resolved(&self) -> bool {
        self.resolved_offset.is_some()
    }
}

/// Reason for stopping execution
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StopReason {
    /// Hit a breakpoint (resolved to breakpoint ID)
    Breakpoint(BreakpointId),
    /// Hit a breakpoint (raw address from target)
    BreakpointHit(u64),
    /// Step completed
    Step,
    /// Program exited normally
    Exited(i32),
    /// Program was killed by signal
    Signaled(i32),
    /// Stopped by other signal
    Signal(i32),
    /// Program crashed or hit an error
    Error(String),
    /// User requested pause
    Pause,
}

/// Current state of the debugger
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DebugState {
    /// Not attached to any program
    Idle,
    /// Program is running
    Running,
    /// Program is stopped
    Stopped(StopReason),
}

impl Default for DebugState {
    fn default() -> Self {
        DebugState::Idle
    }
}

/// A value stored in a register
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RegisterValue {
    /// 64-bit integer value
    Int64(i64),
    /// 64-bit unsigned value
    Uint64(u64),
    /// Boolean value
    Bool(bool),
    /// Undefined/uninitialized
    Undefined,
}

impl RegisterValue {
    /// Get the value as an i64
    pub fn as_i64(&self) -> Option<i64> {
        match self {
            RegisterValue::Int64(v) => Some(*v),
            RegisterValue::Uint64(v) => Some(*v as i64),
            RegisterValue::Bool(v) => Some(if *v { 1 } else { 0 }),
            RegisterValue::Undefined => None,
        }
    }

    /// Get the value as a u64
    pub fn as_u64(&self) -> Option<u64> {
        match self {
            RegisterValue::Int64(v) => Some(*v as u64),
            RegisterValue::Uint64(v) => Some(*v),
            RegisterValue::Bool(v) => Some(if *v { 1 } else { 0 }),
            RegisterValue::Undefined => None,
        }
    }
}

/// A stack frame in the call stack
#[derive(Debug, Clone)]
pub struct StackFrame {
    /// Frame index (0 = current frame)
    pub index: u32,
    /// Function name
    pub function_name: String,
    /// Source file path
    pub source_file: Option<String>,
    /// Source line number
    pub line: Option<u32>,
    /// Source column
    pub col: Option<u32>,
    /// Code offset within the function
    pub code_offset: u32,
    /// Frame pointer
    pub frame_pointer: Option<u64>,
    /// Return address
    pub return_address: Option<u64>,
}

/// Main debugger instance
pub struct Debugger {
    /// Debug information for the program
    debug_info: Option<DebugInfo>,
    /// Current debugging state
    state: DebugState,
    /// All breakpoints
    breakpoints: HashMap<BreakpointId, Breakpoint>,
    /// Next breakpoint ID to assign
    next_breakpoint_id: u32,
    /// Current code offset (program counter)
    current_offset: u32,
    /// Current function ID
    current_func_id: Option<u32>,
    /// Register values (virtual registers from MIR)
    registers: HashMap<u32, RegisterValue>,
    /// Call stack
    call_stack: Vec<StackFrame>,
}

impl Default for Debugger {
    fn default() -> Self {
        Self::new()
    }
}

impl Debugger {
    /// Create a new debugger instance
    pub fn new() -> Self {
        Self {
            debug_info: None,
            state: DebugState::Idle,
            breakpoints: HashMap::new(),
            next_breakpoint_id: 1,
            current_offset: 0,
            current_func_id: None,
            registers: HashMap::new(),
            call_stack: Vec::new(),
        }
    }

    /// Load debug information
    pub fn load_debug_info(&mut self, debug_info: DebugInfo) {
        self.debug_info = Some(debug_info);
    }

    /// Get the current debug information
    pub fn debug_info(&self) -> Option<&DebugInfo> {
        self.debug_info.as_ref()
    }

    /// Get the current state
    pub fn state(&self) -> &DebugState {
        &self.state
    }

    /// Set the current state
    pub fn set_state(&mut self, state: DebugState) {
        self.state = state;
    }

    /// Get the current code offset
    pub fn current_offset(&self) -> u32 {
        self.current_offset
    }

    /// Set the current code offset
    pub fn set_current_offset(&mut self, offset: u32) {
        self.current_offset = offset;
    }

    /// Get the current function ID
    pub fn current_func_id(&self) -> Option<u32> {
        self.current_func_id
    }

    /// Set the current function ID
    pub fn set_current_func_id(&mut self, func_id: Option<u32>) {
        self.current_func_id = func_id;
    }

    /// Get a breakpoint by ID
    pub fn get_breakpoint(&self, id: BreakpointId) -> Option<&Breakpoint> {
        self.breakpoints.get(&id)
    }

    /// Get a mutable breakpoint by ID
    pub fn get_breakpoint_mut(&mut self, id: BreakpointId) -> Option<&mut Breakpoint> {
        self.breakpoints.get_mut(&id)
    }

    /// Get all breakpoints
    pub fn breakpoints(&self) -> impl Iterator<Item = &Breakpoint> {
        self.breakpoints.values()
    }

    /// Add a breakpoint
    pub fn add_breakpoint(&mut self, mut bp: Breakpoint) -> BreakpointId {
        let id = BreakpointId(self.next_breakpoint_id);
        self.next_breakpoint_id += 1;
        bp.id = id;
        self.breakpoints.insert(id, bp);
        id
    }

    /// Remove a breakpoint
    pub fn remove_breakpoint(&mut self, id: BreakpointId) -> Option<Breakpoint> {
        self.breakpoints.remove(&id)
    }

    /// Get a register value
    pub fn get_register(&self, reg: u32) -> RegisterValue {
        self.registers
            .get(&reg)
            .cloned()
            .unwrap_or(RegisterValue::Undefined)
    }

    /// Set a register value
    pub fn set_register(&mut self, reg: u32, value: RegisterValue) {
        self.registers.insert(reg, value);
    }

    /// Get the call stack
    pub fn call_stack(&self) -> &[StackFrame] {
        &self.call_stack
    }

    /// Push a stack frame
    pub fn push_frame(&mut self, frame: StackFrame) {
        self.call_stack.push(frame);
    }

    /// Pop a stack frame
    pub fn pop_frame(&mut self) -> Option<StackFrame> {
        self.call_stack.pop()
    }

    /// Clear the call stack
    pub fn clear_call_stack(&mut self) {
        self.call_stack.clear();
    }

    /// Reset the debugger state
    pub fn reset(&mut self) {
        self.state = DebugState::Idle;
        self.current_offset = 0;
        self.current_func_id = None;
        self.registers.clear();
        self.call_stack.clear();
        // Keep breakpoints and debug info
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_breakpoint_at_source() {
        let bp = Breakpoint::at_source(
            BreakpointId(1),
            "test.skyl".to_string(),
            10,
            Some(5),
        );
        assert_eq!(bp.id, BreakpointId(1));
        assert!(bp.enabled);
        assert_eq!(bp.hit_count, 0);
        assert!(!bp.is_resolved());

        match &bp.kind {
            BreakpointKind::Source { file, line, col } => {
                assert_eq!(file, "test.skyl");
                assert_eq!(*line, 10);
                assert_eq!(*col, Some(5));
            }
            _ => panic!("expected source breakpoint"),
        }
    }

    #[test]
    fn test_breakpoint_at_function() {
        let bp = Breakpoint::at_function(BreakpointId(2), "main".to_string());
        assert_eq!(bp.id, BreakpointId(2));
        assert!(bp.enabled);
        assert!(!bp.is_resolved());

        match &bp.kind {
            BreakpointKind::Function { name } => {
                assert_eq!(name, "main");
            }
            _ => panic!("expected function breakpoint"),
        }
    }

    #[test]
    fn test_breakpoint_at_address() {
        let bp = Breakpoint::at_address(BreakpointId(3), 0x100);
        assert_eq!(bp.id, BreakpointId(3));
        assert!(bp.enabled);
        assert!(bp.is_resolved());
        assert_eq!(bp.resolved_offset, Some(0x100));
    }

    #[test]
    fn test_debug_state_default() {
        let state = DebugState::default();
        assert_eq!(state, DebugState::Idle);
    }

    #[test]
    fn test_register_value_conversions() {
        let int_val = RegisterValue::Int64(42);
        assert_eq!(int_val.as_i64(), Some(42));
        assert_eq!(int_val.as_u64(), Some(42));

        let uint_val = RegisterValue::Uint64(100);
        assert_eq!(uint_val.as_i64(), Some(100));
        assert_eq!(uint_val.as_u64(), Some(100));

        let bool_true = RegisterValue::Bool(true);
        assert_eq!(bool_true.as_i64(), Some(1));
        assert_eq!(bool_true.as_u64(), Some(1));

        let bool_false = RegisterValue::Bool(false);
        assert_eq!(bool_false.as_i64(), Some(0));
        assert_eq!(bool_false.as_u64(), Some(0));

        let undef = RegisterValue::Undefined;
        assert_eq!(undef.as_i64(), None);
        assert_eq!(undef.as_u64(), None);
    }

    #[test]
    fn test_debugger_new() {
        let dbg = Debugger::new();
        assert_eq!(*dbg.state(), DebugState::Idle);
        assert!(dbg.debug_info().is_none());
        assert_eq!(dbg.current_offset(), 0);
        assert!(dbg.current_func_id().is_none());
    }

    #[test]
    fn test_debugger_default() {
        let dbg = Debugger::default();
        assert_eq!(*dbg.state(), DebugState::Idle);
    }

    #[test]
    fn test_debugger_state() {
        let mut dbg = Debugger::new();
        dbg.set_state(DebugState::Running);
        assert_eq!(*dbg.state(), DebugState::Running);

        dbg.set_state(DebugState::Stopped(StopReason::Step));
        assert_eq!(*dbg.state(), DebugState::Stopped(StopReason::Step));
    }

    #[test]
    fn test_debugger_breakpoints() {
        let mut dbg = Debugger::new();

        let bp = Breakpoint::at_source(
            BreakpointId(0), // Will be reassigned
            "test.skyl".to_string(),
            5,
            None,
        );
        let id = dbg.add_breakpoint(bp);
        assert_eq!(id, BreakpointId(1));

        let bp = dbg.get_breakpoint(id).unwrap();
        assert_eq!(bp.id, id);

        // Add another breakpoint
        let bp2 = Breakpoint::at_function(BreakpointId(0), "foo".to_string());
        let id2 = dbg.add_breakpoint(bp2);
        assert_eq!(id2, BreakpointId(2));

        // Count breakpoints
        assert_eq!(dbg.breakpoints().count(), 2);

        // Remove first breakpoint
        let removed = dbg.remove_breakpoint(id).unwrap();
        assert_eq!(removed.id, id);
        assert!(dbg.get_breakpoint(id).is_none());
        assert_eq!(dbg.breakpoints().count(), 1);
    }

    #[test]
    fn test_debugger_breakpoint_mut() {
        let mut dbg = Debugger::new();
        let bp = Breakpoint::at_address(BreakpointId(0), 0x50);
        let id = dbg.add_breakpoint(bp);

        let bp_mut = dbg.get_breakpoint_mut(id).unwrap();
        bp_mut.enabled = false;
        bp_mut.hit_count = 5;

        let bp = dbg.get_breakpoint(id).unwrap();
        assert!(!bp.enabled);
        assert_eq!(bp.hit_count, 5);
    }

    #[test]
    fn test_debugger_registers() {
        let mut dbg = Debugger::new();

        // Default is undefined
        assert_eq!(dbg.get_register(0), RegisterValue::Undefined);

        // Set and get
        dbg.set_register(0, RegisterValue::Int64(42));
        assert_eq!(dbg.get_register(0), RegisterValue::Int64(42));

        dbg.set_register(1, RegisterValue::Bool(true));
        assert_eq!(dbg.get_register(1), RegisterValue::Bool(true));
    }

    #[test]
    fn test_debugger_call_stack() {
        let mut dbg = Debugger::new();
        assert!(dbg.call_stack().is_empty());

        let frame = StackFrame {
            index: 0,
            function_name: "main".to_string(),
            source_file: Some("test.skyl".to_string()),
            line: Some(10),
            col: Some(1),
            code_offset: 0,
            frame_pointer: Some(0x7fff0000),
            return_address: Some(0x400100),
        };
        dbg.push_frame(frame);
        assert_eq!(dbg.call_stack().len(), 1);
        assert_eq!(dbg.call_stack()[0].function_name, "main");

        let frame2 = StackFrame {
            index: 1,
            function_name: "helper".to_string(),
            source_file: None,
            line: None,
            col: None,
            code_offset: 100,
            frame_pointer: None,
            return_address: None,
        };
        dbg.push_frame(frame2);
        assert_eq!(dbg.call_stack().len(), 2);

        let popped = dbg.pop_frame().unwrap();
        assert_eq!(popped.function_name, "helper");
        assert_eq!(dbg.call_stack().len(), 1);

        dbg.clear_call_stack();
        assert!(dbg.call_stack().is_empty());
    }

    #[test]
    fn test_debugger_current_offset_and_func() {
        let mut dbg = Debugger::new();
        assert_eq!(dbg.current_offset(), 0);
        assert!(dbg.current_func_id().is_none());

        dbg.set_current_offset(0x100);
        dbg.set_current_func_id(Some(0));

        assert_eq!(dbg.current_offset(), 0x100);
        assert_eq!(dbg.current_func_id(), Some(0));
    }

    #[test]
    fn test_debugger_reset() {
        let mut dbg = Debugger::new();

        // Set up some state
        dbg.set_state(DebugState::Running);
        dbg.set_current_offset(0x100);
        dbg.set_current_func_id(Some(0));
        dbg.set_register(0, RegisterValue::Int64(42));
        dbg.push_frame(StackFrame {
            index: 0,
            function_name: "main".to_string(),
            source_file: None,
            line: None,
            col: None,
            code_offset: 0,
            frame_pointer: None,
            return_address: None,
        });

        let bp = Breakpoint::at_address(BreakpointId(0), 0x50);
        let id = dbg.add_breakpoint(bp);

        // Reset
        dbg.reset();

        // State should be reset
        assert_eq!(*dbg.state(), DebugState::Idle);
        assert_eq!(dbg.current_offset(), 0);
        assert!(dbg.current_func_id().is_none());
        assert_eq!(dbg.get_register(0), RegisterValue::Undefined);
        assert!(dbg.call_stack().is_empty());

        // Breakpoints should be preserved
        assert!(dbg.get_breakpoint(id).is_some());
    }

    #[test]
    fn test_debugger_load_debug_info() {
        use debuginfo::DebugInfo;

        let mut dbg = Debugger::new();
        assert!(dbg.debug_info().is_none());

        let debug_info = DebugInfo::default();
        dbg.load_debug_info(debug_info);
        assert!(dbg.debug_info().is_some());
    }

    #[test]
    fn test_stop_reason_variants() {
        let bp_stop = StopReason::Breakpoint(BreakpointId(1));
        assert_eq!(bp_stop, StopReason::Breakpoint(BreakpointId(1)));

        let step_stop = StopReason::Step;
        assert_eq!(step_stop, StopReason::Step);

        let exit_stop = StopReason::Exited(0);
        assert_eq!(exit_stop, StopReason::Exited(0));

        let error_stop = StopReason::Error("test error".to_string());
        assert_eq!(error_stop, StopReason::Error("test error".to_string()));

        let pause_stop = StopReason::Pause;
        assert_eq!(pause_stop, StopReason::Pause);
    }

    #[test]
    fn test_stack_frame() {
        let frame = StackFrame {
            index: 0,
            function_name: "test_func".to_string(),
            source_file: Some("file.skyl".to_string()),
            line: Some(42),
            col: Some(10),
            code_offset: 0x200,
            frame_pointer: Some(0x7fff1000),
            return_address: Some(0x400200),
        };

        assert_eq!(frame.index, 0);
        assert_eq!(frame.function_name, "test_func");
        assert_eq!(frame.source_file, Some("file.skyl".to_string()));
        assert_eq!(frame.line, Some(42));
        assert_eq!(frame.col, Some(10));
        assert_eq!(frame.code_offset, 0x200);
        assert_eq!(frame.frame_pointer, Some(0x7fff1000));
        assert_eq!(frame.return_address, Some(0x400200));
    }
}

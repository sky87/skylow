//! Variable and register inspection

use crate::source_map::SourceMapper;
use crate::types::{Debugger, RegisterValue, StackFrame};
use debuginfo::{FunctionLocals, LocalVariable};

/// Information about a local variable
#[derive(Debug, Clone)]
pub struct VariableInfo {
    /// Variable name
    pub name: String,
    /// Register holding the variable
    pub register: u32,
    /// Current value (if available)
    pub value: RegisterValue,
    /// Whether the variable is currently live
    pub is_live: bool,
}

/// Inspector for examining program state
pub struct Inspector;

impl Inspector {
    /// Get all local variables for the current function
    pub fn locals(debugger: &Debugger) -> Vec<VariableInfo> {
        let func_id = match debugger.current_func_id() {
            Some(id) => id,
            None => return Vec::new(),
        };

        let debug_info = match debugger.debug_info() {
            Some(info) => info,
            None => return Vec::new(),
        };

        let current_offset = debugger.current_offset();

        // Find locals for this function
        let func_locals = match debug_info.locals.iter().find(|l| l.func_id == func_id) {
            Some(locals) => locals,
            None => return Vec::new(),
        };

        func_locals
            .locals
            .iter()
            .map(|local| {
                let is_live =
                    current_offset >= local.live_start && current_offset < local.live_end;
                let value = if is_live {
                    debugger.get_register(local.register)
                } else {
                    RegisterValue::Undefined
                };

                VariableInfo {
                    name: local.name.clone(),
                    register: local.register,
                    value,
                    is_live,
                }
            })
            .collect()
    }

    /// Get a specific local variable by name
    pub fn local_by_name(debugger: &Debugger, name: &str) -> Option<VariableInfo> {
        Self::locals(debugger)
            .into_iter()
            .find(|v| v.name == name)
    }

    /// Get all live local variables at the current offset
    pub fn live_locals(debugger: &Debugger) -> Vec<VariableInfo> {
        Self::locals(debugger)
            .into_iter()
            .filter(|v| v.is_live)
            .collect()
    }

    /// Get the value of a register
    pub fn register(debugger: &Debugger, reg: u32) -> RegisterValue {
        debugger.get_register(reg)
    }

    /// Get all registers with their values
    pub fn all_registers(debugger: &Debugger, max_reg: u32) -> Vec<(u32, RegisterValue)> {
        (0..max_reg)
            .map(|r| (r, debugger.get_register(r)))
            .collect()
    }

    /// Get the current call stack
    pub fn call_stack(debugger: &Debugger) -> Vec<StackFrame> {
        debugger.call_stack().to_vec()
    }

    /// Build the call stack from the current state
    ///
    /// In a real implementation, this would walk the stack frames.
    /// Here we provide a utility to build a frame from the current location.
    pub fn current_frame(debugger: &Debugger) -> Option<StackFrame> {
        let offset = debugger.current_offset();
        let func_name = SourceMapper::function_at_offset(debugger, offset)?;

        let loc = SourceMapper::offset_to_source(debugger, offset);

        Some(StackFrame {
            index: 0,
            function_name: func_name,
            source_file: loc.as_ref().map(|l| l.file.clone()),
            line: loc.as_ref().map(|l| l.line),
            col: loc.as_ref().map(|l| l.col),
            code_offset: offset,
        })
    }

    /// Get the locals definition for a function
    pub fn function_locals_def(debugger: &Debugger, func_id: u32) -> Option<&FunctionLocals> {
        debugger
            .debug_info()?
            .locals
            .iter()
            .find(|l| l.func_id == func_id)
    }

    /// Check if a variable is live at a given offset
    pub fn is_variable_live(local: &LocalVariable, offset: u32) -> bool {
        offset >= local.live_start && offset < local.live_end
    }

    /// Format a register value for display
    pub fn format_value(value: &RegisterValue) -> String {
        match value {
            RegisterValue::Int64(v) => format!("{}", v),
            RegisterValue::Uint64(v) => format!("{}", v),
            RegisterValue::Bool(v) => format!("{}", v),
            RegisterValue::Undefined => "<undefined>".to_string(),
        }
    }

    /// Get variable info with the value formatted as a string
    pub fn variable_display(var: &VariableInfo) -> String {
        if var.is_live {
            format!("{} = {}", var.name, Self::format_value(&var.value))
        } else {
            format!("{} = <not live>", var.name)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use debuginfo::{DebugInfo, LineMapping, LineMappingFlags, SourceFile, Symbol, SymbolKind};

    fn make_debug_info() -> DebugInfo {
        DebugInfo {
            sources: vec![SourceFile {
                id: 0,
                path: "test.skyl".to_string(),
                text: None,
            }],
            symbols: vec![Symbol {
                name: "main".to_string(),
                source_id: 0,
                line: 1,
                col: 1,
                code_offset: 0,
                code_size: 100,
                kind: SymbolKind::Function,
            }],
            line_map: vec![
                LineMapping {
                    func_id: 0,
                    code_offset: 0,
                    source_id: 0,
                    line: 1,
                    col: 1,
                    flags: LineMappingFlags::PROLOGUE,
                },
                LineMapping {
                    func_id: 0,
                    code_offset: 16,
                    source_id: 0,
                    line: 2,
                    col: 3,
                    flags: LineMappingFlags::STATEMENT,
                },
            ],
            locals: vec![FunctionLocals {
                func_id: 0,
                locals: vec![
                    LocalVariable {
                        name: "x".to_string(),
                        register: 0,
                        live_start: 16,
                        live_end: 80,
                    },
                    LocalVariable {
                        name: "y".to_string(),
                        register: 1,
                        live_start: 32,
                        live_end: 64,
                    },
                ],
            }],
        }
    }

    #[test]
    fn test_locals() {
        let mut debugger = Debugger::new();
        debugger.load_debug_info(make_debug_info());
        debugger.set_current_func_id(Some(0));
        debugger.set_current_offset(40);
        debugger.set_register(0, RegisterValue::Int64(42));
        debugger.set_register(1, RegisterValue::Int64(100));

        let locals = Inspector::locals(&debugger);
        assert_eq!(locals.len(), 2);

        let x = locals.iter().find(|v| v.name == "x").unwrap();
        assert!(x.is_live);
        assert_eq!(x.value, RegisterValue::Int64(42));

        let y = locals.iter().find(|v| v.name == "y").unwrap();
        assert!(y.is_live);
        assert_eq!(y.value, RegisterValue::Int64(100));
    }

    #[test]
    fn test_locals_not_live() {
        let mut debugger = Debugger::new();
        debugger.load_debug_info(make_debug_info());
        debugger.set_current_func_id(Some(0));
        debugger.set_current_offset(10); // Before x is live

        let locals = Inspector::locals(&debugger);
        assert_eq!(locals.len(), 2);

        // Neither should be live
        for local in &locals {
            assert!(!local.is_live);
            assert_eq!(local.value, RegisterValue::Undefined);
        }
    }

    #[test]
    fn test_locals_no_func_id() {
        let mut debugger = Debugger::new();
        debugger.load_debug_info(make_debug_info());
        // No func_id set

        let locals = Inspector::locals(&debugger);
        assert!(locals.is_empty());
    }

    #[test]
    fn test_locals_no_debug_info() {
        let debugger = Debugger::new();

        let locals = Inspector::locals(&debugger);
        assert!(locals.is_empty());
    }

    #[test]
    fn test_local_by_name() {
        let mut debugger = Debugger::new();
        debugger.load_debug_info(make_debug_info());
        debugger.set_current_func_id(Some(0));
        debugger.set_current_offset(40);
        debugger.set_register(0, RegisterValue::Int64(42));

        let x = Inspector::local_by_name(&debugger, "x").unwrap();
        assert_eq!(x.name, "x");
        assert_eq!(x.value, RegisterValue::Int64(42));

        assert!(Inspector::local_by_name(&debugger, "z").is_none());
    }

    #[test]
    fn test_live_locals() {
        let mut debugger = Debugger::new();
        debugger.load_debug_info(make_debug_info());
        debugger.set_current_func_id(Some(0));
        debugger.set_current_offset(20); // Only x is live (16-80), y is not (32-64)

        let live = Inspector::live_locals(&debugger);
        assert_eq!(live.len(), 1);
        assert_eq!(live[0].name, "x");
    }

    #[test]
    fn test_register() {
        let mut debugger = Debugger::new();
        debugger.set_register(5, RegisterValue::Int64(123));

        assert_eq!(Inspector::register(&debugger, 5), RegisterValue::Int64(123));
        assert_eq!(Inspector::register(&debugger, 6), RegisterValue::Undefined);
    }

    #[test]
    fn test_all_registers() {
        let mut debugger = Debugger::new();
        debugger.set_register(0, RegisterValue::Int64(10));
        debugger.set_register(2, RegisterValue::Bool(true));

        let regs = Inspector::all_registers(&debugger, 4);
        assert_eq!(regs.len(), 4);
        assert_eq!(regs[0], (0, RegisterValue::Int64(10)));
        assert_eq!(regs[1], (1, RegisterValue::Undefined));
        assert_eq!(regs[2], (2, RegisterValue::Bool(true)));
        assert_eq!(regs[3], (3, RegisterValue::Undefined));
    }

    #[test]
    fn test_call_stack() {
        let mut debugger = Debugger::new();
        debugger.push_frame(StackFrame {
            index: 0,
            function_name: "main".to_string(),
            source_file: Some("test.skyl".to_string()),
            line: Some(10),
            col: Some(1),
            code_offset: 0,
        });

        let stack = Inspector::call_stack(&debugger);
        assert_eq!(stack.len(), 1);
        assert_eq!(stack[0].function_name, "main");
    }

    #[test]
    fn test_current_frame() {
        let mut debugger = Debugger::new();
        debugger.load_debug_info(make_debug_info());
        debugger.set_current_offset(16);

        let frame = Inspector::current_frame(&debugger).unwrap();
        assert_eq!(frame.index, 0);
        assert_eq!(frame.function_name, "main");
        assert_eq!(frame.line, Some(2));
        assert_eq!(frame.code_offset, 16);
    }

    #[test]
    fn test_current_frame_no_debug_info() {
        let debugger = Debugger::new();
        assert!(Inspector::current_frame(&debugger).is_none());
    }

    #[test]
    fn test_function_locals_def() {
        let mut debugger = Debugger::new();
        debugger.load_debug_info(make_debug_info());

        let locals = Inspector::function_locals_def(&debugger, 0).unwrap();
        assert_eq!(locals.func_id, 0);
        assert_eq!(locals.locals.len(), 2);

        assert!(Inspector::function_locals_def(&debugger, 999).is_none());
    }

    #[test]
    fn test_function_locals_def_no_debug_info() {
        let debugger = Debugger::new();
        assert!(Inspector::function_locals_def(&debugger, 0).is_none());
    }

    #[test]
    fn test_is_variable_live() {
        let local = LocalVariable {
            name: "x".to_string(),
            register: 0,
            live_start: 10,
            live_end: 50,
        };

        assert!(!Inspector::is_variable_live(&local, 5));
        assert!(Inspector::is_variable_live(&local, 10));
        assert!(Inspector::is_variable_live(&local, 30));
        assert!(Inspector::is_variable_live(&local, 49));
        assert!(!Inspector::is_variable_live(&local, 50));
        assert!(!Inspector::is_variable_live(&local, 100));
    }

    #[test]
    fn test_format_value() {
        assert_eq!(Inspector::format_value(&RegisterValue::Int64(42)), "42");
        assert_eq!(Inspector::format_value(&RegisterValue::Int64(-10)), "-10");
        assert_eq!(Inspector::format_value(&RegisterValue::Uint64(100)), "100");
        assert_eq!(Inspector::format_value(&RegisterValue::Bool(true)), "true");
        assert_eq!(Inspector::format_value(&RegisterValue::Bool(false)), "false");
        assert_eq!(
            Inspector::format_value(&RegisterValue::Undefined),
            "<undefined>"
        );
    }

    #[test]
    fn test_variable_display() {
        let live_var = VariableInfo {
            name: "x".to_string(),
            register: 0,
            value: RegisterValue::Int64(42),
            is_live: true,
        };
        assert_eq!(Inspector::variable_display(&live_var), "x = 42");

        let dead_var = VariableInfo {
            name: "y".to_string(),
            register: 1,
            value: RegisterValue::Undefined,
            is_live: false,
        };
        assert_eq!(Inspector::variable_display(&dead_var), "y = <not live>");
    }

    #[test]
    fn test_variable_info_clone() {
        let var = VariableInfo {
            name: "test".to_string(),
            register: 5,
            value: RegisterValue::Bool(true),
            is_live: true,
        };
        let cloned = var.clone();
        assert_eq!(cloned.name, "test");
        assert_eq!(cloned.register, 5);
        assert_eq!(cloned.value, RegisterValue::Bool(true));
        assert!(cloned.is_live);
    }
}

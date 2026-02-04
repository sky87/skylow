//! Stepping operations for the debugger

use crate::source_map::SourceMapper;
use crate::types::{DebugState, Debugger, StopReason};
use debuginfo::LineMappingFlags;

/// Step mode for the debugger
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StepMode {
    /// Step to the next instruction (step into)
    Instruction,
    /// Step to the next source line (step over)
    Line,
    /// Step out of the current function
    Out,
}

/// Result of a step operation
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StepResult {
    /// Successfully stepped to a new location
    Stepped {
        /// New code offset
        offset: u32,
        /// New source line (if available)
        line: Option<u32>,
    },
    /// Reached the end of the function/program
    Finished,
    /// Encountered an error
    Error(String),
}

/// Stepping operations
pub struct Stepper;

impl Stepper {
    /// Calculate the next stop point for a step operation
    ///
    /// Returns the code offset to stop at, or None if we should run to completion.
    pub fn calculate_step_target(
        debugger: &Debugger,
        mode: StepMode,
    ) -> Option<u32> {
        let debug_info = debugger.debug_info()?;
        let current_offset = debugger.current_offset();

        match mode {
            StepMode::Instruction => {
                // Find the next line mapping after current offset
                Self::find_next_mapping_offset(debugger, current_offset)
            }
            StepMode::Line => {
                // Find the next line mapping that's on a different source line
                let current_loc = SourceMapper::offset_to_source_from_info(debug_info, current_offset);
                let current_line = current_loc.as_ref().map(|l| l.line);

                let mut best_offset: Option<u32> = None;

                for mapping in &debug_info.line_map {
                    if mapping.code_offset > current_offset {
                        // For line stepping, skip to next different line
                        if current_line.is_none() || Some(mapping.line) != current_line {
                            if best_offset.is_none() || mapping.code_offset < best_offset.unwrap() {
                                best_offset = Some(mapping.code_offset);
                            }
                        }
                    }
                }

                best_offset
            }
            StepMode::Out => {
                // Find the epilogue or function end
                let func_id = debugger.current_func_id()?;

                // Look for epilogue marker
                for mapping in &debug_info.line_map {
                    if mapping.func_id == func_id
                        && mapping.flags.contains(LineMappingFlags::EPILOGUE)
                        && mapping.code_offset > current_offset
                    {
                        return Some(mapping.code_offset);
                    }
                }

                // If no epilogue marker, find the end of the function
                debug_info
                    .symbols
                    .iter()
                    .find(|s| {
                        current_offset >= s.code_offset
                            && current_offset < s.code_offset + s.code_size
                    })
                    .map(|s| s.code_offset + s.code_size - 4) // Assume last instruction
            }
        }
    }

    /// Find the next line mapping offset after the given offset
    fn find_next_mapping_offset(debugger: &Debugger, after_offset: u32) -> Option<u32> {
        let debug_info = debugger.debug_info()?;
        let mut best_offset: Option<u32> = None;

        for mapping in &debug_info.line_map {
            if mapping.code_offset > after_offset {
                if best_offset.is_none() || mapping.code_offset < best_offset.unwrap() {
                    best_offset = Some(mapping.code_offset);
                }
            }
        }

        best_offset
    }

    /// Check if we're at a statement boundary (good place to stop)
    pub fn is_statement_boundary(debugger: &Debugger, offset: u32) -> bool {
        let debug_info = match debugger.debug_info() {
            Some(info) => info,
            None => return false,
        };

        debug_info
            .line_map
            .iter()
            .any(|m| m.code_offset == offset && m.flags.contains(LineMappingFlags::STATEMENT))
    }

    /// Check if we're at a function entry point
    pub fn is_function_entry(debugger: &Debugger, offset: u32) -> bool {
        let debug_info = match debugger.debug_info() {
            Some(info) => info,
            None => return false,
        };

        debug_info.symbols.iter().any(|s| s.code_offset == offset)
    }

    /// Check if we're at a function prologue
    pub fn is_prologue(debugger: &Debugger, offset: u32) -> bool {
        let debug_info = match debugger.debug_info() {
            Some(info) => info,
            None => return false,
        };

        debug_info
            .line_map
            .iter()
            .any(|m| m.code_offset == offset && m.flags.contains(LineMappingFlags::PROLOGUE))
    }

    /// Check if we're at a function epilogue
    pub fn is_epilogue(debugger: &Debugger, offset: u32) -> bool {
        let debug_info = match debugger.debug_info() {
            Some(info) => info,
            None => return false,
        };

        debug_info
            .line_map
            .iter()
            .any(|m| m.code_offset == offset && m.flags.contains(LineMappingFlags::EPILOGUE))
    }

    /// Simulate a step and update the debugger state
    ///
    /// This method calculates where we would stop and updates the debugger's
    /// current offset. In a real implementation, this would involve executing
    /// instructions until the target is reached.
    pub fn simulate_step(debugger: &mut Debugger, mode: StepMode) -> StepResult {
        let target = match Self::calculate_step_target(debugger, mode) {
            Some(offset) => offset,
            None => {
                debugger.set_state(DebugState::Stopped(StopReason::Exited(0)));
                return StepResult::Finished;
            }
        };

        debugger.set_current_offset(target);
        debugger.set_state(DebugState::Stopped(StopReason::Step));

        let line = debugger
            .debug_info()
            .and_then(|info| SourceMapper::offset_to_source_from_info(info, target))
            .map(|loc| loc.line);

        StepResult::Stepped {
            offset: target,
            line,
        }
    }

    /// Get the next statement offset (for "step over" behavior)
    pub fn next_statement_offset(debugger: &Debugger) -> Option<u32> {
        let debug_info = debugger.debug_info()?;
        let current_offset = debugger.current_offset();
        let mut best_offset: Option<u32> = None;

        for mapping in &debug_info.line_map {
            if mapping.code_offset > current_offset
                && mapping.flags.contains(LineMappingFlags::STATEMENT)
            {
                if best_offset.is_none() || mapping.code_offset < best_offset.unwrap() {
                    best_offset = Some(mapping.code_offset);
                }
            }
        }

        best_offset
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use debuginfo::{DebugInfo, LineMapping, SourceFile, Symbol, SymbolKind};

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
                LineMapping {
                    func_id: 0,
                    code_offset: 32,
                    source_id: 0,
                    line: 2,
                    col: 10,
                    flags: LineMappingFlags::empty(),
                },
                LineMapping {
                    func_id: 0,
                    code_offset: 48,
                    source_id: 0,
                    line: 3,
                    col: 3,
                    flags: LineMappingFlags::STATEMENT,
                },
                LineMapping {
                    func_id: 0,
                    code_offset: 80,
                    source_id: 0,
                    line: 4,
                    col: 1,
                    flags: LineMappingFlags::EPILOGUE,
                },
            ],
            locals: vec![],
        }
    }

    #[test]
    fn test_step_instruction() {
        let mut debugger = Debugger::new();
        debugger.load_debug_info(make_debug_info());
        debugger.set_current_offset(0);

        let target = Stepper::calculate_step_target(&debugger, StepMode::Instruction);
        assert_eq!(target, Some(16));

        debugger.set_current_offset(16);
        let target = Stepper::calculate_step_target(&debugger, StepMode::Instruction);
        assert_eq!(target, Some(32));
    }

    #[test]
    fn test_step_line() {
        let mut debugger = Debugger::new();
        debugger.load_debug_info(make_debug_info());
        debugger.set_current_offset(0);

        // From line 1, should go to line 2
        let target = Stepper::calculate_step_target(&debugger, StepMode::Line);
        assert_eq!(target, Some(16));

        // From line 2 (offset 16), should skip 32 (same line) and go to line 3 (48)
        debugger.set_current_offset(16);
        let target = Stepper::calculate_step_target(&debugger, StepMode::Line);
        assert_eq!(target, Some(48));
    }

    #[test]
    fn test_step_out() {
        let mut debugger = Debugger::new();
        debugger.load_debug_info(make_debug_info());
        debugger.set_current_offset(16);
        debugger.set_current_func_id(Some(0));

        let target = Stepper::calculate_step_target(&debugger, StepMode::Out);
        assert_eq!(target, Some(80)); // Epilogue offset
    }

    #[test]
    fn test_step_no_debug_info() {
        let debugger = Debugger::new();

        assert!(Stepper::calculate_step_target(&debugger, StepMode::Instruction).is_none());
        assert!(Stepper::calculate_step_target(&debugger, StepMode::Line).is_none());
        assert!(Stepper::calculate_step_target(&debugger, StepMode::Out).is_none());
    }

    #[test]
    fn test_is_statement_boundary() {
        let mut debugger = Debugger::new();
        debugger.load_debug_info(make_debug_info());

        assert!(!Stepper::is_statement_boundary(&debugger, 0)); // Prologue
        assert!(Stepper::is_statement_boundary(&debugger, 16)); // Statement
        assert!(!Stepper::is_statement_boundary(&debugger, 32)); // Not a statement
        assert!(Stepper::is_statement_boundary(&debugger, 48)); // Statement
    }

    #[test]
    fn test_is_statement_boundary_no_debug_info() {
        let debugger = Debugger::new();
        assert!(!Stepper::is_statement_boundary(&debugger, 0));
    }

    #[test]
    fn test_is_function_entry() {
        let mut debugger = Debugger::new();
        debugger.load_debug_info(make_debug_info());

        assert!(Stepper::is_function_entry(&debugger, 0));
        assert!(!Stepper::is_function_entry(&debugger, 16));
    }

    #[test]
    fn test_is_function_entry_no_debug_info() {
        let debugger = Debugger::new();
        assert!(!Stepper::is_function_entry(&debugger, 0));
    }

    #[test]
    fn test_is_prologue() {
        let mut debugger = Debugger::new();
        debugger.load_debug_info(make_debug_info());

        assert!(Stepper::is_prologue(&debugger, 0));
        assert!(!Stepper::is_prologue(&debugger, 16));
    }

    #[test]
    fn test_is_prologue_no_debug_info() {
        let debugger = Debugger::new();
        assert!(!Stepper::is_prologue(&debugger, 0));
    }

    #[test]
    fn test_is_epilogue() {
        let mut debugger = Debugger::new();
        debugger.load_debug_info(make_debug_info());

        assert!(!Stepper::is_epilogue(&debugger, 0));
        assert!(Stepper::is_epilogue(&debugger, 80));
    }

    #[test]
    fn test_is_epilogue_no_debug_info() {
        let debugger = Debugger::new();
        assert!(!Stepper::is_epilogue(&debugger, 0));
    }

    #[test]
    fn test_simulate_step() {
        let mut debugger = Debugger::new();
        debugger.load_debug_info(make_debug_info());
        debugger.set_current_offset(0);

        let result = Stepper::simulate_step(&mut debugger, StepMode::Instruction);
        match result {
            StepResult::Stepped { offset, line } => {
                assert_eq!(offset, 16);
                assert_eq!(line, Some(2));
            }
            _ => panic!("expected Stepped result"),
        }

        assert_eq!(debugger.current_offset(), 16);
        assert_eq!(*debugger.state(), DebugState::Stopped(StopReason::Step));
    }

    #[test]
    fn test_simulate_step_finished() {
        let mut debugger = Debugger::new();
        debugger.load_debug_info(make_debug_info());
        debugger.set_current_offset(80); // At epilogue

        let result = Stepper::simulate_step(&mut debugger, StepMode::Instruction);
        assert_eq!(result, StepResult::Finished);
        assert_eq!(*debugger.state(), DebugState::Stopped(StopReason::Exited(0)));
    }

    #[test]
    fn test_next_statement_offset() {
        let mut debugger = Debugger::new();
        debugger.load_debug_info(make_debug_info());

        debugger.set_current_offset(0);
        assert_eq!(Stepper::next_statement_offset(&debugger), Some(16));

        debugger.set_current_offset(16);
        assert_eq!(Stepper::next_statement_offset(&debugger), Some(48));

        debugger.set_current_offset(48);
        assert_eq!(Stepper::next_statement_offset(&debugger), None);
    }

    #[test]
    fn test_next_statement_offset_no_debug_info() {
        let debugger = Debugger::new();
        assert!(Stepper::next_statement_offset(&debugger).is_none());
    }

    #[test]
    fn test_step_mode_equality() {
        assert_eq!(StepMode::Instruction, StepMode::Instruction);
        assert_eq!(StepMode::Line, StepMode::Line);
        assert_eq!(StepMode::Out, StepMode::Out);
        assert_ne!(StepMode::Instruction, StepMode::Line);
    }

    #[test]
    fn test_step_result_equality() {
        let result1 = StepResult::Stepped { offset: 10, line: Some(5) };
        let result2 = StepResult::Stepped { offset: 10, line: Some(5) };
        assert_eq!(result1, result2);

        assert_eq!(StepResult::Finished, StepResult::Finished);
        assert_eq!(
            StepResult::Error("test".to_string()),
            StepResult::Error("test".to_string())
        );
    }

    #[test]
    fn test_step_out_no_func_id() {
        let mut debugger = Debugger::new();
        debugger.load_debug_info(make_debug_info());
        debugger.set_current_offset(16);
        // No func_id set

        let target = Stepper::calculate_step_target(&debugger, StepMode::Out);
        assert!(target.is_none());
    }
}

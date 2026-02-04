//! Breakpoint management

use crate::types::{BreakpointId, BreakpointKind, Debugger};
use debuginfo::DebugInfo;

/// Breakpoint management operations
pub struct BreakpointManager;

impl BreakpointManager {
    /// Resolve all pending breakpoints using debug information
    ///
    /// Returns the number of breakpoints that were resolved.
    pub fn resolve_all(debugger: &mut Debugger) -> u32 {
        let debug_info = match debugger.debug_info() {
            Some(info) => info.clone(),
            None => return 0,
        };

        let mut resolved_count = 0;
        let bp_ids: Vec<BreakpointId> = debugger.breakpoints().map(|bp| bp.id).collect();

        for id in bp_ids {
            if let Some(bp) = debugger.get_breakpoint_mut(id) {
                if bp.is_resolved() {
                    continue;
                }

                if let Some(offset) = Self::resolve_breakpoint(&bp.kind, &debug_info) {
                    bp.resolved_offset = Some(offset);
                    resolved_count += 1;
                }
            }
        }

        resolved_count
    }

    /// Resolve a single breakpoint kind to a code offset
    fn resolve_breakpoint(kind: &BreakpointKind, debug_info: &DebugInfo) -> Option<u32> {
        match kind {
            BreakpointKind::Source { file, line, col } => {
                Self::resolve_source_breakpoint(debug_info, file, *line, *col)
            }
            BreakpointKind::Function { name } => {
                Self::resolve_function_breakpoint(debug_info, name)
            }
            BreakpointKind::Address { offset } => Some(*offset),
        }
    }

    /// Resolve a source location breakpoint to a code offset
    fn resolve_source_breakpoint(
        debug_info: &DebugInfo,
        file: &str,
        line: u32,
        col: Option<u32>,
    ) -> Option<u32> {
        // Find the source file ID
        let source_id = debug_info
            .sources
            .iter()
            .find(|s| s.path == file || s.path.ends_with(file))
            .map(|s| s.id)?;

        // Find the best matching line mapping
        let mut best_match: Option<&debuginfo::LineMapping> = None;
        let mut best_distance = u32::MAX;

        for mapping in &debug_info.line_map {
            if mapping.source_id != source_id {
                continue;
            }

            // For exact line match
            if mapping.line == line {
                // If column specified, try to match it
                if let Some(target_col) = col {
                    let col_distance = if mapping.col >= target_col {
                        mapping.col - target_col
                    } else {
                        target_col - mapping.col
                    };

                    if col_distance < best_distance {
                        best_distance = col_distance;
                        best_match = Some(mapping);
                    }
                } else {
                    // No column specified, take first match on the line
                    if best_match.is_none()
                        || mapping.code_offset
                            < best_match.map(|m| m.code_offset).unwrap_or(u32::MAX)
                    {
                        best_match = Some(mapping);
                    }
                }
            }
            // If no exact line match yet, look for closest line after target
            else if mapping.line > line && best_match.is_none() {
                let line_distance = mapping.line - line;
                if line_distance < best_distance {
                    best_distance = line_distance;
                    best_match = Some(mapping);
                }
            }
        }

        best_match.map(|m| m.code_offset)
    }

    /// Resolve a function breakpoint to a code offset
    fn resolve_function_breakpoint(debug_info: &DebugInfo, name: &str) -> Option<u32> {
        debug_info
            .symbols
            .iter()
            .find(|s| s.name == name)
            .map(|s| s.code_offset)
    }

    /// Check if any breakpoint is hit at the given code offset
    pub fn check_breakpoint(debugger: &Debugger, offset: u32) -> Option<BreakpointId> {
        for bp in debugger.breakpoints() {
            if bp.enabled && bp.resolved_offset == Some(offset) {
                return Some(bp.id);
            }
        }
        None
    }

    /// Record a hit on a breakpoint
    pub fn record_hit(debugger: &mut Debugger, id: BreakpointId) {
        if let Some(bp) = debugger.get_breakpoint_mut(id) {
            bp.hit_count += 1;
        }
    }

    /// Enable a breakpoint
    pub fn enable(debugger: &mut Debugger, id: BreakpointId) -> bool {
        if let Some(bp) = debugger.get_breakpoint_mut(id) {
            bp.enabled = true;
            true
        } else {
            false
        }
    }

    /// Disable a breakpoint
    pub fn disable(debugger: &mut Debugger, id: BreakpointId) -> bool {
        if let Some(bp) = debugger.get_breakpoint_mut(id) {
            bp.enabled = false;
            true
        } else {
            false
        }
    }

    /// Get all breakpoints at a specific offset
    pub fn at_offset(debugger: &Debugger, offset: u32) -> Vec<BreakpointId> {
        debugger
            .breakpoints()
            .filter(|bp| bp.resolved_offset == Some(offset))
            .map(|bp| bp.id)
            .collect()
    }

    /// Get all unresolved breakpoints
    pub fn unresolved(debugger: &Debugger) -> Vec<BreakpointId> {
        debugger
            .breakpoints()
            .filter(|bp| !bp.is_resolved())
            .map(|bp| bp.id)
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::Breakpoint;
    use debuginfo::{DebugInfo, LineMapping, LineMappingFlags, SourceFile, Symbol, SymbolKind};

    fn make_debug_info() -> DebugInfo {
        DebugInfo {
            sources: vec![SourceFile {
                id: 0,
                path: "test.skyl".to_string(),
                text: None,
            }],
            symbols: vec![
                Symbol {
                    name: "main".to_string(),
                    source_id: 0,
                    line: 1,
                    col: 1,
                    code_offset: 0,
                    code_size: 100,
                    kind: SymbolKind::Function,
                },
                Symbol {
                    name: "helper".to_string(),
                    source_id: 0,
                    line: 10,
                    col: 1,
                    code_offset: 100,
                    code_size: 50,
                    kind: SymbolKind::Function,
                },
            ],
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
                    line: 3,
                    col: 3,
                    flags: LineMappingFlags::STATEMENT,
                },
                LineMapping {
                    func_id: 0,
                    code_offset: 48,
                    source_id: 0,
                    line: 3,
                    col: 10,
                    flags: LineMappingFlags::empty(),
                },
            ],
            locals: vec![],
        }
    }

    #[test]
    fn test_resolve_source_breakpoint_exact_line() {
        let debug_info = make_debug_info();

        let offset =
            BreakpointManager::resolve_source_breakpoint(&debug_info, "test.skyl", 2, None);
        assert_eq!(offset, Some(16));
    }

    #[test]
    fn test_resolve_source_breakpoint_with_column() {
        let debug_info = make_debug_info();

        // Line 3 has two mappings: col 3 at offset 32, col 10 at offset 48
        // Asking for col 10 should return offset 48
        let offset =
            BreakpointManager::resolve_source_breakpoint(&debug_info, "test.skyl", 3, Some(10));
        assert_eq!(offset, Some(48));

        // Asking for col 3 should return offset 32
        let offset =
            BreakpointManager::resolve_source_breakpoint(&debug_info, "test.skyl", 3, Some(3));
        assert_eq!(offset, Some(32));
    }

    #[test]
    fn test_resolve_source_breakpoint_file_suffix() {
        let debug_info = make_debug_info();

        // Should match by suffix
        let offset =
            BreakpointManager::resolve_source_breakpoint(&debug_info, "skyl", 2, None);
        assert_eq!(offset, Some(16));
    }

    #[test]
    fn test_resolve_source_breakpoint_no_match() {
        let debug_info = make_debug_info();

        let offset =
            BreakpointManager::resolve_source_breakpoint(&debug_info, "nonexistent.skyl", 2, None);
        assert_eq!(offset, None);
    }

    #[test]
    fn test_resolve_function_breakpoint() {
        let debug_info = make_debug_info();

        let offset = BreakpointManager::resolve_function_breakpoint(&debug_info, "main");
        assert_eq!(offset, Some(0));

        let offset = BreakpointManager::resolve_function_breakpoint(&debug_info, "helper");
        assert_eq!(offset, Some(100));

        let offset = BreakpointManager::resolve_function_breakpoint(&debug_info, "nonexistent");
        assert_eq!(offset, None);
    }

    #[test]
    fn test_resolve_all_breakpoints() {
        let mut debugger = Debugger::new();
        debugger.load_debug_info(make_debug_info());

        // Add various breakpoints
        let bp1 = Breakpoint::at_source(BreakpointId(0), "test.skyl".to_string(), 2, None);
        debugger.add_breakpoint(bp1);

        let bp2 = Breakpoint::at_function(BreakpointId(0), "helper".to_string());
        debugger.add_breakpoint(bp2);

        let bp3 = Breakpoint::at_address(BreakpointId(0), 0x200);
        debugger.add_breakpoint(bp3);

        // Resolve all
        let resolved = BreakpointManager::resolve_all(&mut debugger);
        assert_eq!(resolved, 2); // Source and function breakpoints

        // Check they're resolved
        let ids: Vec<_> = debugger.breakpoints().map(|bp| bp.id).collect();
        for id in ids {
            let bp = debugger.get_breakpoint(id).unwrap();
            assert!(bp.is_resolved());
        }
    }

    #[test]
    fn test_resolve_all_no_debug_info() {
        let mut debugger = Debugger::new();

        let bp = Breakpoint::at_source(BreakpointId(0), "test.skyl".to_string(), 2, None);
        debugger.add_breakpoint(bp);

        let resolved = BreakpointManager::resolve_all(&mut debugger);
        assert_eq!(resolved, 0);
    }

    #[test]
    fn test_check_breakpoint() {
        let mut debugger = Debugger::new();

        let bp = Breakpoint::at_address(BreakpointId(0), 0x100);
        let id = debugger.add_breakpoint(bp);

        // Check at the breakpoint offset
        assert_eq!(BreakpointManager::check_breakpoint(&debugger, 0x100), Some(id));

        // Check at a different offset
        assert_eq!(BreakpointManager::check_breakpoint(&debugger, 0x200), None);
    }

    #[test]
    fn test_check_breakpoint_disabled() {
        let mut debugger = Debugger::new();

        let bp = Breakpoint::at_address(BreakpointId(0), 0x100);
        let id = debugger.add_breakpoint(bp);

        BreakpointManager::disable(&mut debugger, id);

        // Should not hit disabled breakpoint
        assert_eq!(BreakpointManager::check_breakpoint(&debugger, 0x100), None);
    }

    #[test]
    fn test_record_hit() {
        let mut debugger = Debugger::new();

        let bp = Breakpoint::at_address(BreakpointId(0), 0x100);
        let id = debugger.add_breakpoint(bp);

        assert_eq!(debugger.get_breakpoint(id).unwrap().hit_count, 0);

        BreakpointManager::record_hit(&mut debugger, id);
        assert_eq!(debugger.get_breakpoint(id).unwrap().hit_count, 1);

        BreakpointManager::record_hit(&mut debugger, id);
        assert_eq!(debugger.get_breakpoint(id).unwrap().hit_count, 2);
    }

    #[test]
    fn test_enable_disable() {
        let mut debugger = Debugger::new();

        let bp = Breakpoint::at_address(BreakpointId(0), 0x100);
        let id = debugger.add_breakpoint(bp);

        assert!(debugger.get_breakpoint(id).unwrap().enabled);

        assert!(BreakpointManager::disable(&mut debugger, id));
        assert!(!debugger.get_breakpoint(id).unwrap().enabled);

        assert!(BreakpointManager::enable(&mut debugger, id));
        assert!(debugger.get_breakpoint(id).unwrap().enabled);

        // Non-existent breakpoint
        assert!(!BreakpointManager::enable(&mut debugger, BreakpointId(999)));
        assert!(!BreakpointManager::disable(&mut debugger, BreakpointId(999)));
    }

    #[test]
    fn test_at_offset() {
        let mut debugger = Debugger::new();

        let bp1 = Breakpoint::at_address(BreakpointId(0), 0x100);
        let id1 = debugger.add_breakpoint(bp1);

        let bp2 = Breakpoint::at_address(BreakpointId(0), 0x100);
        let id2 = debugger.add_breakpoint(bp2);

        let bp3 = Breakpoint::at_address(BreakpointId(0), 0x200);
        debugger.add_breakpoint(bp3);

        let at_100 = BreakpointManager::at_offset(&debugger, 0x100);
        assert_eq!(at_100.len(), 2);
        assert!(at_100.contains(&id1));
        assert!(at_100.contains(&id2));

        let at_300 = BreakpointManager::at_offset(&debugger, 0x300);
        assert!(at_300.is_empty());
    }

    #[test]
    fn test_unresolved() {
        let mut debugger = Debugger::new();

        let bp1 = Breakpoint::at_address(BreakpointId(0), 0x100);
        debugger.add_breakpoint(bp1);

        let bp2 = Breakpoint::at_source(BreakpointId(0), "test.skyl".to_string(), 5, None);
        let id2 = debugger.add_breakpoint(bp2);

        let unresolved = BreakpointManager::unresolved(&debugger);
        assert_eq!(unresolved.len(), 1);
        assert!(unresolved.contains(&id2));
    }

    #[test]
    fn test_record_hit_nonexistent() {
        let mut debugger = Debugger::new();
        // Should not panic
        BreakpointManager::record_hit(&mut debugger, BreakpointId(999));
    }
}

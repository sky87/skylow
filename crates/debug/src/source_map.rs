//! Source mapping between code offsets and source locations

use crate::types::Debugger;
use debuginfo::{DebugInfo, LineMapping, Symbol};

/// Source location information
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceLocation {
    /// Source file path
    pub file: String,
    /// Line number (1-indexed)
    pub line: u32,
    /// Column number (1-indexed)
    pub col: u32,
    /// Function name containing this location
    pub function: Option<String>,
}

/// Source mapping operations
pub struct SourceMapper;

impl SourceMapper {
    /// Map a code offset to a source location
    pub fn offset_to_source(debugger: &Debugger, offset: u32) -> Option<SourceLocation> {
        let debug_info = debugger.debug_info()?;
        Self::offset_to_source_from_info(debug_info, offset)
    }

    /// Map a code offset to a source location using debug info directly
    pub fn offset_to_source_from_info(debug_info: &DebugInfo, offset: u32) -> Option<SourceLocation> {
        // Find the line mapping that contains this offset
        // Use the mapping with the largest code_offset that's <= our target offset
        let mapping = Self::find_mapping_for_offset(debug_info, offset)?;

        // Get the source file name
        let source_file = debug_info
            .sources
            .iter()
            .find(|s| s.id == mapping.source_id)?;

        // Find the function containing this offset
        let function = Self::find_function_at_offset(debug_info, offset);

        Some(SourceLocation {
            file: source_file.path.clone(),
            line: mapping.line,
            col: mapping.col,
            function: function.map(|f| f.name.clone()),
        })
    }

    /// Find the line mapping for a given offset
    fn find_mapping_for_offset(debug_info: &DebugInfo, offset: u32) -> Option<&LineMapping> {
        let mut best_match: Option<&LineMapping> = None;

        for mapping in &debug_info.line_map {
            if mapping.code_offset <= offset {
                if best_match.is_none()
                    || mapping.code_offset > best_match.unwrap().code_offset
                {
                    best_match = Some(mapping);
                }
            }
        }

        best_match
    }

    /// Find the function containing a given offset
    fn find_function_at_offset(debug_info: &DebugInfo, offset: u32) -> Option<&Symbol> {
        debug_info.symbols.iter().find(|s| {
            offset >= s.code_offset && offset < s.code_offset + s.code_size
        })
    }

    /// Map a source location to possible code offsets
    pub fn source_to_offsets(debugger: &Debugger, file: &str, line: u32) -> Vec<u32> {
        let debug_info = match debugger.debug_info() {
            Some(info) => info,
            None => return Vec::new(),
        };

        Self::source_to_offsets_from_info(debug_info, file, line)
    }

    /// Map a source location to code offsets using debug info directly
    pub fn source_to_offsets_from_info(debug_info: &DebugInfo, file: &str, line: u32) -> Vec<u32> {
        // Find the source file ID
        let source_id = match debug_info
            .sources
            .iter()
            .find(|s| s.path == file || s.path.ends_with(file))
        {
            Some(s) => s.id,
            None => return Vec::new(),
        };

        // Find all mappings for this line
        debug_info
            .line_map
            .iter()
            .filter(|m| m.source_id == source_id && m.line == line)
            .map(|m| m.code_offset)
            .collect()
    }

    /// Get the function at a given offset
    pub fn function_at_offset(debugger: &Debugger, offset: u32) -> Option<String> {
        let debug_info = debugger.debug_info()?;
        Self::find_function_at_offset(debug_info, offset).map(|s| s.name.clone())
    }

    /// Get all functions in the debug info
    pub fn all_functions(debugger: &Debugger) -> Vec<String> {
        match debugger.debug_info() {
            Some(info) => info.symbols.iter().map(|s| s.name.clone()).collect(),
            None => Vec::new(),
        }
    }

    /// Get the code range for a function
    pub fn function_code_range(debugger: &Debugger, name: &str) -> Option<(u32, u32)> {
        let debug_info = debugger.debug_info()?;
        debug_info
            .symbols
            .iter()
            .find(|s| s.name == name)
            .map(|s| (s.code_offset, s.code_offset + s.code_size))
    }

    /// Get source text at a given location if available
    pub fn get_source_text(debugger: &Debugger, file: &str) -> Option<String> {
        let debug_info = debugger.debug_info()?;
        debug_info
            .sources
            .iter()
            .find(|s| s.path == file || s.path.ends_with(file))
            .and_then(|s| s.text.clone())
    }

    /// Get a specific line from source text
    pub fn get_source_line(debugger: &Debugger, file: &str, line: u32) -> Option<String> {
        let text = Self::get_source_text(debugger, file)?;
        text.lines()
            .nth((line.saturating_sub(1)) as usize)
            .map(|s| s.to_string())
    }

    /// Get all line mappings for a function
    pub fn function_line_mappings(debugger: &Debugger, func_id: u32) -> Vec<LineMapping> {
        match debugger.debug_info() {
            Some(info) => info
                .line_map
                .iter()
                .filter(|m| m.func_id == func_id)
                .cloned()
                .collect(),
            None => Vec::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use debuginfo::{LineMappingFlags, SourceFile, Symbol, SymbolKind};

    fn make_debug_info() -> DebugInfo {
        DebugInfo {
            sources: vec![
                SourceFile {
                    id: 0,
                    path: "main.skyl".to_string(),
                    text: Some("fn main():\n  let x = 1\n  let y = 2\n".to_string()),
                },
                SourceFile {
                    id: 1,
                    path: "helper.skyl".to_string(),
                    text: None,
                },
            ],
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
                    source_id: 1,
                    line: 1,
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
                    func_id: 1,
                    code_offset: 100,
                    source_id: 1,
                    line: 1,
                    col: 1,
                    flags: LineMappingFlags::PROLOGUE,
                },
            ],
            locals: vec![],
        }
    }

    #[test]
    fn test_offset_to_source_exact() {
        let mut debugger = Debugger::new();
        debugger.load_debug_info(make_debug_info());

        let loc = SourceMapper::offset_to_source(&debugger, 16).unwrap();
        assert_eq!(loc.file, "main.skyl");
        assert_eq!(loc.line, 2);
        assert_eq!(loc.col, 3);
        assert_eq!(loc.function, Some("main".to_string()));
    }

    #[test]
    fn test_offset_to_source_between_mappings() {
        let mut debugger = Debugger::new();
        debugger.load_debug_info(make_debug_info());

        // Offset 20 should map to line 2 (the previous mapping)
        let loc = SourceMapper::offset_to_source(&debugger, 20).unwrap();
        assert_eq!(loc.line, 2);
    }

    #[test]
    fn test_offset_to_source_no_debug_info() {
        let debugger = Debugger::new();
        assert!(SourceMapper::offset_to_source(&debugger, 16).is_none());
    }

    #[test]
    fn test_source_to_offsets() {
        let mut debugger = Debugger::new();
        debugger.load_debug_info(make_debug_info());

        let offsets = SourceMapper::source_to_offsets(&debugger, "main.skyl", 2);
        assert_eq!(offsets, vec![16]);

        let offsets = SourceMapper::source_to_offsets(&debugger, "main.skyl", 1);
        assert_eq!(offsets, vec![0]);
    }

    #[test]
    fn test_source_to_offsets_no_match() {
        let mut debugger = Debugger::new();
        debugger.load_debug_info(make_debug_info());

        let offsets = SourceMapper::source_to_offsets(&debugger, "main.skyl", 999);
        assert!(offsets.is_empty());

        let offsets = SourceMapper::source_to_offsets(&debugger, "nonexistent.skyl", 1);
        assert!(offsets.is_empty());
    }

    #[test]
    fn test_source_to_offsets_no_debug_info() {
        let debugger = Debugger::new();
        let offsets = SourceMapper::source_to_offsets(&debugger, "main.skyl", 2);
        assert!(offsets.is_empty());
    }

    #[test]
    fn test_function_at_offset() {
        let mut debugger = Debugger::new();
        debugger.load_debug_info(make_debug_info());

        assert_eq!(
            SourceMapper::function_at_offset(&debugger, 50),
            Some("main".to_string())
        );
        assert_eq!(
            SourceMapper::function_at_offset(&debugger, 120),
            Some("helper".to_string())
        );
        assert_eq!(SourceMapper::function_at_offset(&debugger, 200), None);
    }

    #[test]
    fn test_all_functions() {
        let mut debugger = Debugger::new();
        debugger.load_debug_info(make_debug_info());

        let funcs = SourceMapper::all_functions(&debugger);
        assert_eq!(funcs.len(), 2);
        assert!(funcs.contains(&"main".to_string()));
        assert!(funcs.contains(&"helper".to_string()));
    }

    #[test]
    fn test_all_functions_no_debug_info() {
        let debugger = Debugger::new();
        let funcs = SourceMapper::all_functions(&debugger);
        assert!(funcs.is_empty());
    }

    #[test]
    fn test_function_code_range() {
        let mut debugger = Debugger::new();
        debugger.load_debug_info(make_debug_info());

        assert_eq!(
            SourceMapper::function_code_range(&debugger, "main"),
            Some((0, 100))
        );
        assert_eq!(
            SourceMapper::function_code_range(&debugger, "helper"),
            Some((100, 150))
        );
        assert_eq!(SourceMapper::function_code_range(&debugger, "nonexistent"), None);
    }

    #[test]
    fn test_get_source_text() {
        let mut debugger = Debugger::new();
        debugger.load_debug_info(make_debug_info());

        let text = SourceMapper::get_source_text(&debugger, "main.skyl");
        assert!(text.is_some());
        assert!(text.unwrap().contains("fn main()"));

        // No text for helper.skyl
        assert!(SourceMapper::get_source_text(&debugger, "helper.skyl").is_none());

        // No such file
        assert!(SourceMapper::get_source_text(&debugger, "nonexistent.skyl").is_none());
    }

    #[test]
    fn test_get_source_line() {
        let mut debugger = Debugger::new();
        debugger.load_debug_info(make_debug_info());

        let line = SourceMapper::get_source_line(&debugger, "main.skyl", 1);
        assert_eq!(line, Some("fn main():".to_string()));

        let line = SourceMapper::get_source_line(&debugger, "main.skyl", 2);
        assert_eq!(line, Some("  let x = 1".to_string()));

        // Line 0 should return first line (saturating sub)
        let line = SourceMapper::get_source_line(&debugger, "main.skyl", 0);
        assert_eq!(line, Some("fn main():".to_string()));

        // Line out of bounds
        assert!(SourceMapper::get_source_line(&debugger, "main.skyl", 999).is_none());
    }

    #[test]
    fn test_function_line_mappings() {
        let mut debugger = Debugger::new();
        debugger.load_debug_info(make_debug_info());

        let mappings = SourceMapper::function_line_mappings(&debugger, 0);
        assert_eq!(mappings.len(), 3);

        let mappings = SourceMapper::function_line_mappings(&debugger, 1);
        assert_eq!(mappings.len(), 1);

        let mappings = SourceMapper::function_line_mappings(&debugger, 999);
        assert!(mappings.is_empty());
    }

    #[test]
    fn test_function_line_mappings_no_debug_info() {
        let debugger = Debugger::new();
        let mappings = SourceMapper::function_line_mappings(&debugger, 0);
        assert!(mappings.is_empty());
    }

    #[test]
    fn test_offset_to_source_from_info_direct() {
        let debug_info = make_debug_info();
        let loc = SourceMapper::offset_to_source_from_info(&debug_info, 32).unwrap();
        assert_eq!(loc.line, 3);
    }

    #[test]
    fn test_source_to_offsets_from_info_direct() {
        let debug_info = make_debug_info();
        let offsets = SourceMapper::source_to_offsets_from_info(&debug_info, "main.skyl", 3);
        assert_eq!(offsets, vec![32]);
    }

    #[test]
    fn test_source_to_offsets_file_suffix_match() {
        let mut debugger = Debugger::new();
        debugger.load_debug_info(make_debug_info());

        // Should match by suffix
        let offsets = SourceMapper::source_to_offsets(&debugger, "skyl", 2);
        // Matches main.skyl line 2
        assert!(offsets.contains(&16));
    }
}

//! Debug information types for SkyDbg format

/// Complete debug information for a compiled program
#[derive(Debug, Clone, PartialEq, Default)]
pub struct DebugInfo {
    /// Source files referenced by the debug info
    pub sources: Vec<SourceFile>,
    /// Symbol table (functions, entry points)
    pub symbols: Vec<Symbol>,
    /// Line number mappings
    pub line_map: Vec<LineMapping>,
    /// Local variable information per function
    pub locals: Vec<FunctionLocals>,
}

/// A source file referenced in debug info
#[derive(Debug, Clone, PartialEq)]
pub struct SourceFile {
    /// Unique ID for this source (index in sources array)
    pub id: u32,
    /// Path or identifier for the source
    pub path: String,
    /// Optional: full source text (for embedded debugging)
    pub text: Option<String>,
}

/// A symbol (function) in the debug info
#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    /// Function/symbol name
    pub name: String,
    /// Source file ID
    pub source_id: u32,
    /// Start line in source
    pub line: u32,
    /// Start column in source
    pub col: u32,
    /// Byte offset of code start
    pub code_offset: u32,
    /// Size of generated code in bytes
    pub code_size: u32,
    /// Symbol kind
    pub kind: SymbolKind,
}

/// Kind of symbol
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    /// Regular function
    Function,
    /// Test function
    Test,
}

/// Mapping from code offset to source location
#[derive(Debug, Clone, PartialEq)]
pub struct LineMapping {
    /// Function ID (index in symbols array)
    pub func_id: u32,
    /// Byte offset within the function's code
    pub code_offset: u32,
    /// Source file ID
    pub source_id: u32,
    /// Line number (1-based)
    pub line: u32,
    /// Column number (1-based)
    pub col: u32,
    /// Flags for this mapping
    pub flags: LineMappingFlags,
}

bitflags::bitflags! {
    /// Flags for line mappings
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
    pub struct LineMappingFlags: u8 {
        /// This is a statement boundary (good breakpoint location)
        const STATEMENT = 0b0000_0001;
        /// This is a basic block entry
        const BLOCK_ENTRY = 0b0000_0010;
        /// This is a function prologue instruction
        const PROLOGUE = 0b0000_0100;
        /// This is a function epilogue instruction
        const EPILOGUE = 0b0000_1000;
    }
}

/// Local variable information for a function
#[derive(Debug, Clone, PartialEq, Default)]
pub struct FunctionLocals {
    /// Function ID (index in symbols array)
    pub func_id: u32,
    /// Local variables in this function
    pub locals: Vec<LocalVariable>,
}

/// A local variable
#[derive(Debug, Clone, PartialEq)]
pub struct LocalVariable {
    /// Variable name
    pub name: String,
    /// Register number where variable is stored
    pub register: u32,
    /// Code range where this variable is live (start offset)
    pub live_start: u32,
    /// Code range where this variable is live (end offset)
    pub live_end: u32,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_debug_info_default() {
        let info = DebugInfo::default();
        assert!(info.sources.is_empty());
        assert!(info.symbols.is_empty());
        assert!(info.line_map.is_empty());
        assert!(info.locals.is_empty());
    }

    #[test]
    fn test_source_file() {
        let source = SourceFile {
            id: 0,
            path: "test.skyl".to_string(),
            text: Some("fn main():".to_string()),
        };
        assert_eq!(source.id, 0);
        assert_eq!(source.path, "test.skyl");
        assert!(source.text.is_some());
    }

    #[test]
    fn test_symbol() {
        let sym = Symbol {
            name: "main".to_string(),
            source_id: 0,
            line: 1,
            col: 1,
            code_offset: 0,
            code_size: 100,
            kind: SymbolKind::Function,
        };
        assert_eq!(sym.name, "main");
        assert_eq!(sym.kind, SymbolKind::Function);
    }

    #[test]
    fn test_symbol_kind() {
        assert_ne!(SymbolKind::Function, SymbolKind::Test);
    }

    #[test]
    fn test_line_mapping() {
        let mapping = LineMapping {
            func_id: 0,
            code_offset: 16,
            source_id: 0,
            line: 5,
            col: 3,
            flags: LineMappingFlags::STATEMENT,
        };
        assert_eq!(mapping.func_id, 0);
        assert_eq!(mapping.code_offset, 16);
        assert!(mapping.flags.contains(LineMappingFlags::STATEMENT));
    }

    #[test]
    fn test_line_mapping_flags() {
        let flags = LineMappingFlags::STATEMENT | LineMappingFlags::BLOCK_ENTRY;
        assert!(flags.contains(LineMappingFlags::STATEMENT));
        assert!(flags.contains(LineMappingFlags::BLOCK_ENTRY));
        assert!(!flags.contains(LineMappingFlags::PROLOGUE));
    }

    #[test]
    fn test_line_mapping_flags_default() {
        let flags = LineMappingFlags::default();
        assert!(flags.is_empty());
    }

    #[test]
    fn test_function_locals() {
        let locals = FunctionLocals {
            func_id: 0,
            locals: vec![LocalVariable {
                name: "x".to_string(),
                register: 0,
                live_start: 0,
                live_end: 100,
            }],
        };
        assert_eq!(locals.func_id, 0);
        assert_eq!(locals.locals.len(), 1);
        assert_eq!(locals.locals[0].name, "x");
    }

    #[test]
    fn test_function_locals_default() {
        let locals = FunctionLocals::default();
        assert_eq!(locals.func_id, 0);
        assert!(locals.locals.is_empty());
    }

    #[test]
    fn test_local_variable() {
        let var = LocalVariable {
            name: "counter".to_string(),
            register: 5,
            live_start: 10,
            live_end: 50,
        };
        assert_eq!(var.name, "counter");
        assert_eq!(var.register, 5);
        assert_eq!(var.live_start, 10);
        assert_eq!(var.live_end, 50);
    }
}

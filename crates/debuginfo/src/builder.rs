//! Debug information builder

use crate::types::{
    DebugInfo, FunctionLocals, LineMapping, LineMappingFlags, LocalVariable, SourceFile, Symbol,
    SymbolKind,
};
use std::collections::HashMap;

/// Builder for constructing DebugInfo
#[derive(Debug, Default)]
pub struct DebugInfoBuilder {
    /// Source files by path
    source_map: HashMap<String, u32>,
    /// Source files in order
    sources: Vec<SourceFile>,
    /// Symbols (functions)
    symbols: Vec<Symbol>,
    /// Line mappings
    line_map: Vec<LineMapping>,
    /// Function locals
    locals: Vec<FunctionLocals>,
    /// Current function being built (if any)
    current_func: Option<u32>,
}

impl DebugInfoBuilder {
    /// Create a new debug info builder
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a source file, returns its ID
    ///
    /// If the source was already added, returns the existing ID.
    pub fn add_source(&mut self, path: &str, text: Option<&str>) -> u32 {
        if let Some(&id) = self.source_map.get(path) {
            return id;
        }

        let id = self.sources.len() as u32;
        self.sources.push(SourceFile {
            id,
            path: path.to_string(),
            text: text.map(String::from),
        });
        self.source_map.insert(path.to_string(), id);
        id
    }

    /// Begin a new function, returns its function ID
    pub fn begin_function(
        &mut self,
        name: &str,
        source_id: u32,
        line: u32,
        col: u32,
        kind: SymbolKind,
    ) -> u32 {
        let func_id = self.symbols.len() as u32;
        self.symbols.push(Symbol {
            name: name.to_string(),
            source_id,
            line,
            col,
            code_offset: 0,
            code_size: 0,
            kind,
        });
        self.locals.push(FunctionLocals {
            func_id,
            locals: Vec::new(),
        });
        self.current_func = Some(func_id);
        func_id
    }

    /// Set the code range for the current function
    pub fn set_function_code_range(&mut self, func_id: u32, offset: u32, size: u32) {
        if let Some(sym) = self.symbols.get_mut(func_id as usize) {
            sym.code_offset = offset;
            sym.code_size = size;
        }
    }

    /// Add a line mapping for the current function
    pub fn add_line_mapping(
        &mut self,
        func_id: u32,
        code_offset: u32,
        source_id: u32,
        line: u32,
        col: u32,
        flags: LineMappingFlags,
    ) {
        self.line_map.push(LineMapping {
            func_id,
            code_offset,
            source_id,
            line,
            col,
            flags,
        });
    }

    /// Add a local variable to a function
    pub fn add_local(
        &mut self,
        func_id: u32,
        name: &str,
        register: u32,
        live_start: u32,
        live_end: u32,
    ) {
        if let Some(locals) = self.locals.get_mut(func_id as usize) {
            locals.locals.push(LocalVariable {
                name: name.to_string(),
                register,
                live_start,
                live_end,
            });
        }
    }

    /// Finish the current function
    pub fn end_function(&mut self) {
        self.current_func = None;
    }

    /// Get the current function ID (if any)
    pub fn current_function(&self) -> Option<u32> {
        self.current_func
    }

    /// Build the final DebugInfo
    pub fn build(self) -> DebugInfo {
        DebugInfo {
            sources: self.sources,
            symbols: self.symbols,
            line_map: self.line_map,
            locals: self.locals,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_builder_new() {
        let builder = DebugInfoBuilder::new();
        assert!(builder.current_function().is_none());
    }

    #[test]
    fn test_builder_default() {
        let builder = DebugInfoBuilder::default();
        let info = builder.build();
        assert!(info.sources.is_empty());
        assert!(info.symbols.is_empty());
    }

    #[test]
    fn test_add_source() {
        let mut builder = DebugInfoBuilder::new();
        let id1 = builder.add_source("test.skyl", None);
        let id2 = builder.add_source("test2.skyl", Some("source text"));
        let id3 = builder.add_source("test.skyl", None); // duplicate

        assert_eq!(id1, 0);
        assert_eq!(id2, 1);
        assert_eq!(id3, 0); // returns existing ID

        let info = builder.build();
        assert_eq!(info.sources.len(), 2);
        assert_eq!(info.sources[0].path, "test.skyl");
        assert!(info.sources[0].text.is_none());
        assert_eq!(info.sources[1].text, Some("source text".to_string()));
    }

    #[test]
    fn test_begin_function() {
        let mut builder = DebugInfoBuilder::new();
        let source_id = builder.add_source("test.skyl", None);
        let func_id = builder.begin_function("main", source_id, 1, 1, SymbolKind::Function);

        assert_eq!(func_id, 0);
        assert_eq!(builder.current_function(), Some(0));

        let info = builder.build();
        assert_eq!(info.symbols.len(), 1);
        assert_eq!(info.symbols[0].name, "main");
        assert_eq!(info.symbols[0].kind, SymbolKind::Function);
    }

    #[test]
    fn test_set_function_code_range() {
        let mut builder = DebugInfoBuilder::new();
        let source_id = builder.add_source("test.skyl", None);
        let func_id = builder.begin_function("main", source_id, 1, 1, SymbolKind::Function);
        builder.set_function_code_range(func_id, 100, 200);

        let info = builder.build();
        assert_eq!(info.symbols[0].code_offset, 100);
        assert_eq!(info.symbols[0].code_size, 200);
    }

    #[test]
    fn test_add_line_mapping() {
        let mut builder = DebugInfoBuilder::new();
        let source_id = builder.add_source("test.skyl", None);
        let func_id = builder.begin_function("main", source_id, 1, 1, SymbolKind::Function);
        builder.add_line_mapping(func_id, 0, source_id, 1, 1, LineMappingFlags::STATEMENT);
        builder.add_line_mapping(func_id, 16, source_id, 2, 5, LineMappingFlags::empty());

        let info = builder.build();
        assert_eq!(info.line_map.len(), 2);
        assert_eq!(info.line_map[0].code_offset, 0);
        assert_eq!(info.line_map[1].line, 2);
    }

    #[test]
    fn test_add_local() {
        let mut builder = DebugInfoBuilder::new();
        let source_id = builder.add_source("test.skyl", None);
        let func_id = builder.begin_function("main", source_id, 1, 1, SymbolKind::Function);
        builder.add_local(func_id, "x", 0, 0, 100);
        builder.add_local(func_id, "y", 1, 10, 80);

        let info = builder.build();
        assert_eq!(info.locals.len(), 1);
        assert_eq!(info.locals[0].locals.len(), 2);
        assert_eq!(info.locals[0].locals[0].name, "x");
        assert_eq!(info.locals[0].locals[1].name, "y");
    }

    #[test]
    fn test_end_function() {
        let mut builder = DebugInfoBuilder::new();
        let source_id = builder.add_source("test.skyl", None);
        builder.begin_function("main", source_id, 1, 1, SymbolKind::Function);
        assert!(builder.current_function().is_some());
        builder.end_function();
        assert!(builder.current_function().is_none());
    }

    #[test]
    fn test_multiple_functions() {
        let mut builder = DebugInfoBuilder::new();
        let source_id = builder.add_source("test.skyl", None);

        let func1 = builder.begin_function("func1", source_id, 1, 1, SymbolKind::Function);
        builder.set_function_code_range(func1, 0, 50);
        builder.end_function();

        let func2 = builder.begin_function("test1", source_id, 10, 1, SymbolKind::Test);
        builder.set_function_code_range(func2, 50, 100);
        builder.end_function();

        let info = builder.build();
        assert_eq!(info.symbols.len(), 2);
        assert_eq!(info.symbols[0].name, "func1");
        assert_eq!(info.symbols[0].kind, SymbolKind::Function);
        assert_eq!(info.symbols[1].name, "test1");
        assert_eq!(info.symbols[1].kind, SymbolKind::Test);
    }

    #[test]
    fn test_complete_workflow() {
        let mut builder = DebugInfoBuilder::new();

        // Add sources
        let src = builder.add_source("main.skyl", Some("fn main():\n  assert(1 == 1)"));

        // Build function
        let func_id = builder.begin_function("main", src, 1, 1, SymbolKind::Function);
        builder.set_function_code_range(func_id, 0, 64);

        // Add line mappings
        builder.add_line_mapping(func_id, 0, src, 1, 1, LineMappingFlags::PROLOGUE);
        builder.add_line_mapping(func_id, 16, src, 2, 3, LineMappingFlags::STATEMENT);
        builder.add_line_mapping(func_id, 48, src, 2, 3, LineMappingFlags::EPILOGUE);

        // Add local
        builder.add_local(func_id, "r0", 0, 16, 48);

        builder.end_function();

        let info = builder.build();
        assert_eq!(info.sources.len(), 1);
        assert_eq!(info.symbols.len(), 1);
        assert_eq!(info.line_map.len(), 3);
        assert_eq!(info.locals.len(), 1);
        assert_eq!(info.locals[0].locals.len(), 1);
    }
}

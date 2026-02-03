//! Source module and location types for tracking source origin.
//!
//! This module provides types for representing source locations throughout
//! the compiler pipeline, from parsing through code generation.

/// A position in source code.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct SourceLoc {
    /// Byte offset in the source text.
    pub offset: u32,
    /// Line number (1-based).
    pub line: u32,
    /// Column number (1-based).
    pub col: u32,
}

impl SourceLoc {
    /// Create a new source location.
    pub fn new(offset: u32, line: u32, col: u32) -> Self {
        Self { offset, line, col }
    }
}

/// The kind of source module.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SourceModuleKind {
    /// The prelude (built-in syntax definitions).
    Prelude,
    /// A source file loaded from disk.
    File,
    /// Synthetic source created for inline tests.
    Synthetic,
}

/// A source module representing a single unit of source code.
///
/// Source modules are arena-allocated and immutable. Each module contains
/// the full source text and metadata about its origin.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SourceModule<'a> {
    /// The full source text.
    pub text: &'a str,
    /// The identifier for this source (file path, "<prelude>", or test name).
    pub id: &'a str,
    /// The kind of source module.
    pub kind: SourceModuleKind,
}

impl<'a> SourceModule<'a> {
    /// Create a new source module.
    pub fn new(text: &'a str, id: &'a str, kind: SourceModuleKind) -> Self {
        Self { text, id, kind }
    }

    /// Create a prelude source module.
    pub fn prelude(text: &'a str) -> Self {
        Self::new(text, "<prelude>", SourceModuleKind::Prelude)
    }

    /// Create a file source module.
    pub fn file(text: &'a str, path: &'a str) -> Self {
        Self::new(text, path, SourceModuleKind::File)
    }

    /// Create a synthetic source module (for tests).
    pub fn synthetic(text: &'a str, name: &'a str) -> Self {
        Self::new(text, name, SourceModuleKind::Synthetic)
    }

    /// Get a slice of the source text.
    pub fn slice(&self, start: usize, end: usize) -> &'a str {
        self.text.get(start..end).unwrap_or("")
    }
}

/// Source span information referencing a range within a source module.
///
/// This is the primary type for tracking source locations throughout the
/// compiler. It contains a reference to the source module and the span
/// within it, allowing source text to be retrieved on demand.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SourceInfo<'a> {
    /// The source module containing this span.
    pub module: &'a SourceModule<'a>,
    /// Start position of the span.
    pub start: SourceLoc,
    /// Byte offset of the end of the span (exclusive).
    pub end: u32,
}

impl<'a> SourceInfo<'a> {
    /// Create a new SourceInfo.
    pub fn new(module: &'a SourceModule<'a>, start: SourceLoc, end: u32) -> Self {
        Self { module, start, end }
    }

    /// Get the source text for this span.
    pub fn text(&self) -> &'a str {
        self.module.slice(self.start.offset as usize, self.end as usize)
    }

    /// Get the line number (1-based).
    pub fn line(&self) -> u32 {
        self.start.line
    }

    /// Get the column number (1-based).
    pub fn col(&self) -> u32 {
        self.start.col
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_source_loc_new() {
        let loc = SourceLoc::new(10, 2, 5);
        assert_eq!(loc.offset, 10);
        assert_eq!(loc.line, 2);
        assert_eq!(loc.col, 5);
    }

    #[test]
    fn test_source_loc_default() {
        let loc = SourceLoc::default();
        assert_eq!(loc.offset, 0);
        assert_eq!(loc.line, 0);
        assert_eq!(loc.col, 0);
    }

    #[test]
    fn test_source_module_prelude() {
        let module = SourceModule::prelude("syntax int [0-9]+");
        assert_eq!(module.id, "<prelude>");
        assert_eq!(module.kind, SourceModuleKind::Prelude);
        assert_eq!(module.text, "syntax int [0-9]+");
    }

    #[test]
    fn test_source_module_file() {
        let module = SourceModule::file("fn main():", "test.skyl");
        assert_eq!(module.id, "test.skyl");
        assert_eq!(module.kind, SourceModuleKind::File);
    }

    #[test]
    fn test_source_module_synthetic() {
        let module = SourceModule::synthetic("test foo:", "test_case");
        assert_eq!(module.id, "test_case");
        assert_eq!(module.kind, SourceModuleKind::Synthetic);
    }

    #[test]
    fn test_source_module_slice() {
        let module = SourceModule::prelude("hello world");
        assert_eq!(module.slice(0, 5), "hello");
        assert_eq!(module.slice(6, 11), "world");
        // Out of bounds returns empty
        assert_eq!(module.slice(100, 200), "");
    }

    #[test]
    fn test_source_module_new() {
        let module = SourceModule::new("text", "id", SourceModuleKind::File);
        assert_eq!(module.text, "text");
        assert_eq!(module.id, "id");
        assert_eq!(module.kind, SourceModuleKind::File);
    }

    #[test]
    fn test_source_info() {
        let module = SourceModule::synthetic("hello world", "test");
        let info = SourceInfo::new(&module, SourceLoc::new(0, 1, 1), 5);
        assert_eq!(info.text(), "hello");
        assert_eq!(info.line(), 1);
        assert_eq!(info.col(), 1);
    }

    #[test]
    fn test_source_info_middle() {
        let module = SourceModule::synthetic("hello world", "test");
        let info = SourceInfo::new(&module, SourceLoc::new(6, 1, 7), 11);
        assert_eq!(info.text(), "world");
    }
}

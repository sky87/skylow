//! Parser trait definition for common interface.
//!
//! This trait defines the common interface implemented by both
//! `InterpretedParser` and `VMParser`.

use common::{SourceModule, StringInterner};

use crate::node::{ParseError, SyntaxNode};
use crate::syntax::SyntaxRule;

/// Common interface for parsers.
///
/// This trait is implemented by both the interpreted parser and the VM-based
/// parser, allowing syntaxlang functions to work with either implementation.
pub trait Parser<'a>: Sized {
    // =========================================================================
    // Core parsing
    // =========================================================================

    /// Check if the parser has reached end of file.
    fn is_eof(&self) -> bool;

    /// Parse the next command from the source.
    ///
    /// Returns None at EOF or on parse failure. On failure, call `error()` to get details.
    fn next_command(&mut self) -> Option<&'a SyntaxNode<'a>>;

    /// Parse from current position using the given category.
    fn parse_category(&mut self, category: &str) -> Option<&'a SyntaxNode<'a>>;

    /// Get the parse error from the last failed parse.
    fn error(&self) -> Option<ParseError>;

    /// Skip to the next line (for error recovery).
    fn skip_to_next_line(&mut self);

    /// Switch to a new source module while keeping registered rules.
    ///
    /// Resets position to offset 0, line 1, col 1.
    /// Used after parsing a prelude to parse user code with correct offsets.
    fn set_source(&mut self, module: &'a SourceModule<'a>);

    // =========================================================================
    // Rule management
    // =========================================================================

    /// Add a syntax rule to the grammar.
    fn add_rule(&mut self, rule: &'a SyntaxRule<'a>);

    // =========================================================================
    // String interner access
    // =========================================================================

    /// Get mutable access to the parser's string interner.
    ///
    /// This allows callers to share the parser's arena-backed string interner
    /// rather than creating separate interners.
    fn strings_mut(&mut self) -> &mut StringInterner<'a>;

    // =========================================================================
    // Debug utilities
    // =========================================================================

    /// Enable or disable parse tracing.
    fn set_trace(&mut self, enabled: bool);

    /// Dump parser state to stderr.
    ///
    /// Uses `&mut self` because some implementations need to finalize state.
    fn dump(&mut self);

    /// Dump registered rules to stderr.
    fn dump_rules(&self);
}

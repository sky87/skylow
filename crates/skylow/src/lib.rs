//! SkyLow Compiler Library
//!
//! This library provides the high-level driver for parsing SkyLow source files.

pub mod driver;

// Re-export parser types for convenience
pub use skylow_parser::{
    format_errors, format_node, syntax_node_to_string, Atom, AtomWithQuant, CharClass,
    InterpretedParser, ParseError, Parser, Quantifier, SourceLoc, SyntaxNode, SyntaxRule, VMParser,
};

// Re-export driver
pub use driver::{Driver, ParseResult};

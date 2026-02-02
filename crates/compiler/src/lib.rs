//! SkyLow Compiler Library
//!
//! This library provides the high-level driver for parsing SkyLow source files
//! and running tests.

pub mod driver;
pub mod runner;

// Re-export parser types for convenience
pub use parser::{
    format_errors, format_node, syntax_node_to_string, Atom, AtomWithQuant, CharClass,
    InterpretedParser, ParseError, Parser, Quantifier, SourceLoc, SyntaxNode, SyntaxRule, VMParser,
};

// Re-export driver
pub use driver::{Driver, ParseResult};

// Re-export runner
pub use runner::{Compiler, MainResult, MainRunner, RunResult, TestResult, TestRunner};

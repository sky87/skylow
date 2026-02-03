//! SkyLow Parser
//!
//! A customizable parser where grammar rules can be dynamically added.
//! Supports both interpreter-based and bytecode VM execution.
//!
//! # Overview
//!
//! The parser processes source as a sequence of **commands**. Each command is either:
//! - A syntax declaration (which modifies the grammar for subsequent commands)
//! - An expression (which produces output)
//!
//! # Example
//!
//! ```ignore
//! use bumpalo::Bump;
//! use skylow_parser::{InterpretedParser, Parser};
//!
//! let arena = Bump::new();
//! let mut parser = InterpretedParser::new(&arena, source);
//!
//! while !parser.is_eof() {
//!     match parser.next_command() {
//!         Some(node) => {
//!             // Process the command
//!         }
//!         None => {
//!             if let Some(err) = parser.error() {
//!                 // Handle error
//!             }
//!             parser.skip_to_next_line();
//!         }
//!     }
//! }
//! ```
//!
//! # Public API
//!
//! Both parser types implement the [`Parser`] trait with these methods:
//!
//! - [`Parser::next_command`] - Parse the next command
//! - [`Parser::add_rule`] - Add a syntax rule to the grammar
//! - [`Parser::error`] - Get the parse error from the last failed parse
//! - [`Parser::is_eof`] - Check if all input has been consumed
//! - [`Parser::skip_to_next_line`] - Skip to next line (error recovery)
//!
//! Debug utilities:
//! - [`Parser::set_trace`] - Enable/disable parse tracing
//! - [`Parser::dump`] - Dump parser state
//! - [`Parser::dump_rules`] - Dump registered rules
//!
//! # Parser Implementations
//!
//! - [`InterpretedParser`] - Direct rule interpretation (simpler, good for debugging)
//! - [`VMParser`] - Bytecode VM execution (compiles rules to bytecode)
//!
//! Both implement the same [`Parser`] trait, so syntaxlang functions work with either.

pub mod constants;
pub mod format;
mod node;
mod parser_trait;
mod parser_interp;
pub mod parser_vm;
mod syntax;
mod syntaxlang;

// Re-export from skylow-common
pub use common::{debug, intern};
pub use common::{create_logger, Logger};

// Re-export public types
pub use format::{format_errors, format_node, syntax_node_to_string};
pub use node::{ParseError, SourceInfo, SourceLoc, SourceModule, SyntaxNode};
pub use parser_trait::Parser;
pub use parser_interp::InterpretedParser;
pub use parser_vm::{CompiledGrammar, VMParser};
pub use syntax::{Atom, AtomWithQuant, CharClass, Quantifier, RuleTable, SyntaxRule};

// Re-export syntaxlang helpers for use by the compiler library
// These functions are now generic and work with any Parser implementation
pub use syntaxlang::{
    add_expr_rule, add_implicit_whitespace, add_syntax_decl_command_rule,
    extract_and_register_rule, extract_pattern_atoms, get_child_by_category,
    get_children_by_category, get_node_text, init_syntaxlang, is_lexical_pattern,
    parse_charset_content, unescape_string,
};

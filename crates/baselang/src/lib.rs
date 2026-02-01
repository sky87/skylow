//! BaseLang - Typed AST for SkyLow
//!
//! This crate converts raw SyntaxNode trees from the parser into a typed AST
//! suitable for further compilation passes. It includes the prelude syntax
//! definitions for arithmetic, comparisons, and test infrastructure.

pub mod ast;
pub mod lower;
mod parse;

pub use ast::{BinOp, CmpOp, Expr, Program, SourceInfo, Stmt, TestDecl};
pub use lower::{lower_program, LowerError};
pub use parse::{parse_with_prelude, ParseResult};

/// The prelude syntax definitions for BaseLang.
/// Includes integer literals, arithmetic, comparisons, and test infrastructure.
pub const PRELUDE: &str = include_str!("prelude.skyl");

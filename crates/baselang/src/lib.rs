//! BaseLang - Typed AST for SkyLow
//!
//! This crate converts raw SyntaxNode trees from the parser into a typed AST
//! suitable for further compilation passes. It includes the prelude syntax
//! definitions for arithmetic, comparisons, and test infrastructure.

pub mod ast;
pub mod lower;
mod parse;
pub mod typecheck;

pub use ast::{BinOp, CmpOp, Decl, DeclKind, Expr, ExprKind, FnParam, NodeId, Program, SourceInfo, Stmt, StmtKind, Type, new_node_id, reset_node_ids};
pub use lower::{lower_program, LowerError};
pub use parse::{parse_with_prelude, parse_with_prelude_named, ParseResult};
pub use typecheck::{typecheck_program, CheckedType, CompilationContext, FnSignature, TypeError, TypeChecker};

/// The prelude syntax definitions for BaseLang.
/// Includes integer literals, arithmetic, comparisons, and test infrastructure.
pub const PRELUDE: &str = include_str!("prelude.skyl");

//! Typed AST for BaseLang
//!
//! This module defines the AST types used after lowering from SyntaxNode.
//! All types use arena allocation for zero-copy references.

/// Source location information for error reporting
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SourceInfo<'a> {
    /// Line number (1-based)
    pub line: u32,
    /// Column number (1-based)
    pub col: u32,
    /// Source text of the expression
    pub source: &'a str,
}

/// Binary arithmetic operations
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

/// Comparison operations
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CmpOp {
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
}

/// Expression AST
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Expr<'a> {
    /// Integer literal
    Int(i64),
    /// Binary arithmetic operation
    BinOp {
        op: BinOp,
        left: &'a Expr<'a>,
        right: &'a Expr<'a>,
    },
    /// Comparison operation
    Cmp {
        op: CmpOp,
        left: &'a Expr<'a>,
        right: &'a Expr<'a>,
    },
    /// Parenthesized expression (kept for debugging, semantically transparent)
    Paren(&'a Expr<'a>),
}

/// Statement AST
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Stmt<'a> {
    /// Assert statement: assert(condition) with source info for error reporting
    Assert {
        expr: &'a Expr<'a>,
        info: SourceInfo<'a>,
    },
}

/// Test declaration
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TestDecl<'a> {
    /// Test name (e.g., "arithmetic works as expected")
    pub name: &'a str,
    /// Body statements
    pub body: &'a [Stmt<'a>],
}

/// Function declaration
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct FnDecl<'a> {
    /// Function name (e.g., "main")
    pub name: &'a str,
    /// Body statements
    pub body: &'a [Stmt<'a>],
    /// Source info for the function declaration (for error reporting)
    pub info: SourceInfo<'a>,
}

/// A complete program (collection of test declarations and functions)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Program<'a> {
    pub tests: &'a [TestDecl<'a>],
    pub functions: &'a [FnDecl<'a>],
}

impl<'a> Default for Program<'a> {
    fn default() -> Self {
        Program {
            tests: &[],
            functions: &[],
        }
    }
}

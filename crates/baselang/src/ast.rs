//! Typed AST for BaseLang
//!
//! This module defines the AST types used after lowering from SyntaxNode.
//! All types use arena allocation for zero-copy references.
//!
//! Each AST node has a consistent structure: an outer struct containing
//! source info and an inner enum (or data struct) with the actual node data.

// Re-export SourceInfo from common for use by AST consumers
pub use common::SourceInfo;

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

/// Expression AST node
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Expr<'a> {
    pub info: SourceInfo<'a>,
    pub kind: ExprKind<'a>,
}

/// Expression kinds
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ExprKind<'a> {
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

/// Statement AST node
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Stmt<'a> {
    pub info: SourceInfo<'a>,
    pub kind: StmtKind<'a>,
}

/// Statement kinds
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum StmtKind<'a> {
    /// Assert statement: assert(condition)
    Assert { expr: &'a Expr<'a> },
}

/// Declaration AST node
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Decl<'a> {
    pub info: SourceInfo<'a>,
    pub kind: DeclKind<'a>,
}

/// Declaration kinds
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum DeclKind<'a> {
    /// Test declaration
    Test {
        name: &'a str,
        body: &'a [Stmt<'a>],
    },
    /// Function declaration
    Fn {
        name: &'a str,
        body: &'a [Stmt<'a>],
    },
}

/// A complete program (collection of declarations)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Program<'a> {
    pub decls: &'a [Decl<'a>],
}

impl<'a> Program<'a> {
    /// Get all test declarations
    pub fn tests(&self) -> impl Iterator<Item = &Decl<'a>> {
        self.decls.iter().filter(|d| matches!(d.kind, DeclKind::Test { .. }))
    }

    /// Get all function declarations
    pub fn functions(&self) -> impl Iterator<Item = &Decl<'a>> {
        self.decls.iter().filter(|d| matches!(d.kind, DeclKind::Fn { .. }))
    }
}

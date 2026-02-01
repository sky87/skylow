//! Typed AST for BaseLang
//!
//! This module defines the AST types used after lowering from SyntaxNode.

/// Source location information for error reporting
#[derive(Debug, Clone, PartialEq)]
pub struct SourceInfo {
    /// Line number (1-based)
    pub line: u32,
    /// Column number (1-based)
    pub col: u32,
    /// Source text of the expression
    pub source: String,
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
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// Integer literal
    Int(i64),
    /// Binary arithmetic operation
    BinOp {
        op: BinOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    /// Comparison operation
    Cmp {
        op: CmpOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    /// Parenthesized expression (kept for debugging, semantically transparent)
    Paren(Box<Expr>),
}

/// Statement AST
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    /// Assert statement: assert(condition) with source info for error reporting
    Assert { expr: Expr, info: SourceInfo },
}

/// Test declaration
#[derive(Debug, Clone, PartialEq)]
pub struct TestDecl {
    /// Test name (e.g., "arithmetic works as expected")
    pub name: String,
    /// Body statements
    pub body: Vec<Stmt>,
}

/// Function declaration
#[derive(Debug, Clone, PartialEq)]
pub struct FnDecl {
    /// Function name (e.g., "main")
    pub name: String,
    /// Body statements
    pub body: Vec<Stmt>,
    /// Source info for the function declaration (for error reporting)
    pub info: SourceInfo,
}

/// A complete program (collection of test declarations and functions)
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Program {
    pub tests: Vec<TestDecl>,
    pub functions: Vec<FnDecl>,
}

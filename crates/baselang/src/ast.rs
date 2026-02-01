//! Typed AST for BaseLang
//!
//! This module defines the AST types used after lowering from SyntaxNode.

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
    /// Assert statement: assert(condition)
    Assert(Expr),
}

/// Test declaration
#[derive(Debug, Clone, PartialEq)]
pub struct TestDecl {
    /// Test name (e.g., "arithmetic works as expected")
    pub name: String,
    /// Body statements
    pub body: Vec<Stmt>,
}

/// A complete program (collection of test declarations)
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Program {
    pub tests: Vec<TestDecl>,
}

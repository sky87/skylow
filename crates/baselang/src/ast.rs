//! Typed AST for BaseLang
//!
//! This module defines the AST types used after lowering from SyntaxNode.
//! All types use arena allocation for zero-copy references.
//!
//! Each AST node has a consistent structure: an outer struct containing
//! source info and an inner enum (or data struct) with the actual node data.
//! Each node also has a unique ID for type checking and analysis.

use std::sync::atomic::{AtomicU32, Ordering};

// Re-export SourceInfo from common for use by AST consumers
pub use common::SourceInfo;

/// Unique identifier for AST nodes
pub type NodeId = u32;

/// Global counter for generating unique node IDs
static NEXT_NODE_ID: AtomicU32 = AtomicU32::new(0);

/// Generate a new unique node ID
pub fn new_node_id() -> NodeId {
    NEXT_NODE_ID.fetch_add(1, Ordering::Relaxed)
}

/// Reset the node ID counter (useful for testing)
pub fn reset_node_ids() {
    NEXT_NODE_ID.store(0, Ordering::Relaxed);
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

/// Type annotation
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    /// 64-bit signed integer
    I64,
}

/// Function parameter
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct FnParam<'a> {
    /// Parameter name
    pub name: &'a str,
    /// Parameter type
    pub ty: Type,
    /// Source location info
    pub info: SourceInfo<'a>,
    /// Unique node ID
    pub id: NodeId,
}

/// Expression AST node
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Expr<'a> {
    pub info: SourceInfo<'a>,
    pub kind: ExprKind<'a>,
    /// Unique node ID for type checking
    pub id: NodeId,
}

/// Expression kinds
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ExprKind<'a> {
    /// Integer literal
    Int(i64),
    /// Variable reference
    Var(&'a str),
    /// Function call
    Call {
        name: &'a str,
        args: &'a [Expr<'a>],
    },
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
    /// Unique node ID
    pub id: NodeId,
}

/// Statement kinds
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum StmtKind<'a> {
    /// Assert statement: assert(condition)
    Assert { expr: &'a Expr<'a> },
    /// Return statement: return expr
    Return { expr: &'a Expr<'a> },
}

/// Declaration AST node
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Decl<'a> {
    pub info: SourceInfo<'a>,
    pub kind: DeclKind<'a>,
    /// Unique node ID
    pub id: NodeId,
}

/// Declaration kinds
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum DeclKind<'a> {
    /// Test declaration
    Test {
        name: &'a str,
        body: &'a [Stmt<'a>],
    },
    /// Function declaration with parameters and return type
    Fn {
        name: &'a str,
        params: &'a [FnParam<'a>],
        return_type: Type,
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

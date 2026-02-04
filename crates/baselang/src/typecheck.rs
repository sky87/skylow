//! Type checker for BaseLang
//!
//! This module provides type checking for BaseLang programs.
//! Type information is stored out-of-band using maps indexed by NodeId.

use std::collections::HashMap;

use crate::ast::{Decl, DeclKind, Expr, ExprKind, NodeId, Program, Stmt, StmtKind, Type};
use common::SourceInfo;

/// Checked type for an expression
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CheckedType {
    /// 64-bit signed integer
    I64,
    /// Boolean (result of comparisons)
    Bool,
    /// Unit type (no value)
    Unit,
    /// Type error occurred
    Error,
}

impl From<Type> for CheckedType {
    fn from(ty: Type) -> Self {
        match ty {
            Type::I64 => CheckedType::I64,
        }
    }
}

/// Function signature
#[derive(Debug, Clone)]
pub struct FnSignature {
    /// Parameter types
    pub params: Vec<CheckedType>,
    /// Return type
    pub return_type: CheckedType,
}

/// Type checking error
#[derive(Debug, Clone)]
pub struct TypeError {
    /// Error message
    pub message: String,
    /// Line number (1-based)
    pub line: u32,
    /// Column number (1-based)
    pub col: u32,
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}: {}", self.line, self.col, self.message)
    }
}

impl std::error::Error for TypeError {}

/// Compilation context for type checking
///
/// Stores type information out-of-band, indexed by NodeId.
#[derive(Debug, Default)]
pub struct CompilationContext {
    /// Types of expressions, indexed by NodeId
    expr_types: HashMap<NodeId, CheckedType>,
    /// Function signatures, indexed by function name
    fn_signatures: HashMap<String, FnSignature>,
    /// Variable types in current scope, indexed by variable name
    var_types: HashMap<String, CheckedType>,
    /// Errors encountered during type checking
    errors: Vec<TypeError>,
}

impl CompilationContext {
    /// Create a new empty compilation context
    pub fn new() -> Self {
        Self::default()
    }

    /// Get the type of an expression by its NodeId
    pub fn get_expr_type(&self, id: NodeId) -> Option<CheckedType> {
        self.expr_types.get(&id).copied()
    }

    /// Set the type of an expression
    pub fn set_expr_type(&mut self, id: NodeId, ty: CheckedType) {
        self.expr_types.insert(id, ty);
    }

    /// Get a function signature by name
    pub fn get_fn_signature(&self, name: &str) -> Option<&FnSignature> {
        self.fn_signatures.get(name)
    }

    /// Register a function signature
    pub fn register_fn(&mut self, name: String, sig: FnSignature) {
        self.fn_signatures.insert(name, sig);
    }

    /// Get a variable's type in the current scope
    pub fn get_var_type(&self, name: &str) -> Option<CheckedType> {
        self.var_types.get(name).copied()
    }

    /// Set a variable's type in the current scope
    pub fn set_var_type(&mut self, name: String, ty: CheckedType) {
        self.var_types.insert(name, ty);
    }

    /// Clear variable types (for entering a new scope)
    pub fn clear_vars(&mut self) {
        self.var_types.clear();
    }

    /// Add an error
    pub fn add_error(&mut self, message: String, info: &SourceInfo) {
        self.errors.push(TypeError {
            message,
            line: info.line(),
            col: info.col(),
        });
    }

    /// Get all errors
    pub fn errors(&self) -> &[TypeError] {
        &self.errors
    }

    /// Check if there are any errors
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    /// Take errors out of the context
    pub fn take_errors(&mut self) -> Vec<TypeError> {
        std::mem::take(&mut self.errors)
    }
}

/// Type checker for BaseLang programs
pub struct TypeChecker<'a> {
    ctx: &'a mut CompilationContext,
    /// Current function's return type (for checking return statements)
    current_return_type: Option<CheckedType>,
}

impl<'a> TypeChecker<'a> {
    /// Create a new type checker with the given context
    pub fn new(ctx: &'a mut CompilationContext) -> Self {
        Self {
            ctx,
            current_return_type: None,
        }
    }

    /// Type check a complete program
    pub fn check_program(&mut self, program: &Program) {
        // First pass: register all function signatures
        for decl in program.decls {
            if let DeclKind::Fn {
                name,
                params,
                return_type,
                ..
            } = &decl.kind
            {
                let sig = FnSignature {
                    params: params.iter().map(|p| p.ty.into()).collect(),
                    return_type: (*return_type).into(),
                };
                self.ctx.register_fn((*name).to_string(), sig);
            }
        }

        // Second pass: type check all declarations
        for decl in program.decls {
            self.check_decl(decl);
        }
    }

    /// Type check a declaration
    fn check_decl(&mut self, decl: &Decl) {
        match &decl.kind {
            DeclKind::Test { body, .. } => {
                self.ctx.clear_vars();
                self.current_return_type = None;
                for stmt in *body {
                    self.check_stmt(stmt);
                }
            }
            DeclKind::Fn {
                params,
                return_type,
                body,
                ..
            } => {
                self.ctx.clear_vars();
                self.current_return_type = Some((*return_type).into());

                // Register parameters as variables
                for param in *params {
                    self.ctx
                        .set_var_type(param.name.to_string(), param.ty.into());
                }

                // Check function body
                for stmt in *body {
                    self.check_stmt(stmt);
                }
            }
        }
    }

    /// Type check a statement
    fn check_stmt(&mut self, stmt: &Stmt) {
        match &stmt.kind {
            StmtKind::Assert { expr } => {
                let ty = self.check_expr(expr);
                // Assert expects a boolean (comparison result) or integer (truthy)
                if ty != CheckedType::Bool && ty != CheckedType::I64 && ty != CheckedType::Error {
                    self.ctx.add_error(
                        format!("assert expects a boolean or integer, got {:?}", ty),
                        &stmt.info,
                    );
                }
            }
            StmtKind::Return { expr } => {
                let ty = self.check_expr(expr);
                if let Some(expected) = self.current_return_type {
                    if ty != expected && ty != CheckedType::Error {
                        self.ctx.add_error(
                            format!(
                                "return type mismatch: expected {:?}, got {:?}",
                                expected, ty
                            ),
                            &stmt.info,
                        );
                    }
                } else {
                    self.ctx.add_error(
                        "return statement outside of function".to_string(),
                        &stmt.info,
                    );
                }
            }
        }
    }

    /// Type check an expression and return its type
    fn check_expr(&mut self, expr: &Expr) -> CheckedType {
        let ty = match &expr.kind {
            ExprKind::Int(_) => CheckedType::I64,

            ExprKind::Var(name) => {
                if let Some(ty) = self.ctx.get_var_type(name) {
                    ty
                } else {
                    self.ctx
                        .add_error(format!("undefined variable: {}", name), &expr.info);
                    CheckedType::Error
                }
            }

            ExprKind::Call { name, args } => {
                if let Some(sig) = self.ctx.get_fn_signature(name).cloned() {
                    // Check argument count
                    if args.len() != sig.params.len() {
                        self.ctx.add_error(
                            format!(
                                "function {} expects {} arguments, got {}",
                                name,
                                sig.params.len(),
                                args.len()
                            ),
                            &expr.info,
                        );
                        return CheckedType::Error;
                    }

                    // Check argument types
                    for (i, (arg, expected_ty)) in args.iter().zip(sig.params.iter()).enumerate() {
                        let arg_ty = self.check_expr(arg);
                        if arg_ty != *expected_ty && arg_ty != CheckedType::Error {
                            self.ctx.add_error(
                                format!(
                                    "argument {} of {} has wrong type: expected {:?}, got {:?}",
                                    i + 1,
                                    name,
                                    expected_ty,
                                    arg_ty
                                ),
                                &arg.info,
                            );
                        }
                    }

                    sig.return_type
                } else {
                    self.ctx
                        .add_error(format!("undefined function: {}", name), &expr.info);
                    CheckedType::Error
                }
            }

            ExprKind::BinOp { op, left, right } => {
                let left_ty = self.check_expr(left);
                let right_ty = self.check_expr(right);

                // Binary ops require both operands to be I64
                if left_ty != CheckedType::I64 && left_ty != CheckedType::Error {
                    self.ctx.add_error(
                        format!(
                            "left operand of {:?} must be I64, got {:?}",
                            op, left_ty
                        ),
                        &left.info,
                    );
                }
                if right_ty != CheckedType::I64 && right_ty != CheckedType::Error {
                    self.ctx.add_error(
                        format!(
                            "right operand of {:?} must be I64, got {:?}",
                            op, right_ty
                        ),
                        &right.info,
                    );
                }

                CheckedType::I64
            }

            ExprKind::Cmp { op, left, right } => {
                let left_ty = self.check_expr(left);
                let right_ty = self.check_expr(right);

                // Comparison ops require both operands to be I64
                if left_ty != CheckedType::I64 && left_ty != CheckedType::Error {
                    self.ctx.add_error(
                        format!(
                            "left operand of {:?} must be I64, got {:?}",
                            op, left_ty
                        ),
                        &left.info,
                    );
                }
                if right_ty != CheckedType::I64 && right_ty != CheckedType::Error {
                    self.ctx.add_error(
                        format!(
                            "right operand of {:?} must be I64, got {:?}",
                            op, right_ty
                        ),
                        &right.info,
                    );
                }

                CheckedType::Bool
            }

            ExprKind::Paren(inner) => self.check_expr(inner),
        };

        // Store the type in the context
        self.ctx.set_expr_type(expr.id, ty);
        ty
    }
}

/// Type check a program, returning any errors
pub fn typecheck_program(program: &Program) -> Result<CompilationContext, Vec<TypeError>> {
    let mut ctx = CompilationContext::new();
    let mut checker = TypeChecker::new(&mut ctx);
    checker.check_program(program);

    if ctx.has_errors() {
        Err(ctx.take_errors())
    } else {
        Ok(ctx)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{new_node_id, reset_node_ids, BinOp, CmpOp, FnParam};
    use bumpalo::Bump;
    use common::{SourceLoc, SourceModule};

    fn make_test_info<'a>(arena: &'a Bump, text: &'a str) -> SourceInfo<'a> {
        let module = arena.alloc(SourceModule::synthetic(text, "<test>"));
        SourceInfo::new(module, SourceLoc::new(0, 1, 1), text.len() as u32)
    }

    #[test]
    fn test_simple_expression() {
        reset_node_ids();
        let arena = Bump::new();
        let info = make_test_info(&arena, "1 + 2");

        let left = arena.alloc(Expr {
            info,
            kind: ExprKind::Int(1),
            id: new_node_id(),
        });
        let right = arena.alloc(Expr {
            info,
            kind: ExprKind::Int(2),
            id: new_node_id(),
        });
        let expr = arena.alloc(Expr {
            info,
            kind: ExprKind::BinOp {
                op: BinOp::Add,
                left,
                right,
            },
            id: new_node_id(),
        });

        let stmt = Stmt {
            info,
            kind: StmtKind::Assert { expr },
            id: new_node_id(),
        };

        let decl = Decl {
            info,
            kind: DeclKind::Test {
                name: "test",
                body: arena.alloc_slice_copy(&[stmt]),
            },
            id: new_node_id(),
        };

        let program = Program {
            decls: arena.alloc_slice_copy(&[decl]),
        };

        let result = typecheck_program(&program);
        assert!(result.is_ok());

        let ctx = result.unwrap();
        assert_eq!(ctx.get_expr_type(left.id), Some(CheckedType::I64));
        assert_eq!(ctx.get_expr_type(right.id), Some(CheckedType::I64));
        assert_eq!(ctx.get_expr_type(expr.id), Some(CheckedType::I64));
    }

    #[test]
    fn test_function_call() {
        reset_node_ids();
        let arena = Bump::new();
        let info = make_test_info(&arena, "add(1, 2)");

        // fn add(a: I64, b: I64) -> I64: return a + b
        let param_a = FnParam {
            name: "a",
            ty: Type::I64,
            info,
            id: new_node_id(),
        };
        let param_b = FnParam {
            name: "b",
            ty: Type::I64,
            info,
            id: new_node_id(),
        };

        let var_a = arena.alloc(Expr {
            info,
            kind: ExprKind::Var("a"),
            id: new_node_id(),
        });
        let var_b = arena.alloc(Expr {
            info,
            kind: ExprKind::Var("b"),
            id: new_node_id(),
        });
        let add_expr = arena.alloc(Expr {
            info,
            kind: ExprKind::BinOp {
                op: BinOp::Add,
                left: var_a,
                right: var_b,
            },
            id: new_node_id(),
        });
        let return_stmt = Stmt {
            info,
            kind: StmtKind::Return { expr: add_expr },
            id: new_node_id(),
        };

        let fn_decl = Decl {
            info,
            kind: DeclKind::Fn {
                name: "add",
                params: arena.alloc_slice_copy(&[param_a, param_b]),
                return_type: Type::I64,
                body: arena.alloc_slice_copy(&[return_stmt]),
            },
            id: new_node_id(),
        };

        // test: assert(add(1, 2) == 3)
        let arg1 = arena.alloc(Expr {
            info,
            kind: ExprKind::Int(1),
            id: new_node_id(),
        });
        let arg2 = arena.alloc(Expr {
            info,
            kind: ExprKind::Int(2),
            id: new_node_id(),
        });
        let call_expr = arena.alloc(Expr {
            info,
            kind: ExprKind::Call {
                name: "add",
                args: arena.alloc_slice_copy(&[*arg1, *arg2]),
            },
            id: new_node_id(),
        });
        let three = arena.alloc(Expr {
            info,
            kind: ExprKind::Int(3),
            id: new_node_id(),
        });
        let cmp_expr = arena.alloc(Expr {
            info,
            kind: ExprKind::Cmp {
                op: CmpOp::Eq,
                left: call_expr,
                right: three,
            },
            id: new_node_id(),
        });
        let assert_stmt = Stmt {
            info,
            kind: StmtKind::Assert { expr: cmp_expr },
            id: new_node_id(),
        };
        let test_decl = Decl {
            info,
            kind: DeclKind::Test {
                name: "test",
                body: arena.alloc_slice_copy(&[assert_stmt]),
            },
            id: new_node_id(),
        };

        let program = Program {
            decls: arena.alloc_slice_copy(&[fn_decl, test_decl]),
        };

        let result = typecheck_program(&program);
        assert!(result.is_ok(), "errors: {:?}", result.err());

        let ctx = result.unwrap();
        assert_eq!(ctx.get_expr_type(call_expr.id), Some(CheckedType::I64));
        assert_eq!(ctx.get_expr_type(cmp_expr.id), Some(CheckedType::Bool));
    }

    #[test]
    fn test_undefined_variable() {
        reset_node_ids();
        let arena = Bump::new();
        let info = make_test_info(&arena, "x");

        let var_expr = arena.alloc(Expr {
            info,
            kind: ExprKind::Var("x"),
            id: new_node_id(),
        });
        let stmt = Stmt {
            info,
            kind: StmtKind::Assert { expr: var_expr },
            id: new_node_id(),
        };
        let decl = Decl {
            info,
            kind: DeclKind::Test {
                name: "test",
                body: arena.alloc_slice_copy(&[stmt]),
            },
            id: new_node_id(),
        };
        let program = Program {
            decls: arena.alloc_slice_copy(&[decl]),
        };

        let result = typecheck_program(&program);
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert_eq!(errors.len(), 1);
        assert!(errors[0].message.contains("undefined variable"));
    }

    #[test]
    fn test_undefined_function() {
        reset_node_ids();
        let arena = Bump::new();
        let info = make_test_info(&arena, "foo()");

        let call_expr = arena.alloc(Expr {
            info,
            kind: ExprKind::Call {
                name: "foo",
                args: &[],
            },
            id: new_node_id(),
        });
        let stmt = Stmt {
            info,
            kind: StmtKind::Assert { expr: call_expr },
            id: new_node_id(),
        };
        let decl = Decl {
            info,
            kind: DeclKind::Test {
                name: "test",
                body: arena.alloc_slice_copy(&[stmt]),
            },
            id: new_node_id(),
        };
        let program = Program {
            decls: arena.alloc_slice_copy(&[decl]),
        };

        let result = typecheck_program(&program);
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert_eq!(errors.len(), 1);
        assert!(errors[0].message.contains("undefined function"));
    }

    #[test]
    fn test_wrong_argument_count() {
        reset_node_ids();
        let arena = Bump::new();
        let info = make_test_info(&arena, "add(1)");

        // fn add(a: I64, b: I64) -> I64
        let param_a = FnParam {
            name: "a",
            ty: Type::I64,
            info,
            id: new_node_id(),
        };
        let param_b = FnParam {
            name: "b",
            ty: Type::I64,
            info,
            id: new_node_id(),
        };
        let var_a = arena.alloc(Expr {
            info,
            kind: ExprKind::Var("a"),
            id: new_node_id(),
        });
        let return_stmt = Stmt {
            info,
            kind: StmtKind::Return { expr: var_a },
            id: new_node_id(),
        };
        let fn_decl = Decl {
            info,
            kind: DeclKind::Fn {
                name: "add",
                params: arena.alloc_slice_copy(&[param_a, param_b]),
                return_type: Type::I64,
                body: arena.alloc_slice_copy(&[return_stmt]),
            },
            id: new_node_id(),
        };

        // Call with wrong number of args
        let arg1 = arena.alloc(Expr {
            info,
            kind: ExprKind::Int(1),
            id: new_node_id(),
        });
        let call_expr = arena.alloc(Expr {
            info,
            kind: ExprKind::Call {
                name: "add",
                args: arena.alloc_slice_copy(&[*arg1]),
            },
            id: new_node_id(),
        });
        let stmt = Stmt {
            info,
            kind: StmtKind::Assert { expr: call_expr },
            id: new_node_id(),
        };
        let test_decl = Decl {
            info,
            kind: DeclKind::Test {
                name: "test",
                body: arena.alloc_slice_copy(&[stmt]),
            },
            id: new_node_id(),
        };

        let program = Program {
            decls: arena.alloc_slice_copy(&[fn_decl, test_decl]),
        };

        let result = typecheck_program(&program);
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(errors[0].message.contains("expects 2 arguments, got 1"));
    }

    #[test]
    fn test_return_type_mismatch() {
        reset_node_ids();
        let arena = Bump::new();
        let info = make_test_info(&arena, "return 1 == 2");

        // fn foo() -> I64: return 1 == 2  (returns Bool, not I64)
        let one = arena.alloc(Expr {
            info,
            kind: ExprKind::Int(1),
            id: new_node_id(),
        });
        let two = arena.alloc(Expr {
            info,
            kind: ExprKind::Int(2),
            id: new_node_id(),
        });
        let cmp = arena.alloc(Expr {
            info,
            kind: ExprKind::Cmp {
                op: CmpOp::Eq,
                left: one,
                right: two,
            },
            id: new_node_id(),
        });
        let return_stmt = Stmt {
            info,
            kind: StmtKind::Return { expr: cmp },
            id: new_node_id(),
        };
        let fn_decl = Decl {
            info,
            kind: DeclKind::Fn {
                name: "foo",
                params: &[],
                return_type: Type::I64,
                body: arena.alloc_slice_copy(&[return_stmt]),
            },
            id: new_node_id(),
        };

        let program = Program {
            decls: arena.alloc_slice_copy(&[fn_decl]),
        };

        let result = typecheck_program(&program);
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(errors[0].message.contains("return type mismatch"));
    }

    #[test]
    fn test_compilation_context() {
        let mut ctx = CompilationContext::new();

        // Test expr types
        ctx.set_expr_type(1, CheckedType::I64);
        assert_eq!(ctx.get_expr_type(1), Some(CheckedType::I64));
        assert_eq!(ctx.get_expr_type(2), None);

        // Test function signatures
        ctx.register_fn(
            "add".to_string(),
            FnSignature {
                params: vec![CheckedType::I64, CheckedType::I64],
                return_type: CheckedType::I64,
            },
        );
        let sig = ctx.get_fn_signature("add").unwrap();
        assert_eq!(sig.params.len(), 2);
        assert_eq!(sig.return_type, CheckedType::I64);
        assert!(ctx.get_fn_signature("foo").is_none());

        // Test variable types
        ctx.set_var_type("x".to_string(), CheckedType::I64);
        assert_eq!(ctx.get_var_type("x"), Some(CheckedType::I64));
        ctx.clear_vars();
        assert_eq!(ctx.get_var_type("x"), None);

        // Test errors
        assert!(!ctx.has_errors());
        let arena = Bump::new();
        let info = make_test_info(&arena, "test");
        ctx.add_error("test error".to_string(), &info);
        assert!(ctx.has_errors());
        assert_eq!(ctx.errors().len(), 1);

        let errors = ctx.take_errors();
        assert_eq!(errors.len(), 1);
        assert!(!ctx.has_errors());
    }

    #[test]
    fn test_type_error_display() {
        let err = TypeError {
            message: "test error".to_string(),
            line: 10,
            col: 5,
        };
        assert_eq!(format!("{}", err), "10:5: test error");
    }

    #[test]
    fn test_checked_type_from_type() {
        assert_eq!(CheckedType::from(Type::I64), CheckedType::I64);
    }

    #[test]
    fn test_paren_expression() {
        reset_node_ids();
        let arena = Bump::new();
        let info = make_test_info(&arena, "(1)");

        let inner = arena.alloc(Expr {
            info,
            kind: ExprKind::Int(1),
            id: new_node_id(),
        });
        let paren = arena.alloc(Expr {
            info,
            kind: ExprKind::Paren(inner),
            id: new_node_id(),
        });
        let stmt = Stmt {
            info,
            kind: StmtKind::Assert { expr: paren },
            id: new_node_id(),
        };
        let decl = Decl {
            info,
            kind: DeclKind::Test {
                name: "test",
                body: arena.alloc_slice_copy(&[stmt]),
            },
            id: new_node_id(),
        };
        let program = Program {
            decls: arena.alloc_slice_copy(&[decl]),
        };

        let result = typecheck_program(&program);
        assert!(result.is_ok());
        let ctx = result.unwrap();
        assert_eq!(ctx.get_expr_type(paren.id), Some(CheckedType::I64));
    }

    #[test]
    fn test_full_pipeline_valid() {
        use crate::{lower_program, parse_with_prelude};

        let source = r#"
fn add(a: I64, b: I64) -> I64:
  return a + b

test addition:
  assert(add(2, 3) == 5)
"#;

        let arena = Bump::new();
        let parse_result = parse_with_prelude(&arena, source);
        assert!(parse_result.errors.is_empty(), "parse errors: {:?}", parse_result.errors);

        let program = lower_program(&arena, &parse_result.nodes).expect("lower error");
        let result = typecheck_program(&program);
        assert!(result.is_ok(), "type errors: {:?}", result.err());
    }

    #[test]
    fn test_full_pipeline_wrong_arg_count() {
        use crate::{lower_program, parse_with_prelude};

        let source = r#"
fn add(a: I64, b: I64) -> I64:
  return a + b

test wrong args:
  assert(add(1) == 2)
"#;

        let arena = Bump::new();
        let parse_result = parse_with_prelude(&arena, source);
        assert!(parse_result.errors.is_empty(), "parse errors: {:?}", parse_result.errors);

        let program = lower_program(&arena, &parse_result.nodes).expect("lower error");
        let result = typecheck_program(&program);
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(errors[0].message.contains("expects 2 arguments, got 1"));
    }

    #[test]
    fn test_full_pipeline_undefined_function() {
        use crate::{lower_program, parse_with_prelude};

        let source = r#"
test undefined fn:
  assert(foo() == 1)
"#;

        let arena = Bump::new();
        let parse_result = parse_with_prelude(&arena, source);
        assert!(parse_result.errors.is_empty(), "parse errors: {:?}", parse_result.errors);

        let program = lower_program(&arena, &parse_result.nodes).expect("lower error");
        let result = typecheck_program(&program);
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(errors[0].message.contains("undefined function: foo"));
    }

    #[test]
    fn test_full_pipeline_undefined_variable() {
        use crate::{lower_program, parse_with_prelude};

        let source = r#"
test undefined var:
  assert(x == 1)
"#;

        let arena = Bump::new();
        let parse_result = parse_with_prelude(&arena, source);
        assert!(parse_result.errors.is_empty(), "parse errors: {:?}", parse_result.errors);

        let program = lower_program(&arena, &parse_result.nodes).expect("lower error");
        let result = typecheck_program(&program);
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(errors[0].message.contains("undefined variable: x"));
    }
}

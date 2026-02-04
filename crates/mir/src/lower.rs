//! Lowering from BaseLang AST to MIR

use baselang::{BinOp as AstBinOp, CmpOp as AstCmpOp, DeclKind, Expr, ExprKind, Program, SourceInfo, Stmt, StmtKind};

use crate::ir::{AssertInfo, BinOp, CmpOp, FunctionDebugInfo, InstKind, MirFunction, MirProgram, Reg, SourceSpan};

/// Convert a SourceInfo to an owned SourceSpan
fn to_source_span(info: &SourceInfo) -> SourceSpan {
    // For now, we use the same line/col for end since SourceInfo doesn't track end position
    // TODO: Add proper end position tracking when needed
    SourceSpan {
        source_id: info.module.id.to_owned(),
        line: info.line(),
        col: info.col(),
        end_line: info.line(),
        end_col: info.col(),
    }
}

/// Lower a BaseLang Program to MIR
pub fn lower_program(program: &Program) -> MirProgram {
    let mut mir = MirProgram::default();

    for decl in program.decls {
        match &decl.kind {
            DeclKind::Test { name, body } => {
                let mut func = MirFunction::new((*name).to_owned());
                // Set debug info from declaration
                func.debug_info = FunctionDebugInfo {
                    span: Some(to_source_span(&decl.info)),
                    source_id: Some(decl.info.module.id.to_owned()),
                };
                for stmt in *body {
                    lower_stmt(&mut func, stmt);
                }
                func.emit(InstKind::Ret);
                mir.functions.push(func);
            }
            DeclKind::Fn { name, body } => {
                let mut func = MirFunction::new_function((*name).to_owned());
                // Set debug info from declaration
                func.debug_info = FunctionDebugInfo {
                    span: Some(to_source_span(&decl.info)),
                    source_id: Some(decl.info.module.id.to_owned()),
                };
                for stmt in *body {
                    lower_stmt(&mut func, stmt);
                }
                func.emit(InstKind::Ret);
                mir.functions.push(func);
            }
        }
    }

    mir
}

fn lower_stmt(func: &mut MirFunction, stmt: &Stmt) {
    match &stmt.kind {
        StmtKind::Assert { expr } => {
            let cond = lower_expr(func, expr);
            // Convert BaseLang SourceInfo to MIR AssertInfo
            let assert_info = AssertInfo {
                line: stmt.info.line(),
                col: stmt.info.col(),
                source: stmt.info.text().to_owned(),
            };
            let span = to_source_span(&stmt.info);
            func.emit_assert_with_span(cond, assert_info, span);
        }
    }
}

fn lower_expr(func: &mut MirFunction, expr: &Expr) -> Reg {
    let span = to_source_span(&expr.info);
    match &expr.kind {
        ExprKind::Int(value) => {
            let dst = func.alloc_reg();
            func.emit_with_span(InstKind::LoadImm { dst, value: *value }, span);
            dst
        }
        ExprKind::BinOp { op, left, right } => {
            let left_reg = lower_expr(func, left);
            let right_reg = lower_expr(func, right);
            let dst = func.alloc_reg();
            let mir_op = match op {
                AstBinOp::Add => BinOp::Add,
                AstBinOp::Sub => BinOp::Sub,
                AstBinOp::Mul => BinOp::Mul,
                AstBinOp::Div => BinOp::Div,
            };
            func.emit_with_span(InstKind::BinOp { op: mir_op, dst, left: left_reg, right: right_reg }, span);
            dst
        }
        ExprKind::Cmp { op, left, right } => {
            let left_reg = lower_expr(func, left);
            let right_reg = lower_expr(func, right);
            let dst = func.alloc_reg();
            let mir_op = match op {
                AstCmpOp::Eq => CmpOp::Eq,
                AstCmpOp::Neq => CmpOp::Neq,
                AstCmpOp::Lt => CmpOp::Lt,
                AstCmpOp::Lte => CmpOp::Lte,
                AstCmpOp::Gt => CmpOp::Gt,
                AstCmpOp::Gte => CmpOp::Gte,
            };
            func.emit_with_span(InstKind::Cmp { op: mir_op, dst, left: left_reg, right: right_reg }, span);
            dst
        }
        ExprKind::Paren(inner) => lower_expr(func, inner),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use baselang::{Decl, DeclKind, Expr, ExprKind, Program, SourceInfo, Stmt, StmtKind};
    use bumpalo::Bump;
    use common::{SourceLoc, SourceModule};
    use crate::ir::FunctionKind;

    /// Helper to create a SourceInfo for tests
    fn make_test_info<'a>(arena: &'a Bump, text: &'a str) -> SourceInfo<'a> {
        let module = arena.alloc(SourceModule::synthetic(text, "<test>"));
        SourceInfo::new(module, SourceLoc::new(0, 1, 1), text.len() as u32)
    }

    #[test]
    fn test_lower_simple() {
        let arena = Bump::new();

        let info = make_test_info(&arena, "1 == 1");
        let left = arena.alloc(Expr { info, kind: ExprKind::Int(1) });
        let right = arena.alloc(Expr { info, kind: ExprKind::Int(1) });
        let cmp_expr = arena.alloc(Expr {
            info,
            kind: ExprKind::Cmp {
                op: AstCmpOp::Eq,
                left,
                right,
            },
        });

        let stmt = Stmt {
            info,
            kind: StmtKind::Assert { expr: cmp_expr },
        };

        let decl = Decl {
            info,
            kind: DeclKind::Test {
                name: "simple",
                body: arena.alloc_slice_copy(&[stmt]),
            },
        };

        let program = Program {
            decls: arena.alloc_slice_copy(&[decl]),
        };

        let mir = lower_program(&program);
        assert_eq!(mir.functions.len(), 1);
        assert_eq!(mir.functions[0].name, "simple");
        // LoadImm(1), LoadImm(1), Cmp, Assert, Ret
        assert_eq!(mir.functions[0].instructions.len(), 5);
        // Verify assert info is stored
        assert_eq!(mir.functions[0].asserts.len(), 1);
        assert_eq!(mir.functions[0].asserts[0].source, "1 == 1");
    }

    #[test]
    fn test_lower_fn_decl() {
        let arena = Bump::new();

        let info = make_test_info(&arena, "1 == 1");
        let left = arena.alloc(Expr { info, kind: ExprKind::Int(1) });
        let right = arena.alloc(Expr { info, kind: ExprKind::Int(1) });
        let cmp_expr = arena.alloc(Expr {
            info,
            kind: ExprKind::Cmp {
                op: AstCmpOp::Eq,
                left,
                right,
            },
        });

        let stmt = Stmt {
            info,
            kind: StmtKind::Assert { expr: cmp_expr },
        };

        let fn_info = make_test_info(&arena, "fn main():");
        let decl = Decl {
            info: fn_info,
            kind: DeclKind::Fn {
                name: "main",
                body: arena.alloc_slice_copy(&[stmt]),
            },
        };

        let program = Program {
            decls: arena.alloc_slice_copy(&[decl]),
        };

        let mir = lower_program(&program);
        assert_eq!(mir.functions.len(), 1);
        assert_eq!(mir.functions[0].name, "main");
        assert_eq!(mir.functions[0].kind, FunctionKind::Function);
        assert_eq!(mir.functions[0].instructions.len(), 5);
    }

    #[test]
    fn test_lower_test_and_fn() {
        let arena = Bump::new();

        let test_info = make_test_info(&arena, "1");
        let test_expr = arena.alloc(Expr { info: test_info, kind: ExprKind::Int(1) });
        let test_stmt = Stmt {
            info: test_info,
            kind: StmtKind::Assert { expr: test_expr },
        };
        let test_decl = Decl {
            info: test_info,
            kind: DeclKind::Test {
                name: "test1",
                body: arena.alloc_slice_copy(&[test_stmt]),
            },
        };

        let fn_info = make_test_info(&arena, "fn main():");
        let fn_expr = arena.alloc(Expr { info: fn_info, kind: ExprKind::Int(1) });
        let fn_stmt = Stmt {
            info: fn_info,
            kind: StmtKind::Assert { expr: fn_expr },
        };
        let fn_decl = Decl {
            info: fn_info,
            kind: DeclKind::Fn {
                name: "main",
                body: arena.alloc_slice_copy(&[fn_stmt]),
            },
        };

        let program = Program {
            decls: arena.alloc_slice_copy(&[test_decl, fn_decl]),
        };

        let mir = lower_program(&program);
        assert_eq!(mir.functions.len(), 2);
        // Order is preserved from decls
        assert_eq!(mir.functions[0].kind, FunctionKind::Test);
        assert_eq!(mir.functions[1].kind, FunctionKind::Function);
    }
}

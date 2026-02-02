//! Lowering from BaseLang AST to MIR

use baselang::{BinOp as AstBinOp, CmpOp as AstCmpOp, Expr, Program, Stmt};

use crate::ir::{AssertInfo, BinOp, CmpOp, Inst, MirFunction, MirProgram, Reg};

/// Lower a BaseLang Program to MIR
pub fn lower_program(program: &Program) -> MirProgram {
    let mut mir = MirProgram::default();

    // Lower test declarations
    for test in &program.tests {
        let mut func = MirFunction::new(test.name.clone());

        for stmt in &test.body {
            lower_stmt(&mut func, stmt);
        }

        func.emit(Inst::Ret);
        mir.functions.push(func);
    }

    // Lower function declarations
    for fn_decl in &program.functions {
        let mut func = MirFunction::new_function(fn_decl.name.clone());

        for stmt in &fn_decl.body {
            lower_stmt(&mut func, stmt);
        }

        func.emit(Inst::Ret);
        mir.functions.push(func);
    }

    mir
}

fn lower_stmt(func: &mut MirFunction, stmt: &Stmt) {
    match stmt {
        Stmt::Assert { expr, info } => {
            let cond = lower_expr(func, expr);
            // Convert BaseLang SourceInfo to MIR AssertInfo
            let assert_info = AssertInfo {
                line: info.line,
                col: info.col,
                source: info.source.clone(),
            };
            func.emit_assert(cond, assert_info);
        }
    }
}

fn lower_expr(func: &mut MirFunction, expr: &Expr) -> Reg {
    match expr {
        Expr::Int(value) => {
            let dst = func.alloc_reg();
            func.emit(Inst::LoadImm { dst, value: *value });
            dst
        }
        Expr::BinOp { op, left, right } => {
            let left_reg = lower_expr(func, left);
            let right_reg = lower_expr(func, right);
            let dst = func.alloc_reg();
            let mir_op = match op {
                AstBinOp::Add => BinOp::Add,
                AstBinOp::Sub => BinOp::Sub,
                AstBinOp::Mul => BinOp::Mul,
                AstBinOp::Div => BinOp::Div,
            };
            func.emit(Inst::BinOp { op: mir_op, dst, left: left_reg, right: right_reg });
            dst
        }
        Expr::Cmp { op, left, right } => {
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
            func.emit(Inst::Cmp { op: mir_op, dst, left: left_reg, right: right_reg });
            dst
        }
        Expr::Paren(inner) => lower_expr(func, inner),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use baselang::{Expr, SourceInfo, Stmt, TestDecl, Program};

    #[test]
    fn test_lower_simple() {
        let program = Program {
            tests: vec![TestDecl {
                name: "simple".to_string(),
                body: vec![Stmt::Assert {
                    expr: Expr::Cmp {
                        op: AstCmpOp::Eq,
                        left: Box::new(Expr::Int(1)),
                        right: Box::new(Expr::Int(1)),
                    },
                    info: SourceInfo {
                        line: 1,
                        col: 1,
                        source: "1 == 1".to_string(),
                    },
                }],
            }],
            functions: vec![],
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
        use baselang::{FnDecl, SourceInfo};
        use crate::ir::FunctionKind;

        let program = Program {
            tests: vec![],
            functions: vec![FnDecl {
                name: "main".to_string(),
                body: vec![Stmt::Assert {
                    expr: Expr::Cmp {
                        op: AstCmpOp::Eq,
                        left: Box::new(Expr::Int(1)),
                        right: Box::new(Expr::Int(1)),
                    },
                    info: SourceInfo {
                        line: 1,
                        col: 1,
                        source: "1 == 1".to_string(),
                    },
                }],
                info: SourceInfo {
                    line: 1,
                    col: 1,
                    source: "fn main():".to_string(),
                },
            }],
        };

        let mir = lower_program(&program);
        assert_eq!(mir.functions.len(), 1);
        assert_eq!(mir.functions[0].name, "main");
        assert_eq!(mir.functions[0].kind, FunctionKind::Function);
        assert_eq!(mir.functions[0].instructions.len(), 5);
    }

    #[test]
    fn test_lower_test_and_fn() {
        use baselang::{FnDecl, SourceInfo};
        use crate::ir::FunctionKind;

        let program = Program {
            tests: vec![TestDecl {
                name: "test1".to_string(),
                body: vec![Stmt::Assert {
                    expr: Expr::Int(1),
                    info: SourceInfo {
                        line: 1,
                        col: 1,
                        source: "1".to_string(),
                    },
                }],
            }],
            functions: vec![FnDecl {
                name: "main".to_string(),
                body: vec![Stmt::Assert {
                    expr: Expr::Int(1),
                    info: SourceInfo {
                        line: 1,
                        col: 1,
                        source: "1".to_string(),
                    },
                }],
                info: SourceInfo {
                    line: 1,
                    col: 1,
                    source: "fn main():".to_string(),
                },
            }],
        };

        let mir = lower_program(&program);
        assert_eq!(mir.functions.len(), 2);
        // Tests are lowered first
        assert_eq!(mir.functions[0].kind, FunctionKind::Test);
        assert_eq!(mir.functions[1].kind, FunctionKind::Function);
    }
}

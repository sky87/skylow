//! File-based tests for MIR compilation
//!
//! Tests parse -> lower to BaseLang -> lower to MIR pipeline.

use bumpalo::Bump;
use datatest_stable::harness;
use baselang::{lower_program as lower_to_ast, parse_with_prelude};
use mir::{lower_program, BinOp, CmpOp, Inst, InstKind, MirFunction};
use std::path::Path;

/// Format a register
fn format_reg(r: mir::Reg) -> String {
    format!("r{}", r.0)
}

/// Format an instruction
fn format_inst(inst: &Inst) -> String {
    match &inst.kind {
        InstKind::LoadImm { dst, value } => {
            format!("  {} = load {}", format_reg(*dst), value)
        }
        InstKind::Copy { dst, src } => {
            format!("  {} = copy {}", format_reg(*dst), format_reg(*src))
        }
        InstKind::BinOp { op, dst, left, right } => {
            let op_str = match op {
                BinOp::Add => "add",
                BinOp::Sub => "sub",
                BinOp::Mul => "mul",
                BinOp::Div => "div",
            };
            format!(
                "  {} = {} {}, {}",
                format_reg(*dst),
                op_str,
                format_reg(*left),
                format_reg(*right)
            )
        }
        InstKind::Cmp { op, dst, left, right } => {
            let op_str = match op {
                CmpOp::Eq => "eq",
                CmpOp::Neq => "neq",
                CmpOp::Lt => "lt",
                CmpOp::Lte => "lte",
                CmpOp::Gt => "gt",
                CmpOp::Gte => "gte",
            };
            format!(
                "  {} = {} {}, {}",
                format_reg(*dst),
                op_str,
                format_reg(*left),
                format_reg(*right)
            )
        }
        InstKind::Assert { cond, msg_id } => {
            format!("  assert {}, #{}", format_reg(*cond), msg_id)
        }
        InstKind::Call { dst, func_name, args } => {
            let args_str = args.iter().map(|r| format_reg(*r)).collect::<Vec<_>>().join(", ");
            format!("  {} = call {}({})", format_reg(*dst), func_name, args_str)
        }
        InstKind::RetVal { value } => {
            format!("  retval {}", format_reg(*value))
        }
        InstKind::Ret => "  ret".to_string(),
    }
}

/// Format a MIR function
fn format_function(func: &MirFunction) -> String {
    let mut lines = vec![format!("fn \"{}\":", func.name)];
    for inst in &func.instructions {
        lines.push(format_inst(inst));
    }
    lines.join("\n")
}

/// Format a MIR program
fn format_program(program: &mir::MirProgram) -> String {
    program
        .functions
        .iter()
        .map(format_function)
        .collect::<Vec<_>>()
        .join("\n\n")
}

fn run_test(path: &Path) -> datatest_stable::Result<()> {
    let input = std::fs::read_to_string(path)?;
    let expected_path = format!("{}.expected", path.display());
    let expected = std::fs::read_to_string(&expected_path)?.trim().to_string();

    let arena = Bump::new();
    let parse_result = parse_with_prelude(&arena, &input);

    // Check for parse errors
    if !parse_result.errors.is_empty() {
        let errors: Vec<_> = parse_result
            .errors
            .iter()
            .map(|e| format!("{}:{}: {}", e.loc.line, e.loc.col, e.msg))
            .collect();
        let actual = format!("PARSE ERRORS:\n{}", errors.join("\n"));
        if actual.trim() != expected {
            return Err(format!(
                "Mismatch for {:?}\n\nExpected:\n{}\n\nActual:\n{}",
                path, expected, actual
            )
            .into());
        }
        return Ok(());
    }

    // Lower to BaseLang AST
    let ast = match lower_to_ast(&arena, &parse_result.nodes) {
        Ok(program) => program,
        Err(e) => {
            let actual = format!("LOWER ERROR: {}", e);
            if actual.trim() != expected {
                return Err(format!(
                    "Mismatch for {:?}\n\nExpected:\n{}\n\nActual:\n{}",
                    path, expected, actual
                )
                .into());
            }
            return Ok(());
        }
    };

    // Lower to MIR
    let mir = lower_program(&ast);
    let actual = format_program(&mir);

    if actual.trim() != expected {
        return Err(format!(
            "Mismatch for {:?}\n\nExpected:\n{}\n\nActual:\n{}",
            path, expected, actual
        )
        .into());
    }

    Ok(())
}

harness!(run_test, "tests/compile", r"\.skyl$");

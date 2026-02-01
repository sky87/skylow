//! File-based tests for BaseLang lowering
//!
//! Tests parse + lower pipeline by comparing AST output against expected files.

use bumpalo::Bump;
use datatest_stable::harness;
use skylow_baselang::{lower_program, parse_with_prelude, PRELUDE};
use std::path::Path;

/// Format an expression for output
fn format_expr(expr: &skylow_baselang::Expr, indent: usize) -> String {
    use skylow_baselang::Expr;
    let pad = "  ".repeat(indent);
    match expr {
        Expr::Int(n) => format!("{}Int({})", pad, n),
        Expr::BinOp { op, left, right } => {
            format!(
                "{}BinOp({:?})\n{}\n{}",
                pad,
                op,
                format_expr(left, indent + 1),
                format_expr(right, indent + 1)
            )
        }
        Expr::Cmp { op, left, right } => {
            format!(
                "{}Cmp({:?})\n{}\n{}",
                pad,
                op,
                format_expr(left, indent + 1),
                format_expr(right, indent + 1)
            )
        }
        Expr::Paren(inner) => {
            format!("{}Paren\n{}", pad, format_expr(inner, indent + 1))
        }
    }
}

/// Format a statement for output
fn format_stmt(stmt: &skylow_baselang::Stmt, indent: usize) -> String {
    use skylow_baselang::Stmt;
    let pad = "  ".repeat(indent);
    match stmt {
        Stmt::Assert { expr, .. } => {
            format!("{}Assert\n{}", pad, format_expr(expr, indent + 1))
        }
    }
}

/// Format a test declaration for output
fn format_test(test: &skylow_baselang::TestDecl) -> String {
    let mut lines = vec![format!("Test \"{}\"", test.name)];
    for stmt in &test.body {
        lines.push(format_stmt(stmt, 1));
    }
    lines.join("\n")
}

/// Format a program for output
fn format_program(program: &skylow_baselang::Program) -> String {
    program.tests.iter().map(format_test).collect::<Vec<_>>().join("\n\n")
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
    let combined = format!("{}\n{}", PRELUDE, input);
    let prelude_lines = PRELUDE.lines().count() as u32 + 1; // +1 for newline between prelude and source
    let actual = match lower_program(&parse_result.nodes, &combined, prelude_lines) {
        Ok(program) => format_program(&program),
        Err(e) => format!("LOWER ERROR: {}", e),
    };

    if actual.trim() != expected {
        return Err(format!(
            "Mismatch for {:?}\n\nExpected:\n{}\n\nActual:\n{}",
            path, expected, actual
        )
        .into());
    }

    Ok(())
}

harness!(run_test, "tests/lower", r"\.skyl$");

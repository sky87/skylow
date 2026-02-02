//! File-based tests for JIT execution
//!
//! Tests the full pipeline: parse -> BaseLang -> MIR -> JIT compile -> execute.

use bumpalo::Bump;
use datatest_stable::harness;
use baselang::{lower_program as lower_to_ast, parse_with_prelude, PRELUDE};
use jit::{compile_function, ExecutableMemory};
use mir::lower_program as lower_to_mir;
use std::path::Path;

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
    let combined_ref = arena.alloc_str(&combined);
    let prelude_lines = PRELUDE.lines().count() as u32 + 1; // +1 for newline between prelude and source
    let ast = match lower_to_ast(&arena, &parse_result.nodes, combined_ref, prelude_lines) {
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
    let mir = lower_to_mir(&ast);

    // Compile and execute each test
    let mut results = Vec::new();
    for func in &mir.functions {
        let compiled = compile_function(func);
        match ExecutableMemory::new(&compiled.code) {
            Ok(mem) => {
                let test_fn: extern "C" fn() -> u8 = unsafe { mem.as_fn() };
                let ret = test_fn();
                if ret == 0 {
                    results.push(format!("PASS: {}", func.name));
                } else {
                    // ret is 1-based assert index (0 = success)
                    let assert_idx = (ret - 1) as usize;
                    let failure_message = if let Some(info) = func.asserts.get(assert_idx) {
                        format!(
                            "FAIL: {} - assertion failed at line {}:{}\n  assert({})",
                            func.name, info.line, info.col, info.source
                        )
                    } else {
                        format!("FAIL: {} (assertion {} failed)", func.name, ret)
                    };
                    results.push(failure_message);
                }
            }
            Err(e) => {
                results.push(format!("ERROR: {} ({})", func.name, e));
            }
        }
    }

    let actual = results.join("\n");

    if actual.trim() != expected {
        return Err(format!(
            "Mismatch for {:?}\n\nExpected:\n{}\n\nActual:\n{}",
            path, expected, actual
        )
        .into());
    }

    Ok(())
}

harness!(run_test, "tests/execute", r"\.skyl$");

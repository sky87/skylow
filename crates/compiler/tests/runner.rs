//! File-based integration tests for the test runner
//!
//! Tests the full pipeline: parse -> lower -> MIR -> JIT -> execute.

use datatest_stable::harness;
use compiler::TestRunner;
use std::fs;
use std::path::Path;

fn run_test(path: &Path) -> datatest_stable::Result<()> {
    let source = fs::read_to_string(path)?;
    let expected_path = format!("{}.expected", path.display());
    let expected = fs::read_to_string(&expected_path)?.trim().to_string();

    let runner = TestRunner::new();
    let result = runner.run(&source);

    // Format output
    let mut output_lines = Vec::new();

    // Report parse errors
    if !result.parse_errors.is_empty() {
        output_lines.push("PARSE ERRORS:".to_string());
        for err in &result.parse_errors {
            output_lines.push(format!("  {}", err));
        }
    }

    // Report lowering errors
    if !result.lower_errors.is_empty() {
        output_lines.push("LOWER ERRORS:".to_string());
        for err in &result.lower_errors {
            output_lines.push(format!("  {}", err));
        }
    }

    // Report test results
    for test in &result.results {
        if test.passed {
            output_lines.push(format!("PASS: {}", test.name));
        } else {
            let msg = test.failure_message.as_deref().unwrap_or("unknown");
            output_lines.push(format!("FAIL: {} ({})", test.name, msg));
        }
    }

    let actual = output_lines.join("\n");

    if actual.trim() != expected {
        return Err(format!(
            "Mismatch for {:?}\n\nExpected:\n{}\n\nActual:\n{}",
            path, expected, actual
        )
        .into());
    }

    Ok(())
}

harness!(run_test, "tests/e2e", r"\.skyl$");

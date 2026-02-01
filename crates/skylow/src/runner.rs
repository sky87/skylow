//! Test runner for SkyLow
//!
//! Orchestrates the full pipeline: parse -> lower to BaseLang -> lower to MIR -> JIT compile -> execute.

use bumpalo::Bump;
use skylow_baselang::{lower_program, parse_with_prelude, PRELUDE};
use skylow_jit::{compile_function, ExecutableMemory};
use skylow_mir::lower_program as lower_to_mir;

/// Result of running a single test
#[derive(Debug, Clone)]
pub struct TestResult {
    /// Test name
    pub name: String,
    /// Whether the test passed
    pub passed: bool,
    /// Failure message (if any)
    pub failure_message: Option<String>,
}

/// Result of running all tests in a file
#[derive(Debug)]
pub struct RunResult {
    /// Individual test results
    pub results: Vec<TestResult>,
    /// Parse errors (if any)
    pub parse_errors: Vec<String>,
    /// Lowering errors (if any)
    pub lower_errors: Vec<String>,
}

impl RunResult {
    /// Count of passed tests
    pub fn passed(&self) -> usize {
        self.results.iter().filter(|r| r.passed).count()
    }

    /// Count of failed tests
    pub fn failed(&self) -> usize {
        self.results.iter().filter(|r| !r.passed).count()
    }

    /// Whether all tests passed and there were no errors
    pub fn success(&self) -> bool {
        self.parse_errors.is_empty()
            && self.lower_errors.is_empty()
            && self.results.iter().all(|r| r.passed)
    }
}

/// Test runner that compiles and executes SkyLow tests
pub struct TestRunner {
    _private: (),
}

impl TestRunner {
    /// Create a new test runner
    pub fn new() -> Self {
        Self { _private: () }
    }

    /// Run all tests in a source file
    pub fn run(&self, source: &str) -> RunResult {
        let arena = Bump::new();

        // Parse with prelude
        let parse_result = parse_with_prelude(&arena, source);

        if !parse_result.errors.is_empty() {
            return RunResult {
                results: vec![],
                parse_errors: parse_result
                    .errors
                    .iter()
                    .map(|e| format!("{}:{}: {}", e.loc.line, e.loc.col, e.msg))
                    .collect(),
                lower_errors: vec![],
            };
        }

        // Lower to BaseLang AST
        let combined = format!("{}\n{}", PRELUDE, source);
        let program = match lower_program(&parse_result.nodes, &combined) {
            Ok(p) => p,
            Err(e) => {
                return RunResult {
                    results: vec![],
                    parse_errors: vec![],
                    lower_errors: vec![e.to_string()],
                };
            }
        };

        // Lower to MIR
        let mir_program = lower_to_mir(&program);

        // Compile and run each test
        let mut results = Vec::new();
        for mir_func in &mir_program.functions {
            let compiled = compile_function(mir_func);
            let result = match ExecutableMemory::new(&compiled.code) {
                Ok(mem) => {
                    let test_fn: extern "C" fn() -> u8 = unsafe { mem.as_fn() };
                    let ret = test_fn();
                    if ret == 0 {
                        TestResult {
                            name: mir_func.name.clone(),
                            passed: true,
                            failure_message: None,
                        }
                    } else {
                        TestResult {
                            name: mir_func.name.clone(),
                            passed: false,
                            failure_message: Some(format!("assertion {} failed", ret)),
                        }
                    }
                }
                Err(e) => TestResult {
                    name: mir_func.name.clone(),
                    passed: false,
                    failure_message: Some(format!("JIT compilation failed: {}", e)),
                },
            };
            results.push(result);
        }

        RunResult {
            results,
            parse_errors: vec![],
            lower_errors: vec![],
        }
    }
}

impl Default for TestRunner {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_runner_simple() {
        let runner = TestRunner::new();
        let source = r#"
test simple:
  assert(1 == 1)
"#;
        let result = runner.run(source);
        assert!(result.parse_errors.is_empty());
        assert!(result.lower_errors.is_empty());
        assert_eq!(result.results.len(), 1);
        assert!(result.results[0].passed);
        // Test the result methods
        assert_eq!(result.passed(), 1);
        assert_eq!(result.failed(), 0);
        assert!(result.success());
    }

    #[test]
    fn test_runner_default() {
        let runner = TestRunner::default();
        let source = r#"
test default:
  assert(1 == 1)
"#;
        let result = runner.run(source);
        assert!(result.success());
    }

    #[test]
    fn test_runner_arithmetic() {
        let runner = TestRunner::new();
        // Note: Due to a parser bug with precedence, we need explicit parentheses
        // for expressions where a lower-precedence operator follows a higher one
        let source = r#"
test arithmetic works as expected:
  assert((2 + 2) == 4)
  assert((2 + (3*2)) == 8)
  assert(((1 + 2) * (3 + 4)) == 21)
  assert(((10 / 2) - 3) == 2)
"#;
        let result = runner.run(source);
        assert!(result.parse_errors.is_empty(), "parse errors: {:?}", result.parse_errors);
        assert!(result.lower_errors.is_empty(), "lower errors: {:?}", result.lower_errors);
        assert_eq!(result.results.len(), 1);
        assert!(result.results[0].passed, "test failed: {:?}", result.results[0].failure_message);
    }

    #[test]
    fn test_runner_failing_test() {
        let runner = TestRunner::new();
        let source = r#"
test failing:
  assert(1 == 2)
"#;
        let result = runner.run(source);
        assert!(result.parse_errors.is_empty());
        assert!(result.lower_errors.is_empty());
        assert_eq!(result.results.len(), 1);
        assert!(!result.results[0].passed);
        // Test result methods for failing test
        assert_eq!(result.passed(), 0);
        assert_eq!(result.failed(), 1);
        assert!(!result.success());
    }

    #[test]
    fn test_runner_multiple_tests() {
        let runner = TestRunner::new();
        let source = r#"
test first:
  assert(1 == 1)

test second:
  assert(2 == 2)
"#;
        let result = runner.run(source);
        assert!(result.parse_errors.is_empty());
        assert!(result.lower_errors.is_empty());
        assert_eq!(result.results.len(), 2);
        assert!(result.results.iter().all(|r| r.passed));
    }
}

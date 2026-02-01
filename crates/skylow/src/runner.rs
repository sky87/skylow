//! Test runner for SkyLow
//!
//! Orchestrates the full pipeline: parse -> lower to BaseLang -> lower to MIR -> JIT compile -> execute.

use bumpalo::Bump;
use skylow_baselang::{lower_program, parse_with_prelude, Program, PRELUDE};
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
        // +1 for the newline between prelude and source
        let prelude_lines = PRELUDE.lines().count() as u32 + 1;
        let program = match lower_program(&parse_result.nodes, &combined, prelude_lines) {
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
                        // ret is 1-based assert index (0 = success)
                        let assert_idx = (ret - 1) as usize;
                        let failure_message = if let Some(info) = mir_func.asserts.get(assert_idx) {
                            format!(
                                "assertion failed at line {}:{}\n  assert({})",
                                info.line, info.col, info.source
                            )
                        } else {
                            format!("assertion {} failed", ret)
                        };
                        TestResult {
                            name: mir_func.name.clone(),
                            passed: false,
                            failure_message: Some(failure_message),
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

/// Result of running a main function
#[derive(Debug)]
pub struct MainResult {
    /// Exit code (0 for success, 1 for assertion failure)
    pub exit_code: u8,
    /// Failure message if assertion failed
    pub failure_message: Option<String>,
    /// Parse errors (if any)
    pub parse_errors: Vec<String>,
    /// Lowering errors (if any)
    pub lower_errors: Vec<String>,
}

impl MainResult {
    /// Whether the main function succeeded
    pub fn success(&self) -> bool {
        self.parse_errors.is_empty() && self.lower_errors.is_empty() && self.exit_code == 0
    }
}

/// Runner for main functions using JIT compilation
pub struct MainRunner {
    _private: (),
}

impl MainRunner {
    /// Create a new main runner
    pub fn new() -> Self {
        Self { _private: () }
    }

    /// Run the main function in a source file using JIT compilation
    pub fn run(&self, source: &str) -> MainResult {
        let arena = Bump::new();

        // Parse with prelude
        let parse_result = parse_with_prelude(&arena, source);

        if !parse_result.errors.is_empty() {
            return MainResult {
                exit_code: 1,
                failure_message: None,
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
        let prelude_lines = PRELUDE.lines().count() as u32 + 1;
        let program = match lower_program(&parse_result.nodes, &combined, prelude_lines) {
            Ok(p) => p,
            Err(e) => {
                return MainResult {
                    exit_code: 1,
                    failure_message: None,
                    parse_errors: vec![],
                    lower_errors: vec![e.to_string()],
                };
            }
        };

        // Find main function
        let main_fn = match program.functions.iter().find(|f| f.name == "main") {
            Some(f) => f,
            None => {
                return MainResult {
                    exit_code: 1,
                    failure_message: Some("no main function found".to_string()),
                    parse_errors: vec![],
                    lower_errors: vec![],
                };
            }
        };

        // Lower to MIR (just the main function)
        let single_program = Program {
            tests: vec![],
            functions: vec![main_fn.clone()],
        };
        let mir_program = lower_to_mir(&single_program);
        let mir_func = &mir_program.functions[0];

        // JIT compile and run
        let compiled = compile_function(mir_func);
        match ExecutableMemory::new(&compiled.code) {
            Ok(mem) => {
                let main_fn: extern "C" fn() -> u8 = unsafe { mem.as_fn() };
                let ret = main_fn();
                if ret == 0 {
                    MainResult {
                        exit_code: 0,
                        failure_message: None,
                        parse_errors: vec![],
                        lower_errors: vec![],
                    }
                } else {
                    let assert_idx = (ret - 1) as usize;
                    let failure_message = if let Some(info) = mir_func.asserts.get(assert_idx) {
                        format!(
                            "assertion failed at line {}:{}\n  assert({})",
                            info.line, info.col, info.source
                        )
                    } else {
                        format!("assertion {} failed", ret)
                    };
                    MainResult {
                        exit_code: 1,
                        failure_message: Some(failure_message),
                        parse_errors: vec![],
                        lower_errors: vec![],
                    }
                }
            }
            Err(e) => MainResult {
                exit_code: 1,
                failure_message: Some(format!("JIT compilation failed: {}", e)),
                parse_errors: vec![],
                lower_errors: vec![],
            },
        }
    }
}

impl Default for MainRunner {
    fn default() -> Self {
        Self::new()
    }
}

/// Compiler for generating ELF binaries
pub struct Compiler {
    _private: (),
}

impl Compiler {
    /// Create a new compiler
    pub fn new() -> Self {
        Self { _private: () }
    }

    /// Compile source to an ELF binary
    pub fn compile(&self, source: &str, filename: &str) -> Result<Vec<u8>, String> {
        let arena = Bump::new();

        // Parse with prelude
        let parse_result = parse_with_prelude(&arena, source);

        if !parse_result.errors.is_empty() {
            let errors: Vec<String> = parse_result
                .errors
                .iter()
                .map(|e| format!("{}:{}: {}", e.loc.line, e.loc.col, e.msg))
                .collect();
            return Err(errors.join("\n"));
        }

        // Lower to BaseLang AST
        let combined = format!("{}\n{}", PRELUDE, source);
        let prelude_lines = PRELUDE.lines().count() as u32 + 1;
        let program = lower_program(&parse_result.nodes, &combined, prelude_lines)
            .map_err(|e| e.to_string())?;

        // Find main function
        let main_fn = program
            .functions
            .iter()
            .find(|f| f.name == "main")
            .ok_or_else(|| "no main function found".to_string())?;

        // Lower to MIR
        let single_program = Program {
            tests: vec![],
            functions: vec![main_fn.clone()],
        };
        let mir_program = lower_to_mir(&single_program);
        let mir_func = &mir_program.functions[0];

        // Generate ELF
        Ok(skylow_elf::generate_elf(mir_func, filename))
    }
}

impl Default for Compiler {
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

    #[test]
    fn test_main_runner_simple() {
        let runner = MainRunner::new();
        let source = r#"
fn main():
  assert(1 == 1)
"#;
        let result = runner.run(source);
        assert!(result.parse_errors.is_empty());
        assert!(result.lower_errors.is_empty());
        assert!(result.success());
        assert_eq!(result.exit_code, 0);
    }

    #[test]
    fn test_main_runner_default() {
        let runner = MainRunner::default();
        let source = r#"
fn main():
  assert(1 == 1)
"#;
        let result = runner.run(source);
        assert!(result.success());
    }

    #[test]
    fn test_main_runner_failing() {
        let runner = MainRunner::new();
        let source = r#"
fn main():
  assert(1 == 2)
"#;
        let result = runner.run(source);
        assert!(!result.success());
        assert_eq!(result.exit_code, 1);
        assert!(result.failure_message.is_some());
    }

    #[test]
    fn test_main_runner_no_main() {
        let runner = MainRunner::new();
        let source = r#"
test foo:
  assert(1 == 1)
"#;
        let result = runner.run(source);
        assert!(!result.success());
        assert_eq!(result.failure_message, Some("no main function found".to_string()));
    }

    #[test]
    fn test_compiler_simple() {
        let compiler = Compiler::new();
        let source = r#"
fn main():
  assert(1 == 1)
"#;
        let result = compiler.compile(source, "test.skyl");
        assert!(result.is_ok());
        let elf = result.unwrap();
        // Check ELF magic
        assert_eq!(&elf[0..4], &[0x7f, b'E', b'L', b'F']);
    }

    #[test]
    fn test_compiler_default() {
        let compiler = Compiler::default();
        let source = r#"
fn main():
  assert(1 == 1)
"#;
        let result = compiler.compile(source, "test.skyl");
        assert!(result.is_ok());
    }

    #[test]
    fn test_compiler_no_main() {
        let compiler = Compiler::new();
        let source = r#"
test foo:
  assert(1 == 1)
"#;
        let result = compiler.compile(source, "test.skyl");
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("no main function found"));
    }
}

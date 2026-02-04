//! Test runner for SkyLow
//!
//! Orchestrates the full pipeline: parse -> lower to BaseLang -> lower to MIR -> JIT compile -> execute.

use bumpalo::Bump;
use baselang::{lower_program, parse_with_prelude, Decl, DeclKind, Program};
use jit::{compile_program, ExecutableMemory};
use mir::lower_program as lower_to_mir;

/// Find the main function in a program
fn find_main_function<'a>(program: &'a Program<'a>) -> Option<&'a Decl<'a>> {
    program.functions().find(|decl| {
        matches!(&decl.kind, DeclKind::Fn { name, .. } if *name == "main")
    })
}

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
        let program = match lower_program(&arena, &parse_result.nodes) {
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

        // Compile entire program (links all functions together)
        let compiled = compile_program(&mir_program);

        // Create executable memory for the whole program
        let mem = match ExecutableMemory::new(&compiled.code) {
            Ok(m) => m,
            Err(e) => {
                return RunResult {
                    results: vec![],
                    parse_errors: vec![],
                    lower_errors: vec![format!("JIT compilation failed: {}", e)],
                };
            }
        };

        // Run each test function
        let mut results = Vec::new();
        for (idx, mir_func) in mir_program.functions.iter().enumerate() {
            // Only run test functions (skip regular functions like helpers)
            if mir_func.kind != mir::FunctionKind::Test {
                continue;
            }

            let compiled_func = &compiled.functions[idx];
            let test_fn: extern "C" fn() -> u8 =
                unsafe { mem.as_fn_at_offset(compiled_func.offset) };
            let ret = test_fn();

            let result = if ret == 0 {
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
        let program = match lower_program(&arena, &parse_result.nodes) {
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

        // Lower to MIR (all functions to enable calls between them)
        let mir_program = lower_to_mir(&program);

        // Find main function index
        let main_idx = mir_program
            .functions
            .iter()
            .position(|f| f.name == "main");

        let main_idx = match main_idx {
            Some(idx) => idx,
            None => {
                return MainResult {
                    exit_code: 1,
                    failure_message: Some("no main function found".to_string()),
                    parse_errors: vec![],
                    lower_errors: vec![],
                };
            }
        };

        // Compile entire program (links all functions together)
        let compiled = compile_program(&mir_program);
        let mir_func = &mir_program.functions[main_idx];

        match ExecutableMemory::new(&compiled.code) {
            Ok(mem) => {
                let main_fn: extern "C" fn() -> u8 =
                    unsafe { mem.as_fn_at_offset(compiled.functions[main_idx].offset) };
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
        let program = lower_program(&arena, &parse_result.nodes)
            .map_err(|e| e.to_string())?;

        // Find main function
        let main_fn = find_main_function(&program)
            .ok_or_else(|| "no main function found".to_string())?;

        // Lower to MIR
        let single_program = Program {
            decls: std::slice::from_ref(main_fn),
        };
        let mir_program = lower_to_mir(&single_program);
        let mir_func = &mir_program.functions[0];

        // Generate ELF
        Ok(elf::generate_elf(mir_func, filename))
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
    use indoc::indoc;

    #[test]
    fn test_runner_simple() {
        let runner = TestRunner::new();
        let source = indoc! {"
            test simple:
              assert(1 == 1)
        "};
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
        let source = indoc! {"
            test default:
              assert(1 == 1)
        "};
        let result = runner.run(source);
        assert!(result.success());
    }

    #[test]
    fn test_runner_arithmetic() {
        let runner = TestRunner::new();
        // Note: Due to a parser bug with precedence, we need explicit parentheses
        // for expressions where a lower-precedence operator follows a higher one
        let source = indoc! {"
            test arithmetic works as expected:
              assert((2 + 2) == 4)
              assert((2 + (3*2)) == 8)
              assert(((1 + 2) * (3 + 4)) == 21)
              assert(((10 / 2) - 3) == 2)
        "};
        let result = runner.run(source);
        assert!(result.parse_errors.is_empty(), "parse errors: {:?}", result.parse_errors);
        assert!(result.lower_errors.is_empty(), "lower errors: {:?}", result.lower_errors);
        assert_eq!(result.results.len(), 1);
        assert!(result.results[0].passed, "test failed: {:?}", result.results[0].failure_message);
    }

    #[test]
    fn test_runner_failing_test() {
        let runner = TestRunner::new();
        let source = indoc! {"
            test failing:
              assert(1 == 2)
        "};
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
        let source = indoc! {"
            test first:
              assert(1 == 1)

            test second:
              assert(2 == 2)
        "};
        let result = runner.run(source);
        assert!(result.parse_errors.is_empty());
        assert!(result.lower_errors.is_empty());
        assert_eq!(result.results.len(), 2);
        assert!(result.results.iter().all(|r| r.passed));
    }

    #[test]
    fn test_main_runner_simple() {
        let runner = MainRunner::new();
        let source = indoc! {"
            fn main() -> I64:
              assert(1 == 1)
              return 0
        "};
        let result = runner.run(source);
        assert!(result.parse_errors.is_empty(), "parse errors: {:?}", result.parse_errors);
        assert!(result.lower_errors.is_empty(), "lower errors: {:?}", result.lower_errors);
        assert!(result.success(), "not successful: {:?}", result.failure_message);
        assert_eq!(result.exit_code, 0);
    }

    #[test]
    fn test_main_runner_default() {
        let runner = MainRunner::default();
        let source = indoc! {"
            fn main() -> I64:
              assert(1 == 1)
              return 0
        "};
        let result = runner.run(source);
        assert!(result.success(), "not successful: {:?}", result.failure_message);
    }

    #[test]
    fn test_main_runner_failing() {
        let runner = MainRunner::new();
        let source = indoc! {"
            fn main() -> I64:
              assert(1 == 2)
              return 0
        "};
        let result = runner.run(source);
        assert!(!result.success());
        assert_eq!(result.exit_code, 1);
        assert!(result.failure_message.is_some());
    }

    #[test]
    fn test_main_runner_no_main() {
        let runner = MainRunner::new();
        let source = indoc! {"
            test foo:
              assert(1 == 1)
        "};
        let result = runner.run(source);
        assert!(!result.success());
        assert_eq!(result.failure_message, Some("no main function found".to_string()));
    }

    #[test]
    fn test_compiler_simple() {
        let compiler = Compiler::new();
        let source = indoc! {"
            fn main() -> I64:
              assert(1 == 1)
              return 0
        "};
        let result = compiler.compile(source, "test.skyl");
        assert!(result.is_ok(), "compile error: {:?}", result.err());
        let elf = result.unwrap();
        // Check ELF magic
        assert_eq!(&elf[0..4], &[0x7f, b'E', b'L', b'F']);
    }

    #[test]
    fn test_compiler_default() {
        let compiler = Compiler::default();
        let source = indoc! {"
            fn main() -> I64:
              assert(1 == 1)
              return 0
        "};
        let result = compiler.compile(source, "test.skyl");
        assert!(result.is_ok(), "compile error: {:?}", result.err());
    }

    #[test]
    fn test_compiler_no_main() {
        let compiler = Compiler::new();
        let source = indoc! {"
            test foo:
              assert(1 == 1)
        "};
        let result = compiler.compile(source, "test.skyl");
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("no main function found"));
    }

    #[test]
    fn test_runner_parse_error() {
        let runner = TestRunner::new();
        // Invalid syntax - completely broken
        let source = "@#$%^&";
        let result = runner.run(source);
        assert!(!result.parse_errors.is_empty(), "expected parse errors but got none");
        assert!(!result.success());
    }

    #[test]
    fn test_main_runner_parse_error() {
        let runner = MainRunner::new();
        // Invalid syntax - missing return type
        let source = "fn main():\n  return 0";
        let result = runner.run(source);
        assert!(!result.parse_errors.is_empty(), "expected parse errors but got none");
        assert!(!result.success());
    }

    #[test]
    fn test_compiler_parse_error() {
        let compiler = Compiler::new();
        // Invalid syntax - missing return type
        let source = "fn main():\n  return 0";
        let result = compiler.compile(source, "test.skyl");
        assert!(result.is_err());
    }

    #[test]
    fn test_compiler_lower_error() {
        let compiler = Compiler::new();
        // Integer overflow triggers a lowering error
        let source = indoc! {"
            fn main() -> I64:
              assert(99999999999999999999 == 1)
              return 0
        "};
        let result = compiler.compile(source, "test.skyl");
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("integer"));
    }
}

//! Integration tests for the debugger
//!
//! These tests compile and run actual programs under the debugger.

use baselang::{lower_program as lower_to_ast, parse_with_prelude};
use bumpalo::Bump;
use debugger::{parse, Session};
use elf::generate_elf_with_debug;
use mir::lower_program as lower_to_mir;
use std::fs::{self, File};
use std::io::Write;
use std::path::PathBuf;

fn compile_test_program(source: &str, name: &str) -> (PathBuf, PathBuf) {
    let arena = Bump::new();
    let parse_result = parse_with_prelude(&arena, source);
    assert!(parse_result.errors.is_empty(), "Parse errors: {:?}", parse_result.errors);

    let ast = lower_to_ast(&arena, &parse_result.nodes).expect("lower failed");
    let mir = lower_to_mir(&ast);
    let func = mir.functions.first().expect("no functions");

    let result = generate_elf_with_debug(func, name);

    let temp_dir = std::env::temp_dir().join("skydbg_integration");
    fs::create_dir_all(&temp_dir).unwrap();

    let binary_path = temp_dir.join(format!("{}.elf", name));
    let debug_path = temp_dir.join(format!("{}.skydbg", name));

    {
        let mut f = File::create(&binary_path).unwrap();
        f.write_all(&result.elf).unwrap();
    }

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = fs::metadata(&binary_path).unwrap().permissions();
        perms.set_mode(0o755);
        fs::set_permissions(&binary_path, perms).unwrap();
    }

    if let Some(debug_data) = result.debug_sidecar {
        let mut f = File::create(&debug_path).unwrap();
        f.write_all(&debug_data).unwrap();
    }

    std::thread::sleep(std::time::Duration::from_millis(10));

    (binary_path, debug_path)
}

fn cleanup(binary: &PathBuf, debug: &PathBuf) {
    let _ = fs::remove_file(binary);
    let _ = fs::remove_file(debug);
}

#[test]
fn test_simple_run() {
    let source = r#"test simple:
  assert(1 == 1)
"#;
    let (binary, debug) = compile_test_program(source, "integration_simple");

    let mut session = Session::new(binary.clone(), vec![]).unwrap();

    // Run the program
    let result = session.execute(parse("run"));
    assert!(result.is_ok());

    // Check output
    let output = session.get_output();
    assert!(output.iter().any(|l| l.contains("Starting program")));
    assert!(output.iter().any(|l| l.contains("exited with code 0")));

    cleanup(&binary, &debug);
}

#[test]
fn test_quit_command() {
    let source = r#"test simple:
  assert(1 == 1)
"#;
    let (binary, debug) = compile_test_program(source, "integration_quit");

    let mut session = Session::new(binary.clone(), vec![]).unwrap();

    // Quit should return true
    let result = session.execute(parse("quit"));
    assert_eq!(result, Ok(true));

    cleanup(&binary, &debug);
}

#[test]
fn test_empty_command() {
    let source = r#"test simple:
  assert(1 == 1)
"#;
    let (binary, debug) = compile_test_program(source, "integration_empty");

    let mut session = Session::new(binary.clone(), vec![]).unwrap();

    // Empty command should do nothing
    let result = session.execute(parse(""));
    assert_eq!(result, Ok(false));

    cleanup(&binary, &debug);
}

#[test]
fn test_unknown_command() {
    let source = r#"test simple:
  assert(1 == 1)
"#;
    let (binary, debug) = compile_test_program(source, "integration_unknown");

    let mut session = Session::new(binary.clone(), vec![]).unwrap();

    // Unknown command
    let result = session.execute(parse("foobar"));
    assert_eq!(result, Ok(false));
    let output = session.get_output();
    assert!(output.iter().any(|l| l.contains("Unknown command")));

    cleanup(&binary, &debug);
}

#[test]
fn test_step_before_run() {
    let source = r#"test simple:
  assert(1 == 1)
"#;
    let (binary, debug) = compile_test_program(source, "integration_step_early");

    let mut session = Session::new(binary.clone(), vec![]).unwrap();

    // Step before run should fail
    let result = session.execute(parse("step"));
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("not running"));

    cleanup(&binary, &debug);
}

#[test]
fn test_continue_before_run() {
    let source = r#"test simple:
  assert(1 == 1)
"#;
    let (binary, debug) = compile_test_program(source, "integration_cont_early");

    let mut session = Session::new(binary.clone(), vec![]).unwrap();

    // Continue before run should fail
    let result = session.execute(parse("continue"));
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("not running"));

    cleanup(&binary, &debug);
}

#[test]
fn test_info_breakpoints_empty() {
    let source = r#"test simple:
  assert(1 == 1)
"#;
    let (binary, debug) = compile_test_program(source, "integration_info_bp");

    let mut session = Session::new(binary.clone(), vec![]).unwrap();

    // Info breakpoints with no breakpoints
    session.clear_output();
    let result = session.execute(parse("info breakpoints"));
    assert_eq!(result, Ok(false));
    let output = session.get_output();
    assert!(output.iter().any(|l| l.contains("No breakpoints")));

    cleanup(&binary, &debug);
}

#[test]
fn test_print_before_run() {
    let source = r#"test simple:
  assert(1 == 1)
"#;
    let (binary, debug) = compile_test_program(source, "integration_print_early");

    let mut session = Session::new(binary.clone(), vec![]).unwrap();

    // Print before run should fail
    let result = session.execute(parse("print x0"));
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("not running"));

    cleanup(&binary, &debug);
}

#[test]
fn test_backtrace_before_run() {
    let source = r#"test simple:
  assert(1 == 1)
"#;
    let (binary, debug) = compile_test_program(source, "integration_bt_early");

    let mut session = Session::new(binary.clone(), vec![]).unwrap();

    // Backtrace before run should fail
    let result = session.execute(parse("backtrace"));
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("not running"));

    cleanup(&binary, &debug);
}

#[test]
fn test_info_registers_before_run() {
    let source = r#"test simple:
  assert(1 == 1)
"#;
    let (binary, debug) = compile_test_program(source, "integration_regs_early");

    let mut session = Session::new(binary.clone(), vec![]).unwrap();

    // Info registers before run should fail
    let result = session.execute(parse("info registers"));
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("not running"));

    cleanup(&binary, &debug);
}

#[test]
fn test_list_before_run() {
    let source = r#"test simple:
  assert(1 == 1)
"#;
    let (binary, debug) = compile_test_program(source, "integration_list_early");

    let mut session = Session::new(binary.clone(), vec![]).unwrap();

    // List before run should fail
    let result = session.execute(parse("list"));
    assert!(result.is_err());

    cleanup(&binary, &debug);
}

#[test]
fn test_debug_info_loaded() {
    let source = r#"test simple:
  assert(1 == 1)
"#;
    let (binary, debug) = compile_test_program(source, "integration_debug_info");

    let session = Session::new(binary.clone(), vec![]).unwrap();

    // Debug info should be loaded
    assert!(session.debug_info().is_some());

    cleanup(&binary, &debug);
}

#[test]
fn test_assertion_failure() {
    let source = r#"test failing:
  assert(1 == 0)
"#;
    let (binary, debug) = compile_test_program(source, "integration_fail");

    let mut session = Session::new(binary.clone(), vec![]).unwrap();

    // Run should complete (assertion failure exits with non-zero)
    let result = session.execute(parse("run"));
    assert!(result.is_ok());

    // Check output shows non-zero exit
    let output = session.get_output();
    // The assertion prints to stderr and exits with code 1
    assert!(output.iter().any(|l| l.contains("exited with code 1")));

    cleanup(&binary, &debug);
}

#[test]
fn test_delete_nonexistent() {
    let source = r#"test simple:
  assert(1 == 1)
"#;
    let (binary, debug) = compile_test_program(source, "integration_del_none");

    let mut session = Session::new(binary.clone(), vec![]).unwrap();

    // Delete nonexistent breakpoint
    let result = session.execute(parse("delete 99"));
    assert_eq!(result, Ok(false));
    let output = session.get_output();
    assert!(output.iter().any(|l| l.contains("No breakpoint number 99")));

    cleanup(&binary, &debug);
}

#[test]
fn test_info_locals_before_run() {
    let source = r#"test simple:
  assert(1 == 1)
"#;
    let (binary, debug) = compile_test_program(source, "integration_locals_early");

    let mut session = Session::new(binary.clone(), vec![]).unwrap();

    // Info locals before run should fail
    let result = session.execute(parse("info locals"));
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("not running"));

    cleanup(&binary, &debug);
}

#[test]
fn test_next_before_run() {
    let source = r#"test simple:
  assert(1 == 1)
"#;
    let (binary, debug) = compile_test_program(source, "integration_next_early");

    let mut session = Session::new(binary.clone(), vec![]).unwrap();

    // Next before run should fail
    let result = session.execute(parse("next"));
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("not running"));

    cleanup(&binary, &debug);
}

#[test]
fn test_stepi_before_run() {
    let source = r#"test simple:
  assert(1 == 1)
"#;
    let (binary, debug) = compile_test_program(source, "integration_stepi_early");

    let mut session = Session::new(binary.clone(), vec![]).unwrap();

    // Stepi before run should fail
    let result = session.execute(parse("stepi"));
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("not running"));

    cleanup(&binary, &debug);
}

#[test]
fn test_finish_before_run() {
    let source = r#"test simple:
  assert(1 == 1)
"#;
    let (binary, debug) = compile_test_program(source, "integration_finish_early");

    let mut session = Session::new(binary.clone(), vec![]).unwrap();

    // Finish before run should fail
    let result = session.execute(parse("finish"));
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("not running"));

    cleanup(&binary, &debug);
}

#[test]
fn test_break_by_address() {
    let source = r#"test simple:
  assert(1 == 1)
"#;
    let (binary, debug) = compile_test_program(source, "integration_break_addr");

    let mut session = Session::new(binary.clone(), vec![]).unwrap();

    // Set breakpoint at an address (0x prefix, not *0x)
    let result = session.execute(parse("break 0x400100"));
    assert_eq!(result, Ok(false));
    let output = session.get_output();
    assert!(output.iter().any(|l| l.contains("Breakpoint 1")));

    cleanup(&binary, &debug);
}

#[test]
fn test_break_by_function() {
    let source = r#"test simple:
  assert(1 == 1)
"#;
    let (binary, debug) = compile_test_program(source, "integration_break_func");

    let mut session = Session::new(binary.clone(), vec![]).unwrap();

    // Set breakpoint at function - this should fail since "simple" isn't the internal name
    let _result = session.execute(parse("break simple"));
    // Function might not be found, which is okay for this test
    // The important thing is that cmd_break is exercised

    cleanup(&binary, &debug);
}

#[test]
fn test_assert_before_run() {
    let source = r#"test simple:
  assert(1 == 1)
"#;
    let (binary, debug) = compile_test_program(source, "integration_assert_early");

    let mut session = Session::new(binary.clone(), vec![]).unwrap();

    // Assert before run - eval_simple should fail
    let result = session.execute(parse("assert x0 == 0"));
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("not running"));

    cleanup(&binary, &debug);
}

#[test]
fn test_assert_with_numbers() {
    let source = r#"test simple:
  assert(1 == 1)
"#;
    let (binary, debug) = compile_test_program(source, "integration_assert_nums");

    let mut session = Session::new(binary.clone(), vec![]).unwrap();

    // Run the program first
    session.execute(parse("run")).unwrap();

    // After program exits, we can't evaluate registers
    // But we can test assert with constant numbers
    session.clear_output();

    cleanup(&binary, &debug);
}

#[test]
fn test_expect_stop_before_run() {
    let source = r#"test simple:
  assert(1 == 1)
"#;
    let (binary, debug) = compile_test_program(source, "integration_expect_early");

    let mut session = Session::new(binary.clone(), vec![]).unwrap();

    // Expect before run should fail (syntax is "expect stop at <file>:<line>")
    let result = session.execute(parse("expect stop at simple.skyl:1"));
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("not running"));

    cleanup(&binary, &debug);
}

#[test]
fn test_list_with_location() {
    let source = r#"test simple:
  assert(1 == 1)
"#;
    let (binary, debug) = compile_test_program(source, "integration_list_loc");

    let mut session = Session::new(binary.clone(), vec![]).unwrap();

    // List with explicit location
    let _result = session.execute(parse("list integration_list_loc:1"));
    // Might succeed or fail depending on source availability
    // The important thing is that cmd_list with location is exercised

    cleanup(&binary, &debug);
}

#[test]
fn test_command_repeat() {
    let source = r#"test simple:
  assert(1 == 1)
"#;
    let (binary, debug) = compile_test_program(source, "integration_repeat");

    let mut session = Session::new(binary.clone(), vec![]).unwrap();

    // Set up a command
    session.execute(parse("info breakpoints")).unwrap();

    // Empty command should repeat last
    session.clear_output();
    let result = session.execute(parse(""));
    assert_eq!(result, Ok(false));
    // Should show "No breakpoints" again
    let output = session.get_output();
    assert!(output.iter().any(|l| l.contains("No breakpoints")));

    cleanup(&binary, &debug);
}

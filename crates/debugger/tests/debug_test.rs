//! File-based debugger tests
//!
//! Each test consists of:
//! - A `.skyl` source file
//! - A `.dbg` debugger script with the same name
//! - A `.dbg.expected` file with expected output

use baselang::{lower_program as lower_to_ast, parse_with_prelude_named};
use bumpalo::Bump;
use datatest_stable::harness;
use debugger::Session;
use elf::generate_elf_from_program_with_debug;
use mir::lower_program as lower_to_mir;
use std::fs::{self, File};
use std::io::Write;
use std::path::Path;

fn run_test(path: &Path) -> datatest_stable::Result<()> {
    // Read source file
    let source = fs::read_to_string(path)?;

    // Read script file (same name with .dbg extension)
    let script_path = path.with_extension("dbg");
    if !script_path.exists() {
        return Err(format!("No script file: {}", script_path.display()).into());
    }
    let script = fs::read_to_string(&script_path)?;

    // Read expected output
    let expected_path = format!("{}.expected", script_path.display());
    let expected = fs::read_to_string(&expected_path)?.trim().to_string();

    // Compile source to MIR
    let arena = Bump::new();
    let filename = path.file_name().unwrap().to_str().unwrap();
    let parse_result = parse_with_prelude_named(&arena, &source, filename);

    if !parse_result.errors.is_empty() {
        let errors: Vec<_> = parse_result
            .errors
            .iter()
            .map(|e| format!("{}:{}: {}", e.loc.line, e.loc.col, e.msg))
            .collect();
        return Err(format!("Parse errors:\n{}", errors.join("\n")).into());
    }

    let ast = lower_to_ast(&arena, &parse_result.nodes)
        .map_err(|e| format!("Lower error: {}", e))?;

    let mir = lower_to_mir(&ast);

    // Verify we have at least one function
    if mir.functions.is_empty() {
        return Err("No functions in program".into());
    }

    // Generate ELF with debug info for the whole program
    let result = generate_elf_from_program_with_debug(&mir, filename);

    // Write to temp files
    let temp_dir = std::env::temp_dir().join("skydbg_test");
    fs::create_dir_all(&temp_dir)?;

    let binary_path = temp_dir.join(format!("{}.elf", filename));
    let debug_path = temp_dir.join(format!("{}.skydbg", filename));

    {
        let mut binary_file = File::create(&binary_path)?;
        binary_file.write_all(&result.elf)?;
        // File closes when binary_file goes out of scope
    }

    // Make executable
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = fs::metadata(&binary_path)?.permissions();
        perms.set_mode(0o755);
        fs::set_permissions(&binary_path, perms)?;
    }

    if let Some(debug_data) = result.debug_sidecar {
        let mut debug_file = File::create(&debug_path)?;
        debug_file.write_all(&debug_data)?;
        // File closes when debug_file goes out of scope
    }

    // Small delay to ensure file system has finished writing
    std::thread::sleep(std::time::Duration::from_millis(10));

    // Create debugger session
    let mut session = Session::new(binary_path.clone(), vec![])
        .map_err(|e| format!("Failed to create session: {}", e))?;

    // Execute script commands
    for line in script.lines() {
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with('#') {
            continue;
        }

        let cmd = debugger::parse(trimmed);
        match session.execute(cmd) {
            Ok(true) => break, // Quit
            Ok(false) => {}
            Err(e) => {
                return Err(format!("Command '{}' failed: {}", trimmed, e).into());
            }
        }
    }

    // Get session output
    let actual = session.get_output().join("\n");

    // Clean up temp files
    let _ = fs::remove_file(&binary_path);
    let _ = fs::remove_file(&debug_path);

    if actual.trim() != expected {
        return Err(format!(
            "Output mismatch for {:?}\n\nExpected:\n{}\n\nActual:\n{}",
            path, expected, actual
        )
        .into());
    }

    Ok(())
}

harness!(run_test, "tests/debug", r"\.skyl$");

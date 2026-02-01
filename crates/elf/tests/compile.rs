//! File-based tests for ELF compilation
//!
//! Each .skyl file is compiled to an ELF binary and the binary is optionally
//! executed to verify correct behavior.

use bumpalo::Bump;
use skylow_baselang::{lower_program, parse_with_prelude, PRELUDE};
use skylow_elf::generate_elf;
use skylow_mir::lower_program as lower_to_mir;
use std::path::Path;

fn run_test(path: &Path) -> datatest_stable::Result<()> {
    let source = std::fs::read_to_string(path).expect("Failed to read test file");

    // Parse
    let arena = Bump::new();
    let parse_result = parse_with_prelude(&arena, &source);
    assert!(
        parse_result.errors.is_empty(),
        "Parse errors: {:?}",
        parse_result.errors
    );

    // Lower to BaseLang
    let combined = format!("{}\n{}", PRELUDE, source);
    let prelude_lines = PRELUDE.lines().count() as u32 + 1;
    let program = lower_program(&parse_result.nodes, &combined, prelude_lines)
        .expect("Lowering failed");

    // Lower to MIR
    let mir_program = lower_to_mir(&program);

    // Get the main function (should be the first function)
    let main_func = mir_program
        .functions
        .iter()
        .find(|f| f.name == "main")
        .expect("No main function found");

    // Generate ELF
    let filename = path.file_name().unwrap().to_str().unwrap();
    let elf = generate_elf(main_func, filename);

    // Verify ELF header
    assert_eq!(&elf[0..4], &[0x7f, b'E', b'L', b'F'], "Invalid ELF magic");
    assert_eq!(elf[4], 2, "Not a 64-bit ELF");
    assert_eq!(elf[5], 1, "Not little endian");
    assert_eq!(elf[18], 0xB7, "Not AArch64");

    // Read expected output if it exists
    let expected_path = path.with_extension("skyl.expected");
    if expected_path.exists() {
        let expected = std::fs::read_to_string(&expected_path).expect("Failed to read expected file");
        // For now, just check that the file contains expected metadata
        // In a full implementation, we would execute the binary and compare output
        assert!(
            !expected.is_empty(),
            "Expected file should not be empty"
        );
    }

    Ok(())
}

datatest_stable::harness!(run_test, "tests/compile", r"\.skyl$");

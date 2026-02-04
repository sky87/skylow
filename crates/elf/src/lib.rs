//! ELF binary generation for SkyLow
//!
//! This crate generates Linux AArch64 ELF executables from MIR functions.

pub mod aarch64;
pub mod elf64;

use debuginfo::write_skydbg;
use mir::MirFunction;

use crate::aarch64::codegen::{compile_binary, compile_binary_with_debug};
use crate::elf64::{
    generate_elf_header, generate_program_header, header_size, BASE_ADDR, SEGMENT_RX,
};

/// Generate a complete ELF executable from a MIR function
pub fn generate_elf(func: &MirFunction, filename: &str) -> Vec<u8> {
    // Compile the function to machine code
    let compiled = compile_binary(func, filename);

    // Calculate layout using a single LOAD segment for simplicity:
    // - ELF header (64 bytes)
    // - 1 program header (56 bytes)
    // - Code section (variable)
    // - Padding to 8-byte boundary
    // - Read-only data section (variable)

    let num_phdrs = 1u16;
    let headers_size = header_size(num_phdrs);
    let code_offset = headers_size;
    let code_size = compiled.code.len();

    // Align rodata to 8 bytes after code
    let rodata_offset = (code_offset + code_size + 7) & !7;
    let rodata_size = compiled.rodata.len();

    // Total file size and segment size
    let total_size = rodata_offset + rodata_size;
    let segment_size = total_size - code_offset;

    // Entry point is at the start of code
    let entry_addr = BASE_ADDR + code_offset as u64 + compiled.entry_offset as u64;

    // Rodata virtual address (for patching message pointers)
    let rodata_vaddr = BASE_ADDR + rodata_offset as u64;

    // Generate the ELF binary
    let mut elf = Vec::with_capacity(total_size);

    // ELF header
    elf.extend_from_slice(&generate_elf_header(entry_addr, num_phdrs));

    // Single program header for code + rodata (RX - need execute for code, read for rodata)
    let segment_vaddr = BASE_ADDR + code_offset as u64;
    elf.extend_from_slice(&generate_program_header(
        code_offset as u64,
        segment_vaddr,
        segment_size as u64,
        segment_size as u64,
        SEGMENT_RX,
        0x1000,
    ));

    // Code section - need to patch rodata addresses
    let mut code = compiled.code.clone();
    patch_rodata_addresses(&mut code, rodata_vaddr);
    elf.extend_from_slice(&code);

    // Padding between code and rodata
    let padding = rodata_offset - (code_offset + code_size);
    elf.extend(std::iter::repeat(0u8).take(padding));

    // Read-only data section
    elf.extend_from_slice(&compiled.rodata);

    elf
}

/// Result of ELF generation with debug info
pub struct ElfWithDebug {
    /// The ELF binary
    pub elf: Vec<u8>,
    /// The debug info sidecar file (.skydbg format), if debug info was available
    pub debug_sidecar: Option<Vec<u8>>,
}

/// Generate a complete ELF executable with optional debug sidecar file
///
/// Returns the ELF binary and an optional .skydbg sidecar file containing
/// debug information.
pub fn generate_elf_with_debug(func: &MirFunction, filename: &str) -> ElfWithDebug {
    // Compile the function to machine code with debug info
    let compiled = compile_binary_with_debug(func, filename);

    // Calculate layout using a single LOAD segment for simplicity:
    // - ELF header (64 bytes)
    // - 1 program header (56 bytes)
    // - Code section (variable)
    // - Padding to 8-byte boundary
    // - Read-only data section (variable)

    let num_phdrs = 1u16;
    let headers_size = header_size(num_phdrs);
    let code_offset = headers_size;
    let code_size = compiled.code.len();

    // Align rodata to 8 bytes after code
    let rodata_offset = (code_offset + code_size + 7) & !7;
    let rodata_size = compiled.rodata.len();

    // Total file size and segment size
    let total_size = rodata_offset + rodata_size;
    let segment_size = total_size - code_offset;

    // Entry point is at the start of code
    let entry_addr = BASE_ADDR + code_offset as u64 + compiled.entry_offset as u64;

    // Rodata virtual address (for patching message pointers)
    let rodata_vaddr = BASE_ADDR + rodata_offset as u64;

    // Generate the ELF binary
    let mut elf = Vec::with_capacity(total_size);

    // ELF header
    elf.extend_from_slice(&generate_elf_header(entry_addr, num_phdrs));

    // Single program header for code + rodata (RX - need execute for code, read for rodata)
    let segment_vaddr = BASE_ADDR + code_offset as u64;
    elf.extend_from_slice(&generate_program_header(
        code_offset as u64,
        segment_vaddr,
        segment_size as u64,
        segment_size as u64,
        SEGMENT_RX,
        0x1000,
    ));

    // Code section - need to patch rodata addresses
    let mut code = compiled.code.clone();
    patch_rodata_addresses(&mut code, rodata_vaddr);
    elf.extend_from_slice(&code);

    // Padding between code and rodata
    let padding = rodata_offset - (code_offset + code_size);
    elf.extend(std::iter::repeat(0u8).take(padding));

    // Read-only data section
    elf.extend_from_slice(&compiled.rodata);

    // Generate debug sidecar if we have debug info
    let debug_sidecar = compiled.debug_info.map(|debug_info| {
        let mut sidecar = Vec::new();
        write_skydbg(&debug_info, &mut sidecar).expect("failed to serialize debug info");
        sidecar
    });

    ElfWithDebug { elf, debug_sidecar }
}

/// Patch the rodata addresses in the generated code
///
/// The code generator emits placeholder offsets for message buffers.
/// We need to find these and patch them with actual virtual addresses.
fn patch_rodata_addresses(code: &mut [u8], rodata_vaddr: u64) {
    // This is a simplified patching approach. We scan the code for
    // MOVZ/MOVK sequences that load message offsets and patch them
    // to load actual virtual addresses.
    //
    // For each MOVZ X1, #offset instruction followed by potential MOVK,
    // we patch to load rodata_vaddr + offset.
    //
    // MOVZ X1, #imm: 0xd2800001 | (imm << 5)
    // The instruction format is:
    // 1 10 100101 hw imm16 Rd
    // where hw = 00 for LSL #0, 01 for LSL #16, etc.

    let mut i = 0;
    while i + 4 <= code.len() {
        let inst = u32::from_le_bytes([code[i], code[i + 1], code[i + 2], code[i + 3]]);

        // Check if this is MOVZ X1, #imm (for message offset)
        // Pattern: 0xd2800001 with imm16 in bits 5-20
        if (inst & 0xffe0001f) == 0xd2800001 {
            // Extract the immediate (offset within rodata)
            let offset = ((inst >> 5) & 0xffff) as u64;

            // Calculate the actual virtual address
            let addr = rodata_vaddr + offset;

            // Re-encode as MOVZ X1, #addr[15:0]
            let imm0 = (addr & 0xffff) as u32;
            let new_inst = 0xd2800001 | (imm0 << 5);
            code[i..i + 4].copy_from_slice(&new_inst.to_le_bytes());

            // Check for following MOVK instructions and patch them too
            let mut j = i + 4;
            let mut shift = 16u32;
            while j + 4 <= code.len() && shift < 64 {
                let next_inst =
                    u32::from_le_bytes([code[j], code[j + 1], code[j + 2], code[j + 3]]);

                // MOVK X1, #imm, LSL #shift
                // f2a00001 for LSL #16, f2c00001 for LSL #32, f2e00001 for LSL #48
                let movk_base = match shift {
                    16 => 0xf2a00001u32,
                    32 => 0xf2c00001u32,
                    48 => 0xf2e00001u32,
                    _ => break,
                };

                if (next_inst & 0xffe0001f) == movk_base {
                    let imm_shifted = ((addr >> shift) & 0xffff) as u32;
                    let new_movk = movk_base | (imm_shifted << 5);
                    code[j..j + 4].copy_from_slice(&new_movk.to_le_bytes());
                    j += 4;
                    shift += 16;
                } else {
                    break;
                }
            }
        }
        i += 4;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use debuginfo::read_skydbg;
    use mir::{AssertInfo, CmpOp, FunctionDebugInfo, InstKind, MirFunction, Reg, SourceSpan};

    #[test]
    fn test_generate_elf_simple() {
        let mut func = MirFunction::new_function("main".to_string());
        func.emit(InstKind::LoadImm {
            dst: Reg(0),
            value: 1,
        });
        func.emit(InstKind::LoadImm {
            dst: Reg(1),
            value: 1,
        });
        func.emit(InstKind::Cmp {
            op: CmpOp::Eq,
            dst: Reg(2),
            left: Reg(0),
            right: Reg(1),
        });
        func.emit_assert(
            Reg(2),
            AssertInfo {
                line: 1,
                col: 1,
                source: "1 == 1".to_string(),
            },
        );
        func.emit(InstKind::Ret);

        let elf = generate_elf(&func, "test.skyl");

        // Check ELF magic
        assert_eq!(&elf[0..4], &[0x7f, b'E', b'L', b'F']);

        // Check it's a 64-bit ELF
        assert_eq!(elf[4], 2);

        // Check it's little endian
        assert_eq!(elf[5], 1);

        // Check machine type is AArch64 (183 = 0xB7)
        assert_eq!(elf[18], 0xB7);
        assert_eq!(elf[19], 0x00);
    }

    #[test]
    fn test_patch_rodata() {
        // Create a minimal code sequence with a MOVZ X1 instruction
        let mut code = vec![0; 8];
        // MOVZ X1, #0x10 (offset 16 in rodata)
        let movz = 0xd2800001u32 | (0x10 << 5);
        code[0..4].copy_from_slice(&movz.to_le_bytes());

        // Patch with rodata_vaddr = 0x401000
        patch_rodata_addresses(&mut code, 0x401000);

        // The patched instruction should load 0x401010 (base + offset)
        let patched = u32::from_le_bytes([code[0], code[1], code[2], code[3]]);
        let patched_imm = (patched >> 5) & 0xffff;
        assert_eq!(patched_imm, 0x1010); // Low 16 bits of 0x401010
    }

    #[test]
    fn test_generate_elf_with_debug() {
        let mut func = MirFunction::new_function("main".to_string());
        func.debug_info = FunctionDebugInfo {
            span: Some(SourceSpan {
                source_id: "test.skyl".to_string(),
                line: 1,
                col: 1,
                end_line: 3,
                end_col: 1,
            }),
            source_id: Some("test.skyl".to_string()),
        };

        let span = SourceSpan {
            source_id: "test.skyl".to_string(),
            line: 2,
            col: 3,
            end_line: 2,
            end_col: 20,
        };

        func.emit_with_span(
            InstKind::LoadImm {
                dst: Reg(0),
                value: 1,
            },
            span.clone(),
        );
        func.emit_with_span(
            InstKind::LoadImm {
                dst: Reg(1),
                value: 1,
            },
            span.clone(),
        );
        func.emit_with_span(
            InstKind::Cmp {
                op: CmpOp::Eq,
                dst: Reg(2),
                left: Reg(0),
                right: Reg(1),
            },
            span,
        );
        func.emit_assert(
            Reg(2),
            AssertInfo {
                line: 2,
                col: 3,
                source: "1 == 1".to_string(),
            },
        );
        func.emit(InstKind::Ret);

        let result = generate_elf_with_debug(&func, "test.skyl");

        // Check ELF is valid
        assert_eq!(&result.elf[0..4], &[0x7f, b'E', b'L', b'F']);

        // Check we have debug sidecar
        assert!(result.debug_sidecar.is_some());

        // Verify debug sidecar can be read back
        let sidecar = result.debug_sidecar.unwrap();
        let debug_info = read_skydbg(&mut &sidecar[..]).unwrap();

        assert_eq!(debug_info.sources.len(), 1);
        assert_eq!(debug_info.symbols.len(), 1);
        assert_eq!(debug_info.symbols[0].name, "main");
        assert!(!debug_info.line_map.is_empty());
    }

    #[test]
    fn test_generate_elf_with_debug_no_spans() {
        let mut func = MirFunction::new_function("main".to_string());
        func.emit(InstKind::LoadImm {
            dst: Reg(0),
            value: 1,
        });
        func.emit(InstKind::Assert { cond: Reg(0), msg_id: 0 });
        func.asserts.push(AssertInfo {
            line: 1,
            col: 1,
            source: "1".to_string(),
        });
        func.emit(InstKind::Ret);

        let result = generate_elf_with_debug(&func, "test.skyl");

        // Check ELF is valid
        assert_eq!(&result.elf[0..4], &[0x7f, b'E', b'L', b'F']);

        // Should still have debug sidecar even without span info
        assert!(result.debug_sidecar.is_some());

        let sidecar = result.debug_sidecar.unwrap();
        let debug_info = read_skydbg(&mut &sidecar[..]).unwrap();

        // Should have the function symbol
        assert_eq!(debug_info.symbols.len(), 1);
    }

    #[test]
    fn test_execute_minimal_elf() {
        use std::fs::{self, File};
        use std::io::Write;
        use std::os::unix::fs::PermissionsExt;
        use std::process::Command;

        // Minimal function: just return (which calls exit(0))
        let mut func = MirFunction::new_function("main".to_string());
        func.emit(InstKind::Ret);

        let elf = generate_elf(&func, "test.skyl");

        // Write to temp file
        let binary_path = "/tmp/test_elf_minimal.elf";
        {
            let mut f = File::create(binary_path).unwrap();
            f.write_all(&elf).unwrap();
        }

        // Make executable
        let mut perms = fs::metadata(binary_path).unwrap().permissions();
        perms.set_mode(0o755);
        fs::set_permissions(binary_path, perms).unwrap();

        // Small delay to ensure file is written
        std::thread::sleep(std::time::Duration::from_millis(10));

        // Execute and check exit code
        let output = Command::new(binary_path)
            .output()
            .expect("Failed to execute binary");

        // Clean up
        let _ = fs::remove_file(binary_path);

        // Should exit with code 0
        assert!(
            output.status.success(),
            "Binary exited with non-zero status: {}. stderr: {}",
            output.status,
            String::from_utf8_lossy(&output.stderr)
        );
    }

    #[test]
    fn test_execute_elf_with_loads() {
        use std::fs::{self, File};
        use std::io::Write;
        use std::os::unix::fs::PermissionsExt;
        use std::process::Command;

        // Function with loads and compare but no assert
        let mut func = MirFunction::new_function("main".to_string());
        func.emit(InstKind::LoadImm {
            dst: Reg(0),
            value: 1,
        });
        func.emit(InstKind::LoadImm {
            dst: Reg(1),
            value: 1,
        });
        func.emit(InstKind::Cmp {
            op: CmpOp::Eq,
            dst: Reg(2),
            left: Reg(0),
            right: Reg(1),
        });
        func.emit(InstKind::Ret);

        let elf = generate_elf(&func, "test.skyl");

        // Write to temp file
        let binary_path = "/tmp/test_elf_loads.elf";
        {
            let mut f = File::create(binary_path).unwrap();
            f.write_all(&elf).unwrap();
        }

        // Make executable
        let mut perms = fs::metadata(binary_path).unwrap().permissions();
        perms.set_mode(0o755);
        fs::set_permissions(binary_path, perms).unwrap();

        std::thread::sleep(std::time::Duration::from_millis(10));

        let output = Command::new(binary_path)
            .output()
            .expect("Failed to execute binary");

        let _ = fs::remove_file(binary_path);

        assert!(
            output.status.success(),
            "Binary exited with non-zero status: {}. stderr: {}",
            output.status,
            String::from_utf8_lossy(&output.stderr)
        );
    }

    #[test]
    fn test_execute_generated_elf_with_assert() {
        use std::fs::{self, File};
        use std::io::Write;
        use std::os::unix::fs::PermissionsExt;
        use std::process::Command;

        let mut func = MirFunction::new_function("main".to_string());
        func.emit(InstKind::LoadImm {
            dst: Reg(0),
            value: 1,
        });
        func.emit(InstKind::LoadImm {
            dst: Reg(1),
            value: 1,
        });
        func.emit(InstKind::Cmp {
            op: CmpOp::Eq,
            dst: Reg(2),
            left: Reg(0),
            right: Reg(1),
        });
        func.emit_assert(
            Reg(2),
            AssertInfo {
                line: 1,
                col: 1,
                source: "1 == 1".to_string(),
            },
        );
        func.emit(InstKind::Ret);

        let elf = generate_elf(&func, "test.skyl");

        // Write to temp file
        let binary_path = "/tmp/test_elf_execute.elf";
        {
            let mut f = File::create(binary_path).unwrap();
            f.write_all(&elf).unwrap();
        }

        // Make executable
        let mut perms = fs::metadata(binary_path).unwrap().permissions();
        perms.set_mode(0o755);
        fs::set_permissions(binary_path, perms).unwrap();

        // Small delay to ensure file is written
        std::thread::sleep(std::time::Duration::from_millis(10));

        // Execute and check exit code
        let output = Command::new(binary_path)
            .output()
            .expect("Failed to execute binary");

        // Should exit with code 0 (assert passed)
        if !output.status.success() {
            // Keep binary for debugging
            eprintln!("Binary kept at: {}", binary_path);

            // Dump code bytes
            eprintln!("Code bytes (first 200):");
            for (i, byte) in elf.iter().skip(176).take(200).enumerate() {
                if i % 16 == 0 && i > 0 {
                    eprintln!();
                }
                if i % 4 == 0 {
                    eprint!(" ");
                }
                eprint!("{:02x}", byte);
            }
            eprintln!();

            panic!(
                "Binary exited with non-zero status: {}. stderr: {}",
                output.status,
                String::from_utf8_lossy(&output.stderr)
            );
        }

        // Clean up on success
        let _ = fs::remove_file(binary_path);
    }
}

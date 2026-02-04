//! AArch64 code generation for binary executables
//!
//! Generates position-independent code that uses Linux syscalls for I/O.

use codegen::{lower_instructions, lower_instructions_with_debug, ArmReg, Backend, Emitter};
use debuginfo::{DebugInfo, DebugInfoBuilder, SymbolKind};
use mir::{FunctionKind, MirFunction};
use std::collections::HashMap;

/// Linux syscall numbers for AArch64
const SYS_WRITE: u16 = 64;
const SYS_EXIT: u16 = 93;

/// File descriptor for stderr
const STDERR: u64 = 2;

/// Result of binary compilation
pub struct CompiledBinary {
    /// The generated machine code
    pub code: Vec<u8>,
    /// Read-only data section (assertion messages)
    pub rodata: Vec<u8>,
    /// Offset of the _start entry point within code
    pub entry_offset: usize,
    /// Optional debug information
    pub debug_info: Option<DebugInfo>,
}

/// Information about a stored assertion message
struct AssertMessage {
    /// Offset within rodata
    offset: usize,
    /// Length of the message
    len: usize,
}

/// ELF backend for standalone executables
///
/// Generates code that writes assertion failures to stderr and exits with code 1,
/// or exits with code 0 on success.
struct ElfBackend<'a> {
    messages: &'a [AssertMessage],
}

impl<'a> Backend for ElfBackend<'a> {
    fn emit_prologue(&self, emit: &mut Emitter) {
        // Save link register and callee-saved registers
        emit.stp_pre(ArmReg::X29, ArmReg::X30, -16);
        emit.stp_pre(ArmReg::X19, ArmReg::X20, -16);
        emit.stp_pre(ArmReg::X21, ArmReg::X22, -16);
    }

    fn emit_epilogue(&self, emit: &mut Emitter) {
        // Success: exit(0)
        emit.mov_imm16(ArmReg::X0, 0); // Exit code 0
        emit.mov_imm16(ArmReg::X8, SYS_EXIT);
        emit.svc(0);
    }

    fn emit_assert_failure(&self, emit: &mut Emitter, cond_hw: ArmReg, msg_id: u32) -> usize {
        // CBNZ - if condition is true (non-zero), skip the failure handling
        let branch_pos = emit.cbnz(cond_hw, 0); // Will patch later

        // Emit failure code inline (will be skipped if assertion passes)
        let msg = &self.messages[msg_id as usize];

        // write(stderr, msg, len)
        // X0 = fd (stderr = 2)
        // X1 = buf (will be filled in by linker/loader based on rodata)
        // X2 = count
        // X8 = syscall number

        // Load the message offset as a placeholder that will be patched later
        // with the actual virtual address (rodata_vaddr + offset).
        // We use mov_imm64_addr which always emits MOVZ + MOVK to ensure
        // there's a MOVK instruction available for patching the high bits.
        emit.mov_imm16(ArmReg::X0, STDERR as u16); // fd = stderr
        emit.mov_imm64_addr(ArmReg::X1, msg.offset as u64); // buf = offset (placeholder)
        emit.mov_imm64(ArmReg::X2, msg.len as i64); // count = length
        emit.mov_imm16(ArmReg::X8, SYS_WRITE);
        emit.svc(0);

        // exit(1)
        emit.mov_imm16(ArmReg::X0, 1); // Exit code 1
        emit.mov_imm16(ArmReg::X8, SYS_EXIT);
        emit.svc(0);

        branch_pos
    }
}

/// Compile a MIR function to binary code that writes assertion failures to stderr
pub fn compile_binary(func: &MirFunction, filename: &str) -> CompiledBinary {
    let mut rodata = Vec::new();
    let mut messages: Vec<AssertMessage> = Vec::new();

    // Pre-generate all assertion messages and store in rodata
    for info in &func.asserts {
        let msg = format!(
            "assertion failed at {}:{}:{}\n  assert({})\n",
            filename, info.line, info.col, info.source
        );
        let offset = rodata.len();
        rodata.extend_from_slice(msg.as_bytes());
        messages.push(AssertMessage {
            offset,
            len: msg.len(),
        });
    }

    let backend = ElfBackend { messages: &messages };
    let mut emitter = Emitter::new();

    // Generate _start (entry point)
    let entry_offset = emitter.len();

    // Emit prologue
    backend.emit_prologue(&mut emitter);

    // Lower all instructions
    lower_instructions(&mut emitter, &backend, &func.instructions);

    // Align code to 8 bytes
    emitter.align(8);

    CompiledBinary {
        code: emitter.into_code(),
        rodata,
        entry_offset,
        debug_info: None,
    }
}

/// Compile a MIR function to binary code with debug information
pub fn compile_binary_with_debug(func: &MirFunction, filename: &str) -> CompiledBinary {
    let mut rodata = Vec::new();
    let mut messages: Vec<AssertMessage> = Vec::new();
    let mut builder = DebugInfoBuilder::new();
    let mut source_map: HashMap<String, u32> = HashMap::new();

    // Pre-generate all assertion messages and store in rodata
    for info in &func.asserts {
        let msg = format!(
            "assertion failed at {}:{}:{}\n  assert({})\n",
            filename, info.line, info.col, info.source
        );
        let offset = rodata.len();
        rodata.extend_from_slice(msg.as_bytes());
        messages.push(AssertMessage {
            offset,
            len: msg.len(),
        });
    }

    let backend = ElfBackend { messages: &messages };
    let mut emitter = Emitter::new();

    // Generate _start (entry point)
    let entry_offset = emitter.len();

    // Emit prologue
    let prologue_start = emitter.len();
    backend.emit_prologue(&mut emitter);
    let prologue_end = emitter.len();

    // Add source file
    let source_id = if let Some(ref src_id) = func.debug_info.source_id {
        builder.add_source(src_id, None)
    } else {
        builder.add_source(filename, None)
    };

    // Begin function
    let (line, col) = func
        .debug_info
        .span
        .as_ref()
        .map(|s| (s.line, s.col))
        .unwrap_or((1, 1));

    let kind = match func.kind {
        FunctionKind::Test => SymbolKind::Test,
        FunctionKind::Function => SymbolKind::Function,
    };

    let func_id = builder.begin_function(&func.name, source_id, line, col, kind);

    // Lower all instructions with debug info collection
    let result = lower_instructions_with_debug(
        &mut emitter,
        &backend,
        &func.instructions,
        func_id,
        &mut |path, _| {
            if let Some(&id) = source_map.get(path) {
                id
            } else {
                let id = builder.add_source(path, None);
                source_map.insert(path.to_string(), id);
                id
            }
        },
    );

    // Add line mappings to builder
    for mapping in &result.line_mappings {
        builder.add_line_mapping(
            mapping.func_id,
            mapping.code_offset + prologue_end as u32,
            mapping.source_id,
            mapping.line,
            mapping.col,
            mapping.flags,
        );
    }

    // Align code to 8 bytes
    emitter.align(8);

    let code_size = emitter.len() as u32;
    builder.set_function_code_range(func_id, prologue_start as u32, code_size);
    builder.end_function();

    CompiledBinary {
        code: emitter.into_code(),
        rodata,
        entry_offset,
        debug_info: Some(builder.build()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use mir::{AssertInfo, CmpOp, FunctionDebugInfo, InstKind, MirFunction, Reg, SourceSpan};

    #[test]
    fn test_compile_simple() {
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

        let binary = compile_binary(&func, "test.skyl");
        assert!(!binary.code.is_empty());
        assert!(!binary.rodata.is_empty());
        assert!(binary.debug_info.is_none());
    }

    #[test]
    fn test_compile_with_debug() {
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

        let binary = compile_binary_with_debug(&func, "test.skyl");
        assert!(!binary.code.is_empty());
        assert!(!binary.rodata.is_empty());
        assert!(binary.debug_info.is_some());

        let debug_info = binary.debug_info.unwrap();
        assert_eq!(debug_info.sources.len(), 1);
        assert_eq!(debug_info.sources[0].path, "test.skyl");
        assert_eq!(debug_info.symbols.len(), 1);
        assert_eq!(debug_info.symbols[0].name, "main");
        assert_eq!(debug_info.symbols[0].kind, SymbolKind::Function);
        assert!(!debug_info.line_map.is_empty());
    }
}

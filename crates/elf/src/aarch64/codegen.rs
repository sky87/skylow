//! AArch64 code generation for binary executables
//!
//! Generates position-independent code that uses Linux syscalls for I/O.

use codegen::{lower_instructions, lower_instructions_with_debug, ArmReg, Backend, CallSite, Emitter};
use debuginfo::{DebugInfo, DebugInfoBuilder, SymbolKind};
use mir::{FunctionKind, MirFunction, MirProgram};
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

/// ELF backend for standalone executables (main function)
///
/// Generates code that writes assertion failures to stderr and exits with code 1,
/// or exits with code 0 on success.
struct ElfBackend<'a> {
    messages: &'a [AssertMessage],
}

impl<'a> Backend for ElfBackend<'a> {
    fn emit_prologue(&self, emit: &mut Emitter) {
        // Save frame pointer and link register
        emit.stp_pre(ArmReg::X29, ArmReg::X30, -48);
        // Set up frame pointer for stack unwinding
        emit.mov_from_sp(ArmReg::X29);
        // Save callee-saved registers we use
        emit.stp_offset(ArmReg::X19, ArmReg::X20, 16);
        emit.stp_offset(ArmReg::X21, ArmReg::X22, 32);
    }

    fn emit_epilogue(&self, emit: &mut Emitter) {
        // Success: exit(0)
        emit.mov_imm16(ArmReg::X0, 0); // Exit code 0
        emit.mov_imm16(ArmReg::X8, SYS_EXIT);
        emit.svc(0);
    }

    fn emit_epilogue_after_return(&self, emit: &mut Emitter) {
        // Return value is already in X0, use it as exit code
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

/// ELF backend for helper functions in multi-function programs
///
/// Unlike ElfBackend, this returns to caller instead of calling exit.
struct ElfFunctionBackend<'a> {
    messages: &'a [AssertMessage],
}

impl<'a> Backend for ElfFunctionBackend<'a> {
    fn emit_prologue(&self, emit: &mut Emitter) {
        // Save frame pointer and link register (needed for function calls)
        // STP x29, x30, [sp, #-48]!
        emit.stp_pre(ArmReg::X29, ArmReg::X30, -48);
        // Set up frame pointer for stack unwinding
        // MOV x29, sp
        emit.mov_from_sp(ArmReg::X29);
        // Save callee-saved registers we use
        // STP x19, x20, [sp, #16]
        emit.stp_offset(ArmReg::X19, ArmReg::X20, 16);
        // STP x21, x22, [sp, #32]
        emit.stp_offset(ArmReg::X21, ArmReg::X22, 32);
    }

    fn emit_epilogue(&self, emit: &mut Emitter) {
        // Success path: return 0
        emit.mov_imm16(ArmReg::X0, 0);

        // Restore callee-saved registers
        emit.ldp_offset(ArmReg::X21, ArmReg::X22, 32);
        emit.ldp_offset(ArmReg::X19, ArmReg::X20, 16);
        // Restore frame pointer and link register
        emit.ldp_post(ArmReg::X29, ArmReg::X30, 48);
        emit.ret();
    }

    fn emit_epilogue_after_return(&self, emit: &mut Emitter) {
        // Return value is already in X0
        // Restore callee-saved registers
        emit.ldp_offset(ArmReg::X21, ArmReg::X22, 32);
        emit.ldp_offset(ArmReg::X19, ArmReg::X20, 16);
        // Restore frame pointer and link register
        emit.ldp_post(ArmReg::X29, ArmReg::X30, 48);
        emit.ret();
    }

    fn emit_assert_failure(&self, emit: &mut Emitter, cond_hw: ArmReg, msg_id: u32) -> usize {
        // CBNZ - if condition is true (non-zero), skip the failure handling
        let branch_pos = emit.cbnz(cond_hw, 0); // Will patch later

        // Emit failure code inline (will be skipped if assertion passes)
        let msg = &self.messages[msg_id as usize];

        // write(stderr, msg, len)
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

    // Lower all instructions (including parameter setup)
    lower_instructions(&mut emitter, &backend, func);

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

    // Lower all instructions with debug info collection (including parameter setup)
    let result = lower_instructions_with_debug(
        &mut emitter,
        &backend,
        func,
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

/// A compiled function in a program with its entry point offset and call sites
pub struct CompiledBinaryFunction {
    /// Name of the function
    pub name: String,
    /// Offset within the combined code buffer where this function starts
    pub offset: usize,
    /// Call sites that need to be patched
    pub call_sites: Vec<CallSite>,
}

/// Result of compiling a program to binary code
pub struct CompiledBinaryProgram {
    /// Combined machine code for all functions
    pub code: Vec<u8>,
    /// Read-only data section (assertion messages)
    pub rodata: Vec<u8>,
    /// Compiled functions with their offsets
    pub functions: Vec<CompiledBinaryFunction>,
    /// Offset of the entry point (main function) within code
    pub entry_offset: usize,
    /// Optional debug information
    pub debug_info: Option<DebugInfo>,
}

/// Compile a MIR program to binary code with all functions linked
pub fn compile_program_binary(program: &MirProgram, filename: &str) -> CompiledBinaryProgram {
    let mut rodata = Vec::new();
    let mut messages: Vec<AssertMessage> = Vec::new();

    // Pre-generate all assertion messages from all functions
    for func in &program.functions {
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
    }

    // Create backends - entry function uses ElfBackend (exits), others use ElfFunctionBackend (returns)
    let main_backend = ElfBackend { messages: &messages };
    let func_backend = ElfFunctionBackend { messages: &messages };
    let mut emit = Emitter::new();
    let mut compiled_functions = Vec::new();
    let mut entry_offset = 0;

    // Check if there's a main function - if not, first function is entry
    let has_main = program.functions.iter().any(|f| f.name == "main");

    // First pass: compile all functions and collect their offsets
    for (idx, mir_func) in program.functions.iter().enumerate() {
        let func_offset = emit.len();
        let is_main = mir_func.name == "main";
        // Entry function: main if exists, otherwise first function
        let is_entry = is_main || (!has_main && idx == 0);

        // Track entry function
        if is_entry {
            entry_offset = func_offset;
        }

        // Use appropriate backend for this function
        let result = if is_entry {
            // Entry function: exits with exit(0) or exit(return_value)
            main_backend.emit_prologue(&mut emit);
            lower_instructions_with_debug(
                &mut emit,
                &main_backend,
                mir_func,
                0,
                &mut |_, _| 0,
            )
        } else {
            // Helper function: properly saves/restores registers and returns
            func_backend.emit_prologue(&mut emit);
            lower_instructions_with_debug(
                &mut emit,
                &func_backend,
                mir_func,
                0,
                &mut |_, _| 0,
            )
        };

        // Align to 8 bytes for next function
        emit.align(8);

        compiled_functions.push(CompiledBinaryFunction {
            name: mir_func.name.clone(),
            offset: func_offset,
            call_sites: result.call_sites,
        });
    }

    // Build function address map
    let func_offsets: HashMap<&str, usize> = compiled_functions
        .iter()
        .map(|f| (f.name.as_str(), f.offset))
        .collect();

    // Get mutable code for patching
    let mut code = emit.into_code();

    // Second pass: patch all call sites
    for func in &compiled_functions {
        for call_site in &func.call_sites {
            if let Some(&target_offset) = func_offsets.get(call_site.func_name.as_str()) {
                // call_site.bl_pos is already the absolute position in the code buffer
                let bl_abs_pos = call_site.bl_pos;
                // Calculate the relative offset from BL to target (in bytes)
                let rel_offset = target_offset as i64 - bl_abs_pos as i64;
                // BL uses 26-bit signed offset in 4-byte units
                let imm26 = (rel_offset >> 2) as i32;

                // Patch the BL instruction: opcode (0x94) | imm26
                let bl_inst = 0x94000000u32 | ((imm26 as u32) & 0x03FFFFFF);
                code[bl_abs_pos..bl_abs_pos + 4].copy_from_slice(&bl_inst.to_le_bytes());
            }
            // If function not found, leave BL as-is (will crash, but that's a bug in the source)
        }
    }

    CompiledBinaryProgram {
        code,
        rodata,
        functions: compiled_functions,
        entry_offset,
        debug_info: None,
    }
}

/// Compile a MIR program to binary code with debug information
pub fn compile_program_binary_with_debug(program: &MirProgram, filename: &str) -> CompiledBinaryProgram {
    let mut rodata = Vec::new();
    let mut messages: Vec<AssertMessage> = Vec::new();
    let mut builder = DebugInfoBuilder::new();
    let mut source_map: HashMap<String, u32> = HashMap::new();

    // Pre-generate all assertion messages from all functions
    for func in &program.functions {
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
    }

    // Create backends - entry function uses ElfBackend (exits), others use ElfFunctionBackend (returns)
    let main_backend = ElfBackend { messages: &messages };
    let func_backend = ElfFunctionBackend { messages: &messages };
    let mut emit = Emitter::new();
    let mut compiled_functions = Vec::new();
    let mut entry_offset = 0;

    // Check if there's a main function - if not, first function is entry
    let has_main = program.functions.iter().any(|f| f.name == "main");

    // First pass: compile all functions and collect their offsets
    for (idx, mir_func) in program.functions.iter().enumerate() {
        let func_offset = emit.len();
        let is_main = mir_func.name == "main";
        // Entry function: main if exists, otherwise first function
        let is_entry = is_main || (!has_main && idx == 0);

        // Track entry function
        if is_entry {
            entry_offset = func_offset;
        }

        // Add source file
        let source_id = if let Some(ref src_id) = mir_func.debug_info.source_id {
            if let Some(&id) = source_map.get(src_id) {
                id
            } else {
                let id = builder.add_source(src_id, None);
                source_map.insert(src_id.clone(), id);
                id
            }
        } else {
            if let Some(&id) = source_map.get(filename) {
                id
            } else {
                let id = builder.add_source(filename, None);
                source_map.insert(filename.to_string(), id);
                id
            }
        };

        // Begin function in debug info
        let (line, col) = mir_func
            .debug_info
            .span
            .as_ref()
            .map(|s| (s.line, s.col))
            .unwrap_or((1, 1));

        let kind = match mir_func.kind {
            FunctionKind::Test => SymbolKind::Test,
            FunctionKind::Function => SymbolKind::Function,
        };

        let dbg_func_id = builder.begin_function(&mir_func.name, source_id, line, col, kind);

        // Use appropriate backend for this function
        let prologue_start = emit.len();
        let result = if is_entry {
            main_backend.emit_prologue(&mut emit);
            let prologue_end = emit.len();
            let res = lower_instructions_with_debug(
                &mut emit,
                &main_backend,
                mir_func,
                dbg_func_id,
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
            // Add line mappings
            for mapping in &res.line_mappings {
                builder.add_line_mapping(
                    mapping.func_id,
                    mapping.code_offset + prologue_end as u32,
                    mapping.source_id,
                    mapping.line,
                    mapping.col,
                    mapping.flags,
                );
            }
            res
        } else {
            func_backend.emit_prologue(&mut emit);
            let prologue_end = emit.len();
            let res = lower_instructions_with_debug(
                &mut emit,
                &func_backend,
                mir_func,
                dbg_func_id,
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
            // Add line mappings
            for mapping in &res.line_mappings {
                builder.add_line_mapping(
                    mapping.func_id,
                    mapping.code_offset + prologue_end as u32,
                    mapping.source_id,
                    mapping.line,
                    mapping.col,
                    mapping.flags,
                );
            }
            res
        };

        // Align to 8 bytes for next function
        emit.align(8);

        let code_size = emit.len() as u32;
        builder.set_function_code_range(dbg_func_id, prologue_start as u32, code_size);

        // Add local variable debug info
        // Parameters are treated as locals for debugging (they live in registers too)
        for param in &mir_func.params {
            builder.add_local(dbg_func_id, &param.name, param.reg.0, 0, code_size);
        }
        // Add actual local variables
        for local in &mir_func.locals {
            builder.add_local(dbg_func_id, &local.name, local.reg.0, 0, code_size);
        }

        builder.end_function();

        compiled_functions.push(CompiledBinaryFunction {
            name: mir_func.name.clone(),
            offset: func_offset,
            call_sites: result.call_sites,
        });
    }

    // Build function address map
    let func_offsets: HashMap<&str, usize> = compiled_functions
        .iter()
        .map(|f| (f.name.as_str(), f.offset))
        .collect();

    // Get mutable code for patching
    let mut code = emit.into_code();

    // Second pass: patch all call sites
    for func in &compiled_functions {
        for call_site in &func.call_sites {
            if let Some(&target_offset) = func_offsets.get(call_site.func_name.as_str()) {
                let bl_abs_pos = call_site.bl_pos;
                let rel_offset = target_offset as i64 - bl_abs_pos as i64;
                let imm26 = (rel_offset >> 2) as i32;
                let bl_inst = 0x94000000u32 | ((imm26 as u32) & 0x03FFFFFF);
                code[bl_abs_pos..bl_abs_pos + 4].copy_from_slice(&bl_inst.to_le_bytes());
            }
        }
    }

    CompiledBinaryProgram {
        code,
        rodata,
        functions: compiled_functions,
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

    #[test]
    fn test_compile_program_binary() {
        use mir::{BinOp, MirProgram};

        // Create a program with two functions: add and main
        let mut program = MirProgram::default();

        // Helper function: add(a, b) -> a + b
        let mut add_func = MirFunction::new_function("add".to_string());
        let param_a = Reg(0);
        let param_b = Reg(1);
        add_func.params = vec![
            mir::MirParam { name: "a".to_string(), reg: param_a },
            mir::MirParam { name: "b".to_string(), reg: param_b },
        ];
        add_func.next_reg = 2; // Parameters consume first 2 regs
        let result = add_func.alloc_reg();
        add_func.emit(InstKind::BinOp {
            op: BinOp::Add,
            dst: result,
            left: param_a,
            right: param_b,
        });
        add_func.emit(InstKind::RetVal { value: result });
        program.functions.push(add_func);

        // Main function that calls add
        let mut main_func = MirFunction::new_function("main".to_string());
        let arg1 = main_func.alloc_reg();
        let arg2 = main_func.alloc_reg();
        let call_result = main_func.alloc_reg();
        let expected = main_func.alloc_reg();
        let cmp_result = main_func.alloc_reg();

        main_func.emit(InstKind::LoadImm { dst: arg1, value: 2 });
        main_func.emit(InstKind::LoadImm { dst: arg2, value: 3 });
        main_func.emit(InstKind::Call {
            dst: call_result,
            func_name: "add".to_string(),
            args: vec![arg1, arg2],
        });
        main_func.emit(InstKind::LoadImm { dst: expected, value: 5 });
        main_func.emit(InstKind::Cmp {
            op: CmpOp::Eq,
            dst: cmp_result,
            left: call_result,
            right: expected,
        });
        main_func.emit_assert(
            cmp_result,
            AssertInfo {
                line: 5,
                col: 3,
                source: "add(2, 3) == 5".to_string(),
            },
        );
        main_func.emit(InstKind::Ret);
        program.functions.push(main_func);

        // Compile the program
        let compiled = compile_program_binary(&program, "test.skyl");

        // Verify structure
        assert!(!compiled.code.is_empty());
        assert!(!compiled.rodata.is_empty());
        assert_eq!(compiled.functions.len(), 2);
        assert_eq!(compiled.functions[0].name, "add");
        assert_eq!(compiled.functions[1].name, "main");
        assert!(compiled.debug_info.is_none());

        // Main should be the entry point
        assert_eq!(compiled.entry_offset, compiled.functions[1].offset);
    }
}

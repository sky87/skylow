//! AArch64 code generation from MIR
//!
//! Compiles MIR functions to AArch64 machine code.

use codegen::{lower_instructions, lower_instructions_with_debug, ArmReg, Backend, CallSite, Emitter};
use debuginfo::{DebugInfo, DebugInfoBuilder, SymbolKind};
use mir::{FunctionKind, MirFunction, MirProgram};
use std::collections::HashMap;

/// Result of compiling a test function
pub struct CompiledTest {
    /// The compiled machine code
    pub code: Vec<u8>,
    /// Number of assert statements in the test
    pub num_asserts: u32,
    /// Optional debug information
    pub debug_info: Option<DebugInfo>,
}

/// A compiled function with its entry point offset and call sites
pub struct CompiledFunction {
    /// Name of the function
    pub name: String,
    /// Offset within the combined code buffer where this function starts
    pub offset: usize,
    /// Number of asserts in this function
    pub num_asserts: u32,
    /// Call sites that need to be patched (relative to function start)
    pub call_sites: Vec<CallSite>,
}

/// Result of compiling a whole program
pub struct CompiledProgram {
    /// Combined machine code for all functions
    pub code: Vec<u8>,
    /// Compiled functions with their offsets
    pub functions: Vec<CompiledFunction>,
}

/// JIT backend for test functions
///
/// Generates code that returns 0 on success or (failed_assert_index + 1) on failure.
struct JitBackend;

impl Backend for JitBackend {
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
        // CBNZ to skip failure path (branch if NOT zero = condition is true)
        let cbnz_pos = emit.cbnz(cond_hw, 0); // placeholder offset

        // Failure path: return (msg_id + 1)
        emit.mov_imm16(ArmReg::X0, (msg_id + 1) as u16);
        emit.ldp_offset(ArmReg::X21, ArmReg::X22, 32);
        emit.ldp_offset(ArmReg::X19, ArmReg::X20, 16);
        emit.ldp_post(ArmReg::X29, ArmReg::X30, 48);
        emit.ret();

        cbnz_pos
    }
}

/// Compile a MIR function to AArch64 machine code.
///
/// The generated function has signature: `extern "C" fn() -> u8`
/// Returns 0 on success, or (failed_assert_index + 1) on failure.
pub fn compile_function(func: &MirFunction) -> CompiledTest {
    let mut emit = Emitter::new();
    let backend = JitBackend;

    // Emit prologue
    backend.emit_prologue(&mut emit);

    // Lower all instructions (including parameter setup)
    let num_asserts = lower_instructions(&mut emit, &backend, func);

    CompiledTest {
        code: emit.into_code(),
        num_asserts,
        debug_info: None,
    }
}

/// Compile a MIR function to AArch64 machine code with debug information.
///
/// The generated function has signature: `extern "C" fn() -> u8`
/// Returns 0 on success, or (failed_assert_index + 1) on failure.
pub fn compile_function_with_debug(func: &MirFunction) -> CompiledTest {
    let mut emit = Emitter::new();
    let backend = JitBackend;
    let mut builder = DebugInfoBuilder::new();
    let mut source_map: HashMap<String, u32> = HashMap::new();

    // Record prologue position
    let prologue_start = emit.len();
    backend.emit_prologue(&mut emit);
    let prologue_end = emit.len();

    // Add source file from debug info
    let source_id = if let Some(ref src_id) = func.debug_info.source_id {
        builder.add_source(src_id, None)
    } else {
        builder.add_source("<unknown>", None)
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
        &mut emit,
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
            mapping.code_offset + prologue_end as u32, // Adjust for prologue
            mapping.source_id,
            mapping.line,
            mapping.col,
            mapping.flags,
        );
    }

    let code_size = emit.len() as u32;
    builder.set_function_code_range(func_id, prologue_start as u32, code_size);
    builder.end_function();

    CompiledTest {
        code: emit.into_code(),
        num_asserts: result.num_asserts,
        debug_info: Some(builder.build()),
    }
}

/// Compile a MIR program (multiple functions) to linked AArch64 machine code.
///
/// All functions are compiled into a single code buffer with call sites properly
/// linked so that functions can call each other.
pub fn compile_program(program: &MirProgram) -> CompiledProgram {
    let backend = JitBackend;
    let mut emit = Emitter::new();
    let mut compiled_functions = Vec::new();

    // First pass: compile all functions and collect their offsets
    for mir_func in &program.functions {
        let func_offset = emit.len();

        // Emit prologue
        backend.emit_prologue(&mut emit);

        // Lower all instructions (including parameter setup)
        let result = lower_instructions_with_debug(
            &mut emit,
            &backend,
            mir_func,
            0,
            &mut |_, _| 0,
        );

        compiled_functions.push(CompiledFunction {
            name: mir_func.name.clone(),
            offset: func_offset,
            num_asserts: result.num_asserts,
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

    CompiledProgram {
        code,
        functions: compiled_functions,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ExecutableMemory;
    use mir::{BinOp, CmpOp, FunctionDebugInfo, InstKind, MirFunction, SourceSpan};

    fn run_test(func: &MirFunction) -> u8 {
        let compiled = compile_function(func);
        let mem = ExecutableMemory::new(&compiled.code).expect("allocation failed");
        let test_fn: extern "C" fn() -> u8 = unsafe { mem.as_fn() };
        test_fn()
    }

    #[test]
    fn test_simple_assert_pass() {
        let mut func = MirFunction::new("test".to_string());
        let r0 = func.alloc_reg();
        let r1 = func.alloc_reg();
        let r2 = func.alloc_reg();

        func.emit(InstKind::LoadImm { dst: r0, value: 1 });
        func.emit(InstKind::LoadImm { dst: r1, value: 1 });
        func.emit(InstKind::Cmp { op: CmpOp::Eq, dst: r2, left: r0, right: r1 });
        func.emit(InstKind::Assert { cond: r2, msg_id: 0 });
        func.emit(InstKind::Ret);

        assert_eq!(run_test(&func), 0);
    }

    #[test]
    fn test_simple_assert_fail() {
        let mut func = MirFunction::new("test".to_string());
        let r0 = func.alloc_reg();
        let r1 = func.alloc_reg();
        let r2 = func.alloc_reg();

        func.emit(InstKind::LoadImm { dst: r0, value: 1 });
        func.emit(InstKind::LoadImm { dst: r1, value: 2 });
        func.emit(InstKind::Cmp { op: CmpOp::Eq, dst: r2, left: r0, right: r1 });
        func.emit(InstKind::Assert { cond: r2, msg_id: 0 });
        func.emit(InstKind::Ret);

        assert_eq!(run_test(&func), 1); // First assert failed
    }

    #[test]
    fn test_arithmetic() {
        let mut func = MirFunction::new("test".to_string());
        let r0 = func.alloc_reg();
        let r1 = func.alloc_reg();
        let r2 = func.alloc_reg();
        let r3 = func.alloc_reg();
        let r4 = func.alloc_reg();

        // 2 + 2 == 4
        func.emit(InstKind::LoadImm { dst: r0, value: 2 });
        func.emit(InstKind::LoadImm { dst: r1, value: 2 });
        func.emit(InstKind::BinOp { op: BinOp::Add, dst: r2, left: r0, right: r1 });
        func.emit(InstKind::LoadImm { dst: r3, value: 4 });
        func.emit(InstKind::Cmp { op: CmpOp::Eq, dst: r4, left: r2, right: r3 });
        func.emit(InstKind::Assert { cond: r4, msg_id: 0 });
        func.emit(InstKind::Ret);

        assert_eq!(run_test(&func), 0);
    }

    #[test]
    fn test_multiply() {
        let mut func = MirFunction::new("test".to_string());
        let r0 = func.alloc_reg();
        let r1 = func.alloc_reg();
        let r2 = func.alloc_reg();
        let r3 = func.alloc_reg();
        let r4 = func.alloc_reg();

        // 3 * 4 == 12
        func.emit(InstKind::LoadImm { dst: r0, value: 3 });
        func.emit(InstKind::LoadImm { dst: r1, value: 4 });
        func.emit(InstKind::BinOp { op: BinOp::Mul, dst: r2, left: r0, right: r1 });
        func.emit(InstKind::LoadImm { dst: r3, value: 12 });
        func.emit(InstKind::Cmp { op: CmpOp::Eq, dst: r4, left: r2, right: r3 });
        func.emit(InstKind::Assert { cond: r4, msg_id: 0 });
        func.emit(InstKind::Ret);

        assert_eq!(run_test(&func), 0);
    }

    #[test]
    fn test_divide() {
        let mut func = MirFunction::new("test".to_string());
        let r0 = func.alloc_reg();
        let r1 = func.alloc_reg();
        let r2 = func.alloc_reg();
        let r3 = func.alloc_reg();
        let r4 = func.alloc_reg();

        // 10 / 2 == 5
        func.emit(InstKind::LoadImm { dst: r0, value: 10 });
        func.emit(InstKind::LoadImm { dst: r1, value: 2 });
        func.emit(InstKind::BinOp { op: BinOp::Div, dst: r2, left: r0, right: r1 });
        func.emit(InstKind::LoadImm { dst: r3, value: 5 });
        func.emit(InstKind::Cmp { op: CmpOp::Eq, dst: r4, left: r2, right: r3 });
        func.emit(InstKind::Assert { cond: r4, msg_id: 0 });
        func.emit(InstKind::Ret);

        assert_eq!(run_test(&func), 0);
    }

    #[test]
    fn test_compile_with_debug_info() {
        let mut func = MirFunction::new("test_debug".to_string());
        func.debug_info = FunctionDebugInfo {
            span: Some(SourceSpan {
                source_id: "test.skyl".to_string(),
                line: 1,
                col: 1,
                end_line: 1,
                end_col: 10,
            }),
            source_id: Some("test.skyl".to_string()),
        };

        let span = SourceSpan {
            source_id: "test.skyl".to_string(),
            line: 2,
            col: 3,
            end_line: 2,
            end_col: 10,
        };

        let r0 = func.alloc_reg();
        let r1 = func.alloc_reg();
        let r2 = func.alloc_reg();

        func.emit_with_span(InstKind::LoadImm { dst: r0, value: 1 }, span.clone());
        func.emit_with_span(InstKind::LoadImm { dst: r1, value: 1 }, span.clone());
        func.emit_with_span(
            InstKind::Cmp { op: CmpOp::Eq, dst: r2, left: r0, right: r1 },
            span.clone(),
        );
        func.emit(InstKind::Assert { cond: r2, msg_id: 0 });
        func.emit(InstKind::Ret);

        let compiled = compile_function_with_debug(&func);
        assert!(compiled.debug_info.is_some());

        let debug_info = compiled.debug_info.unwrap();
        assert_eq!(debug_info.sources.len(), 1);
        assert_eq!(debug_info.sources[0].path, "test.skyl");
        assert_eq!(debug_info.symbols.len(), 1);
        assert_eq!(debug_info.symbols[0].name, "test_debug");
        assert_eq!(debug_info.symbols[0].kind, SymbolKind::Test);
        assert!(!debug_info.line_map.is_empty());

        // Verify the code still works
        let mem = ExecutableMemory::new(&compiled.code).expect("allocation failed");
        let test_fn: extern "C" fn() -> u8 = unsafe { mem.as_fn() };
        assert_eq!(test_fn(), 0);
    }

    #[test]
    fn test_compile_function_with_debug_no_spans() {
        let mut func = MirFunction::new("simple".to_string());
        let r0 = func.alloc_reg();

        func.emit(InstKind::LoadImm { dst: r0, value: 1 });
        func.emit(InstKind::Assert { cond: r0, msg_id: 0 });
        func.emit(InstKind::Ret);

        let compiled = compile_function_with_debug(&func);
        assert!(compiled.debug_info.is_some());

        let debug_info = compiled.debug_info.unwrap();
        // Still has function symbol even without spans
        assert_eq!(debug_info.symbols.len(), 1);
        // No line mappings since no spans
        assert!(debug_info.line_map.is_empty());
    }
}

//! AArch64 instruction lowering
//!
//! Converts MIR instructions to AArch64 machine code using a Backend trait
//! that allows different implementations for JIT and ELF code generation.

use debuginfo::{LineMapping, LineMappingFlags};
use mir::{BinOp, CmpOp, Inst, InstKind};

use super::emit::{ArmReg, Cond, Emitter};
use super::regalloc::SimpleRegAlloc;

/// Backend trait for customizing code generation
///
/// Different backends (JIT, ELF) implement this trait to provide
/// backend-specific prologue, epilogue, and assertion failure handling.
pub trait Backend {
    /// Emit function prologue (save registers, setup frame)
    fn emit_prologue(&self, emit: &mut Emitter);

    /// Emit function epilogue (restore registers, return)
    fn emit_epilogue(&self, emit: &mut Emitter);

    /// Emit assertion failure handling code
    ///
    /// Returns the position of the CBNZ branch instruction for patching.
    /// The caller will patch this branch to skip to the success path.
    fn emit_assert_failure(&self, emit: &mut Emitter, cond_hw: ArmReg, msg_id: u32) -> usize;
}

/// Convert a CmpOp to an AArch64 condition code
pub fn cmp_op_to_cond(op: CmpOp) -> Cond {
    match op {
        CmpOp::Eq => Cond::Eq,
        CmpOp::Neq => Cond::Ne,
        CmpOp::Lt => Cond::Lt,
        CmpOp::Lte => Cond::Le,
        CmpOp::Gt => Cond::Gt,
        CmpOp::Gte => Cond::Ge,
    }
}

/// Lower a LoadImm instruction
pub fn lower_load_imm(emit: &mut Emitter, regalloc: &mut SimpleRegAlloc, dst: mir::Reg, value: i64) {
    let hw_reg = regalloc.alloc(dst);
    emit.mov_imm64(hw_reg, value);
}

/// Lower a BinOp instruction
pub fn lower_binop(
    emit: &mut Emitter,
    regalloc: &mut SimpleRegAlloc,
    op: BinOp,
    dst: mir::Reg,
    left: mir::Reg,
    right: mir::Reg,
) {
    let left_hw = regalloc.get(left);
    let right_hw = regalloc.get(right);
    let dst_hw = regalloc.alloc(dst);

    match op {
        BinOp::Add => emit.add_reg(dst_hw, left_hw, right_hw),
        BinOp::Sub => emit.sub_reg(dst_hw, left_hw, right_hw),
        BinOp::Mul => emit.mul_reg(dst_hw, left_hw, right_hw),
        BinOp::Div => emit.sdiv_reg(dst_hw, left_hw, right_hw),
    }
}

/// Lower a Cmp instruction
pub fn lower_cmp(
    emit: &mut Emitter,
    regalloc: &mut SimpleRegAlloc,
    op: CmpOp,
    dst: mir::Reg,
    left: mir::Reg,
    right: mir::Reg,
) {
    let left_hw = regalloc.get(left);
    let right_hw = regalloc.get(right);
    let dst_hw = regalloc.alloc(dst);

    emit.cmp_reg(left_hw, right_hw);
    emit.cset(dst_hw, cmp_op_to_cond(op));
}

/// Result of debug-aware instruction lowering
#[derive(Debug, Clone, Default)]
pub struct LowerResult {
    /// Number of assert statements encountered
    pub num_asserts: u32,
    /// Line mappings for debug info
    pub line_mappings: Vec<LineMapping>,
}

/// Lower MIR instructions to AArch64 machine code
///
/// Returns the number of assert statements encountered.
pub fn lower_instructions<B: Backend>(
    emit: &mut Emitter,
    backend: &B,
    instructions: &[Inst],
) -> u32 {
    lower_instructions_with_debug(emit, backend, instructions, 0, &mut |_, _| 0).num_asserts
}

/// Lower MIR instructions to AArch64 machine code with debug info collection
///
/// This function collects line mappings for each instruction that has span
/// information. The `source_resolver` callback is used to resolve source IDs
/// from source paths.
///
/// Returns a `LowerResult` containing assert count and line mappings.
pub fn lower_instructions_with_debug<B, F>(
    emit: &mut Emitter,
    backend: &B,
    instructions: &[Inst],
    func_id: u32,
    source_resolver: &mut F,
) -> LowerResult
where
    B: Backend,
    F: FnMut(&str, &mut Vec<LineMapping>) -> u32,
{
    let mut regalloc = SimpleRegAlloc::new();
    let mut result = LowerResult::default();

    for inst in instructions {
        // Record code offset before emitting instruction
        let code_offset = emit.len() as u32;

        // If we have span info, create a line mapping
        if let Some(span) = &inst.span {
            let source_id = source_resolver(&span.source_id, &mut result.line_mappings);
            let flags = match &inst.kind {
                InstKind::Assert { .. } => LineMappingFlags::STATEMENT,
                InstKind::Ret => LineMappingFlags::EPILOGUE,
                _ => LineMappingFlags::empty(),
            };
            result.line_mappings.push(LineMapping {
                func_id,
                code_offset,
                source_id,
                line: span.line,
                col: span.col,
                flags,
            });
        }

        match &inst.kind {
            InstKind::LoadImm { dst, value } => {
                lower_load_imm(emit, &mut regalloc, *dst, *value);
            }
            InstKind::BinOp { op, dst, left, right } => {
                lower_binop(emit, &mut regalloc, *op, *dst, *left, *right);
            }
            InstKind::Cmp { op, dst, left, right } => {
                lower_cmp(emit, &mut regalloc, *op, *dst, *left, *right);
            }
            InstKind::Assert { cond, msg_id } => {
                let cond_hw = regalloc.get(*cond);
                let branch_pos = backend.emit_assert_failure(emit, cond_hw, *msg_id);
                // Patch branch immediately to skip to here (continuation point)
                emit.patch_branch(branch_pos, emit.len());
                result.num_asserts += 1;
            }
            InstKind::Ret => {
                backend.emit_epilogue(emit);
            }
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use mir::{InstKind, MirFunction, Reg, SourceSpan};

    /// Test backend that does minimal work
    struct TestBackend;

    impl Backend for TestBackend {
        fn emit_prologue(&self, emit: &mut Emitter) {
            // Just save one pair for testing
            emit.stp_pre(ArmReg::X19, ArmReg::X20, -16);
        }

        fn emit_epilogue(&self, emit: &mut Emitter) {
            emit.mov_imm16(ArmReg::X0, 0);
            emit.ldp_post(ArmReg::X19, ArmReg::X20, 16);
            emit.ret();
        }

        fn emit_assert_failure(&self, emit: &mut Emitter, cond_hw: ArmReg, msg_id: u32) -> usize {
            let branch_pos = emit.cbnz(cond_hw, 0);
            emit.mov_imm16(ArmReg::X0, (msg_id + 1) as u16);
            emit.ldp_post(ArmReg::X19, ArmReg::X20, 16);
            emit.ret();
            branch_pos
        }
    }

    #[test]
    fn test_cmp_op_to_cond() {
        assert_eq!(cmp_op_to_cond(CmpOp::Eq), Cond::Eq);
        assert_eq!(cmp_op_to_cond(CmpOp::Neq), Cond::Ne);
        assert_eq!(cmp_op_to_cond(CmpOp::Lt), Cond::Lt);
        assert_eq!(cmp_op_to_cond(CmpOp::Lte), Cond::Le);
        assert_eq!(cmp_op_to_cond(CmpOp::Gt), Cond::Gt);
        assert_eq!(cmp_op_to_cond(CmpOp::Gte), Cond::Ge);
    }

    #[test]
    fn test_lower_load_imm() {
        let mut emit = Emitter::new();
        let mut regalloc = SimpleRegAlloc::new();
        lower_load_imm(&mut emit, &mut regalloc, Reg(0), 42);
        assert!(emit.len() > 0);
    }

    #[test]
    fn test_lower_binop() {
        let mut emit = Emitter::new();
        let mut regalloc = SimpleRegAlloc::new();
        regalloc.alloc(Reg(0));
        regalloc.alloc(Reg(1));
        lower_binop(&mut emit, &mut regalloc, BinOp::Add, Reg(2), Reg(0), Reg(1));
        assert!(emit.len() > 0);
    }

    #[test]
    fn test_lower_cmp() {
        let mut emit = Emitter::new();
        let mut regalloc = SimpleRegAlloc::new();
        regalloc.alloc(Reg(0));
        regalloc.alloc(Reg(1));
        lower_cmp(&mut emit, &mut regalloc, CmpOp::Eq, Reg(2), Reg(0), Reg(1));
        assert!(emit.len() > 0);
    }

    #[test]
    fn test_lower_instructions() {
        let mut func = MirFunction::new("test".to_string());
        let r0 = func.alloc_reg();
        let r1 = func.alloc_reg();
        let r2 = func.alloc_reg();

        func.emit(InstKind::LoadImm { dst: r0, value: 1 });
        func.emit(InstKind::LoadImm { dst: r1, value: 1 });
        func.emit(InstKind::Cmp { op: CmpOp::Eq, dst: r2, left: r0, right: r1 });
        func.emit(InstKind::Assert { cond: r2, msg_id: 0 });
        func.emit(InstKind::Ret);

        let backend = TestBackend;
        let mut emit = Emitter::new();
        backend.emit_prologue(&mut emit);
        let num_asserts = lower_instructions(&mut emit, &backend, &func.instructions);

        assert_eq!(num_asserts, 1);
        assert!(emit.len() > 0);
    }

    #[test]
    fn test_lower_multiple_asserts() {
        let mut func = MirFunction::new("test".to_string());
        let r0 = func.alloc_reg();
        let r1 = func.alloc_reg();
        let r2 = func.alloc_reg();

        func.emit(InstKind::LoadImm { dst: r0, value: 1 });
        func.emit(InstKind::LoadImm { dst: r1, value: 1 });
        func.emit(InstKind::Cmp { op: CmpOp::Eq, dst: r2, left: r0, right: r1 });
        func.emit(InstKind::Assert { cond: r2, msg_id: 0 });
        func.emit(InstKind::Assert { cond: r2, msg_id: 1 });
        func.emit(InstKind::Ret);

        let backend = TestBackend;
        let mut emit = Emitter::new();
        backend.emit_prologue(&mut emit);
        let num_asserts = lower_instructions(&mut emit, &backend, &func.instructions);

        assert_eq!(num_asserts, 2);
    }

    #[test]
    fn test_lower_all_binops() {
        let mut emit = Emitter::new();
        let mut regalloc = SimpleRegAlloc::new();
        regalloc.alloc(Reg(0));
        regalloc.alloc(Reg(1));

        lower_binop(&mut emit, &mut regalloc, BinOp::Add, Reg(2), Reg(0), Reg(1));
        lower_binop(&mut emit, &mut regalloc, BinOp::Sub, Reg(3), Reg(0), Reg(1));
        lower_binop(&mut emit, &mut regalloc, BinOp::Mul, Reg(4), Reg(0), Reg(1));
        lower_binop(&mut emit, &mut regalloc, BinOp::Div, Reg(5), Reg(0), Reg(1));
        assert!(emit.len() > 0);
    }

    #[test]
    fn test_lower_result_default() {
        let result = LowerResult::default();
        assert_eq!(result.num_asserts, 0);
        assert!(result.line_mappings.is_empty());
    }

    #[test]
    fn test_lower_instructions_with_debug() {
        let mut func = MirFunction::new("test".to_string());
        let r0 = func.alloc_reg();
        let r1 = func.alloc_reg();
        let r2 = func.alloc_reg();

        let span = SourceSpan {
            source_id: "test.skyl".to_string(),
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 10,
        };

        func.emit_with_span(InstKind::LoadImm { dst: r0, value: 1 }, span.clone());
        func.emit_with_span(InstKind::LoadImm { dst: r1, value: 1 }, span.clone());
        func.emit_with_span(
            InstKind::Cmp { op: CmpOp::Eq, dst: r2, left: r0, right: r1 },
            span.clone(),
        );
        func.emit(InstKind::Assert { cond: r2, msg_id: 0 });
        func.emit(InstKind::Ret);

        let backend = TestBackend;
        let mut emit = Emitter::new();
        backend.emit_prologue(&mut emit);

        let mut source_id_map = std::collections::HashMap::new();
        let result = lower_instructions_with_debug(
            &mut emit,
            &backend,
            &func.instructions,
            0,
            &mut |path, _| {
                let next_id = source_id_map.len() as u32;
                *source_id_map.entry(path.to_string()).or_insert(next_id)
            },
        );

        assert_eq!(result.num_asserts, 1);
        assert_eq!(result.line_mappings.len(), 3); // 3 instructions with spans
        assert_eq!(result.line_mappings[0].func_id, 0);
        assert_eq!(result.line_mappings[0].line, 1);
        assert_eq!(result.line_mappings[0].col, 1);
    }

    #[test]
    fn test_lower_instructions_with_debug_flags() {
        let mut func = MirFunction::new("test".to_string());
        let r0 = func.alloc_reg();

        let span = SourceSpan {
            source_id: "test.skyl".to_string(),
            line: 1,
            col: 1,
            end_line: 1,
            end_col: 10,
        };

        func.emit_with_span(InstKind::LoadImm { dst: r0, value: 1 }, span.clone());
        func.emit_assert_with_span(
            r0,
            mir::AssertInfo {
                line: 2,
                col: 1,
                source: "assert(1)".to_string(),
            },
            SourceSpan {
                source_id: "test.skyl".to_string(),
                line: 2,
                col: 1,
                end_line: 2,
                end_col: 10,
            },
        );
        func.emit_with_span(InstKind::Ret, span);

        let backend = TestBackend;
        let mut emit = Emitter::new();
        backend.emit_prologue(&mut emit);

        let result = lower_instructions_with_debug(
            &mut emit,
            &backend,
            &func.instructions,
            0,
            &mut |_, _| 0,
        );

        assert_eq!(result.line_mappings.len(), 3);
        // LoadImm has no special flags
        assert!(result.line_mappings[0].flags.is_empty());
        // Assert has STATEMENT flag
        assert!(result.line_mappings[1].flags.contains(LineMappingFlags::STATEMENT));
        // Ret has EPILOGUE flag
        assert!(result.line_mappings[2].flags.contains(LineMappingFlags::EPILOGUE));
    }
}

//! AArch64 code generation from MIR
//!
//! Compiles MIR functions to AArch64 machine code.

use codegen::{lower_instructions, ArmReg, Backend, Emitter};
use mir::MirFunction;

/// Result of compiling a test function
pub struct CompiledTest {
    /// The compiled machine code
    pub code: Vec<u8>,
    /// Number of assert statements in the test
    pub num_asserts: u32,
}

/// JIT backend for test functions
///
/// Generates code that returns 0 on success or (failed_assert_index + 1) on failure.
struct JitBackend;

impl Backend for JitBackend {
    fn emit_prologue(&self, emit: &mut Emitter) {
        // Save callee-saved registers we use
        // STP x19, x20, [sp, #-32]!
        emit.stp_pre(ArmReg::X19, ArmReg::X20, -32);
        // STP x21, x22, [sp, #-16]!
        emit.stp_pre(ArmReg::X21, ArmReg::X22, -16);
    }

    fn emit_epilogue(&self, emit: &mut Emitter) {
        // Success path: return 0
        emit.mov_imm16(ArmReg::X0, 0);

        // Restore callee-saved registers
        emit.ldp_post(ArmReg::X21, ArmReg::X22, 16);
        emit.ldp_post(ArmReg::X19, ArmReg::X20, 32);
        emit.ret();
    }

    fn emit_assert_failure(&self, emit: &mut Emitter, cond_hw: ArmReg, msg_id: u32) -> usize {
        // CBNZ to skip failure path (branch if NOT zero = condition is true)
        let cbnz_pos = emit.cbnz(cond_hw, 0); // placeholder offset

        // Failure path: return (msg_id + 1)
        emit.mov_imm16(ArmReg::X0, (msg_id + 1) as u16);
        emit.ldp_post(ArmReg::X21, ArmReg::X22, 16);
        emit.ldp_post(ArmReg::X19, ArmReg::X20, 32);
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

    // Lower all instructions
    let num_asserts = lower_instructions(&mut emit, &backend, &func.instructions);

    CompiledTest {
        code: emit.into_code(),
        num_asserts,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ExecutableMemory;
    use mir::{BinOp, CmpOp, Inst, MirFunction};

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

        func.emit(Inst::LoadImm { dst: r0, value: 1 });
        func.emit(Inst::LoadImm { dst: r1, value: 1 });
        func.emit(Inst::Cmp { op: CmpOp::Eq, dst: r2, left: r0, right: r1 });
        func.emit(Inst::Assert { cond: r2, msg_id: 0 });
        func.emit(Inst::Ret);

        assert_eq!(run_test(&func), 0);
    }

    #[test]
    fn test_simple_assert_fail() {
        let mut func = MirFunction::new("test".to_string());
        let r0 = func.alloc_reg();
        let r1 = func.alloc_reg();
        let r2 = func.alloc_reg();

        func.emit(Inst::LoadImm { dst: r0, value: 1 });
        func.emit(Inst::LoadImm { dst: r1, value: 2 });
        func.emit(Inst::Cmp { op: CmpOp::Eq, dst: r2, left: r0, right: r1 });
        func.emit(Inst::Assert { cond: r2, msg_id: 0 });
        func.emit(Inst::Ret);

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
        func.emit(Inst::LoadImm { dst: r0, value: 2 });
        func.emit(Inst::LoadImm { dst: r1, value: 2 });
        func.emit(Inst::BinOp { op: BinOp::Add, dst: r2, left: r0, right: r1 });
        func.emit(Inst::LoadImm { dst: r3, value: 4 });
        func.emit(Inst::Cmp { op: CmpOp::Eq, dst: r4, left: r2, right: r3 });
        func.emit(Inst::Assert { cond: r4, msg_id: 0 });
        func.emit(Inst::Ret);

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
        func.emit(Inst::LoadImm { dst: r0, value: 3 });
        func.emit(Inst::LoadImm { dst: r1, value: 4 });
        func.emit(Inst::BinOp { op: BinOp::Mul, dst: r2, left: r0, right: r1 });
        func.emit(Inst::LoadImm { dst: r3, value: 12 });
        func.emit(Inst::Cmp { op: CmpOp::Eq, dst: r4, left: r2, right: r3 });
        func.emit(Inst::Assert { cond: r4, msg_id: 0 });
        func.emit(Inst::Ret);

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
        func.emit(Inst::LoadImm { dst: r0, value: 10 });
        func.emit(Inst::LoadImm { dst: r1, value: 2 });
        func.emit(Inst::BinOp { op: BinOp::Div, dst: r2, left: r0, right: r1 });
        func.emit(Inst::LoadImm { dst: r3, value: 5 });
        func.emit(Inst::Cmp { op: CmpOp::Eq, dst: r4, left: r2, right: r3 });
        func.emit(Inst::Assert { cond: r4, msg_id: 0 });
        func.emit(Inst::Ret);

        assert_eq!(run_test(&func), 0);
    }
}

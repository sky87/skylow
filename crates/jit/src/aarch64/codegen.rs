//! AArch64 code generation from MIR
//!
//! Compiles MIR functions to AArch64 machine code.

use skylow_mir::{BinOp, CmpOp, Inst, MirFunction, Reg};

use super::emit::{ArmReg, Cond, Emitter};

/// Result of compiling a test function
pub struct CompiledTest {
    /// The compiled machine code
    pub code: Vec<u8>,
    /// Number of assert statements in the test
    pub num_asserts: u32,
}

/// Compile a MIR function to AArch64 machine code.
///
/// The generated function has signature: `extern "C" fn() -> u8`
/// Returns 0 on success, or (failed_assert_index + 1) on failure.
pub fn compile_function(func: &MirFunction) -> CompiledTest {
    let mut emit = Emitter::new();
    let mut regalloc = SimpleRegAlloc::new();
    let mut num_asserts = 0u32;

    // Function prologue - save callee-saved registers we use
    // STP x19, x20, [sp, #-32]!
    emit.stp_pre(ArmReg::X19, ArmReg::X20, -32);
    // STP x21, x22, [sp, #16] - but simpler to just do another pre-index
    emit.stp_pre(ArmReg::X21, ArmReg::X22, -16);

    for inst in &func.instructions {
        compile_inst(&mut emit, &mut regalloc, inst, &mut num_asserts);
    }

    // Success path: return 0
    emit.mov_imm16(ArmReg::X0, 0);

    // Function epilogue
    emit.ldp_post(ArmReg::X21, ArmReg::X22, 16);
    emit.ldp_post(ArmReg::X19, ArmReg::X20, 32);
    emit.ret();

    CompiledTest {
        code: emit.into_code(),
        num_asserts,
    }
}

fn compile_inst(
    emit: &mut Emitter,
    regalloc: &mut SimpleRegAlloc,
    inst: &Inst,
    num_asserts: &mut u32,
) {
    match inst {
        Inst::LoadImm { dst, value } => {
            let hw_reg = regalloc.get(*dst);
            emit.mov_imm64(hw_reg, *value);
        }
        Inst::BinOp { op, dst, left, right } => {
            let left_hw = regalloc.get(*left);
            let right_hw = regalloc.get(*right);
            let dst_hw = regalloc.get(*dst);

            match op {
                BinOp::Add => emit.add_reg(dst_hw, left_hw, right_hw),
                BinOp::Sub => emit.sub_reg(dst_hw, left_hw, right_hw),
                BinOp::Mul => emit.mul_reg(dst_hw, left_hw, right_hw),
                BinOp::Div => emit.sdiv_reg(dst_hw, left_hw, right_hw),
            }
        }
        Inst::Cmp { op, dst, left, right } => {
            let left_hw = regalloc.get(*left);
            let right_hw = regalloc.get(*right);
            let dst_hw = regalloc.get(*dst);

            emit.cmp_reg(left_hw, right_hw);

            let cond = match op {
                CmpOp::Eq => Cond::Eq,
                CmpOp::Neq => Cond::Ne,
                CmpOp::Lt => Cond::Lt,
                CmpOp::Lte => Cond::Le,
                CmpOp::Gt => Cond::Gt,
                CmpOp::Gte => Cond::Ge,
            };
            emit.cset(dst_hw, cond);
        }
        Inst::Assert { cond, msg_id } => {
            let cond_hw = regalloc.get(*cond);

            // CBNZ to skip failure path (branch if NOT zero = condition is true)
            let cbnz_pos = emit.cbnz(cond_hw, 0); // placeholder offset

            // Failure path: return (msg_id + 1)
            emit.mov_imm16(ArmReg::X0, (*msg_id + 1) as u16);
            emit.ldp_post(ArmReg::X21, ArmReg::X22, 16);
            emit.ldp_post(ArmReg::X19, ArmReg::X20, 32);
            emit.ret();

            // Patch the CBNZ to skip to here (success path continues)
            let target = emit.len();
            emit.patch_branch(cbnz_pos, target);

            *num_asserts += 1;
        }
        Inst::Ret => {
            // Ret is handled at the end of compile_function
        }
    }
}

/// Simple register allocator
///
/// Maps virtual registers to hardware registers in a round-robin fashion.
/// Uses callee-saved registers x19-x22.
struct SimpleRegAlloc {
    mapping: Vec<ArmReg>,
}

impl SimpleRegAlloc {
    fn new() -> Self {
        Self { mapping: Vec::new() }
    }

    fn get(&mut self, reg: Reg) -> ArmReg {
        let idx = reg.0 as usize;
        while self.mapping.len() <= idx {
            let next = self.next_reg();
            self.mapping.push(next);
        }
        self.mapping[idx]
    }

    fn next_reg(&self) -> ArmReg {
        // Use callee-saved registers
        const REGS: [ArmReg; 4] = [ArmReg::X19, ArmReg::X20, ArmReg::X21, ArmReg::X22];
        REGS[self.mapping.len() % REGS.len()]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ExecutableMemory;
    use skylow_mir::MirFunction;

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

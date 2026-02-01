//! AArch64 code generation for binary executables
//!
//! Generates position-independent code that uses Linux syscalls for I/O.

use skylow_jit::{ArmReg, Cond, Emitter};
use skylow_mir::{BinOp, CmpOp, Inst, MirFunction, Reg};

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
}

/// A pending assertion branch that needs patching
struct PendingAssert {
    /// Position of the CBNZ instruction
    branch_pos: usize,
}

/// Information about a stored assertion message
struct AssertMessage {
    /// Offset within rodata
    offset: usize,
    /// Length of the message
    len: usize,
}

/// Compile a MIR function to binary code that writes assertion failures to stderr
pub fn compile_binary(func: &MirFunction, filename: &str) -> CompiledBinary {
    let mut emitter = Emitter::new();
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

    // Generate _start (entry point)
    let entry_offset = emitter.len();

    // Prologue: save link register and callee-saved registers
    emitter.stp_pre(ArmReg::X29, ArmReg::X30, -16);
    emitter.stp_pre(ArmReg::X19, ArmReg::X20, -16);
    emitter.stp_pre(ArmReg::X21, ArmReg::X22, -16);

    // Register allocation: use X19-X22 for virtual registers
    let mut reg_alloc = SimpleRegAlloc::new();
    let mut pending_asserts: Vec<PendingAssert> = Vec::new();

    // Compile each instruction
    for inst in &func.instructions {
        match inst {
            Inst::LoadImm { dst, value } => {
                let hw_reg = reg_alloc.alloc(*dst);
                emitter.mov_imm64(hw_reg, *value);
            }
            Inst::BinOp {
                op,
                dst,
                left,
                right,
            } => {
                let left_hw = reg_alloc.get(*left);
                let right_hw = reg_alloc.get(*right);
                let dst_hw = reg_alloc.alloc(*dst);
                match op {
                    BinOp::Add => emitter.add_reg(dst_hw, left_hw, right_hw),
                    BinOp::Sub => emitter.sub_reg(dst_hw, left_hw, right_hw),
                    BinOp::Mul => emitter.mul_reg(dst_hw, left_hw, right_hw),
                    BinOp::Div => emitter.sdiv_reg(dst_hw, left_hw, right_hw),
                }
            }
            Inst::Cmp { op, dst, left, right } => {
                let left_hw = reg_alloc.get(*left);
                let right_hw = reg_alloc.get(*right);
                let dst_hw = reg_alloc.alloc(*dst);
                emitter.cmp_reg(left_hw, right_hw);
                let cond = match op {
                    CmpOp::Eq => Cond::Eq,
                    CmpOp::Neq => Cond::Ne,
                    CmpOp::Lt => Cond::Lt,
                    CmpOp::Lte => Cond::Le,
                    CmpOp::Gt => Cond::Gt,
                    CmpOp::Gte => Cond::Ge,
                };
                emitter.cset(dst_hw, cond);
            }
            Inst::Assert { cond, msg_id } => {
                let cond_hw = reg_alloc.get(*cond);
                // CBNZ - if condition is true (non-zero), skip the failure handling
                let branch_pos = emitter.cbnz(cond_hw, 0); // Will patch later
                pending_asserts.push(PendingAssert { branch_pos });

                // Emit failure code inline (will be skipped if assertion passes)
                emit_assertion_failure(&mut emitter, &messages[*msg_id as usize]);
            }
            Inst::Ret => {
                // Patch all pending assert branches to skip to here
                for pending in &pending_asserts {
                    emitter.patch_branch(pending.branch_pos, emitter.len());
                }
                pending_asserts.clear();

                // Success: exit(0)
                emitter.mov_imm16(ArmReg::X0, 0); // Exit code 0
                emitter.mov_imm16(ArmReg::X8, SYS_EXIT);
                emitter.svc(0);
            }
        }
    }

    // Align code to 8 bytes
    emitter.align(8);

    CompiledBinary {
        code: emitter.into_code(),
        rodata,
        entry_offset,
    }
}

/// Emit code to write assertion failure message to stderr and exit(1)
fn emit_assertion_failure(emitter: &mut Emitter, msg: &AssertMessage) {
    // write(stderr, msg, len)
    // X0 = fd (stderr = 2)
    // X1 = buf (will be filled in by linker/loader based on rodata)
    // X2 = count
    // X8 = syscall number

    // For now, we load the message offset as an immediate
    // This will be patched later when we know the final rodata location
    emitter.mov_imm16(ArmReg::X0, STDERR as u16); // fd = stderr
    emitter.mov_imm64(ArmReg::X1, msg.offset as i64); // buf = offset (placeholder)
    emitter.mov_imm64(ArmReg::X2, msg.len as i64); // count = length
    emitter.mov_imm16(ArmReg::X8, SYS_WRITE);
    emitter.svc(0);

    // exit(1)
    emitter.mov_imm16(ArmReg::X0, 1); // Exit code 1
    emitter.mov_imm16(ArmReg::X8, SYS_EXIT);
    emitter.svc(0);
}

/// Simple register allocator mapping virtual registers to hardware registers
struct SimpleRegAlloc {
    mapping: Vec<Option<ArmReg>>,
    next_hw_reg: usize,
}

impl SimpleRegAlloc {
    const HW_REGS: [ArmReg; 4] = [ArmReg::X19, ArmReg::X20, ArmReg::X21, ArmReg::X22];

    fn new() -> Self {
        Self {
            mapping: Vec::new(),
            next_hw_reg: 0,
        }
    }

    fn alloc(&mut self, reg: Reg) -> ArmReg {
        let idx = reg.0 as usize;
        while self.mapping.len() <= idx {
            self.mapping.push(None);
        }
        let hw_reg = Self::HW_REGS[self.next_hw_reg % Self::HW_REGS.len()];
        self.mapping[idx] = Some(hw_reg);
        self.next_hw_reg += 1;
        hw_reg
    }

    fn get(&self, reg: Reg) -> ArmReg {
        self.mapping[reg.0 as usize].expect("register not allocated")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use skylow_mir::{AssertInfo, MirFunction};

    #[test]
    fn test_compile_simple() {
        let mut func = MirFunction::new_function("main".to_string());
        func.emit(Inst::LoadImm {
            dst: Reg(0),
            value: 1,
        });
        func.emit(Inst::LoadImm {
            dst: Reg(1),
            value: 1,
        });
        func.emit(Inst::Cmp {
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
        func.emit(Inst::Ret);

        let binary = compile_binary(&func, "test.skyl");
        assert!(!binary.code.is_empty());
        assert!(!binary.rodata.is_empty());
    }

    #[test]
    fn test_reg_alloc() {
        let mut alloc = SimpleRegAlloc::new();
        let r0 = alloc.alloc(Reg(0));
        let r1 = alloc.alloc(Reg(1));
        assert_eq!(alloc.get(Reg(0)), r0);
        assert_eq!(alloc.get(Reg(1)), r1);
    }
}

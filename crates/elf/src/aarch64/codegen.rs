//! AArch64 code generation for binary executables
//!
//! Generates position-independent code that uses Linux syscalls for I/O.

use codegen::{lower_instructions, ArmReg, Backend, Emitter};
use mir::MirFunction;

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

        // For now, we load the message offset as an immediate
        // This will be patched later when we know the final rodata location
        emit.mov_imm16(ArmReg::X0, STDERR as u16); // fd = stderr
        emit.mov_imm64(ArmReg::X1, msg.offset as i64); // buf = offset (placeholder)
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
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use mir::{AssertInfo, CmpOp, Inst, MirFunction, Reg};

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
}

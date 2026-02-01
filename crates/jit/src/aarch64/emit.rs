//! AArch64 instruction encoding
//!
//! Low-level byte emission for AArch64 instructions.

/// AArch64 general purpose registers (64-bit)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
#[allow(dead_code)] // We define all registers even if not currently used
pub enum ArmReg {
    X0 = 0,
    X1 = 1,
    X2 = 2,
    X3 = 3,
    X4 = 4,
    X5 = 5,
    X6 = 6,
    X7 = 7,
    X8 = 8,
    X9 = 9,
    X10 = 10,
    X11 = 11,
    X12 = 12,
    X13 = 13,
    X14 = 14,
    X15 = 15,
    X19 = 19, // Callee-saved
    X20 = 20,
    X21 = 21,
    X22 = 22,
    X23 = 23,
    X24 = 24,
    X25 = 25,
    X26 = 26,
    X27 = 27,
    X28 = 28,
    X29 = 29, // Frame pointer
    X30 = 30, // Link register
}

/// Code emitter for AArch64
pub struct Emitter {
    code: Vec<u8>,
}

impl Emitter {
    pub fn new() -> Self {
        Self { code: Vec::new() }
    }

    pub fn into_code(self) -> Vec<u8> {
        self.code
    }

    fn emit32(&mut self, inst: u32) {
        self.code.extend_from_slice(&inst.to_le_bytes());
    }

    /// Current code length
    pub fn len(&self) -> usize {
        self.code.len()
    }

    /// MOV Xd, #imm16 (MOVZ with shift 0)
    pub fn mov_imm16(&mut self, rd: ArmReg, imm: u16) {
        // MOVZ Xd, #imm16, LSL #0
        // 1 10 100101 00 imm16 Rd
        let inst = 0xd2800000 | ((imm as u32) << 5) | (rd as u32);
        self.emit32(inst);
    }

    /// MOV Xd, #imm64 using MOVZ + MOVK sequence
    pub fn mov_imm64(&mut self, rd: ArmReg, imm: i64) {
        let imm = imm as u64;

        // MOVZ for bits 0-15
        let inst = 0xd2800000 | (((imm & 0xffff) as u32) << 5) | (rd as u32);
        self.emit32(inst);

        // MOVK for bits 16-31 if needed
        if imm > 0xffff {
            let inst = 0xf2a00000 | ((((imm >> 16) & 0xffff) as u32) << 5) | (rd as u32);
            self.emit32(inst);
        }

        // MOVK for bits 32-47 if needed
        if imm > 0xffffffff {
            let inst = 0xf2c00000 | ((((imm >> 32) & 0xffff) as u32) << 5) | (rd as u32);
            self.emit32(inst);
        }

        // MOVK for bits 48-63 if needed
        if imm > 0xffffffffffff {
            let inst = 0xf2e00000 | ((((imm >> 48) & 0xffff) as u32) << 5) | (rd as u32);
            self.emit32(inst);
        }
    }

    /// MOV Xd, Xm (ORR Xd, XZR, Xm)
    #[allow(dead_code)] // Will be used for register-to-register moves
    pub fn mov_reg(&mut self, rd: ArmReg, rm: ArmReg) {
        // ORR Xd, XZR, Xm
        // 1 01 01010 00 0 Rm 000000 11111 Rd
        let inst = 0xaa0003e0 | ((rm as u32) << 16) | (rd as u32);
        self.emit32(inst);
    }

    /// ADD Xd, Xn, Xm
    pub fn add_reg(&mut self, rd: ArmReg, rn: ArmReg, rm: ArmReg) {
        // ADD Xd, Xn, Xm
        // 1 00 01011 00 0 Rm 000000 Rn Rd
        let inst = 0x8b000000 | ((rm as u32) << 16) | ((rn as u32) << 5) | (rd as u32);
        self.emit32(inst);
    }

    /// SUB Xd, Xn, Xm
    pub fn sub_reg(&mut self, rd: ArmReg, rn: ArmReg, rm: ArmReg) {
        // SUB Xd, Xn, Xm
        // 1 10 01011 00 0 Rm 000000 Rn Rd
        let inst = 0xcb000000 | ((rm as u32) << 16) | ((rn as u32) << 5) | (rd as u32);
        self.emit32(inst);
    }

    /// MUL Xd, Xn, Xm (MADD Xd, Xn, Xm, XZR)
    pub fn mul_reg(&mut self, rd: ArmReg, rn: ArmReg, rm: ArmReg) {
        // MADD Xd, Xn, Xm, XZR
        // 1 00 11011 000 Rm 0 11111 Rn Rd
        let inst = 0x9b007c00 | ((rm as u32) << 16) | ((rn as u32) << 5) | (rd as u32);
        self.emit32(inst);
    }

    /// SDIV Xd, Xn, Xm
    pub fn sdiv_reg(&mut self, rd: ArmReg, rn: ArmReg, rm: ArmReg) {
        // SDIV Xd, Xn, Xm
        // 1 00 11010 110 Rm 00001 1 Rn Rd
        let inst = 0x9ac00c00 | ((rm as u32) << 16) | ((rn as u32) << 5) | (rd as u32);
        self.emit32(inst);
    }

    /// CMP Xn, Xm (SUBS XZR, Xn, Xm)
    pub fn cmp_reg(&mut self, rn: ArmReg, rm: ArmReg) {
        // SUBS XZR, Xn, Xm
        // 1 11 01011 00 0 Rm 000000 Rn 11111
        let inst = 0xeb00001f | ((rm as u32) << 16) | ((rn as u32) << 5);
        self.emit32(inst);
    }

    /// CSET Xd, cond (CSINC Xd, XZR, XZR, invert(cond))
    pub fn cset(&mut self, rd: ArmReg, cond: Cond) {
        // CSINC Xd, XZR, XZR, invert(cond)
        // 1 00 11010 100 11111 cond 0 11111 Rd
        let inv_cond = cond.invert() as u32;
        let inst = 0x9a9f07e0 | (inv_cond << 12) | (rd as u32);
        self.emit32(inst);
    }

    /// CBZ Xn, offset (branch if zero)
    /// Returns offset of instruction for patching
    #[allow(dead_code)] // Will be used for conditional branching
    pub fn cbz(&mut self, rn: ArmReg, offset: i32) -> usize {
        let pos = self.code.len();
        // CBZ Xn, offset
        // 1 011010 0 imm19 Rn
        let imm19 = ((offset >> 2) as u32) & 0x7ffff;
        let inst = 0xb4000000 | (imm19 << 5) | (rn as u32);
        self.emit32(inst);
        pos
    }

    /// CBNZ Xn, offset (branch if not zero)
    pub fn cbnz(&mut self, rn: ArmReg, offset: i32) -> usize {
        let pos = self.code.len();
        // CBNZ Xn, offset
        // 1 011010 1 imm19 Rn
        let imm19 = ((offset >> 2) as u32) & 0x7ffff;
        let inst = 0xb5000000 | (imm19 << 5) | (rn as u32);
        self.emit32(inst);
        pos
    }

    /// Patch a CBZ/CBNZ instruction with new offset
    pub fn patch_branch(&mut self, pos: usize, target: usize) {
        let offset = (target as i32 - pos as i32) >> 2;
        let imm19 = (offset as u32) & 0x7ffff;
        let old_inst = u32::from_le_bytes([
            self.code[pos],
            self.code[pos + 1],
            self.code[pos + 2],
            self.code[pos + 3],
        ]);
        let new_inst = (old_inst & !0x00ffffe0) | (imm19 << 5);
        self.code[pos..pos + 4].copy_from_slice(&new_inst.to_le_bytes());
    }

    /// RET (return to link register)
    pub fn ret(&mut self) {
        // RET X30
        // 1101011 0 0 10 11111 0000 0 0 11110 00000
        self.emit32(0xd65f03c0);
    }

    /// STP Xt1, Xt2, [SP, #imm]! (pre-index store pair)
    pub fn stp_pre(&mut self, rt1: ArmReg, rt2: ArmReg, imm: i16) {
        // STP Xt1, Xt2, [SP, #imm]!
        // 1 0 101 0 011 1 imm7 Rt2 11111 Rt1
        let imm7 = ((imm / 8) as u32) & 0x7f;
        let inst = 0xa9800000 | (imm7 << 15) | ((rt2 as u32) << 10) | (31 << 5) | (rt1 as u32);
        self.emit32(inst);
    }

    /// LDP Xt1, Xt2, [SP], #imm (post-index load pair)
    pub fn ldp_post(&mut self, rt1: ArmReg, rt2: ArmReg, imm: i16) {
        // LDP Xt1, Xt2, [SP], #imm
        // 1 0 101 0 001 1 imm7 Rt2 11111 Rt1
        let imm7 = ((imm / 8) as u32) & 0x7f;
        let inst = 0xa8c00000 | (imm7 << 15) | ((rt2 as u32) << 10) | (31 << 5) | (rt1 as u32);
        self.emit32(inst);
    }
}

impl Default for Emitter {
    fn default() -> Self {
        Self::new()
    }
}

/// AArch64 condition codes
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Cond {
    Eq = 0,  // Equal
    Ne = 1,  // Not equal
    Lt = 11, // Signed less than
    Le = 13, // Signed less than or equal
    Gt = 12, // Signed greater than
    Ge = 10, // Signed greater than or equal
}

impl Cond {
    pub fn invert(self) -> Self {
        match self {
            Cond::Eq => Cond::Ne,
            Cond::Ne => Cond::Eq,
            Cond::Lt => Cond::Ge,
            Cond::Le => Cond::Gt,
            Cond::Gt => Cond::Le,
            Cond::Ge => Cond::Lt,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ret() {
        let mut emit = Emitter::new();
        emit.ret();
        assert_eq!(&emit.code, &[0xc0, 0x03, 0x5f, 0xd6]);
    }

    #[test]
    fn test_mov_imm16() {
        let mut emit = Emitter::new();
        emit.mov_imm16(ArmReg::X0, 42);
        // MOVZ X0, #42
        let expected: u32 = 0xd2800000 | (42 << 5) | 0;
        assert_eq!(emit.code, expected.to_le_bytes());
    }

    #[test]
    fn test_mov_reg() {
        let mut emit = Emitter::new();
        emit.mov_reg(ArmReg::X0, ArmReg::X19);
        assert_eq!(emit.len(), 4);
    }

    #[test]
    fn test_cbz() {
        let mut emit = Emitter::new();
        let pos = emit.cbz(ArmReg::X0, 0);
        assert_eq!(pos, 0);
        assert_eq!(emit.len(), 4);
    }

    #[test]
    fn test_default() {
        let emit = Emitter::default();
        assert_eq!(emit.len(), 0);
    }

    #[test]
    fn test_cond_invert() {
        assert_eq!(Cond::Eq.invert(), Cond::Ne);
        assert_eq!(Cond::Ne.invert(), Cond::Eq);
        assert_eq!(Cond::Lt.invert(), Cond::Ge);
        assert_eq!(Cond::Ge.invert(), Cond::Lt);
        assert_eq!(Cond::Gt.invert(), Cond::Le);
        assert_eq!(Cond::Le.invert(), Cond::Gt);
    }
}

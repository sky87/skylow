//! Simple register allocator for AArch64
//!
//! Maps virtual registers to hardware registers in a round-robin fashion.
//! Uses callee-saved registers x19-x22.

use super::emit::ArmReg;
use mir::Reg;

/// Simple register allocator mapping virtual registers to hardware registers
pub struct SimpleRegAlloc {
    mapping: Vec<Option<ArmReg>>,
    next_hw_reg: usize,
}

impl SimpleRegAlloc {
    const HW_REGS: [ArmReg; 4] = [ArmReg::X19, ArmReg::X20, ArmReg::X21, ArmReg::X22];

    pub fn new() -> Self {
        Self {
            mapping: Vec::new(),
            next_hw_reg: 0,
        }
    }

    /// Allocate a hardware register for a destination virtual register
    pub fn alloc(&mut self, reg: Reg) -> ArmReg {
        let idx = reg.0 as usize;
        while self.mapping.len() <= idx {
            self.mapping.push(None);
        }
        let hw_reg = Self::HW_REGS[self.next_hw_reg % Self::HW_REGS.len()];
        self.mapping[idx] = Some(hw_reg);
        self.next_hw_reg += 1;
        hw_reg
    }

    /// Get the hardware register for a source virtual register
    pub fn get(&self, reg: Reg) -> ArmReg {
        self.mapping[reg.0 as usize].expect("register not allocated")
    }
}

impl Default for SimpleRegAlloc {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_alloc_and_get() {
        let mut alloc = SimpleRegAlloc::new();
        let r0 = alloc.alloc(Reg(0));
        let r1 = alloc.alloc(Reg(1));
        assert_eq!(alloc.get(Reg(0)), r0);
        assert_eq!(alloc.get(Reg(1)), r1);
    }

    #[test]
    fn test_default() {
        let alloc = SimpleRegAlloc::default();
        assert_eq!(alloc.mapping.len(), 0);
    }

    #[test]
    fn test_round_robin() {
        let mut alloc = SimpleRegAlloc::new();
        // Allocate more than 4 registers to test round-robin
        let r0 = alloc.alloc(Reg(0));
        let r1 = alloc.alloc(Reg(1));
        let r2 = alloc.alloc(Reg(2));
        let r3 = alloc.alloc(Reg(3));
        let r4 = alloc.alloc(Reg(4));

        assert_eq!(r0, ArmReg::X19);
        assert_eq!(r1, ArmReg::X20);
        assert_eq!(r2, ArmReg::X21);
        assert_eq!(r3, ArmReg::X22);
        assert_eq!(r4, ArmReg::X19); // wraps around
    }
}

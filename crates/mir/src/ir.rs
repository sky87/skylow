//! MIR types - Register-based intermediate representation

/// Virtual register
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Reg(pub u32);

/// Binary arithmetic operations
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

/// Comparison operations
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CmpOp {
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
}

/// MIR instruction
#[derive(Debug, Clone, PartialEq)]
pub enum Inst {
    /// Load immediate value into register
    LoadImm { dst: Reg, value: i64 },
    /// Binary arithmetic operation
    BinOp { op: BinOp, dst: Reg, left: Reg, right: Reg },
    /// Comparison operation (result is 0 or 1)
    Cmp { op: CmpOp, dst: Reg, left: Reg, right: Reg },
    /// Assert condition (fail if zero)
    Assert { cond: Reg, msg_id: u32 },
    /// Return from function
    Ret,
}

/// A compiled MIR function (test body)
#[derive(Debug, Clone)]
pub struct MirFunction {
    pub name: String,
    pub instructions: Vec<Inst>,
    pub next_reg: u32,
}

impl MirFunction {
    pub fn new(name: String) -> Self {
        Self {
            name,
            instructions: Vec::new(),
            next_reg: 0,
        }
    }

    pub fn alloc_reg(&mut self) -> Reg {
        let reg = Reg(self.next_reg);
        self.next_reg += 1;
        reg
    }

    pub fn emit(&mut self, inst: Inst) {
        self.instructions.push(inst);
    }
}

/// A complete MIR program
#[derive(Debug, Clone, Default)]
pub struct MirProgram {
    pub functions: Vec<MirFunction>,
}

//! MIR types - Register-based intermediate representation

/// Source span information (owned, for debug info)
///
/// Unlike SourceInfo which borrows from a SourceModule, this type
/// owns all its data and can be stored in MIR without lifetime constraints.
#[derive(Debug, Clone, PartialEq)]
pub struct SourceSpan {
    /// Source identifier (file path, "<prelude>", etc.)
    pub source_id: String,
    /// Start line number (1-based)
    pub line: u32,
    /// Start column number (1-based)
    pub col: u32,
    /// End line number (1-based)
    pub end_line: u32,
    /// End column number (1-based)
    pub end_col: u32,
}

/// Source location information for assertion failure reporting
#[derive(Debug, Clone, PartialEq)]
pub struct AssertInfo {
    /// Line number (1-based)
    pub line: u32,
    /// Column number (1-based)
    pub col: u32,
    /// Source text of the assertion expression
    pub source: String,
}

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

/// MIR instruction with optional source span
#[derive(Debug, Clone, PartialEq)]
pub struct Inst {
    /// The instruction kind
    pub kind: InstKind,
    /// Optional source span for debug info
    pub span: Option<SourceSpan>,
}

impl Inst {
    /// Create an instruction without span information
    pub fn new(kind: InstKind) -> Self {
        Self { kind, span: None }
    }

    /// Create an instruction with span information
    pub fn with_span(kind: InstKind, span: SourceSpan) -> Self {
        Self { kind, span: Some(span) }
    }
}

/// MIR instruction kinds
#[derive(Debug, Clone, PartialEq)]
pub enum InstKind {
    /// Load immediate value into register
    LoadImm { dst: Reg, value: i64 },
    /// Copy from one register to another
    Copy { dst: Reg, src: Reg },
    /// Binary arithmetic operation
    BinOp { op: BinOp, dst: Reg, left: Reg, right: Reg },
    /// Comparison operation (result is 0 or 1)
    Cmp { op: CmpOp, dst: Reg, left: Reg, right: Reg },
    /// Assert condition (fail if zero)
    Assert { cond: Reg, msg_id: u32 },
    /// Call a function by name, storing result in dst
    Call { dst: Reg, func_name: String, args: Vec<Reg> },
    /// Return from function with value
    RetVal { value: Reg },
    /// Return from function (for tests/void functions)
    Ret,
}

/// Whether this MIR function is a test or a regular function
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionKind {
    /// A test function (from `test name:`)
    Test,
    /// A regular function (from `fn name():`)
    Function,
}

/// Debug information for a function
#[derive(Debug, Clone, Default)]
pub struct FunctionDebugInfo {
    /// Source span for the function declaration
    pub span: Option<SourceSpan>,
    /// Source identifier (file path)
    pub source_id: Option<String>,
}

/// Function parameter info
#[derive(Debug, Clone, PartialEq)]
pub struct MirParam {
    /// Parameter name
    pub name: String,
    /// Register holding this parameter
    pub reg: Reg,
}

/// Local variable info for debugging
#[derive(Debug, Clone, PartialEq)]
pub struct MirLocal {
    /// Variable name
    pub name: String,
    /// Register holding this variable
    pub reg: Reg,
}

/// A compiled MIR function (test body or function body)
#[derive(Debug, Clone)]
pub struct MirFunction {
    pub name: String,
    /// Function parameters (mapped to registers)
    pub params: Vec<MirParam>,
    /// Local variables (mapped to registers)
    pub locals: Vec<MirLocal>,
    pub instructions: Vec<Inst>,
    pub next_reg: u32,
    /// Assertion info indexed by msg_id (0-based)
    pub asserts: Vec<AssertInfo>,
    /// Whether this is a test or a regular function
    pub kind: FunctionKind,
    /// Debug information for this function
    pub debug_info: FunctionDebugInfo,
}

impl MirFunction {
    pub fn new(name: String) -> Self {
        Self {
            name,
            params: Vec::new(),
            locals: Vec::new(),
            instructions: Vec::new(),
            next_reg: 0,
            asserts: Vec::new(),
            kind: FunctionKind::Test,
            debug_info: FunctionDebugInfo::default(),
        }
    }

    pub fn new_function(name: String) -> Self {
        Self {
            name,
            params: Vec::new(),
            locals: Vec::new(),
            instructions: Vec::new(),
            next_reg: 0,
            asserts: Vec::new(),
            kind: FunctionKind::Function,
            debug_info: FunctionDebugInfo::default(),
        }
    }

    pub fn alloc_reg(&mut self) -> Reg {
        let reg = Reg(self.next_reg);
        self.next_reg += 1;
        reg
    }

    /// Emit an instruction without span information
    pub fn emit(&mut self, kind: InstKind) {
        self.instructions.push(Inst::new(kind));
    }

    /// Emit an instruction with span information
    pub fn emit_with_span(&mut self, kind: InstKind, span: SourceSpan) {
        self.instructions.push(Inst::with_span(kind, span));
    }

    /// Add an assert instruction with source info, returns the msg_id
    pub fn emit_assert(&mut self, cond: Reg, info: AssertInfo) -> u32 {
        let msg_id = self.asserts.len() as u32;
        self.asserts.push(info);
        self.instructions.push(Inst::new(InstKind::Assert { cond, msg_id }));
        msg_id
    }

    /// Add an assert instruction with source info and span, returns the msg_id
    pub fn emit_assert_with_span(&mut self, cond: Reg, info: AssertInfo, span: SourceSpan) -> u32 {
        let msg_id = self.asserts.len() as u32;
        self.asserts.push(info);
        self.instructions.push(Inst::with_span(InstKind::Assert { cond, msg_id }, span));
        msg_id
    }
}

/// A complete MIR program
#[derive(Debug, Clone, Default)]
pub struct MirProgram {
    pub functions: Vec<MirFunction>,
}

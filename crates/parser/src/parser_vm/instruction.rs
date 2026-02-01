//! Bytecode instruction encoding and opcodes.
//!
//! Instructions are 32-bit words: 8-bit opcode + 24-bit operand.

/// Opcode constants (8-bit)
pub mod op {
    // Input matching
    pub const LITERAL: u8 = 0x01;
    pub const CHAR_CLASS: u8 = 0x02;
    pub const CHAR_SET: u8 = 0x03;

    // Control flow
    pub const JUMP: u8 = 0x10;
    pub const CHOICE: u8 = 0x11;
    pub const COMMIT: u8 = 0x12;
    pub const FAIL: u8 = 0x13;
    pub const END: u8 = 0x14;

    // Category/rule dispatch
    pub const CALL: u8 = 0x20;
    pub const RETURN: u8 = 0x21;

    // Quantifier support (reserved for future use)
    #[allow(dead_code)]
    pub const LOOP_START: u8 = 0x40;
    #[allow(dead_code)]
    pub const LOOP_END: u8 = 0x41;

    // Indentation
    pub const INDENT_ANCHOR: u8 = 0x50;
    pub const INDENT_MODE: u8 = 0x51;
    pub const SKIP_WS: u8 = 0x52;

    // Precedence (reserved for future Pratt parsing enhancements)
    #[allow(dead_code)]
    pub const CHECK_PREC: u8 = 0x70;
    #[allow(dead_code)]
    pub const SET_PREC: u8 = 0x71;
}

// Indent mode constants for INDENT_MODE operand
pub const INDENT_NONE: u32 = 0;
pub const INDENT_LAX: u32 = 1;
pub const INDENT_STRICT: u32 = 2;

// Skip whitespace flags
pub const SKIP_WS_ALL: u32 = 0;
pub const SKIP_WS_INDENT: u32 = 1;

/// Encode an instruction from opcode and operand
#[inline]
pub fn encode(opcode: u8, operand: u32) -> u32 {
    ((opcode as u32) << 24) | (operand & 0x00FF_FFFF)
}

/// Decode opcode from instruction
#[inline]
pub fn opcode(instr: u32) -> u8 {
    (instr >> 24) as u8
}

/// Decode operand from instruction (24-bit unsigned)
#[inline]
pub fn operand(instr: u32) -> u32 {
    instr & 0x00FF_FFFF
}

/// Decode operand as signed offset
#[inline]
pub fn operand_signed(instr: u32) -> i32 {
    let raw = instr & 0x00FF_FFFF;
    // Sign-extend from 24 bits
    if raw & 0x0080_0000 != 0 {
        (raw | 0xFF00_0000) as i32
    } else {
        raw as i32
    }
}

/// Encode a signed offset into 24-bit operand
#[inline]
pub fn encode_signed(offset: i32) -> u32 {
    (offset as u32) & 0x00FF_FFFF
}


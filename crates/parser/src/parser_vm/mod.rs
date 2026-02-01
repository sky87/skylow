//! Parsing Virtual Machine
//!
//! A bytecode virtual machine for parsing that compiles syntax rules to instructions,
//! enabling efficient execution with linear dispatch.
//!
//! # Architecture
//!
//! The VM compiles syntax rules into bytecode which is then executed to parse input.
//! This approach offers better cache behavior than interpreting rule structures directly.
//!
//! # Instruction Encoding
//!
//! Instructions are 32-bit words: 8-bit opcode + 24-bit operand.

mod charset;
mod compiler;
mod grammar;
mod instruction;
mod vm;
mod vmparser;

// Re-export public types
pub use charset::CompiledCharSet;
pub use compiler::Compiler;
pub use grammar::{CompiledGrammar, CompiledRule, DispatchTable};
pub use instruction::{
    encode, encode_signed, op, opcode, operand, operand_signed, INDENT_LAX, INDENT_NONE,
    INDENT_STRICT, SKIP_WS_ALL, SKIP_WS_INDENT,
};
pub use vm::{IndentMode, VM};
pub use vmparser::VMParser;

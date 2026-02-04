//! Debug information crate for SkyLow
//!
//! This crate provides types and utilities for generating and managing
//! debug information that enables source-level debugging of compiled code.
//!
//! The main types are:
//! - [`DebugInfo`] - Complete debug information for a program
//! - [`DebugInfoBuilder`] - Builder for constructing debug info during compilation
//!
//! Debug info can be serialized to the `.skydbg` binary format for external
//! tools and deserialized for runtime debugging support.

pub mod builder;
pub mod deserialize;
pub mod serialize;
pub mod types;

pub use builder::DebugInfoBuilder;
pub use deserialize::{read_skydbg, ReadError};
pub use serialize::write_skydbg;
pub use types::{
    DebugInfo, FunctionLocals, LineMapping, LineMappingFlags, LocalVariable, SourceFile, Symbol,
    SymbolKind,
};

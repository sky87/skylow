//! Common utilities for SkyLow crates.
//!
//! This crate provides shared infrastructure used across the SkyLow workspace:
//!
//! - [`debug`] - Per-module logging controlled via `DEBUG` environment variable
//! - [`intern`] - String interning using arena allocation
//! - [`source`] - Source module representation for tracking source origin

pub mod debug;
pub mod intern;
pub mod source;

pub use debug::{create_logger, Logger};
pub use intern::StringInterner;
pub use source::{SourceInfo, SourceLoc, SourceModule, SourceModuleKind};

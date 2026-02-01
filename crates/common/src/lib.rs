//! Common utilities for SkyLow crates.
//!
//! This crate provides shared infrastructure used across the SkyLow workspace:
//!
//! - [`debug`] - Per-module logging controlled via `DEBUG` environment variable
//! - [`intern`] - String interning using arena allocation

pub mod debug;
pub mod intern;

pub use debug::{create_logger, Logger};
pub use intern::StringInterner;

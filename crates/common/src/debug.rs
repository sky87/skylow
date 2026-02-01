//! Debug infrastructure with per-module loggers.
//!
//! Control via DEBUG environment variable:
//! - `DEBUG=*` - Enable all loggers
//! - `DEBUG=parser` - Enable only parser
//! - `DEBUG=parser,intern` - Enable multiple
//!
//! Verbosity via DEBUG_VERBOSITY (0-3, default 1)

use std::collections::HashSet;
use std::env;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::OnceLock;

// ============================================================================
// Configuration
// ============================================================================

#[derive(Clone)]
enum EnabledConfig {
    All,
    None,
    Some(HashSet<String>),
}

struct GlobalConfig {
    enabled: EnabledConfig,
    verbosity: u8,
}

static CONFIG: OnceLock<GlobalConfig> = OnceLock::new();

fn get_config() -> &'static GlobalConfig {
    CONFIG.get_or_init(|| {
        let enabled = match env::var("DEBUG").ok().as_deref() {
            None | Some("") => EnabledConfig::None,
            Some("*") | Some("1") | Some("true") => EnabledConfig::All,
            Some(value) => {
                let set: HashSet<_> = value.split(',')
                    .map(|s| s.trim().to_string())
                    .filter(|s| !s.is_empty())
                    .collect();
                if set.is_empty() { EnabledConfig::None } else { EnabledConfig::Some(set) }
            }
        };
        let verbosity = env::var("DEBUG_VERBOSITY")
            .ok()
            .and_then(|v| v.parse().ok())
            .map(|v: u8| v.min(3))
            .unwrap_or(1);
        GlobalConfig { enabled, verbosity }
    })
}

fn is_enabled(name: &str) -> bool {
    match &get_config().enabled {
        EnabledConfig::None => false,
        EnabledConfig::All => true,
        EnabledConfig::Some(set) => set.contains(name),
    }
}

fn verbosity() -> u8 {
    get_config().verbosity
}

// ============================================================================
// Logger
// ============================================================================

pub struct Logger {
    name: &'static str,
    enabled: bool,
    indent: AtomicUsize,
}

impl Logger {
    pub const fn disabled() -> Self {
        Self { name: "", enabled: false, indent: AtomicUsize::new(0) }
    }

    fn active(name: &'static str) -> Self {
        Self { name, enabled: true, indent: AtomicUsize::new(0) }
    }

    #[inline]
    pub fn enabled(&self) -> bool {
        self.enabled
    }

    fn prefix(&self) -> String {
        let indent = self.indent.load(Ordering::Relaxed);
        format!("{}[{}]", "  ".repeat(indent), self.name)
    }

    #[inline]
    pub fn log(&self, msg: &str) {
        if self.enabled && verbosity() >= 1 {
            eprintln!("{} {}", self.prefix(), msg);
        }
    }

    #[inline]
    pub fn detail(&self, msg: &str) {
        if self.enabled && verbosity() >= 2 {
            eprintln!("{} {}", self.prefix(), msg);
        }
    }

    #[inline]
    pub fn success(&self, msg: &str) {
        if self.enabled && verbosity() >= 1 {
            eprintln!("{} OK: {}", self.prefix(), msg);
        }
    }

    #[inline]
    pub fn fail(&self, msg: &str) {
        if self.enabled && verbosity() >= 1 {
            eprintln!("{} FAIL: {}", self.prefix(), msg);
        }
    }

    #[inline]
    pub fn push_indent(&self) {
        if self.enabled {
            self.indent.fetch_add(1, Ordering::Relaxed);
        }
    }

    #[inline]
    pub fn pop_indent(&self) {
        if self.enabled {
            let _ = self.indent.fetch_update(Ordering::Relaxed, Ordering::Relaxed, |v| {
                if v > 0 { Some(v - 1) } else { Some(0) }
            });
        }
    }
}

// ============================================================================
// Factory
// ============================================================================

/// Create a logger. The name must be a static string.
pub fn create_logger(name: &'static str) -> Logger {
    if is_enabled(name) {
        Logger::active(name)
    } else {
        Logger::disabled()
    }
}

// ============================================================================
// Macros - avoid format! cost when disabled
// ============================================================================

#[macro_export]
macro_rules! log {
    ($logger:expr, $($arg:tt)*) => {
        if $logger.enabled() {
            $logger.log(&format!($($arg)*));
        }
    };
}

#[macro_export]
macro_rules! log_detail {
    ($logger:expr, $($arg:tt)*) => {
        if $logger.enabled() {
            $logger.detail(&format!($($arg)*));
        }
    };
}

#[macro_export]
macro_rules! log_success {
    ($logger:expr, $($arg:tt)*) => {
        if $logger.enabled() {
            $logger.success(&format!($($arg)*));
        }
    };
}

#[macro_export]
macro_rules! log_fail {
    ($logger:expr, $($arg:tt)*) => {
        if $logger.enabled() {
            $logger.fail(&format!($($arg)*));
        }
    };
}

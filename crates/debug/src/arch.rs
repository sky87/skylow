//! Architecture abstraction for multi-platform debugging support.

/// Architecture-specific information for debugging.
pub struct ArchInfo {
    /// Architecture name (e.g. "aarch64", "x86_64")
    pub name: &'static str,
    /// Number of general-purpose registers
    pub gpr_count: usize,
    /// Names of general-purpose registers (indexed 0..gpr_count)
    pub gpr_names: &'static [&'static str],
    /// Name of the processor status register (e.g. "pstate", "eflags")
    pub status_reg_name: &'static str,
    /// Bytes to subtract from a return address to land within the call instruction.
    /// 4 for AArch64 (fixed-size instructions), 1 for x86_64 (variable-length).
    pub return_addr_decrement: u32,
}

impl ArchInfo {
    /// Parse a GPR name to its index. Returns `None` if the name is not a valid GPR.
    pub fn parse_gpr(&self, name: &str) -> Option<usize> {
        let lower = name.to_lowercase();
        self.gpr_names.iter().position(|&n| n == lower)
    }

    /// Get the name of a GPR by index. Panics if index >= gpr_count.
    pub fn gpr_name(&self, index: usize) -> &'static str {
        self.gpr_names[index]
    }
}

static AARCH64_GPR_NAMES: [&str; 31] = [
    "x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7",
    "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15",
    "x16", "x17", "x18", "x19", "x20", "x21", "x22", "x23",
    "x24", "x25", "x26", "x27", "x28", "x29", "x30",
];

/// AArch64 architecture info.
pub static AARCH64: ArchInfo = ArchInfo {
    name: "aarch64",
    gpr_count: 31,
    gpr_names: &AARCH64_GPR_NAMES,
    status_reg_name: "pstate",
    return_addr_decrement: 4,
};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_aarch64_constants() {
        assert_eq!(AARCH64.name, "aarch64");
        assert_eq!(AARCH64.gpr_count, 31);
        assert_eq!(AARCH64.gpr_names.len(), 31);
        assert_eq!(AARCH64.status_reg_name, "pstate");
        assert_eq!(AARCH64.return_addr_decrement, 4);
    }

    #[test]
    fn test_parse_gpr_valid() {
        assert_eq!(AARCH64.parse_gpr("x0"), Some(0));
        assert_eq!(AARCH64.parse_gpr("x10"), Some(10));
        assert_eq!(AARCH64.parse_gpr("x30"), Some(30));
    }

    #[test]
    fn test_parse_gpr_case_insensitive() {
        assert_eq!(AARCH64.parse_gpr("X0"), Some(0));
        assert_eq!(AARCH64.parse_gpr("X15"), Some(15));
        assert_eq!(AARCH64.parse_gpr("X30"), Some(30));
    }

    #[test]
    fn test_parse_gpr_invalid() {
        assert_eq!(AARCH64.parse_gpr("x31"), None);
        assert_eq!(AARCH64.parse_gpr("x99"), None);
        assert_eq!(AARCH64.parse_gpr("sp"), None);
        assert_eq!(AARCH64.parse_gpr("pc"), None);
        assert_eq!(AARCH64.parse_gpr("pstate"), None);
        assert_eq!(AARCH64.parse_gpr("foo"), None);
        assert_eq!(AARCH64.parse_gpr(""), None);
    }

    #[test]
    fn test_gpr_name() {
        assert_eq!(AARCH64.gpr_name(0), "x0");
        assert_eq!(AARCH64.gpr_name(10), "x10");
        assert_eq!(AARCH64.gpr_name(30), "x30");
    }

    #[test]
    fn test_gpr_names_sequential() {
        for i in 0..31 {
            assert_eq!(AARCH64.gpr_name(i), format!("x{}", i));
        }
    }
}

//! Compiled grammar containing bytecode and dispatch tables.

use super::charset::CompiledCharSet;
use super::instruction::{encode, encode_signed, op, opcode, operand, operand_signed};

/// Compiled grammar containing bytecode and dispatch tables
pub struct CompiledGrammar<'a> {
    /// Bytecode buffer
    pub code: Vec<u32>,

    /// String table for literals
    pub strings: Vec<&'a str>,

    /// Character set table
    pub charsets: Vec<CompiledCharSet>,

    /// Category entry points: category_id -> bytecode offset
    pub category_entries: Vec<usize>,

    /// Category names for lookup
    pub category_names: Vec<&'a str>,

    /// Rule info for each compiled rule
    pub rules: Vec<CompiledRule<'a>>,

    /// Category dispatch tables
    pub dispatch_tables: Vec<DispatchTable>,
}

/// Information about a compiled rule
pub struct CompiledRule<'a> {
    pub category: &'a str,
    pub name: &'a str,
    pub bytecode_offset: usize,
    pub is_left_recursive: bool,
}

/// Dispatch table for a category
pub struct DispatchTable {
    /// Non-left-recursive rules (prefix) in priority order (latest first)
    pub prefix_rules: Vec<usize>, // indices into CompiledGrammar.rules
    /// Left-recursive rules (infix/postfix) in priority order
    pub infix_rules: Vec<usize>,
}

impl<'a> CompiledGrammar<'a> {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            strings: Vec::new(),
            charsets: Vec::new(),
            category_entries: Vec::new(),
            category_names: Vec::new(),
            rules: Vec::new(),
            dispatch_tables: Vec::new(),
        }
    }

    /// Dump the compiled grammar to stderr for debugging
    pub fn dump(&self) {
        use std::io::Write;
        let stderr = std::io::stderr();
        let mut out = stderr.lock();

        writeln!(out, "\n{:=^60}", " COMPILED GRAMMAR DUMP ").ok();

        // String table
        writeln!(out, "\n--- String Table ({} entries) ---", self.strings.len()).ok();
        for (i, s) in self.strings.iter().enumerate() {
            writeln!(out, "  [{:3}] {:?}", i, s).ok();
        }

        // Charset table
        writeln!(out, "\n--- Charset Table ({} entries) ---", self.charsets.len()).ok();
        for (i, cs) in self.charsets.iter().enumerate() {
            let neg = if cs.is_negated() { "^" } else { "" };
            // Build a string showing which chars match
            let mut chars = String::new();
            for c in 0u8..128 {
                if cs.matches(c as char) {
                    if c.is_ascii_graphic() {
                        chars.push(c as char);
                    } else {
                        chars.push_str(&format!("\\x{:02x}", c));
                    }
                }
            }
            if chars.len() > 40 {
                chars.truncate(40);
                chars.push_str("...");
            }
            writeln!(out, "  [{:3}] [{}{}]", i, neg, chars).ok();
        }

        // Categories
        writeln!(out, "\n--- Categories ({} entries) ---", self.category_names.len()).ok();
        for (i, name) in self.category_names.iter().enumerate() {
            let dispatch = &self.dispatch_tables[i];
            writeln!(out, "  [{:3}] {} (entry: {})", i, name, self.category_entries[i]).ok();
            write!(out, "        prefix_rules: [").ok();
            for (j, &rule_idx) in dispatch.prefix_rules.iter().enumerate() {
                if j > 0 { write!(out, ", ").ok(); }
                write!(out, "{}", self.rules[rule_idx].name).ok();
            }
            writeln!(out, "]").ok();
            write!(out, "        infix_rules:  [").ok();
            for (j, &rule_idx) in dispatch.infix_rules.iter().enumerate() {
                if j > 0 { write!(out, ", ").ok(); }
                write!(out, "{}", self.rules[rule_idx].name).ok();
            }
            writeln!(out, "]").ok();
        }

        // Rules
        writeln!(out, "\n--- Rules ({} entries) ---", self.rules.len()).ok();
        for (i, rule) in self.rules.iter().enumerate() {
            let lr = if rule.is_left_recursive { " [LR]" } else { "" };
            writeln!(out, "  [{:3}] {}.{}{} @ offset {}",
                i, rule.category, rule.name, lr, rule.bytecode_offset).ok();
        }

        // Bytecode disassembly
        writeln!(out, "\n--- Bytecode ({} instructions) ---", self.code.len()).ok();
        self.disassemble(&mut out);

        writeln!(out, "\n{:=^60}\n", "").ok();
    }

    /// Disassemble bytecode to a writer
    pub fn disassemble<W: std::io::Write>(&self, out: &mut W) {
        let mut pc = 0;
        while pc < self.code.len() {
            let instr = self.code[pc];
            let opc = opcode(instr);
            let oper = operand(instr);

            // Find which rule this belongs to
            let mut rule_label = String::new();
            for rule in &self.rules {
                if rule.bytecode_offset == pc {
                    rule_label = format!("\n  ; === {}.{} ===", rule.category, rule.name);
                }
            }
            if !rule_label.is_empty() {
                writeln!(out, "{}", rule_label).ok();
            }

            let desc = match opc {
                op::LITERAL => {
                    let s = self.strings.get(oper as usize).map(|s| *s).unwrap_or("???");
                    format!("LITERAL      {} ({:?})", oper, s)
                }
                op::CHAR_CLASS => {
                    let name = match oper {
                        0 => "Digit",
                        1 => "Word",
                        2 => "Whitespace",
                        _ => "???",
                    };
                    format!("CHAR_CLASS   {} ({})", oper, name)
                }
                op::CHAR_SET => format!("CHAR_SET     {}", oper),
                op::JUMP => {
                    let offset = operand_signed(instr);
                    let target = (pc as i32 + offset) as usize;
                    format!("JUMP         {} -> @{}", offset, target)
                }
                op::CHOICE => {
                    let offset = operand_signed(instr);
                    let target = (pc as i32 + offset) as usize;
                    format!("CHOICE       {} -> @{}", offset, target)
                }
                op::COMMIT => {
                    let offset = operand_signed(instr);
                    let target = (pc as i32 + offset) as usize;
                    format!("COMMIT       {} -> @{}", offset, target)
                }
                op::FAIL => "FAIL".to_string(),
                op::END => "END".to_string(),
                op::CALL => {
                    let cat = self.category_names.get(oper as usize).map(|s| *s).unwrap_or("???");
                    format!("CALL         {} ({})", oper, cat)
                }
                op::RETURN => "RETURN".to_string(),
                op::INDENT_ANCHOR => "INDENT_ANCHOR".to_string(),
                op::INDENT_MODE => {
                    let mode = match oper {
                        0 => "None",
                        1 => "Lax",
                        2 => "Strict",
                        _ => "???",
                    };
                    format!("INDENT_MODE  {} ({})", oper, mode)
                }
                op::SKIP_WS => {
                    let mode = match oper {
                        0 => "All",
                        1 => "Indent",
                        _ => "???",
                    };
                    format!("SKIP_WS      {} ({})", oper, mode)
                }
                _ => format!("??? opcode={:#04x} oper={}", opc, oper),
            };

            writeln!(out, "  {:4}: {}", pc, desc).ok();
            pc += 1;
        }
    }

    /// Intern a string, returning its ID
    pub fn intern_string(&mut self, s: &'a str) -> u32 {
        for (i, existing) in self.strings.iter().enumerate() {
            if *existing == s {
                return i as u32;
            }
        }
        let id = self.strings.len() as u32;
        self.strings.push(s);
        id
    }

    /// Add a character set, returning its ID
    pub fn add_charset(&mut self, ranges: &[(char, char)], negated: bool) -> u32 {
        let charset = CompiledCharSet::new(ranges, negated);
        let id = self.charsets.len() as u32;
        self.charsets.push(charset);
        id
    }

    /// Get or create category ID
    pub fn get_or_create_category(&mut self, name: &'a str) -> u32 {
        for (i, existing) in self.category_names.iter().enumerate() {
            if *existing == name {
                return i as u32;
            }
        }
        let id = self.category_names.len() as u32;
        self.category_names.push(name);
        self.category_entries.push(0); // Will be set later
        self.dispatch_tables.push(DispatchTable {
            prefix_rules: Vec::new(),
            infix_rules: Vec::new(),
        });
        id
    }

    /// Get category ID (returns None if not found)
    pub fn get_category(&self, name: &str) -> Option<u32> {
        for (i, existing) in self.category_names.iter().enumerate() {
            if *existing == name {
                return Some(i as u32);
            }
        }
        None
    }

    /// Emit an instruction
    pub fn emit(&mut self, instr: u32) -> usize {
        let offset = self.code.len();
        self.code.push(instr);
        offset
    }

    /// Get current code offset
    pub fn current_offset(&self) -> usize {
        self.code.len()
    }

    /// Patch a jump instruction at the given offset
    pub fn patch_jump(&mut self, offset: usize, target: usize) {
        let instr = self.code[offset];
        let opc = opcode(instr);
        let rel_offset = (target as i32) - (offset as i32);
        self.code[offset] = encode(opc, encode_signed(rel_offset));
    }
}

impl<'a> Default for CompiledGrammar<'a> {
    fn default() -> Self {
        Self::new()
    }
}


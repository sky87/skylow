//! VMParser: Drop-in replacement for InterpretedParser using the VM.

use bumpalo::Bump;
use common::debug::{create_logger, Logger};
use common::intern::StringInterner;
use common::SourceModule;

use crate::node::{ParseError, SourceLoc, SyntaxNode};
use crate::parser_trait::Parser;
use crate::syntax::{Atom, AtomWithQuant, CharClass, Quantifier, SyntaxRule};

use super::compiler::Compiler;
use super::grammar::CompiledGrammar;
use super::vm::{IndentMode, VM};

/// A parser that uses the bytecode VM internally.
///
/// Provides the same interface as `InterpretedParser` but compiles
/// rules to bytecode and executes them via the VM.
///
/// # Architecture
///
/// Rules are compiled to bytecode as they're added via `add_rule()`. When
/// `parse_category()` is called, a VM is created to execute the bytecode.
/// The grammar is stored in the arena for the VM's lifetime.
pub struct VMParser<'a> {
    arena: &'a Bump,
    /// The current source module being parsed.
    module: &'a SourceModule<'a>,
    /// Cached source text from the module (for efficient access).
    source: &'a str,

    // All rules added so far (kept for recompilation)
    rules: Vec<&'a SyntaxRule<'a>>,

    // The compiled grammar (rebuilt when rules change)
    grammar: Option<&'a CompiledGrammar<'a>>,

    // Flag indicating grammar needs recompilation
    grammar_dirty: bool,

    // String interner
    strings: StringInterner<'a>,

    // Input position tracking
    pos: usize,
    line: u32,
    col: u32,

    // Indentation
    indent_stack: Vec<u32>,
    indent_mode: IndentMode,
    pending_anchor: bool,

    // Error tracking
    furthest_pos: usize,
    furthest_loc: SourceLoc,
    furthest_expected: Vec<String>,

    // Execution tracing
    trace_enabled: bool,

    #[allow(dead_code)]
    log: Logger,
}

impl<'a> VMParser<'a> {
    /// Create a new VM-based parser with the given source module.
    pub fn new(arena: &'a Bump, module: &'a SourceModule<'a>) -> Self {
        // Cache the source text for efficient access (modules are immutable)
        let source = module.text;
        Self {
            arena,
            module,
            source,
            rules: Vec::new(),
            grammar: None,
            grammar_dirty: true,
            strings: StringInterner::new(arena),
            pos: 0,
            line: 1,
            col: 1,
            indent_stack: Vec::new(),
            indent_mode: IndentMode::None,
            pending_anchor: false,
            furthest_pos: 0,
            furthest_loc: SourceLoc::new(0, 1, 1),
            furthest_expected: Vec::new(),
            trace_enabled: false,
            log: create_logger("vmparser"),
        }
    }

    // -------------------------------------------------------------------------
    // Public API: State inspection
    // -------------------------------------------------------------------------

    pub fn current_loc(&self) -> SourceLoc {
        SourceLoc::new(self.pos as u32, self.line, self.col)
    }

    pub fn remaining(&self) -> &'a str {
        &self.source[self.pos..]
    }

    // -------------------------------------------------------------------------
    // Public API: Character navigation
    // -------------------------------------------------------------------------

    pub fn peek(&self) -> Option<char> {
        self.source[self.pos..].chars().next()
    }

    pub fn advance(&mut self) -> Option<char> {
        if let Some(c) = self.peek() {
            self.pos += c.len_utf8();
            if c == '\n' {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
            Some(c)
        } else {
            None
        }
    }

    // -------------------------------------------------------------------------
    // Public API: Whitespace handling
    // -------------------------------------------------------------------------

    /// Skip horizontal whitespace (spaces and tabs).
    /// Note: Indent-aware whitespace handling is done by the VM internally.
    pub fn skip_ws(&mut self) {
        while let Some(' ') | Some('\t') = self.peek() {
            self.advance();
        }
    }

    pub fn skip_ws_all(&mut self) {
        loop {
            match self.peek() {
                Some(' ') | Some('\t') | Some('\n') | Some('\r') => {
                    self.advance();
                }
                Some('#') => {
                    while let Some(c) = self.peek() {
                        if c == '\n' {
                            break;
                        }
                        self.advance();
                    }
                }
                _ => break,
            }
        }
    }

    pub fn reset_indent(&mut self) {
        self.indent_stack.clear();
        self.indent_mode = IndentMode::None;
        self.pending_anchor = false;
    }

    // -------------------------------------------------------------------------
    // Public API: Error handling
    // -------------------------------------------------------------------------

    pub fn clear_furthest(&mut self) {
        self.furthest_pos = self.pos;
        self.furthest_loc = self.current_loc();
        self.furthest_expected.clear();
    }

    fn get_source_line(&self, offset: usize) -> &str {
        let start = match self.source[..offset].rfind('\n') {
            Some(i) => i + 1,
            None => 0,
        };
        let end = match self.source[offset..].find('\n') {
            Some(i) => offset + i,
            None => self.source.len(),
        };
        &self.source[start..end]
    }

    pub fn make_error(&self) -> ParseError {
        let loc = self.furthest_loc;
        let source_line = self.get_source_line(self.furthest_pos).to_string();

        let expected_str = if self.furthest_expected.is_empty() {
            "valid token".to_string()
        } else {
            self.furthest_expected.join(" or ")
        };

        let found_str = if self.furthest_pos >= self.source.len() {
            "end of file".to_string()
        } else {
            let c = self.source[self.furthest_pos..].chars().next().unwrap();
            if c.is_whitespace() {
                "whitespace".to_string()
            } else {
                format!("'{}'", c)
            }
        };

        let msg = format!("expected {}, found {}", expected_str, found_str);

        ParseError {
            msg,
            loc,
            source_line,
        }
    }

    /// Check if the current line starts with a VM directive and handle it.
    /// Returns true if a directive was handled.
    pub fn check_vm_directive(&mut self) -> bool {
        if self.remaining().starts_with("#!vm:dump") {
            // Skip to end of line
            while let Some(c) = self.peek() {
                self.advance();
                if c == '\n' {
                    break;
                }
            }
            self.dump();
            true
        } else if self.remaining().starts_with("#!vm:trace") {
            // Enable tracing
            while let Some(c) = self.peek() {
                self.advance();
                if c == '\n' {
                    break;
                }
            }
            self.trace_enabled = true;
            eprintln!("[TRACE] Execution tracing enabled");
            true
        } else if self.remaining().starts_with("#!vm:rules") {
            // Just dump rules without full grammar
            while let Some(c) = self.peek() {
                self.advance();
                if c == '\n' {
                    break;
                }
            }
            self.dump_rules();
            true
        } else {
            false
        }
    }

    // -------------------------------------------------------------------------
    // Private: Grammar finalization
    // -------------------------------------------------------------------------

    /// Recompile all rules into a fresh grammar
    fn finalize_grammar(&mut self) {
        if self.grammar_dirty {
            let mut compiler = Compiler::new(self.arena);
            for rule in &self.rules {
                compiler.compile_rule(rule, &mut self.strings);
            }
            let grammar = compiler.finish();
            self.grammar = Some(self.arena.alloc(grammar));
            self.grammar_dirty = false;
        }
    }

    // -------------------------------------------------------------------------
    // Private: Parse category implementation
    // -------------------------------------------------------------------------

    fn parse_category_impl(&mut self, category: &str) -> Option<&'a SyntaxNode<'a>> {
        self.skip_ws();

        // Ensure grammar is finalized
        self.finalize_grammar();

        let grammar = match self.grammar {
            Some(g) => g,
            None => return None,
        };

        // Create VM with current position state
        let mut vm = VM::new(self.arena, grammar, self.module);
        vm.pos = self.pos;
        vm.line = self.line;
        vm.col = self.col;
        vm.indent_stack = std::mem::take(&mut self.indent_stack);
        vm.indent_mode = self.indent_mode;
        vm.trace_enabled = self.trace_enabled;
        vm.pending_anchor = self.pending_anchor;
        vm.furthest_pos = self.furthest_pos;
        vm.furthest_loc = self.furthest_loc;
        vm.furthest_expected = std::mem::take(&mut self.furthest_expected);

        // Parse
        let result = vm.parse_category_internal(category);

        // Restore state from VM
        self.pos = vm.pos;
        self.line = vm.line;
        self.col = vm.col;
        self.indent_stack = vm.indent_stack;
        self.indent_mode = vm.indent_mode;
        self.pending_anchor = vm.pending_anchor;
        self.furthest_pos = vm.furthest_pos;
        self.furthest_loc = vm.furthest_loc;
        self.furthest_expected = vm.furthest_expected;

        result
    }
}

// =============================================================================
// Parser Trait Implementation
// =============================================================================

impl<'a> Parser<'a> for VMParser<'a> {
    fn is_eof(&self) -> bool {
        self.pos >= self.source.len()
    }

    fn next_command(&mut self) -> Option<&'a SyntaxNode<'a>> {
        // Skip all whitespace including newlines
        self.skip_ws_all();
        if self.is_eof() {
            return None;
        }

        // Reset indent state for each command
        self.reset_indent();

        // Clear furthest failure tracking
        self.clear_furthest();

        // Parse a Command
        self.parse_category("Command")
    }

    fn parse_category(&mut self, category: &str) -> Option<&'a SyntaxNode<'a>> {
        self.parse_category_impl(category)
    }

    fn error(&self) -> Option<ParseError> {
        if self.furthest_expected.is_empty() && self.furthest_pos == 0 {
            return None;
        }
        Some(self.make_error())
    }

    fn skip_to_next_line(&mut self) {
        loop {
            match self.advance() {
                Some('\n') | None => break,
                _ => {}
            }
        }
    }

    fn set_source(&mut self, module: &'a SourceModule<'a>) {
        self.module = module;
        self.source = module.text;
        self.pos = 0;
        self.line = 1;
        self.col = 1;
        self.furthest_pos = 0;
        self.furthest_loc = SourceLoc::new(0, 1, 1);
        self.furthest_expected.clear();
        self.indent_stack.clear();
        self.indent_mode = IndentMode::None;
        self.pending_anchor = false;
    }

    fn add_rule(&mut self, rule: &'a SyntaxRule<'a>) {
        self.rules.push(rule);
        self.grammar_dirty = true;
    }

    fn strings_mut(&mut self) -> &mut StringInterner<'a> {
        &mut self.strings
    }

    fn set_trace(&mut self, enabled: bool) {
        self.trace_enabled = enabled;
    }

    fn dump(&mut self) {
        use std::io::Write;
        let stderr = std::io::stderr();
        let mut out = stderr.lock();

        writeln!(out, "\n{:=^60}", " VM PARSER STATE ").ok();

        // Parser position
        writeln!(out, "\n--- Parser Position ---").ok();
        writeln!(out, "  pos: {} (line {}, col {})", self.pos, self.line, self.col).ok();
        writeln!(out, "  source length: {}", self.source.len()).ok();
        if self.pos < self.source.len() {
            let preview_end = (self.pos + 40).min(self.source.len());
            let mut preview = String::new();
            for c in self.source[self.pos..preview_end].chars() {
                if c == '\n' {
                    preview.push('\u{21b5}');
                } else {
                    preview.push(c);
                }
            }
            writeln!(out, "  remaining: {:?}...", preview).ok();
        }

        // Indent state
        writeln!(out, "\n--- Indent State ---").ok();
        writeln!(out, "  mode: {:?}", self.indent_mode).ok();
        writeln!(out, "  pending_anchor: {}", self.pending_anchor).ok();
        writeln!(out, "  indent_stack: {:?}", self.indent_stack).ok();

        // Error tracking
        writeln!(out, "\n--- Error Tracking ---").ok();
        writeln!(out, "  furthest_pos: {} (line {}, col {})",
            self.furthest_pos, self.furthest_loc.line, self.furthest_loc.col).ok();
        writeln!(out, "  furthest_expected: {:?}", self.furthest_expected).ok();

        // Rules
        writeln!(out, "\n--- Registered Rules ({}) ---", self.rules.len()).ok();
        for (i, rule) in self.rules.iter().enumerate() {
            let lr = if rule.is_left_recursive { " [LR]" } else { "" };
            writeln!(out, "  [{:3}] {}.{}{}", i, rule.category, rule.name, lr).ok();
        }

        // Dump compiled grammar if available
        if self.grammar_dirty {
            writeln!(out, "\n[Grammar needs recompilation - compiling now...]").ok();
            self.finalize_grammar();
        }

        if let Some(grammar) = self.grammar {
            grammar.dump();
        } else {
            writeln!(out, "\n[No compiled grammar]").ok();
        }

        writeln!(out, "{:=^60}\n", "").ok();
    }

    fn dump_rules(&self) {
        use std::io::Write;
        let stderr = std::io::stderr();
        let mut out = stderr.lock();

        writeln!(out, "\n--- Registered Rules ({}) ---", self.rules.len()).ok();
        for (i, rule) in self.rules.iter().enumerate() {
            let lr = if rule.is_left_recursive { " [LR]" } else { "" };
            writeln!(out, "  [{:3}] {}.{}{}", i, rule.category, rule.name, lr).ok();
            // Also dump pattern atoms
            write!(out, "        pattern: ").ok();
            for (j, aq) in rule.pattern.iter().enumerate() {
                if j > 0 { write!(out, " ").ok(); }
                write!(out, "{}", format_atom_quant(aq)).ok();
            }
            writeln!(out).ok();
        }
        writeln!(out).ok();
    }
}

// =============================================================================
// Debug Formatting Helpers
// =============================================================================

/// Format an AtomWithQuant for debug output
fn format_atom_quant(aq: &AtomWithQuant) -> String {
    let atom_str = match &aq.atom {
        Atom::Literal(s) => format!("{:?}", s),
        Atom::CharSet { ranges, negated } => {
            let neg = if *negated { "^" } else { "" };
            let mut s = String::new();
            for (lo, hi) in *ranges {
                if lo == hi {
                    s.push(*lo);
                } else {
                    s.push(*lo);
                    s.push_str("..");
                    s.push(*hi);
                }
            }
            format!("[{}{}]", neg, s)
        }
        Atom::CharClass(c) => match c {
            CharClass::Digit => "\\d".to_string(),
            CharClass::Word => "\\w".to_string(),
            CharClass::Whitespace => "\\s".to_string(),
        },
        Atom::Group { alternatives } => {
            let mut alts: Vec<String> = Vec::new();
            for alt in *alternatives {
                let mut parts: Vec<String> = Vec::new();
                for a in *alt {
                    parts.push(format_atom_quant(a));
                }
                alts.push(parts.join(" "));
            }
            format!("({})", alts.join(" | "))
        }
        Atom::CategoryRef(c) => c.to_string(),
        Atom::IndentAnchor => "|>".to_string(),
        Atom::IndentStrict => ">>".to_string(),
        Atom::IndentLax => ">".to_string(),
        Atom::IndentAwareWs => "WS*".to_string(),
    };

    let quant_str = match aq.quant {
        Quantifier::One => "",
        Quantifier::Optional => "?",
        Quantifier::Star => "*",
        Quantifier::Plus => "+",
    };

    let prec_str = match aq.precedence {
        Some(p) => format!("@{}", p),
        None => String::new(),
    };

    format!("{}{}{}", atom_str, quant_str, prec_str)
}


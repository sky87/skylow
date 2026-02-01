//! Parsing Virtual Machine execution.

use bumpalo::{collections::Vec as BumpVec, Bump};
use skylow_common::debug::{create_logger, Logger};
use skylow_common::intern::StringInterner;
use skylow_common::{log, log_detail, log_fail, log_success};

use crate::node::{SourceLoc, SyntaxNode};
use crate::syntax::CharClass;

use super::grammar::CompiledGrammar;
use super::instruction::{
    op, opcode, operand, operand_signed, INDENT_LAX, INDENT_NONE, INDENT_STRICT,
};

// Internal node names
const NODE_LITERAL: &str = "_literal";
const NODE_CHAR: &str = "_char";
const NODE_CHARCLASS: &str = "_charclass";
const NODE_CHARSET: &str = "_charset";
const NODE_INDENT: &str = "_indent";
const NODE_ANCHOR: &str = "_anchor";
const NODE_STRICT: &str = "_strict";
const NODE_LAX: &str = "_lax";
const NODE_WS: &str = "_ws";

/// Indentation constraint mode
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IndentMode {
    None,
    Lax,
    Strict,
}

/// Call frame for category calls (used for recursive calls)
#[derive(Clone)]
#[allow(dead_code)]
struct CallFrame {
    return_pc: usize,
    node_stack_depth: usize,
    category_id: u32,
}

/// Backtrack point
#[derive(Clone)]
struct BacktrackPoint {
    pc: usize,
    pos: usize,
    line: u32,
    col: u32,
    call_stack_depth: usize,
    node_stack_depth: usize,
    children_depth: usize, // depth of local children vec at time of CHOICE
    indent_stack_depth: usize,
    indent_mode: IndentMode,
    pending_anchor: bool,
}

/// Node builder on the stack (reserved for nested node construction)
#[allow(dead_code)]
struct NodeBuilder<'a> {
    category: &'a str,
    rule: &'a str,
    start: SourceLoc,
    children: BumpVec<'a, &'a SyntaxNode<'a>>,
}

/// Parsing Virtual Machine
pub struct VM<'a, 'src> {
    // Bytecode
    grammar: &'a CompiledGrammar<'a>,

    // Input
    source: &'src str,
    pub pos: usize,
    pub line: u32,
    pub col: u32,

    // Call stack
    call_stack: Vec<CallFrame>,

    // Backtrack stack
    backtrack_stack: Vec<BacktrackPoint>,

    // Node building
    node_stack: Vec<NodeBuilder<'a>>,

    // Indentation
    pub indent_stack: Vec<u32>,
    pub indent_mode: IndentMode,
    pub pending_anchor: bool,

    // Error tracking
    pub furthest_pos: usize,
    pub furthest_loc: SourceLoc,
    pub furthest_expected: Vec<String>,

    // Execution tracing
    pub trace_enabled: bool,
    trace_depth: usize,

    // Arena for allocations
    arena: &'a Bump,

    // String interner
    strings: StringInterner<'a>,

    // Logger
    log: Logger,
}

impl<'a, 'src> VM<'a, 'src> {
    pub fn new(arena: &'a Bump, grammar: &'a CompiledGrammar<'a>, source: &'src str) -> Self {
        Self {
            grammar,
            source,
            pos: 0,
            line: 1,
            col: 1,
            call_stack: Vec::new(),
            backtrack_stack: Vec::new(),
            node_stack: Vec::new(),
            indent_stack: Vec::new(),
            indent_mode: IndentMode::None,
            pending_anchor: false,
            furthest_pos: 0,
            furthest_loc: SourceLoc::new(0, 1, 1),
            furthest_expected: Vec::new(),
            trace_enabled: false,
            trace_depth: 0,
            arena,
            strings: StringInterner::new(arena),
            log: create_logger("parservm"),
        }
    }

    // -------------------------------------------------------------------------
    // Input Navigation
    // -------------------------------------------------------------------------

    #[inline]
    fn remaining(&self) -> &'src str {
        &self.source[self.pos..]
    }

    #[inline]
    fn peek(&self) -> Option<char> {
        self.source[self.pos..].chars().next()
    }

    #[inline]
    fn advance(&mut self) -> Option<char> {
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

    fn current_loc(&self) -> SourceLoc {
        SourceLoc::new(self.pos as u32, self.line, self.col)
    }

    // -------------------------------------------------------------------------
    // Error Tracking
    // -------------------------------------------------------------------------

    fn record_expected(&mut self, expected: &str) {
        if self.pos > self.furthest_pos {
            self.furthest_pos = self.pos;
            self.furthest_loc = self.current_loc();
            self.furthest_expected.clear();
            self.furthest_expected.push(expected.to_string());
        } else if self.pos == self.furthest_pos {
            let mut found = false;
            for exp in &self.furthest_expected {
                if exp == expected {
                    found = true;
                    break;
                }
            }
            if !found {
                self.furthest_expected.push(expected.to_string());
            }
        }
    }

    // -------------------------------------------------------------------------
    // Whitespace Handling
    // -------------------------------------------------------------------------

    fn skip_ws(&mut self) -> bool {
        loop {
            match self.peek() {
                Some(' ') | Some('\t') => {
                    self.advance();
                }
                Some('\n') if !self.indent_stack.is_empty() || self.pending_anchor => {
                    let saved_pos = self.pos;
                    let saved_line = self.line;
                    let saved_col = self.col;

                    self.advance();
                    while let Some(' ') | Some('\t') = self.peek() {
                        self.advance();
                    }
                    if !self.indent_stack.is_empty() && !self.indent_check() {
                        self.pos = saved_pos;
                        self.line = saved_line;
                        self.col = saved_col;
                        return false;
                    }
                }
                Some('#') if !self.indent_stack.is_empty() || self.pending_anchor => {
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

        if self.pending_anchor {
            self.indent_stack.push(self.col);
            self.pending_anchor = false;
        }

        true
    }

    fn indent_anchor(&self) -> Option<u32> {
        self.indent_stack.last().copied()
    }

    fn indent_check(&self) -> bool {
        let anchor = match self.indent_anchor() {
            Some(a) => a,
            None => return true,
        };
        match self.indent_mode {
            IndentMode::None => true,
            IndentMode::Lax => self.col >= anchor,
            IndentMode::Strict => self.col > anchor,
        }
    }

    // -------------------------------------------------------------------------
    // Matching
    // -------------------------------------------------------------------------

    fn match_literal(&mut self, str_id: u32) -> bool {
        let lit = self.grammar.strings[str_id as usize];
        if self.remaining().starts_with(lit) {
            for _ in lit.chars() {
                self.advance();
            }
            true
        } else {
            let expected = format!("\"{}\"", lit);
            self.record_expected(&expected);
            false
        }
    }

    fn match_char_class(&mut self, class_id: u32) -> Option<char> {
        let c = self.peek();
        let class = match class_id {
            0 => CharClass::Digit,
            1 => CharClass::Word,
            2 => CharClass::Whitespace,
            _ => return None,
        };
        let matches = match c {
            Some(c) => match class {
                CharClass::Digit => c.is_ascii_digit(),
                CharClass::Word => c.is_ascii_alphanumeric() || c == '_',
                CharClass::Whitespace => c.is_ascii_whitespace(),
            },
            None => false,
        };
        if matches {
            self.advance();
            c
        } else {
            let expected = match class {
                CharClass::Digit => "digit",
                CharClass::Word => "word character",
                CharClass::Whitespace => "whitespace",
            };
            self.record_expected(expected);
            None
        }
    }

    fn match_char_set(&mut self, charset_id: u32) -> Option<char> {
        let c = match self.peek() {
            Some(c) => c,
            None => {
                self.record_expected("character");
                return None;
            }
        };
        let charset = &self.grammar.charsets[charset_id as usize];
        if charset.matches(c) {
            self.advance();
            Some(c)
        } else {
            self.record_expected("character");
            None
        }
    }

    // -------------------------------------------------------------------------
    // Public API
    // -------------------------------------------------------------------------

    /// Parse a category and return the resulting node
    pub fn parse_category_internal(&mut self, category: &str) -> Option<&'a SyntaxNode<'a>> {
        let category_id = match self.grammar.get_category(category) {
            Some(id) => id,
            None => {
                self.record_expected(category);
                return None;
            }
        };

        self.parse_category_by_id_with_prec(category_id, 0)
    }

    fn parse_category_by_id(&mut self, category_id: u32, min_prec: u32) -> Option<&'a SyntaxNode<'a>> {
        self.parse_category_by_id_with_prec(category_id, min_prec)
    }

    fn parse_category_by_id_with_prec(&mut self, category_id: u32, min_prec: u32) -> Option<&'a SyntaxNode<'a>> {
        let dispatch_table = &self.grammar.dispatch_tables[category_id as usize];
        let category_name = self.grammar.category_names[category_id as usize];
        let start = self.current_loc();

        if self.trace_enabled {
            let indent = "  ".repeat(self.trace_depth);
            eprintln!("{}[TRACE] parse_category {} at pos {} (L{}:{})",
                indent, category_name, self.pos, start.line, start.col);
            if self.pos < self.source.len() {
                let preview_end = (self.pos + 20).min(self.source.len());
                let mut preview = String::new();
                for c in self.source[self.pos..preview_end].chars() {
                    if c == '\n' {
                        preview.push('\u{21b5}');
                    } else {
                        preview.push(c);
                    }
                }
                eprintln!("{}         remaining: {:?}", indent, preview);
            }
            self.trace_depth += 1;
        }

        log!(self.log, "parse {} at L{}:{}", category_name, start.line, start.col);
        self.log.push_indent();

        // Phase 1: Try prefix rules (non-left-recursive)
        let mut left: Option<&'a SyntaxNode<'a>> = None;

        for &rule_idx in &dispatch_table.prefix_rules {
            let rule = &self.grammar.rules[rule_idx];

            if self.trace_enabled {
                let indent = "  ".repeat(self.trace_depth);
                eprintln!("{}[TRACE] try prefix rule {}.{} (idx {})",
                    indent, rule.category, rule.name, rule_idx);
            }

            log_detail!(self.log, "try rule {}", rule.name);

            // Save state
            let saved_pos = self.pos;
            let saved_line = self.line;
            let saved_col = self.col;
            let saved_indent_depth = self.indent_stack.len();
            let saved_indent_mode = self.indent_mode;
            let saved_pending_anchor = self.pending_anchor;

            if let Some(node) = self.execute_rule(rule_idx, None) {
                if self.trace_enabled {
                    let indent = "  ".repeat(self.trace_depth);
                    eprintln!("{}[TRACE] SUCCESS: matched {}.{}", indent, rule.category, rule.name);
                }
                log_success!(self.log, "matched {}", rule.name);
                left = Some(node);
                break;
            }

            if self.trace_enabled {
                let indent = "  ".repeat(self.trace_depth);
                eprintln!("{}[TRACE] FAILED: {}.{}, restoring pos {} -> {}",
                    indent, rule.category, rule.name, self.pos, saved_pos);
            }

            // Restore state
            self.pos = saved_pos;
            self.line = saved_line;
            self.col = saved_col;
            self.indent_stack.truncate(saved_indent_depth);
            self.indent_mode = saved_indent_mode;
            self.pending_anchor = saved_pending_anchor;
        }

        if self.trace_enabled {
            self.trace_depth = self.trace_depth.saturating_sub(1);
        }

        let mut left = match left {
            Some(l) => l,
            None => {
                if self.trace_enabled {
                    let indent = "  ".repeat(self.trace_depth);
                    eprintln!("{}[TRACE] no prefix rule matched for {}", indent, category_name);
                }
                log_fail!(self.log, "no prefix rule matched for {}", category_name);
                self.log.pop_indent();
                return None;
            }
        };

        // Phase 2: Pratt loop for infix rules (left-recursive)
        'outer: loop {
            for &rule_idx in &dispatch_table.infix_rules {
                let rule = &self.grammar.rules[rule_idx];

                // Check precedence constraint: only apply this infix rule if its
                // operator precedence is >= min_prec
                let rule_prec = rule.operator_precedence.unwrap_or(0);
                if self.trace_enabled {
                    eprintln!("[DEBUG] Pratt: trying {} (rule_prec={}, min_prec={})", rule.name, rule_prec, min_prec);
                }
                if rule_prec < min_prec {
                    if self.trace_enabled {
                        eprintln!("[DEBUG] Pratt: skipping {} due to precedence", rule.name);
                    }
                    continue;
                }

                log_detail!(self.log, "try left-recursive rule {} (prec={})", rule.name, rule_prec);

                // Save state
                let saved_pos = self.pos;
                let saved_line = self.line;
                let saved_col = self.col;
                let saved_indent_depth = self.indent_stack.len();
                let saved_indent_mode = self.indent_mode;
                let saved_pending_anchor = self.pending_anchor;

                if let Some(node) = self.execute_rule(rule_idx, Some(left)) {
                    log_success!(self.log, "extended with {}", rule.name);
                    left = node;
                    continue 'outer;
                }

                // Restore state
                self.pos = saved_pos;
                self.line = saved_line;
                self.col = saved_col;
                self.indent_stack.truncate(saved_indent_depth);
                self.indent_mode = saved_indent_mode;
                self.pending_anchor = saved_pending_anchor;
            }
            break;
        }

        self.log.pop_indent();
        Some(left)
    }

    /// Execute a single rule's bytecode
    fn execute_rule(
        &mut self,
        rule_idx: usize,
        left_node: Option<&'a SyntaxNode<'a>>,
    ) -> Option<&'a SyntaxNode<'a>> {
        let rule = &self.grammar.rules[rule_idx];
        let mut pc = rule.bytecode_offset;
        let start = self.current_loc();

        // Start building node
        let mut children = BumpVec::new_in(self.arena);
        if let Some(left) = left_node {
            children.push(left);
        }

        // Save indent state for rule-level restoration
        let saved_indent_depth = self.indent_stack.len();
        let saved_indent_mode = self.indent_mode;
        let saved_pending_anchor = self.pending_anchor;

        // Track the initial backtrack stack depth for this rule
        let initial_backtrack_depth = self.backtrack_stack.len();

        // Helper macro to handle failure with backtracking
        macro_rules! handle_failure {
            () => {
                // Try to backtrack within this rule's scope
                if self.backtrack_stack.len() > initial_backtrack_depth {
                    if let Some(bp) = self.backtrack_stack.pop() {
                        pc = bp.pc;
                        self.pos = bp.pos;
                        self.line = bp.line;
                        self.col = bp.col;
                        self.call_stack.truncate(bp.call_stack_depth);
                        self.node_stack.truncate(bp.node_stack_depth);
                        children.truncate(bp.children_depth);
                        self.indent_stack.truncate(bp.indent_stack_depth);
                        self.indent_mode = bp.indent_mode;
                        self.pending_anchor = bp.pending_anchor;
                        continue;
                    }
                }
                // No backtrack point - rule fails
                self.indent_stack.truncate(saved_indent_depth);
                self.indent_mode = saved_indent_mode;
                self.pending_anchor = saved_pending_anchor;
                // Clean up any backtrack points we added
                self.backtrack_stack.truncate(initial_backtrack_depth);
                return None;
            };
        }

        loop {
            if pc >= self.grammar.code.len() {
                break;
            }

            let instr = self.grammar.code[pc];
            let opc = opcode(instr);
            let oper = operand(instr);

            match opc {
                op::LITERAL => {
                    // Skip whitespace before matching literal (for grammar rules)
                    if !self.skip_ws() {
                        handle_failure!();
                    }
                    let lit_start = self.current_loc();
                    if self.match_literal(oper) {
                        let lit_str = self.grammar.strings[oper as usize];
                        let text = self.arena.alloc_str(lit_str);
                        let node = self.arena.alloc(SyntaxNode::leaf(
                            self.strings.intern(NODE_LITERAL),
                            self.strings.intern(NODE_LITERAL),
                            lit_start,
                            self.pos as u32,
                            text,
                        ));
                        children.push(node);
                        pc += 1;
                    } else {
                        handle_failure!();
                    }
                }

                op::CHAR_CLASS => {
                    let char_start = self.current_loc();
                    if let Some(c) = self.match_char_class(oper) {
                        let mut buf = [0u8; 4];
                        let text = self.arena.alloc_str(c.encode_utf8(&mut buf));
                        let node = self.arena.alloc(SyntaxNode::leaf(
                            self.strings.intern(NODE_CHAR),
                            self.strings.intern(NODE_CHARCLASS),
                            char_start,
                            self.pos as u32,
                            text,
                        ));
                        children.push(node);
                        pc += 1;
                    } else {
                        handle_failure!();
                    }
                }

                op::CHAR_SET => {
                    let char_start = self.current_loc();
                    if let Some(c) = self.match_char_set(oper) {
                        let mut buf = [0u8; 4];
                        let text = self.arena.alloc_str(c.encode_utf8(&mut buf));
                        let node = self.arena.alloc(SyntaxNode::leaf(
                            self.strings.intern(NODE_CHAR),
                            self.strings.intern(NODE_CHARSET),
                            char_start,
                            self.pos as u32,
                            text,
                        ));
                        children.push(node);
                        pc += 1;
                    } else {
                        handle_failure!();
                    }
                }

                op::CALL => {
                    if !self.skip_ws() {
                        handle_failure!();
                    }
                    // Decode category_id (bits 0-15) and min_prec (bits 16-23)
                    let category_id = oper & 0xFFFF;
                    let min_prec = (oper >> 16) & 0xFF;
                    if self.trace_enabled {
                        let cat_name = self.grammar.category_names.get(category_id as usize).unwrap_or(&"?");
                        eprintln!("[DEBUG] CALL {} with min_prec={}", cat_name, min_prec);
                    }
                    if let Some(node) = self.parse_category_by_id(category_id, min_prec) {
                        children.push(node);
                        pc += 1;
                    } else {
                        handle_failure!();
                    }
                }

                op::INDENT_ANCHOR => {
                    self.pending_anchor = true;
                    let node_start = self.current_loc();
                    let node = self.arena.alloc(SyntaxNode::leaf(
                        self.strings.intern(NODE_INDENT),
                        self.strings.intern(NODE_ANCHOR),
                        node_start,
                        self.pos as u32,
                        "",
                    ));
                    children.push(node);
                    pc += 1;
                }

                op::INDENT_MODE => {
                    let node_start = self.current_loc();
                    match oper {
                        INDENT_NONE => self.indent_mode = IndentMode::None,
                        INDENT_LAX => {
                            self.indent_mode = IndentMode::Lax;
                            let node = self.arena.alloc(SyntaxNode::leaf(
                                self.strings.intern(NODE_INDENT),
                                self.strings.intern(NODE_LAX),
                                node_start,
                                self.pos as u32,
                                "",
                            ));
                            children.push(node);
                        }
                        INDENT_STRICT => {
                            self.indent_mode = IndentMode::Strict;
                            let node = self.arena.alloc(SyntaxNode::leaf(
                                self.strings.intern(NODE_INDENT),
                                self.strings.intern(NODE_STRICT),
                                node_start,
                                self.pos as u32,
                                "",
                            ));
                            children.push(node);
                        }
                        _ => {}
                    }
                    pc += 1;
                }

                op::SKIP_WS => {
                    let start_pos = self.pos;
                    // SKIP_WS_INDENT: skip whitespace respecting indentation
                    if !self.skip_ws() {
                        handle_failure!();
                    }
                    // Add whitespace node if we consumed whitespace
                    if self.pos > start_pos {
                        let ws_start = SourceLoc::new(start_pos as u32, self.line, self.col);
                        let node = self.arena.alloc(SyntaxNode::leaf(
                            self.strings.intern(NODE_WS),
                            self.strings.intern(NODE_WS),
                            ws_start,
                            self.pos as u32,
                            "",
                        ));
                        children.push(node);
                    }
                    pc += 1;
                }

                op::CHOICE => {
                    // Push backtrack point
                    let offset = operand_signed(instr);
                    self.backtrack_stack.push(BacktrackPoint {
                        pc: ((pc as i32) + offset) as usize,
                        pos: self.pos,
                        line: self.line,
                        col: self.col,
                        call_stack_depth: self.call_stack.len(),
                        node_stack_depth: self.node_stack.len(),
                        children_depth: children.len(),
                        indent_stack_depth: self.indent_stack.len(),
                        indent_mode: self.indent_mode,
                        pending_anchor: self.pending_anchor,
                    });
                    pc += 1;
                }

                op::COMMIT => {
                    // Pop backtrack point and jump
                    self.backtrack_stack.pop();
                    let offset = operand_signed(instr);
                    pc = ((pc as i32) + offset) as usize;
                }

                op::FAIL => {
                    // Explicit failure - try to backtrack
                    handle_failure!();
                }

                op::JUMP => {
                    let offset = operand_signed(instr);
                    pc = ((pc as i32) + offset) as usize;
                }

                op::END | op::RETURN => {
                    break;
                }

                _ => {
                    pc += 1;
                }
            }
        }

        // Clean up backtrack points from this rule
        self.backtrack_stack.truncate(initial_backtrack_depth);

        // Restore indent state
        self.indent_stack.truncate(saved_indent_depth);
        self.indent_mode = saved_indent_mode;
        self.pending_anchor = saved_pending_anchor;

        // Build final node
        let children_slice = children.into_bump_slice();
        let node = self.arena.alloc(SyntaxNode::branch(
            rule.category,
            rule.name,
            start,
            self.pos as u32,
            children_slice,
        ));

        Some(node)
    }
}


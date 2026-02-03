use bumpalo::{collections::Vec as BumpVec, Bump};
use common::debug::{create_logger, Logger};
use common::intern::StringInterner;
use common::{log, log_detail, log_fail, log_success, SourceInfo, SourceLoc, SourceModule};

use crate::node::{ParseError, SyntaxNode};
use crate::parser_trait::Parser;
use crate::syntax::{Atom, AtomWithQuant, CharClass, Quantifier, RuleTable, SyntaxRule};

// =============================================================================
// Local Constants: Internal Node Names
// =============================================================================
// These are category/rule names for internal parse tree nodes that are not
// visible in the final output (they start with '_').

const NODE_LITERAL: &str = "_literal";
const NODE_CHAR: &str = "_char";
const NODE_CHARCLASS: &str = "_charclass";
const NODE_CHARSET: &str = "_charset";
const NODE_GROUP: &str = "_group";
const NODE_INDENT: &str = "_indent";
const NODE_ANCHOR: &str = "_anchor";
const NODE_STRICT: &str = "_strict";
const NODE_LAX: &str = "_lax";
const NODE_WS: &str = "_ws";

/// Get the operator precedence from a left-recursive rule.
/// This is the precedence of the first atom after the initial CategoryRef,
/// skipping any whitespace atoms that may have been inserted.
fn get_rule_operator_precedence(rule: &SyntaxRule) -> Option<u32> {
    use crate::syntax::Atom;

    // For left-recursive rules, find the operator precedence by looking at
    // the first significant atom after the initial CategoryRef (left operand).
    // We need to skip WS atoms that may have been inserted by add_implicit_whitespace.
    let mut found_initial_cat_ref = false;
    for aq in rule.pattern.iter() {
        // Skip the initial CategoryRef (left operand)
        if !found_initial_cat_ref {
            if matches!(aq.atom, Atom::CategoryRef(_)) {
                found_initial_cat_ref = true;
                continue;
            }
            // First element is not a CategoryRef - rule is not truly left-recursive
            break;
        }

        // Skip whitespace atoms
        if matches!(aq.atom, Atom::IndentAwareWs) {
            continue;
        }

        // Found the first significant atom after the left operand
        // Return its precedence (may be None if not annotated)
        return aq.precedence;
    }
    None
}

/// Indentation constraint mode for indent-sensitive parsing.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum IndentMode {
    /// No indentation checking.
    None,
    /// Lax mode: current column must be >= anchor column.
    Lax,
    /// Strict mode: current column must be > anchor column.
    Strict,
}

/// Checkpoint for parser backtracking.
#[derive(Debug, Clone)]
pub(crate) struct ParserCheckpoint {
    pos: usize,
    line: u32,
    col: u32,
    indent_depth: usize,
    indent_mode: IndentMode,
    pending_anchor: bool,
}

/// Interpreted parser for the SkyLow language.
///
/// Implements a Pratt parser with backtracking support for parsing expressions
/// according to dynamically registered syntax rules.
///
/// # Usage
///
/// ```ignore
/// let arena = Bump::new();
/// let mut parser = InterpretedParser::new(&arena, source);
///
/// while !parser.is_eof() {
///     match parser.parse_category("Command") {
///         Some(node) => { /* process node */ }
///         None => {
///             if let Some(err) = parser.error() {
///                 eprintln!("{}", err.msg);
///             }
///         }
///     }
/// }
/// ```
pub struct InterpretedParser<'a> {
    /// The current source module being parsed.
    module: &'a SourceModule<'a>,
    /// Cached source text from the module (for efficient access).
    source: &'a str,
    pos: usize,
    line: u32,
    col: u32,

    // Indentation tracking for indent-sensitive parsing.
    indent_stack: Vec<u32>,
    indent_mode: IndentMode,
    pending_anchor: bool,

    // Furthest failure tracking for error reporting.
    furthest_pos: usize,
    furthest_loc: SourceLoc,
    furthest_expected: Vec<String>,

    arena: &'a Bump,
    rules: RuleTable<'a>,
    strings: StringInterner<'a>,

    log: Logger,
    trace_enabled: bool,
}

impl<'a> InterpretedParser<'a> {
    // =========================================================================
    // Constructor
    // =========================================================================

    /// Create a new parser for the given source module.
    pub fn new(arena: &'a Bump, module: &'a SourceModule<'a>) -> Self {
        // Cache the source text for efficient access (modules are immutable)
        let source = module.text;
        Self {
            module,
            source,
            pos: 0,
            line: 1,
            col: 1,
            indent_stack: Vec::new(),
            indent_mode: IndentMode::None,
            pending_anchor: false,
            furthest_pos: 0,
            furthest_loc: SourceLoc::new(0, 1, 1),
            furthest_expected: Vec::new(),
            log: create_logger("parser"),
            arena,
            rules: RuleTable::new(arena),
            strings: StringInterner::new(arena),
            trace_enabled: false,
        }
    }

    // =========================================================================
    // Internal API (crate-visible)
    // =========================================================================

    /// Get the current source location.
    pub(crate) fn current_loc(&self) -> SourceLoc {
        SourceLoc::new(self.pos as u32, self.line, self.col)
    }

    /// Get the remaining unparsed source text.
    pub(crate) fn remaining(&self) -> &'a str {
        &self.source[self.pos..]
    }

    /// Create a SourceInfo for the given span.
    fn make_info(&self, start: SourceLoc, end: u32) -> SourceInfo<'a> {
        SourceInfo::new(self.module, start, end)
    }

    /// Clear furthest failure tracking for a new top-level parse.
    fn clear_furthest(&mut self) {
        self.furthest_pos = self.pos;
        self.furthest_loc = self.current_loc();
        self.furthest_expected.clear();
    }

    /// Save the current parser state for later restoration.
    fn save(&self) -> ParserCheckpoint {
        ParserCheckpoint {
            pos: self.pos,
            line: self.line,
            col: self.col,
            indent_depth: self.indent_stack.len(),
            indent_mode: self.indent_mode,
            pending_anchor: self.pending_anchor,
        }
    }

    /// Restore parser state from a checkpoint.
    fn restore(&mut self, cp: ParserCheckpoint) {
        self.pos = cp.pos;
        self.line = cp.line;
        self.col = cp.col;
        self.indent_stack.truncate(cp.indent_depth);
        self.indent_mode = cp.indent_mode;
        self.pending_anchor = cp.pending_anchor;
    }

    /// Look at the next character without consuming it.
    fn peek(&self) -> Option<char> {
        self.source[self.pos..].chars().next()
    }

    /// Consume and return the next character, updating position tracking.
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

    /// Skip indent-aware whitespace.
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

                    // Skip this newline and any following blank lines
                    loop {
                        self.advance(); // Skip newline
                        // Skip spaces/tabs on the new line
                        while let Some(' ') | Some('\t') = self.peek() {
                            self.advance();
                        }
                        // Skip comment if present
                        if self.peek() == Some('#') {
                            while let Some(c) = self.peek() {
                                if c == '\n' {
                                    break;
                                }
                                self.advance();
                            }
                        }
                        // If next char is newline, this is a blank line - continue skipping
                        if self.peek() != Some('\n') {
                            break;
                        }
                    }

                    // Now check indent on the first non-blank line
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

    /// Skip all whitespace including newlines (for top-level parsing).
    fn skip_ws_all(&mut self) {
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

    /// Reset indent state for a new top-level command.
    fn reset_indent(&mut self) {
        self.indent_stack.clear();
        self.indent_mode = IndentMode::None;
        self.pending_anchor = false;
    }

    // =========================================================================
    // Private Implementation
    // =========================================================================

    fn make_error_internal(&self) -> ParseError {
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

    fn record_failure(&mut self, expected: &str) {
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

    fn get_source_line(&self, offset: usize) -> &str {
        let start = self.source[..offset]
            .rfind('\n')
            .map(|i| i + 1)
            .unwrap_or(0);

        let end = self.source[offset..]
            .find('\n')
            .map(|i| offset + i)
            .unwrap_or(self.source.len());

        &self.source[start..end]
    }

    fn indent_anchor(&self) -> Option<u32> {
        self.indent_stack.last().copied()
    }

    fn indent_check(&self) -> bool {
        let anchor = self.indent_anchor().expect("indent_check called with empty stack");
        match self.indent_mode {
            IndentMode::None => true,
            IndentMode::Lax => self.col >= anchor,
            IndentMode::Strict => self.col > anchor,
        }
    }

    fn match_literal(&mut self, lit: &str) -> bool {
        if self.remaining().starts_with(lit) {
            for _ in lit.chars() {
                self.advance();
            }
            true
        } else {
            let expected = format!("\"{}\"", lit);
            self.record_failure(&expected);
            false
        }
    }

    fn match_char_class(&mut self, class: CharClass) -> Option<char> {
        let c = self.peek();
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
            self.record_failure(expected);
            None
        }
    }

    fn match_char_set(&mut self, ranges: &[(char, char)], negated: bool) -> Option<char> {
        let c = match self.peek() {
            Some(c) => c,
            None => {
                self.record_failure("character");
                return None;
            }
        };
        let mut in_set = false;
        for &(lo, hi) in ranges {
            if c >= lo && c <= hi {
                in_set = true;
                break;
            }
        }
        if in_set != negated {
            self.advance();
            Some(c)
        } else {
            self.record_failure("character");
            None
        }
    }

    fn match_atom_once(&mut self, atom: &Atom<'a>, min_prec: u32) -> Option<&'a SyntaxNode<'a>> {
        let start = self.current_loc();

        match atom {
            Atom::Literal(lit) => {
                if self.match_literal(lit) {
                    let text = self.arena.alloc_str(lit);
                    let info = self.make_info(start, self.pos as u32);
                    let node = self.arena.alloc(SyntaxNode::leaf(
                        self.strings.intern(NODE_LITERAL),
                        self.strings.intern(NODE_LITERAL),
                        info,
                        text,
                    ));
                    Some(node)
                } else {
                    None
                }
            }

            Atom::CharClass(class) => {
                if let Some(c) = self.match_char_class(*class) {
                    let mut buf = [0u8; 4];
                    let text = self.arena.alloc_str(c.encode_utf8(&mut buf));
                    let info = self.make_info(start, self.pos as u32);
                    let node = self.arena.alloc(SyntaxNode::leaf(
                        self.strings.intern(NODE_CHAR),
                        self.strings.intern(NODE_CHARCLASS),
                        info,
                        text,
                    ));
                    Some(node)
                } else {
                    None
                }
            }

            Atom::CharSet { ranges, negated } => {
                if let Some(c) = self.match_char_set(ranges, *negated) {
                    let mut buf = [0u8; 4];
                    let text = self.arena.alloc_str(c.encode_utf8(&mut buf));
                    let info = self.make_info(start, self.pos as u32);
                    let node = self.arena.alloc(SyntaxNode::leaf(
                        self.strings.intern(NODE_CHAR),
                        self.strings.intern(NODE_CHARSET),
                        info,
                        text,
                    ));
                    Some(node)
                } else {
                    None
                }
            }

            Atom::Group { alternatives } => {
                for alt in *alternatives {
                    let cp = self.save();
                    if let Some(children) = self.match_pattern(alt) {
                        let children_slice = children.into_bump_slice();
                        let info = self.make_info(start, self.pos as u32);
                        let node = self.arena.alloc(SyntaxNode::branch(
                            self.strings.intern(NODE_GROUP),
                            self.strings.intern(NODE_GROUP),
                            info,
                            children_slice,
                        ));
                        return Some(node);
                    }
                    self.restore(cp);
                }
                None
            }

            Atom::CategoryRef(cat) => self.parse_category_with_precedence(cat, min_prec),

            Atom::IndentAnchor => {
                self.pending_anchor = true;
                let info = self.make_info(start, self.pos as u32);
                let node = self.arena.alloc(SyntaxNode::leaf(
                    self.strings.intern(NODE_INDENT),
                    self.strings.intern(NODE_ANCHOR),
                    info,
                    "",
                ));
                Some(node)
            }

            Atom::IndentStrict => {
                self.indent_mode = IndentMode::Strict;
                let info = self.make_info(start, self.pos as u32);
                let node = self.arena.alloc(SyntaxNode::leaf(
                    self.strings.intern(NODE_INDENT),
                    self.strings.intern(NODE_STRICT),
                    info,
                    "",
                ));
                Some(node)
            }

            Atom::IndentLax => {
                self.indent_mode = IndentMode::Lax;
                let info = self.make_info(start, self.pos as u32);
                let node = self.arena.alloc(SyntaxNode::leaf(
                    self.strings.intern(NODE_INDENT),
                    self.strings.intern(NODE_LAX),
                    info,
                    "",
                ));
                Some(node)
            }

            Atom::IndentAwareWs => {
                let start_pos = self.pos;
                if self.skip_ws() {
                    if self.pos > start_pos {
                        let info = self.make_info(start, self.pos as u32);
                        let node = self.arena.alloc(SyntaxNode::leaf(
                            self.strings.intern(NODE_WS),
                            self.strings.intern(NODE_WS),
                            info,
                            "",
                        ));
                        Some(node)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
        }
    }

    fn match_atom(
        &mut self,
        atom_quant: &AtomWithQuant<'a>,
        out: &mut BumpVec<'a, &'a SyntaxNode<'a>>,
    ) -> bool {
        // For CategoryRef atoms, use the specified precedence constraint
        let min_prec = atom_quant.precedence.unwrap_or(0);

        match atom_quant.quant {
            Quantifier::One => {
                if !self.skip_ws() {
                    return false;
                }
                if let Some(node) = self.match_atom_once(&atom_quant.atom, min_prec) {
                    out.push(node);
                    true
                } else {
                    false
                }
            }

            Quantifier::Optional => {
                if !self.skip_ws() {
                    return true;
                }
                if let Some(node) = self.match_atom_once(&atom_quant.atom, min_prec) {
                    out.push(node);
                }
                true
            }

            Quantifier::Star => {
                if !self.skip_ws() {
                    return true;
                }
                loop {
                    let cp = self.save();
                    if let Some(node) = self.match_atom_once(&atom_quant.atom, min_prec) {
                        out.push(node);
                    } else {
                        self.restore(cp);
                        break;
                    }
                }
                true
            }

            Quantifier::Plus => {
                if !self.skip_ws() {
                    return false;
                }
                if let Some(node) = self.match_atom_once(&atom_quant.atom, min_prec) {
                    out.push(node);
                } else {
                    return false;
                }
                loop {
                    let cp = self.save();
                    if let Some(node) = self.match_atom_once(&atom_quant.atom, min_prec) {
                        out.push(node);
                    } else {
                        self.restore(cp);
                        break;
                    }
                }
                true
            }
        }
    }

    fn match_pattern(
        &mut self,
        pattern: &[AtomWithQuant<'a>],
    ) -> Option<BumpVec<'a, &'a SyntaxNode<'a>>> {
        let saved_indent_depth = self.indent_stack.len();
        let saved_indent_mode = self.indent_mode;
        let saved_pending_anchor = self.pending_anchor;

        let mut children = BumpVec::new_in(self.arena);
        for atom_quant in pattern {
            if !self.match_atom(atom_quant, &mut children) {
                self.indent_stack.truncate(saved_indent_depth);
                self.indent_mode = saved_indent_mode;
                self.pending_anchor = saved_pending_anchor;
                return None;
            }
        }

        self.indent_stack.truncate(saved_indent_depth);
        self.indent_mode = saved_indent_mode;
        self.pending_anchor = saved_pending_anchor;

        Some(children)
    }

    fn parse_category_impl(&mut self, category: &str, min_prec: u32) -> Option<&'a SyntaxNode<'a>> {
        let start = self.current_loc();
        let rules = self.rules.for_category(category);

        log!(self.log, "parse {} at L{}:{} (min_prec={})", category, start.line, start.col, min_prec);
        self.log.push_indent();

        let mut left: Option<&'a SyntaxNode<'a>> = None;

        for rule in rules.iter().rev() {
            if rule.is_left_recursive {
                continue;
            }

            log_detail!(self.log, "try rule {}", rule.name);

            let cp = self.save();
            if let Some(children) = self.match_pattern(rule.pattern) {
                let children_slice = children.into_bump_slice();
                let info = self.make_info(start, self.pos as u32);
                let node = self.arena.alloc(SyntaxNode::branch(
                    rule.category,
                    rule.name,
                    info,
                    children_slice,
                ));

                log_success!(self.log, "matched {}", rule.name);
                left = Some(node);
                break;
            }
            self.restore(cp);
        }

        let mut left = match left {
            Some(l) => l,
            None => {
                log_fail!(self.log, "no prefix rule matched for {}", category);
                self.log.pop_indent();
                return None;
            }
        };

        'outer: loop {
            for rule in rules.iter().rev() {
                if !rule.is_left_recursive {
                    continue;
                }

                // Check precedence constraint: only apply this infix rule if its
                // operator precedence is >= min_prec
                let rule_prec = get_rule_operator_precedence(rule).unwrap_or(0);
                if rule_prec < min_prec {
                    continue;
                }

                log_detail!(self.log, "try left-recursive rule {} (prec={})", rule.name, rule_prec);

                let cp = self.save();
                let rest_pattern = &rule.pattern[1..];
                if let Some(mut children) = self.match_pattern(rest_pattern) {
                    children.insert(0, left);
                    let children_slice = children.into_bump_slice();
                    let info = self.make_info(start, self.pos as u32);
                    let node = self.arena.alloc(SyntaxNode::branch(
                        rule.category,
                        rule.name,
                        info,
                        children_slice,
                    ));

                    log_success!(self.log, "extended with {}", rule.name);
                    left = node;
                    continue 'outer;
                }
                self.restore(cp);
            }
            break;
        }

        self.log.pop_indent();
        Some(left)
    }

    fn parse_category_with_precedence(&mut self, category: &str, min_prec: u32) -> Option<&'a SyntaxNode<'a>> {
        self.parse_category_impl(category, min_prec)
    }

    fn parse_category_internal(&mut self, category: &str) -> Option<&'a SyntaxNode<'a>> {
        self.parse_category_impl(category, 0)
    }
}

// =============================================================================
// Parser Trait Implementation
// =============================================================================

impl<'a> Parser<'a> for InterpretedParser<'a> {
    fn is_eof(&self) -> bool {
        self.pos >= self.source.len()
    }

    fn next_command(&mut self) -> Option<&'a SyntaxNode<'a>> {
        // Skip all whitespace including newlines
        self.skip_ws_all();
        if self.pos >= self.source.len() {
            return None;
        }

        // Reset indent state for each command
        self.reset_indent();

        // Clear furthest failure tracking
        self.clear_furthest();

        // Parse a Command
        self.skip_ws();
        self.parse_category_internal("Command")
    }

    fn parse_category(&mut self, category: &str) -> Option<&'a SyntaxNode<'a>> {
        self.skip_ws();
        self.parse_category_internal(category)
    }

    fn error(&self) -> Option<ParseError> {
        if self.furthest_expected.is_empty() && self.furthest_pos == 0 {
            return None;
        }
        Some(self.make_error_internal())
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
        self.rules.add(rule);
    }

    fn strings_mut(&mut self) -> &mut StringInterner<'a> {
        &mut self.strings
    }

    fn set_trace(&mut self, enabled: bool) {
        self.trace_enabled = enabled;
    }

    fn dump(&mut self) {
        eprintln!("=== Parser State ===");
        eprintln!("Position: {} (line {}, col {})", self.pos, self.line, self.col);
        eprintln!("EOF: {}", self.pos >= self.source.len());
        eprintln!("Indent stack: {:?}", self.indent_stack);
        eprintln!("Indent mode: {:?}", self.indent_mode);
        eprintln!("Pending anchor: {}", self.pending_anchor);
        eprintln!("Furthest pos: {} (line {}, col {})",
            self.furthest_pos, self.furthest_loc.line, self.furthest_loc.col);
        eprintln!("Furthest expected: {:?}", self.furthest_expected);
        if self.pos < self.source.len() {
            let preview: String = self.source[self.pos..].chars().take(40).collect();
            eprintln!("Next: {:?}...", preview);
        }
    }

    fn dump_rules(&self) {
        eprintln!("=== Registered Rules ===");
        // Note: RuleTable doesn't expose iteration, so we dump by known categories
        let categories = ["Command", "Expr", "SyntaxDecl", "SyntaxPattern", "SyntaxAtom", "_Ident"];
        for cat in categories {
            let rules = self.rules.for_category(cat);
            if !rules.is_empty() {
                eprintln!("Category '{}':", cat);
                for rule in rules {
                    eprintln!("  - {} (left_recursive: {})", rule.name, rule.is_left_recursive);
                }
            }
        }
    }
}


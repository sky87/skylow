//! Bytecode compiler for syntax rules.

use bumpalo::Bump;
use skylow_common::intern::StringInterner;
use crate::syntax::{Atom, AtomWithQuant, CharClass, Quantifier, SyntaxRule};

use super::grammar::{CompiledGrammar, CompiledRule};
use super::instruction::{encode, encode_signed, op, INDENT_LAX, INDENT_STRICT, SKIP_WS_INDENT};

/// Compile syntax rules into bytecode
pub struct Compiler<'a> {
    grammar: CompiledGrammar<'a>,
    #[allow(dead_code)]
    arena: &'a Bump,
}

impl<'a> Compiler<'a> {
    pub fn new(arena: &'a Bump) -> Self {
        Self {
            grammar: CompiledGrammar::new(),
            arena,
        }
    }

    /// Compile a rule and add it to the grammar
    pub fn compile_rule(
        &mut self,
        rule: &'a SyntaxRule<'a>,
        strings: &mut StringInterner<'a>,
    ) {
        let category_id = self.grammar.get_or_create_category(rule.category);
        let rule_idx = self.grammar.rules.len();

        // Record bytecode offset for this rule
        let bytecode_offset = self.grammar.current_offset();

        // Compile the pattern
        let pattern = if rule.is_left_recursive {
            // Skip first atom for left-recursive rules
            &rule.pattern[1..]
        } else {
            rule.pattern
        };

        self.compile_pattern(pattern, strings);

        // Emit END instruction
        self.grammar.emit(encode(op::END, 0));

        // Add rule info
        self.grammar.rules.push(CompiledRule {
            category: rule.category,
            name: rule.name,
            bytecode_offset,
            is_left_recursive: rule.is_left_recursive,
        });

        // Update dispatch table
        let dispatch = &mut self.grammar.dispatch_tables[category_id as usize];
        if rule.is_left_recursive {
            // Insert at front (latest rules have priority)
            dispatch.infix_rules.insert(0, rule_idx);
        } else {
            dispatch.prefix_rules.insert(0, rule_idx);
        }
    }

    /// Compile a pattern to bytecode
    fn compile_pattern(
        &mut self,
        pattern: &[AtomWithQuant<'a>],
        strings: &mut StringInterner<'a>,
    ) {
        for atom_quant in pattern {
            self.compile_atom_with_quant(atom_quant, strings);
        }
    }

    /// Compile an atom with quantifier
    fn compile_atom_with_quant(
        &mut self,
        aq: &AtomWithQuant<'a>,
        strings: &mut StringInterner<'a>,
    ) {
        // Special case: IndentAwareWs with Star quantifier (WS*)
        // skip_ws() already consumes all available whitespace, so don't compile as a loop
        if matches!(aq.atom, Atom::IndentAwareWs) && matches!(aq.quant, Quantifier::Star) {
            self.grammar.emit(encode(op::SKIP_WS, SKIP_WS_INDENT));
            return;
        }

        match aq.quant {
            Quantifier::One => {
                self.compile_atom(&aq.atom, strings);
            }

            Quantifier::Optional => {
                // CHOICE skip
                // <atom>
                // skip:
                let choice_offset = self.grammar.emit(encode(op::CHOICE, 0));
                self.compile_atom(&aq.atom, strings);
                let skip_offset = self.grammar.current_offset();
                self.grammar.patch_jump(choice_offset, skip_offset);
            }

            Quantifier::Star => {
                // loop:
                //   CHOICE done
                //   <atom>
                //   JUMP loop
                // done:
                let loop_offset = self.grammar.current_offset();
                let choice_offset = self.grammar.emit(encode(op::CHOICE, 0));
                self.compile_atom(&aq.atom, strings);
                let rel_offset = (loop_offset as i32) - (self.grammar.current_offset() as i32);
                self.grammar.emit(encode(op::JUMP, encode_signed(rel_offset)));
                let done_offset = self.grammar.current_offset();
                self.grammar.patch_jump(choice_offset, done_offset);
            }

            Quantifier::Plus => {
                // <atom>
                // loop:
                //   CHOICE done
                //   <atom>
                //   JUMP loop
                // done:
                self.compile_atom(&aq.atom, strings);
                let loop_offset = self.grammar.current_offset();
                let choice_offset = self.grammar.emit(encode(op::CHOICE, 0));
                self.compile_atom(&aq.atom, strings);
                let rel_offset = (loop_offset as i32) - (self.grammar.current_offset() as i32);
                self.grammar.emit(encode(op::JUMP, encode_signed(rel_offset)));
                let done_offset = self.grammar.current_offset();
                self.grammar.patch_jump(choice_offset, done_offset);
            }
        }
    }

    /// Compile a single atom
    fn compile_atom(&mut self, atom: &Atom<'a>, strings: &mut StringInterner<'a>) {
        match atom {
            Atom::Literal(lit) => {
                let str_id = self.grammar.intern_string(lit);
                self.grammar.emit(encode(op::LITERAL, str_id));
            }

            Atom::CharClass(class) => {
                let class_id = match class {
                    CharClass::Digit => 0,
                    CharClass::Word => 1,
                    CharClass::Whitespace => 2,
                };
                self.grammar.emit(encode(op::CHAR_CLASS, class_id));
            }

            Atom::CharSet { ranges, negated } => {
                let charset_id = self.grammar.add_charset(ranges, *negated);
                self.grammar.emit(encode(op::CHAR_SET, charset_id));
            }

            Atom::Group { alternatives } => {
                if alternatives.len() == 1 {
                    // Single alternative - just compile it
                    self.compile_pattern(alternatives[0], strings);
                } else {
                    // Multiple alternatives:
                    // CHOICE alt2
                    // <alt1>
                    // JUMP done
                    // alt2:
                    // CHOICE alt3
                    // <alt2>
                    // JUMP done
                    // ...
                    // altN:
                    // <altN>
                    // done:

                    let mut jump_offsets = Vec::new();

                    for (i, alt) in alternatives.iter().enumerate() {
                        // Patch previous CHOICE to point here (start of this alternative)
                        // This is done BEFORE compiling the alternative

                        if i < alternatives.len() - 1 {
                            // Not the last alternative - emit CHOICE pointing to next alt
                            // We'll patch this after compiling the next alternative
                            let choice_offset = self.grammar.emit(encode(op::CHOICE, 0));

                            self.compile_pattern(alt, strings);

                            // Emit JUMP to done (will patch later)
                            jump_offsets.push(self.grammar.emit(encode(op::JUMP, 0)));

                            // Now patch the CHOICE to point to the next instruction
                            let next_alt_offset = self.grammar.current_offset();
                            self.grammar.patch_jump(choice_offset, next_alt_offset);
                        } else {
                            // Last alternative - no CHOICE needed, no JUMP needed
                            self.compile_pattern(alt, strings);
                        }
                    }

                    // Patch all JUMPs to point to done
                    let done_offset = self.grammar.current_offset();
                    for jump_off in jump_offsets {
                        self.grammar.patch_jump(jump_off, done_offset);
                    }
                }
            }

            Atom::CategoryRef(cat) => {
                let category_id = self.grammar.get_or_create_category(cat);
                self.grammar.emit(encode(op::CALL, category_id));
            }

            Atom::IndentAnchor => {
                self.grammar.emit(encode(op::INDENT_ANCHOR, 0));
            }

            Atom::IndentStrict => {
                self.grammar.emit(encode(op::INDENT_MODE, INDENT_STRICT));
            }

            Atom::IndentLax => {
                self.grammar.emit(encode(op::INDENT_MODE, INDENT_LAX));
            }

            Atom::IndentAwareWs => {
                self.grammar.emit(encode(op::SKIP_WS, SKIP_WS_INDENT));
            }
        }
    }

    /// Finish compilation and return the grammar
    pub fn finish(self) -> CompiledGrammar<'a> {
        self.grammar
    }
}


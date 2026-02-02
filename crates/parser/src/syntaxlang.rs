//! Bootstrap grammar and syntax rule extraction for SkyLow.
//!
//! This module provides:
//! - Bootstrap rules for parsing syntax declarations (`init_syntaxlang`)
//! - Functions to extract syntax rules from parsed AST nodes
//! - Helper functions for working with syntax nodes

use bumpalo::Bump;
use common::StringInterner;

use crate::constants::{
    CAT_COMMAND, CAT_EXPR, CAT_IDENT, CAT_SYNTAX_ATOM, CAT_SYNTAX_DECL, CAT_SYNTAX_PATTERN,
    RULE_EXPR, RULE_SYNTAX, RULE_SYNTAX_CATEGORY, RULE_SYNTAX_DECL,
};
use crate::node::SyntaxNode;
use crate::parser_trait::Parser;
use crate::syntax::{Atom, AtomWithQuant, CharClass, Quantifier, SyntaxRule};

// =============================================================================
// Local Constants: Bootstrap Rule Names
// =============================================================================
// These rule names are only used within this module for defining and matching
// bootstrap grammar rules.

const RULE_IDENT: &str = "ident";
const RULE_LITERAL: &str = "literal";
const RULE_CATEGORY_REF: &str = "categoryRef";
const RULE_CHAR_CLASS: &str = "charClass";
const RULE_CHARSET: &str = "charset";
const RULE_INDENT_ANCHOR: &str = "indentAnchor";
const RULE_INDENT_LAX: &str = "indentLax";
const RULE_INDENT_STRICT: &str = "indentStrict";
const RULE_GROUP: &str = "group";
const RULE_QUANTIFIED: &str = "quantified";
const RULE_PRECEDENCE: &str = "precedence";
const RULE_ATOM: &str = "atom";
const RULE_SEQ: &str = "seq";
const RULE_ALT: &str = "alt";
const RULE_SYNTAX_NO_CATEGORY: &str = "syntaxNoCategory";

// =============================================================================
// Bootstrap Grammar Initialization
// =============================================================================

/// Initialize built-in rules for parsing syntax declarations.
///
/// This registers the bootstrap grammar that allows parsing `syntax` declarations,
/// which in turn define user syntax rules. The bootstrap grammar includes:
///
/// - `_Ident`: Identifiers (internal category to avoid conflicts)
/// - `SyntaxAtom`: Pattern atoms (literals, charsets, groups, etc.)
/// - `SyntaxPattern`: Patterns (sequences, alternations)
/// - `SyntaxDecl`: Syntax declarations
/// - `Command`: Top-level commands (comments, syntax declarations, expressions)
///
/// This function is generic over any parser implementing the `Parser` trait,
/// allowing it to work with both `InterpretedParser` and `VMParser`.
pub fn init_syntaxlang<'a, 'src, P: Parser<'a, 'src>>(
    arena: &'a Bump,
    strings: &mut StringInterner<'a>,
    parser: &mut P,
) {
    // Pre-intern all literal strings used in bootstrap patterns
    let s_quote = strings.intern("\"");
    let s_backslash = strings.intern("\\");
    let s_indent_anchor = strings.intern("|>");
    let s_indent_lax = strings.intern(">");
    let s_indent_strict = strings.intern(">>");
    let s_lparen = strings.intern("(");
    let s_rparen = strings.intern(")");
    let s_at = strings.intern("@");
    let s_pipe = strings.intern("|");
    let s_syntax = strings.intern("syntax");
    let s_colon = strings.intern(":");
    let s_syntax_category = strings.intern("syntax_category");
    let s_lbracket = strings.intern("[");
    let s_caret = strings.intern("^");
    let s_rbracket = strings.intern("]");

    // Pre-intern category reference strings used in bootstrap patterns
    let cat_syntax_pattern = strings.intern(CAT_SYNTAX_PATTERN);
    let cat_syntax_atom = strings.intern(CAT_SYNTAX_ATOM);
    let cat_ident = strings.intern(CAT_IDENT);

    // Helper to create atoms (now just wraps already-interned strings)
    let lit = |s: &'a str| -> Atom<'a> { Atom::Literal(s) };

    let one = |atom: Atom<'a>| -> AtomWithQuant<'a> {
        AtomWithQuant {
            atom,
            quant: Quantifier::One,
            precedence: None,
        }
    };

    let opt = |atom: Atom<'a>| -> AtomWithQuant<'a> {
        AtomWithQuant {
            atom,
            quant: Quantifier::Optional,
            precedence: None,
        }
    };

    let star = |atom: Atom<'a>| -> AtomWithQuant<'a> {
        AtomWithQuant {
            atom,
            quant: Quantifier::Star,
            precedence: None,
        }
    };

    let plus = |atom: Atom<'a>| -> AtomWithQuant<'a> {
        AtomWithQuant {
            atom,
            quant: Quantifier::Plus,
            precedence: None,
        }
    };

    // Helper to create category reference atoms (wraps already-interned strings)
    let cat_ref = |name: &'a str| -> Atom<'a> { Atom::CategoryRef(name) };

    let char_range = |lo: char, hi: char| -> (char, char) { (lo, hi) };

    let charset = |ranges: &[(char, char)]| -> Atom<'a> {
        Atom::CharSet {
            ranges: arena.alloc_slice_copy(ranges),
            negated: false,
        }
    };

    let negated_charset = |ranges: &[(char, char)]| -> Atom<'a> {
        Atom::CharSet {
            ranges: arena.alloc_slice_copy(ranges),
            negated: true,
        }
    };

    // Indent helpers
    let indent_anchor = || -> AtomWithQuant<'a> {
        AtomWithQuant {
            atom: Atom::IndentAnchor,
            quant: Quantifier::One,
            precedence: None,
        }
    };

    let indent_strict = || -> AtomWithQuant<'a> {
        AtomWithQuant {
            atom: Atom::IndentStrict,
            quant: Quantifier::One,
            precedence: None,
        }
    };

    // Intern category names
    // Use "_Ident" for syntax-related identifiers to avoid conflicts with user-defined "Ident"
    let ident_cat = strings.intern(CAT_IDENT);
    let syntax_atom_cat = strings.intern(CAT_SYNTAX_ATOM);
    let syntax_pattern_cat = strings.intern(CAT_SYNTAX_PATTERN);
    let syntax_decl_cat = strings.intern(CAT_SYNTAX_DECL);

    // _Ident: [a..zA..Z_] [a..zA..Z0..9_]*
    {
        let pattern = arena.alloc_slice_copy(&[
            one(charset(&[
                char_range('a', 'z'),
                char_range('A', 'Z'),
                char_range('_', '_'),
            ])),
            star(charset(&[
                char_range('a', 'z'),
                char_range('A', 'Z'),
                char_range('0', '9'),
                char_range('_', '_'),
            ])),
        ]);
        let rule = arena.alloc(SyntaxRule {
            name: strings.intern(RULE_IDENT),
            category: ident_cat,
            pattern,
            is_left_recursive: false,
        });
        parser.add_rule(rule);
    }

    // SyntaxAtom: literal "\"" [^"]* "\""
    {
        let pattern = arena.alloc_slice_copy(&[
            one(lit(s_quote)),
            star(negated_charset(&[char_range('"', '"')])),
            one(lit(s_quote)),
        ]);
        let rule = arena.alloc(SyntaxRule {
            name: strings.intern(RULE_LITERAL),
            category: syntax_atom_cat,
            pattern,
            is_left_recursive: false,
        });
        parser.add_rule(rule);
    }

    // SyntaxAtom: categoryRef [A..Z] [a..zA..Z0..9]*
    {
        let pattern = arena.alloc_slice_copy(&[
            one(charset(&[char_range('A', 'Z')])),
            star(charset(&[
                char_range('a', 'z'),
                char_range('A', 'Z'),
                char_range('0', '9'),
            ])),
        ]);
        let rule = arena.alloc(SyntaxRule {
            name: strings.intern(RULE_CATEGORY_REF),
            category: syntax_atom_cat,
            pattern,
            is_left_recursive: false,
        });
        parser.add_rule(rule);
    }

    // SyntaxAtom: charClass "\\" [dws]
    {
        let pattern = arena.alloc_slice_copy(&[
            one(lit(s_backslash)),
            one(charset(&[
                char_range('d', 'd'),
                char_range('w', 'w'),
                char_range('s', 's'),
            ])),
        ]);
        let rule = arena.alloc(SyntaxRule {
            name: strings.intern(RULE_CHAR_CLASS),
            category: syntax_atom_cat,
            pattern,
            is_left_recursive: false,
        });
        parser.add_rule(rule);
    }

    // SyntaxAtom: charset "[" ("^")? [^\]]+ "]"
    {
        let pattern = arena.alloc_slice_copy(&[
            one(lit(s_lbracket)),
            opt(lit(s_caret)),
            plus(negated_charset(&[char_range(']', ']')])),
            one(lit(s_rbracket)),
        ]);
        let rule = arena.alloc(SyntaxRule {
            name: strings.intern(RULE_CHARSET),
            category: syntax_atom_cat,
            pattern,
            is_left_recursive: false,
        });
        parser.add_rule(rule);
    }

    // SyntaxAtom: indentAnchor "|>"
    {
        let pattern = arena.alloc_slice_copy(&[one(lit(s_indent_anchor))]);
        let rule = arena.alloc(SyntaxRule {
            name: strings.intern(RULE_INDENT_ANCHOR),
            category: syntax_atom_cat,
            pattern,
            is_left_recursive: false,
        });
        parser.add_rule(rule);
    }

    // SyntaxAtom: indentLax ">"
    // NOTE: Must come before indentStrict so ">>" is tried first (latest wins)
    {
        let pattern = arena.alloc_slice_copy(&[one(lit(s_indent_lax))]);
        let rule = arena.alloc(SyntaxRule {
            name: strings.intern(RULE_INDENT_LAX),
            category: syntax_atom_cat,
            pattern,
            is_left_recursive: false,
        });
        parser.add_rule(rule);
    }

    // SyntaxAtom: indentStrict ">>"
    {
        let pattern = arena.alloc_slice_copy(&[one(lit(s_indent_strict))]);
        let rule = arena.alloc(SyntaxRule {
            name: strings.intern(RULE_INDENT_STRICT),
            category: syntax_atom_cat,
            pattern,
            is_left_recursive: false,
        });
        parser.add_rule(rule);
    }

    // SyntaxAtom: group "(" SyntaxPattern ")"
    {
        let pattern = arena.alloc_slice_copy(&[
            one(lit(s_lparen)),
            one(cat_ref(cat_syntax_pattern)),
            one(lit(s_rparen)),
        ]);
        let rule = arena.alloc(SyntaxRule {
            name: strings.intern(RULE_GROUP),
            category: syntax_atom_cat,
            pattern,
            is_left_recursive: false,
        });
        parser.add_rule(rule);
    }

    // SyntaxAtom: quantified SyntaxAtom [?*+]
    {
        let pattern = arena.alloc_slice_copy(&[
            one(cat_ref(cat_syntax_atom)),
            one(charset(&[
                char_range('?', '?'),
                char_range('*', '*'),
                char_range('+', '+'),
            ])),
        ]);
        let rule = arena.alloc(SyntaxRule {
            name: strings.intern(RULE_QUANTIFIED),
            category: syntax_atom_cat,
            pattern,
            is_left_recursive: true,
        });
        parser.add_rule(rule);
    }

    // SyntaxAtom: precedence SyntaxAtom "@" \d+
    {
        let pattern = arena.alloc_slice_copy(&[
            one(cat_ref(cat_syntax_atom)),
            one(lit(s_at)),
            plus(Atom::CharClass(CharClass::Digit)),
        ]);
        let rule = arena.alloc(SyntaxRule {
            name: strings.intern(RULE_PRECEDENCE),
            category: syntax_atom_cat,
            pattern,
            is_left_recursive: true,
        });
        parser.add_rule(rule);
    }

    // SyntaxPattern: atom SyntaxAtom
    {
        let pattern = arena.alloc_slice_copy(&[one(cat_ref(cat_syntax_atom))]);
        let rule = arena.alloc(SyntaxRule {
            name: strings.intern(RULE_ATOM),
            category: syntax_pattern_cat,
            pattern,
            is_left_recursive: false,
        });
        parser.add_rule(rule);
    }

    // SyntaxPattern: seq SyntaxPattern SyntaxAtom
    {
        let pattern =
            arena.alloc_slice_copy(&[one(cat_ref(cat_syntax_pattern)), one(cat_ref(cat_syntax_atom))]);
        let rule = arena.alloc(SyntaxRule {
            name: strings.intern(RULE_SEQ),
            category: syntax_pattern_cat,
            pattern,
            is_left_recursive: true,
        });
        parser.add_rule(rule);
    }

    // SyntaxPattern: alt SyntaxPattern "|" SyntaxAtom
    {
        let pattern = arena.alloc_slice_copy(&[
            one(cat_ref(cat_syntax_pattern)),
            one(lit(s_pipe)),
            one(cat_ref(cat_syntax_atom)),
        ]);
        let rule = arena.alloc(SyntaxRule {
            name: strings.intern(RULE_ALT),
            category: syntax_pattern_cat,
            pattern,
            is_left_recursive: true,
        });
        parser.add_rule(rule);
    }

    // SyntaxDecl: syntaxNoCategory |> "syntax" Ident >> SyntaxPattern
    // NOTE: Must come before 'syntax' rule so the longer rule is tried first (latest wins)
    {
        let pattern = arena.alloc_slice_copy(&[
            indent_anchor(),
            one(lit(s_syntax)),
            one(cat_ref(cat_ident)),
            indent_strict(),
            one(cat_ref(cat_syntax_pattern)),
        ]);
        let rule = arena.alloc(SyntaxRule {
            name: strings.intern(RULE_SYNTAX_NO_CATEGORY),
            category: syntax_decl_cat,
            pattern,
            is_left_recursive: false,
        });
        parser.add_rule(rule);
    }

    // SyntaxDecl: syntax |> "syntax" Ident >> SyntaxPattern ":" Ident
    {
        let pattern = arena.alloc_slice_copy(&[
            indent_anchor(),
            one(lit(s_syntax)),
            one(cat_ref(cat_ident)),
            indent_strict(),
            one(cat_ref(cat_syntax_pattern)),
            one(lit(s_colon)),
            one(cat_ref(cat_ident)),
        ]);
        let rule = arena.alloc(SyntaxRule {
            name: strings.intern(RULE_SYNTAX),
            category: syntax_decl_cat,
            pattern,
            is_left_recursive: false,
        });
        parser.add_rule(rule);
    }

    // SyntaxDecl: syntaxCategory "syntax_category" Ident
    {
        let pattern = arena.alloc_slice_copy(&[one(lit(s_syntax_category)), one(cat_ref(cat_ident))]);
        let rule = arena.alloc(SyntaxRule {
            name: strings.intern(RULE_SYNTAX_CATEGORY),
            category: syntax_decl_cat,
            pattern,
            is_left_recursive: false,
        });
        parser.add_rule(rule);
    }

    // Note: Comments (#...) are skipped as whitespace by skip_ws_all(), not parsed as Commands
    // Note: Command rules (syntaxDecl, expr) are added by the driver after this function
}

// =============================================================================
// Rule Extraction from Syntax Declarations
// =============================================================================

/// Extract a syntax rule from a parsed SyntaxDecl node and register it.
///
/// This function takes a parsed syntax declaration (from `syntax name pattern` or
/// `syntax name pattern : Category`) and:
/// 1. Extracts the rule name, category, and pattern
/// 2. Detects left-recursive rules
/// 3. Adds implicit whitespace for grammar (non-lexical) rules
/// 4. Applies operator precedence annotations
/// 5. Registers the rule with the parser
///
/// Returns the created rule, or None if extraction failed.
///
/// This function is generic over any parser implementing the `Parser` trait.
pub fn extract_and_register_rule<'a, 's, P: Parser<'a, 's>>(
    arena: &'a Bump,
    strings: &mut StringInterner<'a>,
    parser: &mut P,
    syntax_decl: &'a SyntaxNode<'a>,
    source: &'s str,
) -> Option<&'a SyntaxRule<'a>> {
    // Get rule name from first identifier
    let idents = get_children_by_category(syntax_decl, CAT_IDENT);
    let rule_name = if !idents.is_empty() {
        get_node_text(idents[0], source)
    } else {
        return None;
    };

    // Get pattern node
    let pattern_node = get_child_by_category(syntax_decl, CAT_SYNTAX_PATTERN)?;

    // Get category (if specified, otherwise default to "Expr")
    let category = if syntax_decl.rule == RULE_SYNTAX && idents.len() > 1 {
        get_node_text(idents[idents.len() - 1], source)
    } else {
        CAT_EXPR
    };

    // Extract pattern atoms
    let mut intern_fn = |s: &str| -> &'a str { strings.intern(s) };
    let mut pattern_atoms = extract_pattern_atoms(arena, pattern_node, source, &mut intern_fn);

    // Check if left-recursive (starts with self-category reference)
    let is_left_recursive = if let Some(first) = pattern_atoms.first() {
        if let Atom::CategoryRef(cat) = first.atom {
            cat == category
        } else {
            false
        }
    } else {
        false
    };

    // Add implicit whitespace for grammar (non-lexical) rules
    if !is_lexical_pattern(&pattern_atoms) {
        pattern_atoms = add_implicit_whitespace(arena, &pattern_atoms);
    }

    let pattern_slice = arena.alloc_slice_copy(&pattern_atoms);
    let rule = arena.alloc(SyntaxRule {
        name: strings.intern(rule_name),
        category: strings.intern(category),
        pattern: pattern_slice,
        is_left_recursive,
    });

    parser.add_rule(rule);

    Some(rule)
}

/// Create and register an expr rule for parsing expressions of a given category.
///
/// This creates a `Command` rule that wraps a category reference, allowing
/// expressions of that category to be parsed at the top level.
///
/// This function is generic over any parser implementing the `Parser` trait.
pub fn add_expr_rule<'a, 'src, P: Parser<'a, 'src>>(
    arena: &'a Bump,
    strings: &mut StringInterner<'a>,
    parser: &mut P,
    category: &str,
) {
    let cat_ref = Atom::CategoryRef(strings.intern(category));
    let pattern = arena.alloc_slice_copy(&[AtomWithQuant {
        atom: cat_ref,
        quant: Quantifier::One,
        precedence: None,
    }]);
    let rule = arena.alloc(SyntaxRule {
        name: strings.intern(RULE_EXPR),
        category: strings.intern(CAT_COMMAND),
        pattern,
        is_left_recursive: false,
    });
    parser.add_rule(rule);
}

/// Add the syntaxDecl command rule.
/// Must be called AFTER add_expr_rule so it has higher priority (latest wins).
///
/// This function is generic over any parser implementing the `Parser` trait.
pub fn add_syntax_decl_command_rule<'a, 'src, P: Parser<'a, 'src>>(
    arena: &'a Bump,
    strings: &mut StringInterner<'a>,
    parser: &mut P,
) {
    let pattern = arena.alloc_slice_copy(&[AtomWithQuant {
        atom: Atom::CategoryRef(strings.intern(CAT_SYNTAX_DECL)),
        quant: Quantifier::One,
        precedence: None,
    }]);
    let rule = arena.alloc(SyntaxRule {
        name: strings.intern(RULE_SYNTAX_DECL),
        category: strings.intern(CAT_COMMAND),
        pattern,
        is_left_recursive: false,
    });
    parser.add_rule(rule);
}

// =============================================================================
// Implicit Whitespace Insertion
// =============================================================================

/// Inserts implicit whitespace atoms between pattern elements for grammar rules.
///
/// # Background
///
/// Grammar rules that contain category references (non-lexical rules) need
/// whitespace handling between their elements. For example, in a rule like:
///
/// ```text
/// syntax Expr.add: Expr "+" Expr
/// ```
///
/// We need to allow whitespace around the "+" operator so that both `1+2` and
/// `1 + 2` parse correctly. This function automatically inserts `IndentAwareWs`
/// atoms to handle this.
///
/// # What This Function Does
///
/// 1. **Insert whitespace between elements**: Places `IndentAwareWs*` between
///    consecutive pattern elements.
///
/// 2. **Recursively process groups**: Enters group constructs `(a | b)` and
///    applies whitespace insertion to each alternative.
///
/// 3. **Handle repeating groups**: For `+` or `*` quantified groups, prepends
///    whitespace inside each alternative so repetitions are separated.
///
/// # Examples
///
/// ## Simple multi-element pattern
/// ```text
/// Input:  [Expr, "+", Expr]
/// Output: [Expr, WS*, "+", WS*, Expr]
/// ```
///
/// ## Group with alternatives
/// ```text
/// Input:  [("+"|"-"), Expr]
/// Output: [WS*, ("+"|"-"), WS*, Expr]
/// ```
/// Each alternative inside the group is also processed recursively.
///
/// ## Repeating group (the tricky case)
/// ```text
/// Input:  [Expr, ("," Expr)+]
/// Output: [Expr, WS*, (WS*, ",", WS*, Expr)+]
/// ```
/// Note the `WS*` prepended inside the group - this allows `1, 2, 3` to parse
/// as three repetitions with whitespace between them. Without it, the second
/// iteration would fail to match because there's no whitespace handling before
/// the comma.
///
/// ## Single-element patterns
/// ```text
/// Input:  [Ident]
/// Output: [Ident]  (unchanged - no whitespace needed between zero elements)
///
/// Input:  [(Expr)+]
/// Output: [(WS*, Expr)+]  (single group still processed for repetition)
/// ```
///
/// # Why `IndentAwareWs` with `Star` Quantifier?
///
/// The whitespace atom uses `Quantifier::Star` because whitespace is optional -
/// we want to *allow* whitespace but not *require* it. The `IndentAwareWs` atom
/// respects indentation constraints during parsing.
pub fn add_implicit_whitespace<'a>(
    arena: &'a Bump,
    pattern: &[AtomWithQuant<'a>],
) -> Vec<AtomWithQuant<'a>> {
    // -------------------------------------------------------------------------
    // SECTION 1: Handle single-element patterns (base case optimization)
    // -------------------------------------------------------------------------
    // Patterns with 0 or 1 elements don't need whitespace *between* elements,
    // but we still need to process groups for their internal content and
    // repetition handling.

    if pattern.len() <= 1 {
        if pattern.len() == 1 {
            if let Atom::Group { alternatives } = pattern[0].atom {
                // Recursively process each alternative inside the group
                let mut new_alts: Vec<&[AtomWithQuant]> = Vec::new();
                for alt in alternatives {
                    let new_alt = add_implicit_whitespace(arena, alt);
                    new_alts.push(arena.alloc_slice_copy(&new_alt) as &[AtomWithQuant]);
                }
                let new_alts_slice = arena.alloc_slice_copy(&new_alts);

                // For `+` or `*` quantified groups, prepend WS* inside each alternative.
                // This handles the repetition separation case:
                //   (Expr)+ matching "1 2 3" needs WS between iterations
                //   Without this: first matches "1", second fails (no WS before "2")
                //   With WS* prepended: each iteration starts by consuming optional WS
                if pattern[0].quant == Quantifier::Plus || pattern[0].quant == Quantifier::Star {
                    let ws_atom = AtomWithQuant {
                        atom: Atom::IndentAwareWs,
                        quant: Quantifier::Star,
                        precedence: None,
                    };

                    let mut new_alts_with_ws: Vec<&[AtomWithQuant]> = Vec::new();
                    for alt in new_alts_slice {
                        let mut new_alt = vec![ws_atom];
                        new_alt.extend_from_slice(alt);
                        new_alts_with_ws
                            .push(arena.alloc_slice_copy(&new_alt) as &[AtomWithQuant]);
                    }
                    let new_alts_slice = arena.alloc_slice_copy(&new_alts_with_ws);

                    return vec![AtomWithQuant {
                        atom: Atom::Group {
                            alternatives: new_alts_slice,
                        },
                        quant: pattern[0].quant,
                        precedence: pattern[0].precedence,
                    }];
                }

                // Non-repeating group: just return with recursively processed alternatives
                return vec![AtomWithQuant {
                    atom: Atom::Group {
                        alternatives: new_alts_slice,
                    },
                    quant: pattern[0].quant,
                    precedence: pattern[0].precedence,
                }];
            }
        }
        // Single non-group element or empty pattern: return unchanged
        return pattern.to_vec();
    }

    // -------------------------------------------------------------------------
    // SECTION 2: Process multi-element patterns
    // -------------------------------------------------------------------------
    // For patterns with 2+ elements, insert WS* between each pair and
    // recursively process any groups.

    let mut result = Vec::new();
    let ws_atom = AtomWithQuant {
        atom: Atom::IndentAwareWs,
        quant: Quantifier::Star,
        precedence: None,
    };

    for (i, aq) in pattern.iter().enumerate() {
        // Insert whitespace before each element (except the first)
        if i > 0 {
            result.push(ws_atom);
        }

        // If this element is a group, process it recursively
        if let Atom::Group { alternatives } = aq.atom {
            // Process each alternative inside the group
            let mut new_alts: Vec<&[AtomWithQuant]> = Vec::new();
            for alt in alternatives {
                let new_alt = add_implicit_whitespace(arena, alt);
                new_alts.push(arena.alloc_slice_copy(&new_alt) as &[AtomWithQuant]);
            }
            let new_alts_slice = arena.alloc_slice_copy(&new_alts);

            // For `+` or `*` quantified groups, prepend WS* inside alternatives
            // (same logic as Section 1, but for groups embedded in larger patterns)
            if aq.quant == Quantifier::Plus || aq.quant == Quantifier::Star {
                let mut new_alts_with_ws: Vec<&[AtomWithQuant]> = Vec::new();
                for alt in new_alts_slice {
                    let mut new_alt = vec![ws_atom];
                    new_alt.extend_from_slice(alt);
                    new_alts_with_ws.push(arena.alloc_slice_copy(&new_alt) as &[AtomWithQuant]);
                }
                let new_alts_slice = arena.alloc_slice_copy(&new_alts_with_ws);

                result.push(AtomWithQuant {
                    atom: Atom::Group {
                        alternatives: new_alts_slice,
                    },
                    quant: aq.quant,
                    precedence: aq.precedence,
                });
            } else {
                // Non-repeating group: just use the recursively processed alternatives
                result.push(AtomWithQuant {
                    atom: Atom::Group {
                        alternatives: new_alts_slice,
                    },
                    quant: aq.quant,
                    precedence: aq.precedence,
                });
            }
        } else {
            // Non-group atom: add as-is
            result.push(*aq);
        }
    }

    result
}

// =============================================================================
// AST Helper Functions
// =============================================================================

/// Get the text content of a syntax node.
///
/// For leaf nodes, returns the stored text. For branch nodes, extracts
/// the text from the source using the node's span.
pub fn get_node_text<'a>(node: &'a SyntaxNode<'a>, source: &'a str) -> &'a str {
    if let Some(text) = node.text {
        return text;
    }
    // For branch nodes, extract text from source using span
    let start = node.start.offset as usize;
    let end = node.end_offset as usize;
    &source[start..end]
}

/// Find all child nodes with a given category.
pub fn get_children_by_category<'a>(
    node: &'a SyntaxNode<'a>,
    category: &str,
) -> Vec<&'a SyntaxNode<'a>> {
    let mut result = Vec::new();
    for child in node.children {
        if child.category == category {
            result.push(*child);
        }
    }
    result
}

/// Find the first child node with a given category.
pub fn get_child_by_category<'a>(
    node: &'a SyntaxNode<'a>,
    category: &str,
) -> Option<&'a SyntaxNode<'a>> {
    for child in node.children {
        if child.category == category {
            return Some(*child);
        }
    }
    None
}

// =============================================================================
// String Escaping
// =============================================================================

/// Unescape string literal content, converting escape sequences.
pub fn unescape_string(s: &str) -> String {
    let mut result = String::new();
    let mut chars = s.chars().peekable();
    while let Some(c) = chars.next() {
        if c == '\\' {
            if let Some(&next) = chars.peek() {
                match next {
                    'n' => {
                        result.push('\n');
                        chars.next();
                    }
                    't' => {
                        result.push('\t');
                        chars.next();
                    }
                    'r' => {
                        result.push('\r');
                        chars.next();
                    }
                    '\\' => {
                        result.push('\\');
                        chars.next();
                    }
                    '"' => {
                        result.push('"');
                        chars.next();
                    }
                    ']' => {
                        result.push(']');
                        chars.next();
                    }
                    '^' => {
                        result.push('^');
                        chars.next();
                    }
                    '-' => {
                        result.push('-');
                        chars.next();
                    }
                    '.' => {
                        result.push('.');
                        chars.next();
                    }
                    _ => result.push(c),
                }
            } else {
                result.push(c);
            }
        } else {
            result.push(c);
        }
    }
    result
}

/// Parse character set content (like "a..z", "A..Z0..9_") into ranges.
pub fn parse_charset_content(content: &str) -> (Vec<(char, char)>, bool) {
    let mut ranges = Vec::new();
    let negated = content.starts_with('^');
    let content = if negated { &content[1..] } else { content };

    let unescaped = unescape_string(content);
    let chars: Vec<char> = unescaped.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        let c = chars[i];
        // Check for range pattern: c..d
        if i + 3 < chars.len() && chars[i + 1] == '.' && chars[i + 2] == '.' {
            let end_char = chars[i + 3];
            ranges.push((c, end_char));
            i += 4;
        } else {
            // Single character - treated as range of 1
            ranges.push((c, c));
            i += 1;
        }
    }

    (ranges, negated)
}

// =============================================================================
// Pattern Atom Extraction
// =============================================================================

/// Extract pattern atoms from a SyntaxPattern AST node.
///
/// Recursively traverses the parsed syntax pattern and converts it to
/// a list of `AtomWithQuant` values that can be used for matching.
pub fn extract_pattern_atoms<'a, 'src>(
    arena: &'a Bump,
    node: &'a SyntaxNode<'a>,
    source: &str,
    intern: &mut impl FnMut(&str) -> &'a str,
) -> Vec<AtomWithQuant<'a>> {
    let mut atoms = Vec::new();

    fn process_atom<'a>(
        arena: &'a Bump,
        node: &'a SyntaxNode<'a>,
        source: &str,
        intern: &mut impl FnMut(&str) -> &'a str,
    ) -> Option<(Atom<'a>, Option<Quantifier>, Option<u32>)> {
        match node.rule {
            RULE_LITERAL => {
                let text = get_node_text(node, source);
                // Strip quotes
                let inner = &text[1..text.len() - 1];
                let unescaped = unescape_string(inner);
                let lit_str = intern(&unescaped);
                Some((Atom::Literal(lit_str), None, None))
            }
            RULE_CATEGORY_REF => {
                let text = get_node_text(node, source);
                let cat = intern(text);
                Some((Atom::CategoryRef(cat), None, None))
            }
            RULE_CHAR_CLASS => {
                let text = get_node_text(node, source);
                // Parse \d, \w, \s
                let cls = match text.chars().nth(1) {
                    Some('d') => CharClass::Digit,
                    Some('w') => CharClass::Word,
                    Some('s') => CharClass::Whitespace,
                    _ => return None,
                };
                Some((Atom::CharClass(cls), None, None))
            }
            RULE_CHARSET => {
                let text = get_node_text(node, source);
                // Strip brackets
                let inner = &text[1..text.len() - 1];
                let (ranges, negated) = parse_charset_content(inner);
                let ranges_slice = arena.alloc_slice_copy(&ranges);
                Some((
                    Atom::CharSet {
                        ranges: ranges_slice,
                        negated,
                    },
                    None,
                    None,
                ))
            }
            RULE_INDENT_ANCHOR => Some((Atom::IndentAnchor, None, None)),
            RULE_INDENT_STRICT => Some((Atom::IndentStrict, None, None)),
            RULE_INDENT_LAX => Some((Atom::IndentLax, None, None)),
            RULE_GROUP => {
                // Find SyntaxPattern child
                if let Some(pattern_node) = get_child_by_category(node, CAT_SYNTAX_PATTERN) {
                    let inner_atoms = extract_pattern_atoms(arena, pattern_node, source, intern);

                    // Check if it's an alternation
                    if pattern_node.rule == RULE_ALT {
                        // Parse alternation
                        let alts = extract_alternation(arena, pattern_node, source, intern);
                        let alts_slice = arena.alloc_slice_copy(&alts);
                        Some((Atom::Group { alternatives: alts_slice }, None, None))
                    } else {
                        // Single pattern group
                        let inner_slice = arena.alloc_slice_copy(&inner_atoms);
                        let alts: &[&[AtomWithQuant<'a>]] =
                            arena.alloc_slice_copy(&[inner_slice as &[AtomWithQuant<'a>]]);
                        Some((Atom::Group { alternatives: alts }, None, None))
                    }
                } else {
                    None
                }
            }
            RULE_QUANTIFIED => {
                // First child is the atom, second is the quantifier
                if let Some(inner) = get_child_by_category(node, CAT_SYNTAX_ATOM) {
                    if let Some((atom, _, prec)) = process_atom(arena, inner, source, intern) {
                        let text = get_node_text(node, source);
                        let quant = match text.chars().last() {
                            Some('?') => Some(Quantifier::Optional),
                            Some('*') => Some(Quantifier::Star),
                            Some('+') => Some(Quantifier::Plus),
                            _ => None,
                        };
                        return Some((atom, quant, prec));
                    }
                }
                None
            }
            RULE_PRECEDENCE => {
                // First child is the atom, then @ and digits
                if let Some(inner) = get_child_by_category(node, CAT_SYNTAX_ATOM) {
                    if let Some((atom, quant, _)) = process_atom(arena, inner, source, intern) {
                        let text = get_node_text(node, source);
                        // Find the number after @
                        if let Some(at_pos) = text.rfind('@') {
                            if let Ok(prec) = text[at_pos + 1..].parse::<u32>() {
                                return Some((atom, quant, Some(prec)));
                            }
                        }
                    }
                }
                None
            }
            _ => None,
        }
    }

    fn extract_alternation<'a>(
        arena: &'a Bump,
        node: &'a SyntaxNode<'a>,
        source: &str,
        intern: &mut impl FnMut(&str) -> &'a str,
    ) -> Vec<&'a [AtomWithQuant<'a>]> {
        let mut alternatives = Vec::new();

        if node.rule == RULE_ALT {
            // Get left side (may be another alt or atom/seq)
            if let Some(left) = get_child_by_category(node, CAT_SYNTAX_PATTERN) {
                if left.rule == RULE_ALT {
                    alternatives.extend(extract_alternation(arena, left, source, intern));
                } else {
                    let atoms = extract_pattern_atoms(arena, left, source, intern);
                    let atoms_slice = arena.alloc_slice_copy(&atoms);
                    alternatives.push(atoms_slice as &[AtomWithQuant<'a>]);
                }
            }
            // Get right side (SyntaxAtom)
            let syntax_atoms = get_children_by_category(node, CAT_SYNTAX_ATOM);
            if let Some(right) = syntax_atoms.last() {
                if let Some((atom, quant, prec)) = process_atom(arena, right, source, intern) {
                    let aq = AtomWithQuant {
                        atom,
                        quant: quant.unwrap_or(Quantifier::One),
                        precedence: prec,
                    };
                    let single: &[AtomWithQuant<'a>] = arena.alloc_slice_copy(&[aq]);
                    alternatives.push(single);
                }
            }
        }

        alternatives
    }

    fn collect_atoms<'a>(
        arena: &'a Bump,
        node: &'a SyntaxNode<'a>,
        source: &str,
        intern: &mut impl FnMut(&str) -> &'a str,
        atoms: &mut Vec<AtomWithQuant<'a>>,
    ) {
        match node.rule {
            RULE_ATOM => {
                // Single atom
                if let Some(inner) = get_child_by_category(node, CAT_SYNTAX_ATOM) {
                    if let Some((atom, quant, prec)) = process_atom(arena, inner, source, intern) {
                        atoms.push(AtomWithQuant {
                            atom,
                            quant: quant.unwrap_or(Quantifier::One),
                            precedence: prec,
                        });
                    }
                }
            }
            RULE_SEQ => {
                // Sequence: SyntaxPattern followed by SyntaxAtom
                if let Some(left) = get_child_by_category(node, CAT_SYNTAX_PATTERN) {
                    collect_atoms(arena, left, source, intern, atoms);
                }
                let syntax_atoms = get_children_by_category(node, CAT_SYNTAX_ATOM);
                if let Some(right) = syntax_atoms.last() {
                    if let Some((atom, quant, prec)) = process_atom(arena, right, source, intern) {
                        atoms.push(AtomWithQuant {
                            atom,
                            quant: quant.unwrap_or(Quantifier::One),
                            precedence: prec,
                        });
                    }
                }
            }
            RULE_ALT => {
                // Alternation - create a group
                let alts = extract_alternation(arena, node, source, intern);
                let alts_slice = arena.alloc_slice_copy(&alts);
                atoms.push(AtomWithQuant {
                    atom: Atom::Group {
                        alternatives: alts_slice,
                    },
                    quant: Quantifier::One,
                    precedence: None,
                });
            }
            _ => {}
        }
    }

    collect_atoms(arena, node, source, intern, &mut atoms);
    atoms
}

/// Check if a pattern is lexical (no category references).
///
/// Lexical patterns don't need implicit whitespace insertion since they
/// match character-by-character without token boundaries.
pub fn is_lexical_pattern(pattern: &[AtomWithQuant]) -> bool {
    for aq in pattern {
        match &aq.atom {
            Atom::CategoryRef(_) => return false,
            Atom::Group { alternatives } => {
                for alt in *alternatives {
                    if !is_lexical_pattern(alt) {
                        return false;
                    }
                }
            }
            _ => {}
        }
    }
    true
}

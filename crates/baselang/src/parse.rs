//! Parsing with prelude support
//!
//! This module provides parsing functionality that automatically applies
//! the BaseLang prelude syntax before parsing user code.

use bumpalo::Bump;
use parser::constants::{CAT_EXPR, CAT_IDENT, CAT_SYNTAX_DECL, RULE_SYNTAX_CATEGORY, RULE_SYNTAX_DECL};
use parser::{
    add_expr_rule, add_syntax_decl_command_rule, extract_and_register_rule,
    get_child_by_category, get_node_text, init_syntaxlang, ParseError, Parser, SyntaxNode,
    VMParser,
};

use crate::PRELUDE;

/// Result of parsing a source file with prelude.
pub struct ParseResult<'a> {
    pub nodes: Vec<&'a SyntaxNode<'a>>,
    pub errors: Vec<ParseError>,
}

/// Parse source code with the BaseLang prelude applied.
///
/// This function:
/// 1. Initializes the parser with syntaxlang
/// 2. Processes the prelude to register base syntax rules
/// 3. Parses the user source code
/// 4. Returns the resulting AST nodes
///
/// Uses the parser's internal string interner for all string interning.
pub fn parse_with_prelude<'a>(arena: &'a Bump, source: &str) -> ParseResult<'a> {
    let combined = format!("{}\n{}", PRELUDE, source);
    let mut parser = VMParser::new(arena, &combined);

    init_syntaxlang(arena, &mut parser);
    add_expr_rule(arena, &mut parser, CAT_EXPR);
    add_syntax_decl_command_rule(arena, &mut parser);

    let mut nodes = Vec::new();
    let mut errors = Vec::new();

    // Track how many lines are in the prelude so we can identify user nodes
    let prelude_lines = PRELUDE.lines().count();

    while !parser.is_eof() {
        match parser.next_command() {
            Some(cmd_node) => {
                if cmd_node.rule == RULE_SYNTAX_DECL {
                    let syntax_decl = get_child_by_category(cmd_node, CAT_SYNTAX_DECL)
                        .expect("syntaxDecl command must have SyntaxDecl child");
                    if syntax_decl.rule == RULE_SYNTAX_CATEGORY {
                        if let Some(name_node) = get_child_by_category(syntax_decl, CAT_IDENT) {
                            let category_name = get_node_text(name_node, &combined);
                            add_expr_rule(arena, &mut parser, category_name);
                        }
                    } else {
                        extract_and_register_rule(
                            arena,
                            &mut parser,
                            syntax_decl,
                            &combined,
                        );
                    }
                } else if !cmd_node.children.is_empty() {
                    // Only collect nodes from user code (after prelude)
                    let node = cmd_node.children[0];
                    if node.start.line as usize > prelude_lines {
                        nodes.push(node);
                    }
                }
            }
            None => {
                if parser.is_eof() {
                    break;
                }
                if let Some(err) = parser.error() {
                    // Only report errors from user code
                    if err.loc.line as usize > prelude_lines {
                        errors.push(ParseError {
                            msg: err.msg,
                            loc: err.loc,
                            source_line: err.source_line,
                        });
                    }
                }
                parser.skip_to_next_line();
            }
        }
    }

    ParseResult { nodes, errors }
}

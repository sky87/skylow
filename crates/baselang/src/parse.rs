//! Parsing with prelude support
//!
//! This module provides parsing functionality that automatically applies
//! the BaseLang prelude syntax before parsing user code.

use bumpalo::Bump;
use common::SourceModule;
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
    /// The source module for the user code.
    pub source_module: &'a SourceModule<'a>,
}

/// Parse source code with the BaseLang prelude applied.
///
/// This function:
/// 1. Initializes the parser with syntaxlang
/// 2. Processes the prelude to register base syntax rules
/// 3. Switches to user source and parses it
/// 4. Returns the resulting AST nodes with offsets into user source
///
/// Uses the parser's internal string interner for all string interning.
pub fn parse_with_prelude<'a>(arena: &'a Bump, source: &str) -> ParseResult<'a> {
    // Create prelude source module
    let prelude_module = arena.alloc(SourceModule::prelude(PRELUDE));

    // First parse the prelude to register syntax rules
    let mut parser = VMParser::new(arena, prelude_module);

    init_syntaxlang(arena, &mut parser);
    add_expr_rule(arena, &mut parser, CAT_EXPR);
    add_syntax_decl_command_rule(arena, &mut parser);

    // Parse prelude (syntax declarations)
    while !parser.is_eof() {
        match parser.next_command() {
            Some(cmd_node) => {
                if cmd_node.rule == RULE_SYNTAX_DECL {
                    let syntax_decl = get_child_by_category(cmd_node, CAT_SYNTAX_DECL)
                        .expect("syntaxDecl command must have SyntaxDecl child");
                    if syntax_decl.rule == RULE_SYNTAX_CATEGORY {
                        if let Some(name_node) = get_child_by_category(syntax_decl, CAT_IDENT) {
                            let category_name = get_node_text(name_node, PRELUDE);
                            add_expr_rule(arena, &mut parser, category_name);
                        }
                    } else {
                        extract_and_register_rule(arena, &mut parser, syntax_decl, PRELUDE);
                    }
                }
            }
            None => {
                if parser.is_eof() {
                    break;
                }
                parser.skip_to_next_line();
            }
        }
    }

    // Create source module for user code
    let user_source = arena.alloc_str(source);
    let source_module = arena.alloc(SourceModule::synthetic(user_source, "<input>"));

    // Switch to user source - offsets start at 0, lines at 1
    parser.set_source(source_module);

    let mut nodes = Vec::new();
    let mut errors = Vec::new();

    // Parse user code
    while !parser.is_eof() {
        match parser.next_command() {
            Some(cmd_node) => {
                if cmd_node.rule == RULE_SYNTAX_DECL {
                    let syntax_decl = get_child_by_category(cmd_node, CAT_SYNTAX_DECL)
                        .expect("syntaxDecl command must have SyntaxDecl child");
                    if syntax_decl.rule == RULE_SYNTAX_CATEGORY {
                        if let Some(name_node) = get_child_by_category(syntax_decl, CAT_IDENT) {
                            let category_name = get_node_text(name_node, user_source);
                            add_expr_rule(arena, &mut parser, category_name);
                        }
                    } else {
                        extract_and_register_rule(arena, &mut parser, syntax_decl, user_source);
                    }
                } else if !cmd_node.children.is_empty() {
                    nodes.push(cmd_node.children[0]);
                }
            }
            None => {
                if parser.is_eof() {
                    break;
                }
                if let Some(err) = parser.error() {
                    errors.push(err);
                }
                parser.skip_to_next_line();
            }
        }
    }

    ParseResult { nodes, errors, source_module }
}

//! Driver for SkyLow parsing.

use bumpalo::Bump;
use common::SourceModule;

use parser::constants::{CAT_SYNTAX_DECL, RULE_SYNTAX_CATEGORY, RULE_SYNTAX_DECL};
use parser::{
    add_syntax_decl_command_rule, extract_and_register_rule,
    get_child_by_category, init_syntaxlang, InterpretedParser, ParseError, Parser,
    SyntaxNode, VMParser,
};

/// Result of parsing a source file.
pub struct ParseResult<'a> {
    pub nodes: Vec<&'a SyntaxNode<'a>>,
    pub errors: Vec<ParseError>,
    /// The source module for the parsed code.
    pub source_module: &'a SourceModule<'a>,
}

pub struct Driver<'a> {
    arena: &'a Bump,
    use_interpreter: bool,
}

impl<'a> Driver<'a> {
    /// Create a new driver using VMParser (default).
    pub fn new(arena: &'a Bump) -> Self {
        Self { arena, use_interpreter: false }
    }

    /// Create a driver using the interpreted parser.
    pub fn with_interpreter(arena: &'a Bump) -> Self {
        Self { arena, use_interpreter: true }
    }

    /// Process a source string and return parse results.
    pub fn process(&self, source: &str) -> ParseResult<'a> {
        // Create source module for the input
        let source_text = self.arena.alloc_str(source);
        let module = self.arena.alloc(SourceModule::synthetic(source_text, "<input>"));

        if self.use_interpreter {
            let mut parser = InterpretedParser::new(self.arena, module);
            self.process_with_parser(&mut parser, module)
        } else {
            let mut parser = VMParser::new(self.arena, module);
            self.process_with_parser(&mut parser, module)
        }
    }

    fn process_with_parser<P: Parser<'a>>(
        &self,
        parser: &mut P,
        module: &'a SourceModule<'a>,
    ) -> ParseResult<'a> {
        let source = module.text;
        init_syntaxlang(self.arena, parser);
        // Note: We do NOT add Expr as a top-level command - only Stmt is valid at top-level
        add_syntax_decl_command_rule(self.arena, parser);

        let mut nodes = Vec::new();
        let mut errors = Vec::new();

        while !parser.is_eof() {
            match parser.next_command() {
                Some(cmd_node) => {
                    if cmd_node.rule == RULE_SYNTAX_DECL {
                        let syntax_decl = get_child_by_category(cmd_node, CAT_SYNTAX_DECL)
                            .expect("syntaxDecl command must have SyntaxDecl child");
                        if syntax_decl.rule == RULE_SYNTAX_CATEGORY {
                            // Categories are registered but NOT added as top-level commands
                            // Only syntax declarations are valid at top-level
                        } else {
                            extract_and_register_rule(self.arena, parser, syntax_decl, source);
                        }
                    } else if !cmd_node.children.is_empty() {
                        // Get the expression node (first child of Command)
                        nodes.push(cmd_node.children[0]);
                    }
                }
                None => {
                    if parser.is_eof() {
                        break;
                    }
                    if let Some(err) = parser.error() {
                        errors.push(ParseError {
                            msg: err.msg,
                            loc: err.loc,
                            source_line: err.source_line,
                        });
                    }
                    parser.skip_to_next_line();
                }
            }
        }

        ParseResult { nodes, errors, source_module: module }
    }
}

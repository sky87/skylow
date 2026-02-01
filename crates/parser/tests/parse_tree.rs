//! Parse tree tests that run both InterpretedParser and VMParser on each test file.

use bumpalo::Bump;
use datatest_stable::harness;
use skylow_common::StringInterner;
use skylow_parser::constants::{CAT_EXPR, CAT_SYNTAX_DECL, RULE_SYNTAX_CATEGORY, RULE_SYNTAX_DECL};
use skylow_parser::{
    add_expr_rule, add_syntax_decl_command_rule, extract_and_register_rule, format_errors,
    format_node, get_child_by_category, init_syntaxlang, syntax_node_to_string, CompiledGrammar,
    InterpretedParser, ParseError, Parser, SyntaxNode, VMParser,
};
use std::path::Path;
use std::sync::Once;

static EXERCISE_DEBUG_ONCE: Once = Once::new();

/// Exercise debug methods once for coverage
fn exercise_debug_methods() {
    EXERCISE_DEBUG_ONCE.call_once(|| {
        let arena = Bump::new();
        let mut strings = StringInterner::new(&arena);

        // Exercise InterpretedParser debug methods
        let mut interp = InterpretedParser::new(&arena, "test");
        init_syntaxlang(&arena, &mut strings, &mut interp);
        interp.set_trace(true);
        interp.set_trace(false);
        interp.dump();
        interp.dump_rules();
        let _ = interp.parse_category(CAT_EXPR);

        // Exercise VMParser debug methods
        let mut vm = VMParser::new(&arena, "test");
        init_syntaxlang(&arena, &mut strings, &mut vm);
        vm.set_trace(true);
        vm.set_trace(false);
        vm.dump();
        vm.dump_rules();
        let _ = vm.parse_category(CAT_EXPR);

        // Exercise VM directives
        let mut vm_trace = VMParser::new(&arena, "#!vm:trace\ntest");
        init_syntaxlang(&arena, &mut strings, &mut vm_trace);
        vm_trace.check_vm_directive();

        let mut vm_dump = VMParser::new(&arena, "#!vm:dump\ntest");
        init_syntaxlang(&arena, &mut strings, &mut vm_dump);
        vm_dump.check_vm_directive();

        let mut vm_rules = VMParser::new(&arena, "#!vm:rules\ntest");
        init_syntaxlang(&arena, &mut strings, &mut vm_rules);
        vm_rules.check_vm_directive();

        // Exercise CompiledGrammar::default for coverage
        let _grammar: CompiledGrammar = Default::default();
    });
}

/// Run a parser on the input and collect results
fn run_parser<'a, 'src, P: Parser<'a, 'src>>(
    arena: &'a Bump,
    strings: &mut StringInterner<'a>,
    parser: &mut P,
    input: &'src str,
) -> (Vec<&'a SyntaxNode<'a>>, Vec<ParseError>) {
    init_syntaxlang(arena, strings, parser);
    add_expr_rule(arena, strings, parser, CAT_EXPR);
    add_syntax_decl_command_rule(arena, strings, parser);

    let mut nodes = Vec::new();
    let mut errors = Vec::new();

    while !parser.is_eof() {
        match parser.next_command() {
            Some(cmd_node) => {
                if cmd_node.rule == RULE_SYNTAX_DECL {
                    let syntax_decl = get_child_by_category(cmd_node, CAT_SYNTAX_DECL)
                        .expect("syntaxDecl command must have SyntaxDecl child");
                    if syntax_decl.rule != RULE_SYNTAX_CATEGORY {
                        extract_and_register_rule(arena, strings, parser, syntax_decl, input);
                    }
                } else if let Some(expr_node) = get_child_by_category(cmd_node, CAT_EXPR) {
                    nodes.push(expr_node);
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

    (nodes, errors)
}

/// Format nodes into output string
fn format_output(nodes: &[&SyntaxNode]) -> String {
    let mut outputs: Vec<String> = Vec::new();
    for node in nodes {
        outputs.push(syntax_node_to_string(node));
    }
    outputs.join("\n")
}

/// Format errors into output string
fn format_error_output(errors: &[ParseError], test_name: &str) -> String {
    let mut error_strs: Vec<String> = Vec::new();
    for err in errors {
        error_strs.push(format!(
            "{}:{}:{}: {}",
            test_name, err.loc.line, err.loc.col, err.msg
        ));
        error_strs.push(format!("  {}", err.source_line));
        let spaces = err.loc.col.saturating_sub(1) as usize;
        error_strs.push(format!("  {}^", " ".repeat(spaces)));
    }
    error_strs.join("\n")
}

/// Combine output and errors into final result string
fn combine_output(output: String, errors_str: String) -> String {
    if errors_str.is_empty() {
        output
    } else if output.is_empty() {
        errors_str
    } else {
        format!("{}\n{}", output, errors_str)
    }
}

fn run_test(path: &Path) -> datatest_stable::Result<()> {
    exercise_debug_methods();

    let input = std::fs::read_to_string(path)?;
    let expected_path = format!("{}.expected", path.display());
    let expected = std::fs::read_to_string(&expected_path)?.trim().to_string();
    let test_name = path.file_name().unwrap().to_str().unwrap();

    // Run interpreted parser
    let arena_interp = Bump::new();
    let mut strings_interp = StringInterner::new(&arena_interp);
    let mut parser_interp = InterpretedParser::new(&arena_interp, &input);
    let (nodes_interp, errors_interp) =
        run_parser(&arena_interp, &mut strings_interp, &mut parser_interp, &input);

    // Exercise debug formatting functions for coverage
    for node in &nodes_interp {
        let _ = format_node(node, 0);
    }
    let _ = format_errors(&errors_interp, "test.skyh");

    let actual_interp = combine_output(
        format_output(&nodes_interp),
        format_error_output(&errors_interp, test_name),
    );

    if actual_interp != expected {
        return Err(format!(
            "InterpretedParser mismatch for {:?}\n\nExpected:\n{}\n\nActual:\n{}",
            path, expected, actual_interp
        )
        .into());
    }

    // Run VM parser
    let arena_vm = Bump::new();
    let mut strings_vm = StringInterner::new(&arena_vm);
    let mut parser_vm = VMParser::new(&arena_vm, &input);
    let (nodes_vm, errors_vm) = run_parser(&arena_vm, &mut strings_vm, &mut parser_vm, &input);

    let actual_vm = combine_output(
        format_output(&nodes_vm),
        format_error_output(&errors_vm, test_name),
    );

    if actual_vm != expected {
        return Err(format!(
            "VMParser mismatch for {:?}\n\nExpected:\n{}\n\nActual:\n{}",
            path, expected, actual_vm
        )
        .into());
    }

    Ok(())
}

harness!(run_test, "tests/parse_tree", r"\.skyh$");

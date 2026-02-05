//! Tests for Driver

use bumpalo::Bump;
use compiler::{format_node, syntax_node_to_string, Driver};
use indoc::indoc;

#[test]
fn test_driver_basic() {
    let arena = Bump::new();
    let driver = Driver::new(&arena);

    // Only syntax declarations are valid at top-level
    let source = indoc! {"
        syntax num \\d+ : Expr
    "};

    let result = driver.process(source);
    assert!(result.errors.is_empty(), "Errors: {:?}", result.errors);
    // No nodes - syntax declarations don't produce output nodes
    assert!(result.nodes.is_empty());
}

#[test]
fn test_driver_empty() {
    let arena = Bump::new();
    let driver = Driver::new(&arena);

    let result = driver.process("");
    assert!(result.errors.is_empty());
    assert!(result.nodes.is_empty());
}

#[test]
fn test_driver_error() {
    let arena = Bump::new();
    let driver = Driver::new(&arena);

    let source = "@@@@";
    let result = driver.process(source);
    assert!(!result.errors.is_empty());
}

#[test]
fn test_driver_with_category() {
    let arena = Bump::new();
    let driver = Driver::new(&arena);

    let source = indoc! {"
        syntax_category Term
        syntax num \\d+ : Term
    "};

    let result = driver.process(source);
    assert!(result.errors.is_empty(), "Errors: {:?}", result.errors);
    assert!(result.nodes.is_empty());
}

#[test]
fn test_driver_interpreter_basic() {
    let arena = Bump::new();
    let driver = Driver::with_interpreter(&arena);

    let source = indoc! {"
        syntax num \\d+ : Expr
    "};

    let result = driver.process(source);
    assert!(result.errors.is_empty(), "Errors: {:?}", result.errors);
    assert!(result.nodes.is_empty());
}

#[test]
fn test_driver_interpreter_empty() {
    let arena = Bump::new();
    let driver = Driver::with_interpreter(&arena);

    let result = driver.process("");
    assert!(result.errors.is_empty());
    assert!(result.nodes.is_empty());
}

#[test]
fn test_driver_interpreter_error() {
    let arena = Bump::new();
    let driver = Driver::with_interpreter(&arena);

    let source = "@@@@";
    let result = driver.process(source);
    assert!(!result.errors.is_empty());
}

#[test]
fn test_driver_format_functions() {
    // Test format functions with a node from baselang parsing
    use baselang::parse_with_prelude;

    let arena = Bump::new();
    let source = indoc! {"
        test simple:
          assert(1 == 1)
    "};

    let result = parse_with_prelude(&arena, source);
    assert!(result.errors.is_empty(), "Errors: {:?}", result.errors);
    assert!(!result.nodes.is_empty());

    // Test formatting functions
    let _ = syntax_node_to_string(result.nodes[0]);
    let _ = format_node(result.nodes[0], 0);
}

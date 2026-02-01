//! Tests for Driver

use bumpalo::Bump;
use skylow::{format_node, syntax_node_to_string, Driver};

#[test]
fn test_driver_basic() {
    let arena = Bump::new();
    let driver = Driver::new(&arena);

    let source = r#"
syntax num \d+ : Expr
42
"#;

    let result = driver.process(source);
    assert!(result.errors.is_empty(), "Errors: {:?}", result.errors);
    assert_eq!(result.nodes.len(), 1);

    // Test formatting functions
    let _ = syntax_node_to_string(result.nodes[0]);
    let _ = format_node(result.nodes[0], 0);
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

    let source = r#"
syntax_category Term
syntax num \d+ : Term
42
"#;

    let result = driver.process(source);
    assert!(result.errors.is_empty(), "Errors: {:?}", result.errors);
    assert_eq!(result.nodes.len(), 1);
}

#[test]
fn test_driver_interpreter_basic() {
    let arena = Bump::new();
    let driver = Driver::with_interpreter(&arena);

    let source = r#"
syntax num \d+ : Expr
42
"#;

    let result = driver.process(source);
    assert!(result.errors.is_empty(), "Errors: {:?}", result.errors);
    assert_eq!(result.nodes.len(), 1);
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

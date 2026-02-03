//! Output formatting utilities for syntax nodes.

use crate::node::{ParseError, SyntaxNode};

/// Format a parse tree node for display (debug format).
pub fn format_node(node: &SyntaxNode, indent: usize) -> String {
    let mut result = String::new();
    let prefix = "  ".repeat(indent);

    if let Some(text) = node.text {
        result.push_str(&format!(
            "{}{}:{} \"{}\" @{}:{}\n",
            prefix, node.category, node.rule, text, node.start().line, node.start().col
        ));
    } else {
        result.push_str(&format!(
            "{}{}:{} @{}:{}\n",
            prefix, node.category, node.rule, node.start().line, node.start().col
        ));
        for child in node.children {
            result.push_str(&format_node(child, indent + 1));
        }
    }

    result
}

/// Format parse errors for display with source context.
pub fn format_errors(errors: &[ParseError], filename: &str) -> String {
    let mut result = String::new();
    for err in errors {
        result.push_str(&format!(
            "{}:{}:{}: {}\n",
            filename, err.loc.line, err.loc.col, err.msg
        ));
        result.push_str(&format!("  {}\n", err.source_line));
        let spaces = err.loc.col.saturating_sub(1) as usize;
        result.push_str(&format!("  {}^\n", " ".repeat(spaces)));
    }
    result
}

/// Format a syntax node in s-expression format.
///
/// - Leaf nodes: `[Category.rule text]`
/// - Branch nodes: `(Category.rule child1 child2 ...)`
/// - Internal nodes (starting with `_`) are inlined/hidden in output
pub fn syntax_node_to_string(node: &SyntaxNode) -> String {
    format_node_inner(node)
}

fn collect_visible_children(node: &SyntaxNode) -> Vec<String> {
    let mut result = Vec::new();
    for child in node.children {
        if child.category.starts_with('_') || child.rule.starts_with('_') {
            result.extend(collect_visible_children(child));
        } else {
            result.push(format_node_inner(child));
        }
    }
    result
}

fn collect_leaf_text(node: &SyntaxNode) -> String {
    if let Some(text) = node.text {
        return text.to_string();
    }
    let mut result = String::new();
    for child in node.children {
        result.push_str(&collect_leaf_text(child));
    }
    result
}

fn format_node_inner(node: &SyntaxNode) -> String {
    let name = format!("{}.{}", node.category, node.rule);
    let visible_children = collect_visible_children(node);

    if visible_children.is_empty() {
        let text = collect_leaf_text(node);
        if text.is_empty() {
            format!("[{} ]", name)
        } else {
            format!("[{} {}]", name, text)
        }
    } else {
        format!("({} {})", name, visible_children.join(" "))
    }
}

//! Lowering from SyntaxNode to typed AST
//!
//! This module converts the untyped SyntaxNode tree from the parser
//! into the typed BaseLang AST using arena allocation.

use bumpalo::Bump;
use parser::SyntaxNode;

use crate::ast::{BinOp, CmpOp, Expr, FnDecl, Program, SourceInfo, Stmt, TestDecl};

/// Error during lowering
#[derive(Debug, Clone, PartialEq)]
pub struct LowerError {
    pub msg: String,
    pub line: u32,
    pub col: u32,
}

impl std::fmt::Display for LowerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}: {}", self.line, self.col, self.msg)
    }
}

impl std::error::Error for LowerError {}

/// Lower a list of syntax nodes into a Program.
///
/// The nodes should come from parsing with the BaseLang prelude,
/// which means they'll be test declarations at the top level.
///
/// `line_offset` should be the number of lines in the prelude, so that
/// line numbers in error messages are relative to the user's source file.
pub fn lower_program<'a>(
    arena: &'a Bump,
    nodes: &[&SyntaxNode<'_>],
    source: &'a str,
    line_offset: u32,
) -> Result<Program<'a>, LowerError> {
    let mut tests = Vec::new();
    let mut functions = Vec::new();

    for node in nodes {
        if node.rule == "test" && node.category == "Stmt" {
            tests.push(lower_test(arena, node, source, line_offset)?);
        } else if node.rule == "fn" && node.category == "Stmt" {
            functions.push(lower_fn(arena, node, source, line_offset)?);
        }
        // Ignore other top-level nodes for now
    }

    Ok(Program {
        tests: arena.alloc_slice_copy(&tests),
        functions: arena.alloc_slice_copy(&functions),
    })
}

fn lower_test<'a>(
    arena: &'a Bump,
    node: &SyntaxNode<'_>,
    source: &'a str,
    line_offset: u32,
) -> Result<TestDecl<'a>, LowerError> {
    // Test node structure: test |> "test" [^\n:]+ ":" >> Stmt+
    // The [^\n:]+ captures individual characters as _char:_charset nodes
    // We need to collect them and accumulate the name

    let mut name_chars = Vec::new();
    let mut body = Vec::new();

    for child in node.children {
        if child.category == "Stmt" {
            body.push(lower_stmt(arena, child, source, line_offset)?);
        } else if child.rule == "_charset" && child.category == "_char" {
            // Character set captures individual chars
            if let Some(text) = child.text {
                name_chars.push(text);
            }
        }
    }

    // Concatenate and trim, then allocate in arena
    let name_string: String = name_chars.concat();
    let name = arena.alloc_str(name_string.trim());

    Ok(TestDecl {
        name,
        body: arena.alloc_slice_copy(&body),
    })
}

fn lower_fn<'a>(
    arena: &'a Bump,
    node: &SyntaxNode<'_>,
    source: &'a str,
    line_offset: u32,
) -> Result<FnDecl<'a>, LowerError> {
    // Function node structure: fn |> "fn" [a-z_][a-z0-9_]* "()" ":" >> Stmt+
    // The name is captured by the character class patterns

    let mut name_chars = Vec::new();
    let mut body = Vec::new();

    for child in node.children {
        if child.category == "Stmt" {
            body.push(lower_stmt(arena, child, source, line_offset)?);
        } else if child.rule == "_charset" && child.category == "_char" {
            // Character set captures individual chars
            if let Some(text) = child.text {
                name_chars.push(text);
            }
        }
    }

    // Concatenate and trim, then allocate in arena
    let name_string: String = name_chars.concat();
    let name = arena.alloc_str(name_string.trim());

    // Capture source info for the function declaration
    let (start, line, col, end) = get_full_span(node);
    let adjusted_line = line.saturating_sub(line_offset);
    let fn_text = source.get(start..end).unwrap_or("");
    let info = SourceInfo {
        line: adjusted_line,
        col,
        source: fn_text,
    };

    Ok(FnDecl {
        name,
        body: arena.alloc_slice_copy(&body),
        info,
    })
}

fn lower_stmt<'a>(
    arena: &'a Bump,
    node: &SyntaxNode<'_>,
    source: &'a str,
    line_offset: u32,
) -> Result<Stmt<'a>, LowerError> {
    match node.rule {
        "assert" => {
            // assert "assert" "(" Expr ")"
            let expr_node = find_child_by_category(node, "Expr").ok_or_else(|| LowerError {
                msg: "assert missing expression".to_string(),
                line: node.start.line.saturating_sub(line_offset),
                col: node.start.col,
            })?;
            let expr = lower_expr(arena, expr_node, source)?;

            // Capture source info for error reporting
            // Use the full span of the expression, including nested children
            let (start, line, col, end) = get_full_span(expr_node);
            // Adjust line number to be relative to user's file, not combined source
            let adjusted_line = line.saturating_sub(line_offset);
            let expr_text = &source[start..end];
            let info = SourceInfo {
                line: adjusted_line,
                col,
                source: expr_text,
            };

            Ok(Stmt::Assert {
                expr: arena.alloc(expr),
                info,
            })
        }
        _ => Err(LowerError {
            msg: format!("unknown statement rule: {}", node.rule),
            line: node.start.line.saturating_sub(line_offset),
            col: node.start.col,
        }),
    }
}

fn lower_expr<'a>(
    arena: &'a Bump,
    node: &SyntaxNode<'_>,
    source: &'a str,
) -> Result<Expr<'a>, LowerError> {
    match node.rule {
        "int" => {
            let text = get_text(node, source);
            let value: i64 = text.parse().map_err(|_| LowerError {
                msg: format!("invalid integer: {}", text),
                line: node.start.line,
                col: node.start.col,
            })?;
            Ok(Expr::Int(value))
        }
        "add" | "sub" | "mul" | "div" => {
            let op = match node.rule {
                "add" => BinOp::Add,
                "sub" => BinOp::Sub,
                "mul" => BinOp::Mul,
                "div" => BinOp::Div,
                _ => unreachable!(),
            };
            let (left, right) = get_binary_operands(arena, node, source)?;
            Ok(Expr::BinOp {
                op,
                left: arena.alloc(left),
                right: arena.alloc(right),
            })
        }
        "eq" | "neq" | "lt" | "lte" | "gt" | "gte" => {
            let op = match node.rule {
                "eq" => CmpOp::Eq,
                "neq" => CmpOp::Neq,
                "lt" => CmpOp::Lt,
                "lte" => CmpOp::Lte,
                "gt" => CmpOp::Gt,
                "gte" => CmpOp::Gte,
                _ => unreachable!(),
            };
            let (left, right) = get_binary_operands(arena, node, source)?;
            Ok(Expr::Cmp {
                op,
                left: arena.alloc(left),
                right: arena.alloc(right),
            })
        }
        "paren" => {
            let inner = find_child_by_category(node, "Expr").ok_or_else(|| LowerError {
                msg: "paren missing inner expression".to_string(),
                line: node.start.line,
                col: node.start.col,
            })?;
            Ok(Expr::Paren(arena.alloc(lower_expr(arena, inner, source)?)))
        }
        _ => Err(LowerError {
            msg: format!("unknown expression rule: {}", node.rule),
            line: node.start.line,
            col: node.start.col,
        }),
    }
}

fn get_binary_operands<'a>(
    arena: &'a Bump,
    node: &SyntaxNode<'_>,
    source: &'a str,
) -> Result<(Expr<'a>, Expr<'a>), LowerError> {
    let expr_children: Vec<_> = node
        .children
        .iter()
        .filter(|c| c.category == "Expr")
        .collect();

    if expr_children.len() != 2 {
        return Err(LowerError {
            msg: format!(
                "binary op {} expected 2 Expr children, got {}",
                node.rule,
                expr_children.len()
            ),
            line: node.start.line,
            col: node.start.col,
        });
    }

    let left = lower_expr(arena, expr_children[0], source)?;
    let right = lower_expr(arena, expr_children[1], source)?;
    Ok((left, right))
}

fn find_child_by_category<'a>(
    node: &'a SyntaxNode<'a>,
    category: &str,
) -> Option<&'a SyntaxNode<'a>> {
    node.children.iter().find(|c| c.category == category).copied()
}

fn get_text<'a>(node: &'a SyntaxNode<'a>, source: &'a str) -> &'a str {
    if let Some(text) = node.text {
        text
    } else {
        let start = node.start.offset as usize;
        let end = node.end_offset as usize;
        &source[start..end]
    }
}

/// Get the full span of a node including all its children.
/// Returns (start_offset, line, col, end_offset).
/// This handles left-recursive rules where the node's start might not
/// include the left operand.
fn get_full_span(node: &SyntaxNode<'_>) -> (usize, u32, u32, usize) {
    let mut start = node.start.offset as usize;
    let mut line = node.start.line;
    let mut col = node.start.col;
    let mut end = node.end_offset as usize;

    // Check children for earlier start or later end
    for child in node.children {
        let (child_start, child_line, child_col, child_end) = get_full_span(child);
        if child_start < start {
            start = child_start;
            line = child_line;
            col = child_col;
        }
        if child_end > end {
            end = child_end;
        }
    }

    (start, line, col, end)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse_with_prelude;

    #[test]
    fn test_lower_simple_test() {
        let arena = Bump::new();
        let source = r#"
test simple:
  assert(1 == 1)
"#;
        let result = parse_with_prelude(&arena, source);
        assert!(result.errors.is_empty(), "parse errors: {:?}", result.errors);

        // We need access to the combined source for lowering
        let combined = format!("{}\n{}", crate::PRELUDE, source);
        let combined_ref = arena.alloc_str(&combined);
        let prelude_lines = crate::PRELUDE.lines().count() as u32 + 1; // +1 for newline between prelude and source
        let program = lower_program(&arena, &result.nodes, combined_ref, prelude_lines).expect("lowering failed");

        assert_eq!(program.tests.len(), 1);
        assert_eq!(program.tests[0].name, "simple");
        assert_eq!(program.tests[0].body.len(), 1);
    }

    #[test]
    fn test_lower_arithmetic() {
        let arena = Bump::new();
        let source = r#"
test arithmetic:
  assert((2 + 2) == 4)
  assert((3 * 4) == 12)
"#;
        let result = parse_with_prelude(&arena, source);
        assert!(result.errors.is_empty(), "parse errors: {:?}", result.errors);

        let combined = format!("{}\n{}", crate::PRELUDE, source);
        let combined_ref = arena.alloc_str(&combined);
        let prelude_lines = crate::PRELUDE.lines().count() as u32 + 1; // +1 for newline between prelude and source
        let program = lower_program(&arena, &result.nodes, combined_ref, prelude_lines).expect("lowering failed");

        assert_eq!(program.tests.len(), 1);
        assert_eq!(program.tests[0].name, "arithmetic");
        assert_eq!(program.tests[0].body.len(), 2);
    }

    #[test]
    fn test_lower_error_display() {
        let err = LowerError {
            msg: "test error".to_string(),
            line: 10,
            col: 5,
        };
        let s = format!("{}", err);
        assert_eq!(s, "10:5: test error");
    }

    #[test]
    fn test_lower_all_comparison_ops() {
        let arena = Bump::new();
        let source = r#"
test comparisons:
  assert(1 == 1)
  assert((1 != 2) == 1)
  assert((1 < 2) == 1)
  assert((2 <= 2) == 1)
  assert((3 > 2) == 1)
  assert((2 >= 2) == 1)
"#;
        let result = parse_with_prelude(&arena, source);
        assert!(result.errors.is_empty(), "parse errors: {:?}", result.errors);

        let combined = format!("{}\n{}", crate::PRELUDE, source);
        let combined_ref = arena.alloc_str(&combined);
        let prelude_lines = crate::PRELUDE.lines().count() as u32 + 1; // +1 for newline between prelude and source
        let program = lower_program(&arena, &result.nodes, combined_ref, prelude_lines).expect("lowering failed");

        assert_eq!(program.tests.len(), 1);
        assert_eq!(program.tests[0].body.len(), 6);
    }

    #[test]
    fn test_lower_fn_decl() {
        let arena = Bump::new();
        let source = r#"
fn main():
  assert(1 == 1)
"#;
        let result = parse_with_prelude(&arena, source);
        assert!(result.errors.is_empty(), "parse errors: {:?}", result.errors);

        let combined = format!("{}\n{}", crate::PRELUDE, source);
        let combined_ref = arena.alloc_str(&combined);
        let prelude_lines = crate::PRELUDE.lines().count() as u32 + 1;
        let program = lower_program(&arena, &result.nodes, combined_ref, prelude_lines).expect("lowering failed");

        assert_eq!(program.tests.len(), 0);
        assert_eq!(program.functions.len(), 1);
        assert_eq!(program.functions[0].name, "main");
        assert_eq!(program.functions[0].body.len(), 1);
    }

    #[test]
    fn test_lower_fn_and_test() {
        let arena = Bump::new();
        let source = r#"
fn main():
  assert(1 == 1)

test simple:
  assert(2 == 2)
"#;
        let result = parse_with_prelude(&arena, source);
        assert!(result.errors.is_empty(), "parse errors: {:?}", result.errors);

        let combined = format!("{}\n{}", crate::PRELUDE, source);
        let combined_ref = arena.alloc_str(&combined);
        let prelude_lines = crate::PRELUDE.lines().count() as u32 + 1;
        let program = lower_program(&arena, &result.nodes, combined_ref, prelude_lines).expect("lowering failed");

        assert_eq!(program.tests.len(), 1);
        assert_eq!(program.tests[0].name, "simple");
        assert_eq!(program.functions.len(), 1);
        assert_eq!(program.functions[0].name, "main");
    }
}

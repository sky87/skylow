//! Lowering from SyntaxNode to typed AST
//!
//! This module converts the untyped SyntaxNode tree from the parser
//! into the typed BaseLang AST using arena allocation.

use bumpalo::Bump;
use common::SourceLoc;
use parser::SyntaxNode;

use crate::ast::{BinOp, CmpOp, Decl, DeclKind, Expr, ExprKind, FnParam, Program, SourceInfo, Stmt, StmtKind, Type, new_node_id};

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

/// Lowering context that holds shared state for the lowering pass.
struct Lowerer<'a> {
    arena: &'a Bump,
}

impl<'a> Lowerer<'a> {
    fn new(arena: &'a Bump) -> Self {
        Self { arena }
    }

    fn lower_program(&self, nodes: &[&SyntaxNode<'a>]) -> Result<Program<'a>, LowerError> {
        let mut decls = Vec::new();

        for node in nodes {
            if node.rule == "test" && node.category == "Stmt" {
                decls.push(self.lower_test(node)?);
            } else if node.rule == "fn" && node.category == "Stmt" {
                decls.push(self.lower_fn(node)?);
            }
            // Ignore other top-level nodes for now
        }

        Ok(Program {
            decls: self.arena.alloc_slice_copy(&decls),
        })
    }

    fn lower_test(&self, node: &SyntaxNode<'a>) -> Result<Decl<'a>, LowerError> {
        // Test node structure: test |> "test" [^\n:]+ ":" >> Stmt+
        // The [^\n:]+ captures individual characters as _char:_charset nodes
        // We need to collect them and accumulate the name

        let mut name_chars = Vec::new();
        let mut body = Vec::new();

        for child in node.children {
            if child.category == "Stmt" {
                body.push(self.lower_stmt(child)?);
            } else if child.rule == "_charset" && child.category == "_char" {
                // Character set captures individual chars
                if let Some(text) = child.text {
                    name_chars.push(text);
                }
            }
        }

        // Concatenate and trim, then allocate in arena
        let name_string: String = name_chars.concat();
        let name = self.arena.alloc_str(name_string.trim());

        // Capture source info for the test declaration
        let (start_offset, line, col, end) = get_full_span(node);
        let start = SourceLoc::new(start_offset as u32, line, col);
        let info = SourceInfo::new(node.info.module, start, end as u32);

        Ok(Decl {
            info,
            kind: DeclKind::Test {
                name,
                body: self.arena.alloc_slice_copy(&body),
            },
            id: new_node_id(),
        })
    }

    fn lower_fn(&self, node: &SyntaxNode<'a>) -> Result<Decl<'a>, LowerError> {
        // Function node structure: fn |> "fn" Name "(" params ")" "->" Type ":" >> Stmt+
        // The name is in a Name category child

        let mut params = Vec::new();
        let mut body = Vec::new();
        let mut return_type = Type::I64; // Default return type
        let mut name = "";

        for child in node.children {
            if child.category == "Stmt" {
                body.push(self.lower_stmt(child)?);
            } else if child.category == "Param" {
                params.push(self.lower_param(child)?);
            } else if child.category == "Type" {
                return_type = self.lower_type(child)?;
            } else if child.category == "Name" {
                // Extract function name from Name node
                name = self.extract_identifier_name_from(child);
            }
        }

        // Capture source info for the function declaration
        let (start_offset, line, col, end) = get_full_span(node);
        let start = SourceLoc::new(start_offset as u32, line, col);
        let info = SourceInfo::new(node.info.module, start, end as u32);

        Ok(Decl {
            info,
            kind: DeclKind::Fn {
                name,
                params: self.arena.alloc_slice_copy(&params),
                return_type,
                body: self.arena.alloc_slice_copy(&body),
            },
            id: new_node_id(),
        })
    }

    fn lower_param(&self, node: &SyntaxNode<'a>) -> Result<FnParam<'a>, LowerError> {
        // Param node structure: param Name ":" Type
        let mut param_type = Type::I64;
        let mut name = "";

        for child in node.children {
            if child.category == "Type" {
                param_type = self.lower_type(child)?;
            } else if child.category == "Name" {
                name = self.extract_identifier_name_from(child);
            }
        }

        let (start_offset, line, col, end) = get_full_span(node);
        let start = SourceLoc::new(start_offset as u32, line, col);
        let info = SourceInfo::new(node.info.module, start, end as u32);

        Ok(FnParam {
            name,
            ty: param_type,
            info,
            id: new_node_id(),
        })
    }

    fn lower_type(&self, node: &SyntaxNode<'a>) -> Result<Type, LowerError> {
        match node.rule {
            "typeI64" => Ok(Type::I64),
            _ => Err(LowerError {
                msg: format!("unknown type: {}", node.rule),
                line: node.start().line,
                col: node.start().col,
            }),
        }
    }

    fn lower_stmt(&self, node: &'a SyntaxNode<'a>) -> Result<Stmt<'a>, LowerError> {
        match node.rule {
            "assert" => {
                // assert "assert" "(" Expr ")"
                let expr_node = find_child_by_category(node, "Expr").ok_or_else(|| LowerError {
                    msg: "assert missing expression".to_string(),
                    line: node.start().line,
                    col: node.start().col,
                })?;
                let expr = self.lower_expr(expr_node)?;

                // Capture source info for error reporting
                // Use the full span of the expression, including nested children
                let (start_offset, line, col, end) = get_full_span(expr_node);
                let start = SourceLoc::new(start_offset as u32, line, col);
                let info = SourceInfo::new(node.info.module, start, end as u32);

                Ok(Stmt {
                    info,
                    kind: StmtKind::Assert {
                        expr: self.arena.alloc(expr),
                    },
                    id: new_node_id(),
                })
            }
            "return" => {
                // return "return" Expr
                let expr_node = find_child_by_category(node, "Expr").ok_or_else(|| LowerError {
                    msg: "return missing expression".to_string(),
                    line: node.start().line,
                    col: node.start().col,
                })?;
                let expr = self.lower_expr(expr_node)?;

                let (start_offset, line, col, end) = get_full_span(node);
                let start = SourceLoc::new(start_offset as u32, line, col);
                let info = SourceInfo::new(node.info.module, start, end as u32);

                Ok(Stmt {
                    info,
                    kind: StmtKind::Return {
                        expr: self.arena.alloc(expr),
                    },
                    id: new_node_id(),
                })
            }
            "let" => {
                // let "let" name "=" Expr
                let name = self.extract_identifier_name(node);
                let expr_node = find_child_by_category(node, "Expr").ok_or_else(|| LowerError {
                    msg: "let missing expression".to_string(),
                    line: node.start().line,
                    col: node.start().col,
                })?;
                let expr = self.lower_expr(expr_node)?;

                let (start_offset, line, col, end) = get_full_span(node);
                let start = SourceLoc::new(start_offset as u32, line, col);
                let info = SourceInfo::new(node.info.module, start, end as u32);

                Ok(Stmt {
                    info,
                    kind: StmtKind::Let {
                        name,
                        expr: self.arena.alloc(expr),
                    },
                    id: new_node_id(),
                })
            }
            _ => Err(LowerError {
                msg: format!("unknown statement rule: {}", node.rule),
                line: node.start().line,
                col: node.start().col,
            }),
        }
    }

    fn lower_expr(&self, node: &'a SyntaxNode<'a>) -> Result<Expr<'a>, LowerError> {
        // Capture source info for this expression
        let (start_offset, line, col, end) = get_full_span(node);
        let start = SourceLoc::new(start_offset as u32, line, col);
        let info = SourceInfo::new(node.info.module, start, end as u32);

        let kind = match node.rule {
            "int" => {
                let text = self.get_text(node);
                let value: i64 = text.parse().map_err(|_| LowerError {
                    msg: format!("invalid integer: {}", text),
                    line: node.start().line,
                    col: node.start().col,
                })?;
                ExprKind::Int(value)
            }
            "var" => {
                // Variable reference: extract name from charset chars
                let name = self.extract_identifier_name(node);
                ExprKind::Var(name)
            }
            "call" => {
                // Function call: name(args)
                let name = self.extract_identifier_name(node);
                let expr_children: Vec<_> = node
                    .children
                    .iter()
                    .filter(|c| c.category == "Expr")
                    .collect();
                let mut args = Vec::new();
                for expr_node in expr_children {
                    args.push(self.lower_expr(expr_node)?);
                }
                ExprKind::Call {
                    name,
                    args: self.arena.alloc_slice_copy(&args),
                }
            }
            "add" | "sub" | "mul" | "div" => {
                let op = match node.rule {
                    "add" => BinOp::Add,
                    "sub" => BinOp::Sub,
                    "mul" => BinOp::Mul,
                    "div" => BinOp::Div,
                    _ => unreachable!(),
                };
                let (left, right) = self.get_binary_operands(node)?;
                ExprKind::BinOp {
                    op,
                    left: self.arena.alloc(left),
                    right: self.arena.alloc(right),
                }
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
                let (left, right) = self.get_binary_operands(node)?;
                ExprKind::Cmp {
                    op,
                    left: self.arena.alloc(left),
                    right: self.arena.alloc(right),
                }
            }
            "paren" => {
                let inner = find_child_by_category(node, "Expr").ok_or_else(|| LowerError {
                    msg: "paren missing inner expression".to_string(),
                    line: node.start().line,
                    col: node.start().col,
                })?;
                ExprKind::Paren(self.arena.alloc(self.lower_expr(inner)?))
            }
            _ => {
                return Err(LowerError {
                    msg: format!("unknown expression rule: {}", node.rule),
                    line: node.start().line,
                    col: node.start().col,
                })
            }
        };

        Ok(Expr { info, kind, id: new_node_id() })
    }

    /// Extract identifier name from charset characters in a node.
    /// Handles both direct _charset children and _charset nested in Name category nodes.
    fn extract_identifier_name(&self, node: &SyntaxNode<'a>) -> &'a str {
        let mut name_chars = Vec::new();
        self.collect_charset_chars(node, &mut name_chars);
        let name_string: String = name_chars.concat();
        self.arena.alloc_str(name_string.trim())
    }

    /// Recursively collect _charset characters from a node tree.
    /// This handles both direct _charset children and those nested in Name nodes.
    fn collect_charset_chars(&self, node: &SyntaxNode<'a>, chars: &mut Vec<&'a str>) {
        for child in node.children {
            if child.rule == "_charset" && child.category == "_char" {
                if let Some(text) = child.text {
                    chars.push(text);
                }
            } else if child.category == "Name" {
                // Recurse into Name nodes to find charset characters
                self.collect_charset_chars(child, chars);
            }
        }
    }

    /// Extract identifier name directly from a Name node.
    fn extract_identifier_name_from(&self, node: &SyntaxNode<'a>) -> &'a str {
        let mut name_chars = Vec::new();
        self.collect_charset_chars(node, &mut name_chars);
        let name_string: String = name_chars.concat();
        self.arena.alloc_str(name_string.trim())
    }

    fn get_binary_operands(&self, node: &SyntaxNode<'a>) -> Result<(Expr<'a>, Expr<'a>), LowerError> {
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
                line: node.start().line,
                col: node.start().col,
            });
        }

        let left = self.lower_expr(expr_children[0])?;
        let right = self.lower_expr(expr_children[1])?;
        Ok((left, right))
    }

    fn get_text(&self, node: &SyntaxNode<'a>) -> &'a str {
        if let Some(text) = node.text {
            text
        } else {
            node.info.module.slice(node.start().offset as usize, node.end_offset() as usize)
        }
    }
}

/// Lower a list of syntax nodes into a Program.
///
/// The nodes should come from parsing with the BaseLang prelude,
/// which means they'll be test declarations at the top level.
///
/// Line numbers in the nodes are already adjusted to be relative to
/// the user's source file (the parser resets line info after the prelude).
pub fn lower_program<'a>(
    arena: &'a Bump,
    nodes: &[&SyntaxNode<'a>],
) -> Result<Program<'a>, LowerError> {
    Lowerer::new(arena).lower_program(nodes)
}

fn find_child_by_category<'a>(
    node: &'a SyntaxNode<'a>,
    category: &str,
) -> Option<&'a SyntaxNode<'a>> {
    node.children.iter().find(|c| c.category == category).copied()
}

/// Get the full span of a node including all its children.
/// Returns (start_offset, line, col, end_offset).
/// This handles left-recursive rules where the node's start might not
/// include the left operand.
fn get_full_span(node: &SyntaxNode<'_>) -> (usize, u32, u32, usize) {
    let mut start = node.start().offset as usize;
    let mut line = node.start().line;
    let mut col = node.start().col;
    let mut end = node.end_offset() as usize;

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
    use crate::ast::DeclKind;
    use common::{SourceInfo, SourceModule};
    use indoc::indoc;
    use parser::SyntaxNode;

    #[test]
    fn test_lower_simple_test() {
        let arena = Bump::new();
        let source = indoc! {"
            test simple:
              assert(1 == 1)
        "};
        let result = parse_with_prelude(&arena, source);
        assert!(result.errors.is_empty(), "parse errors: {:?}", result.errors);

        let program = lower_program(&arena, &result.nodes).expect("lowering failed");

        assert_eq!(program.decls.len(), 1);
        match &program.decls[0].kind {
            DeclKind::Test { name, body } => {
                assert_eq!(*name, "simple");
                assert_eq!(body.len(), 1);
            }
            _ => panic!("expected Test declaration"),
        }
    }

    #[test]
    fn test_lower_arithmetic() {
        let arena = Bump::new();
        let source = indoc! {"
            test arithmetic:
              assert((2 + 2) == 4)
              assert((3 * 4) == 12)
        "};
        let result = parse_with_prelude(&arena, source);
        assert!(result.errors.is_empty(), "parse errors: {:?}", result.errors);

        let program = lower_program(&arena, &result.nodes).expect("lowering failed");

        assert_eq!(program.decls.len(), 1);
        match &program.decls[0].kind {
            DeclKind::Test { name, body } => {
                assert_eq!(*name, "arithmetic");
                assert_eq!(body.len(), 2);
            }
            _ => panic!("expected Test declaration"),
        }
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
        let source = indoc! {"
            test comparisons:
              assert(1 == 1)
              assert((1 != 2) == 1)
              assert((1 < 2) == 1)
              assert((2 <= 2) == 1)
              assert((3 > 2) == 1)
              assert((2 >= 2) == 1)
        "};
        let result = parse_with_prelude(&arena, source);
        assert!(result.errors.is_empty(), "parse errors: {:?}", result.errors);

        let program = lower_program(&arena, &result.nodes).expect("lowering failed");

        assert_eq!(program.decls.len(), 1);
        match &program.decls[0].kind {
            DeclKind::Test { body, .. } => {
                assert_eq!(body.len(), 6);
            }
            _ => panic!("expected Test declaration"),
        }
    }

    #[test]
    fn test_lower_fn_decl() {
        let arena = Bump::new();
        let source = indoc! {"
            fn main() -> I64:
              return 0
        "};
        let result = parse_with_prelude(&arena, source);
        assert!(result.errors.is_empty(), "parse errors: {:?}", result.errors);

        let program = lower_program(&arena, &result.nodes).expect("lowering failed");

        assert_eq!(program.decls.len(), 1);
        match &program.decls[0].kind {
            DeclKind::Fn { name, params, body, .. } => {
                assert_eq!(*name, "main");
                assert_eq!(params.len(), 0);
                assert_eq!(body.len(), 1);
            }
            _ => panic!("expected Fn declaration"),
        }
    }

    #[test]
    fn test_lower_fn_and_test() {
        let arena = Bump::new();
        let source = indoc! {"
            fn main() -> I64:
              return 0

            test simple:
              assert(2 == 2)
        "};
        let result = parse_with_prelude(&arena, source);
        assert!(result.errors.is_empty(), "parse errors: {:?}", result.errors);

        let program = lower_program(&arena, &result.nodes).expect("lowering failed");

        assert_eq!(program.decls.len(), 2);
        // First is fn, second is test (order preserved)
        assert!(matches!(program.decls[0].kind, DeclKind::Fn { .. }));
        assert!(matches!(program.decls[1].kind, DeclKind::Test { .. }));
    }

    #[test]
    fn test_lower_integer_overflow() {
        let arena = Bump::new();
        // Integer too large to fit in i64
        let source = indoc! {"
            test overflow:
              assert(99999999999999999999999999999999 == 1)
        "};
        let result = parse_with_prelude(&arena, source);
        assert!(result.errors.is_empty(), "parse errors: {:?}", result.errors);

        let err = lower_program(&arena, &result.nodes).unwrap_err();
        assert!(err.msg.contains("invalid integer"), "expected integer error, got: {}", err.msg);
    }

    #[test]
    fn test_lower_assert_missing_expr() {
        // Create a malformed assert node without an Expr child
        let arena = Bump::new();
        let source = arena.alloc_str("assert()");
        let module = arena.alloc(SourceModule::synthetic(source, "<test>"));
        let info = SourceInfo::new(module, SourceLoc::new(0, 1, 1), 8);

        // Create an assert node with no children (malformed)
        let assert_node = arena.alloc(SyntaxNode::branch("Stmt", "assert", info, &[]));

        let lowerer = Lowerer::new(&arena);
        let err = lowerer.lower_stmt(assert_node).unwrap_err();
        assert!(err.msg.contains("assert missing expression"));
    }

    #[test]
    fn test_lower_paren_missing_expr() {
        // Create a malformed paren node without an Expr child
        let arena = Bump::new();
        let source = arena.alloc_str("()");
        let module = arena.alloc(SourceModule::synthetic(source, "<test>"));
        let info = SourceInfo::new(module, SourceLoc::new(0, 1, 1), 2);

        // Create a paren node with no children (malformed)
        let paren_node = arena.alloc(SyntaxNode::branch("Expr", "paren", info, &[]));

        let lowerer = Lowerer::new(&arena);
        let err = lowerer.lower_expr(paren_node).unwrap_err();
        assert!(err.msg.contains("paren missing inner expression"));
    }

    #[test]
    fn test_program_tests_iterator() {
        let arena = Bump::new();
        let source = indoc! {"
            fn main() -> I64:
              return 0

            test a:
              assert(1 == 1)

            test b:
              assert(2 == 2)
        "};
        let result = parse_with_prelude(&arena, source);
        let program = lower_program(&arena, &result.nodes).expect("lowering failed");

        let tests: Vec<_> = program.tests().collect();
        assert_eq!(tests.len(), 2);

        let functions: Vec<_> = program.functions().collect();
        assert_eq!(functions.len(), 1);
    }
}

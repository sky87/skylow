//! Syntax node types for the parser.

pub use common::{SourceInfo, SourceLoc, SourceModule};

/// Parse error with location and context information.
#[derive(Debug)]
pub struct ParseError {
    pub msg: String,
    pub loc: SourceLoc,
    pub source_line: String,
}

#[derive(Debug)]
pub struct SyntaxNode<'a> {
    pub category: &'a str, // interned
    pub rule: &'a str,     // interned
    /// Source location information (module + span).
    pub info: SourceInfo<'a>,

    /// For leaf nodes: the matched text (slice of source)
    pub text: Option<&'a str>,

    /// For branch nodes: child nodes
    pub children: &'a [&'a SyntaxNode<'a>],
}

impl<'a> SyntaxNode<'a> {
    /// Create a leaf node with matched text
    pub fn leaf(
        category: &'a str,
        rule: &'a str,
        info: SourceInfo<'a>,
        text: &'a str,
    ) -> Self {
        Self {
            category,
            rule,
            info,
            text: Some(text),
            children: &[],
        }
    }

    /// Create a branch node with children
    pub fn branch(
        category: &'a str,
        rule: &'a str,
        info: SourceInfo<'a>,
        children: &'a [&'a SyntaxNode<'a>],
    ) -> Self {
        Self {
            category,
            rule,
            info,
            text: None,
            children,
        }
    }

    /// Get the start position.
    pub fn start(&self) -> SourceLoc {
        self.info.start
    }

    /// Get the end offset.
    pub fn end_offset(&self) -> u32 {
        self.info.end
    }
}

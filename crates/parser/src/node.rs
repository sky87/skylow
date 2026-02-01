#[derive(Debug, Clone, Copy, Default)]
pub struct SourceLoc {
    pub offset: u32,
    pub line: u32,
    pub col: u32,
}

impl SourceLoc {
    pub fn new(offset: u32, line: u32, col: u32) -> Self {
        Self { offset, line, col }
    }
}

/// Parse error with location and context information.
#[derive(Debug)]
pub struct ParseError {
    pub msg: String,
    pub loc: SourceLoc,
    pub source_line: String,
}

#[derive(Debug)]
pub struct SyntaxNode<'a> {
    pub category: &'a str,  // interned
    pub rule: &'a str,      // interned
    pub start: SourceLoc,
    pub end_offset: u32,

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
        start: SourceLoc,
        end_offset: u32,
        text: &'a str,
    ) -> Self {
        Self {
            category,
            rule,
            start,
            end_offset,
            text: Some(text),
            children: &[],
        }
    }

    /// Create a branch node with children
    pub fn branch(
        category: &'a str,
        rule: &'a str,
        start: SourceLoc,
        end_offset: u32,
        children: &'a [&'a SyntaxNode<'a>],
    ) -> Self {
        Self {
            category,
            rule,
            start,
            end_offset,
            text: None,
            children,
        }
    }
}

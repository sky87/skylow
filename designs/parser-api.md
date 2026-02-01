# Parser API Design

## Overview

This document defines a minimal, clean API for the parser. The design is driven by one key constraint: **imports can influence syntax**, so source files must be processed command-by-command, allowing external systems to modify the grammar between parses.

## Core Concept

The parser processes source as a sequence of **commands**. Each command is either:
- A syntax declaration (which modifies the grammar for subsequent commands)
- An expression (which produces output)

External systems need to:
1. Get the next command
2. Inspect it to determine if it affects syntax
3. Modify the grammar if needed (e.g., process an import)
4. Repeat until EOF

## Minimal API

```rust
pub struct Parser<'a, 'src> { /* opaque */ }

impl<'a, 'src> Parser<'a, 'src> {
    /// Create a new parser for the given source.
    pub fn new(arena: &'a Bump, source: &'src str) -> Self;

    /// Parse the next command from the source.
    /// Returns None at EOF or on parse failure.
    /// On failure, call `error()` to get details.
    pub fn next_command(&mut self) -> Option<&'a SyntaxNode<'a>>;

    /// Add a syntax rule to the grammar.
    /// New rules take precedence over existing rules for the same category.
    pub fn add_rule(&mut self, rule: &'a SyntaxRule<'a>);

    /// Get the parse error from the last failed `next_command()`.
    pub fn error(&self) -> Option<ParseError>;

    /// Check if all input has been consumed.
    pub fn is_eof(&self) -> bool;

    // --- Debug utilities ---

    /// Enable or disable parse tracing to stderr.
    pub fn set_trace(&mut self, enabled: bool);

    /// Dump parser state to stderr (position, indent state, error tracking).
    pub fn dump(&self);

    /// Dump registered rules to stderr.
    pub fn dump_rules(&self);

    /// Dump compiled grammar bytecode to stderr.
    pub fn dump_grammar(&self);
}
```

### Justification

| Method | Justification |
|--------|---------------|
| `new` | Required to create a parser instance |
| `next_command` | Core operation - parses one command, handles whitespace/indent internally |
| `add_rule` | Required for imports/syntax declarations to modify grammar |
| `error` | Required for error reporting when parsing fails |
| `is_eof` | Required for the parse loop termination condition |
| `set_trace` | Enables step-by-step tracing for debugging grammar issues |
| `dump` | Inspects parser state when debugging parse failures |
| `dump_rules` | Lists registered rules to verify grammar construction |
| `dump_grammar` | Inspects compiled bytecode for low-level debugging |

## What's NOT Exposed

### Character-level navigation (peek, advance, remaining)
**Reason:** External systems parse commands, not characters. Character navigation is an internal concern of the parser.

### Whitespace handling (skip_ws, skip_ws_all, reset_indent)
**Reason:** Whitespace between commands is an internal concern. `next_command()` handles this automatically:
- Skips leading whitespace
- Resets indent state for each new command
- Handles comments

### Error tracking internals (clear_furthest, furthest_pos, etc.)
**Reason:** Error state is managed internally. After a failed parse, `error()` provides what's needed.

### Memory management (intern, arena)
**Reason:** The parser owns its arena and string interning. External systems don't need direct access.


### VM directives (check_vm_directive)
**Reason:** Implementation detail. If directives are needed, `next_command()` can handle them internally or return them as a special node type.

### Location tracking (current_loc)
**Reason:** Locations are embedded in returned `SyntaxNode`s. External systems don't need the parser's current position.

## Bootstrap Grammar

The parser initializes itself with the syntax language grammar (from `syntaxlang.rs`). This is internal - external systems don't need to call `init_syntaxlang()`.

```rust
impl<'a, 'src> Parser<'a, 'src> {
    pub fn new(arena: &'a Bump, source: &'src str) -> Self {
        let mut parser = Self::new_empty(arena, source);
        init_syntaxlang(&mut parser);  // Internal bootstrap
        parser
    }
}
```

## Usage Pattern

```rust
let arena = Bump::new();
let mut parser = Parser::new(&arena, source);

while !parser.is_eof() {
    match parser.next_command() {
        Some(node) => {
            if is_syntax_decl(node) {
                let rule = extract_rule(node, source);
                parser.add_rule(rule);
            } else if is_import(node) {
                // Load imported file, add its rules
                let imported_rules = process_import(node);
                for rule in imported_rules {
                    parser.add_rule(rule);
                }
            } else {
                // Regular expression - process output
                handle_expression(node);
            }
        }
        None => {
            // Parse error
            if let Some(err) = parser.error() {
                report_error(err);
            }
            // Parser should skip to next line automatically
        }
    }
}
```

## SyntaxRule Construction

External systems (like import processors) need to construct `SyntaxRule` values. The `syntax` module types remain public:

```rust
pub struct SyntaxRule<'a> {
    pub name: &'a str,
    pub category: &'a str,
    pub pattern: &'a [AtomWithQuant<'a>],
    pub is_left_recursive: bool,
}

pub struct AtomWithQuant<'a> {
    pub atom: Atom<'a>,
    pub quant: Quantifier,
    pub precedence: Option<u32>,
}

pub enum Atom<'a> { /* ... */ }
pub enum Quantifier { One, Optional, Star, Plus }
```

These are data structures, not behavior - exposing them is appropriate.

## Error Type

```rust
pub struct ParseError {
    pub msg: String,
    pub loc: SourceLoc,
    pub source_line: String,
}

pub struct SourceLoc {
    pub offset: u32,
    pub line: u32,
    pub col: u32,
}
```

## SyntaxNode Type

```rust
pub struct SyntaxNode<'a> {
    pub category: &'a str,
    pub rule: &'a str,
    pub start: SourceLoc,
    pub end_offset: u32,
    pub text: Option<&'a str>,
    pub children: &'a [&'a SyntaxNode<'a>],
}
```

## Summary

The API surface is:
- `Parser::new`, `next_command`, `add_rule`, `error`, `is_eof`
- `SyntaxRule`, `AtomWithQuant`, `Atom`, `Quantifier` (data types for rule construction)
- `SyntaxNode`, `SourceLoc`, `ParseError` (output types)

Everything else is internal implementation detail.

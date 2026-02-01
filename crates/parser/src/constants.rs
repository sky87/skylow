//! Shared string constants used across multiple modules.
//!
//! This module contains only constants that are referenced in multiple places
//! across the codebase. Module-local constants should be defined in their
//! respective modules.

// =============================================================================
// Category Names
// =============================================================================

/// Internal identifier category (prefixed with _ to avoid conflicts with user-defined "Ident")
pub const CAT_IDENT: &str = "_Ident";

/// Syntax atom category - individual pattern elements
pub const CAT_SYNTAX_ATOM: &str = "SyntaxAtom";

/// Syntax pattern category - sequences and alternations of atoms
pub const CAT_SYNTAX_PATTERN: &str = "SyntaxPattern";

/// Syntax declaration category - syntax definitions
pub const CAT_SYNTAX_DECL: &str = "SyntaxDecl";

/// Command category - top-level statements (syntax decls, expressions)
pub const CAT_COMMAND: &str = "Command";

/// Expression category - default category for user expressions
pub const CAT_EXPR: &str = "Expr";

// =============================================================================
// Rule Names (shared across modules)
// =============================================================================

/// Rule name for syntax declarations with category: `syntax name pattern : Category`
pub const RULE_SYNTAX: &str = "syntax";

/// Rule name for syntax_category declarations
pub const RULE_SYNTAX_CATEGORY: &str = "syntaxCategory";

/// Rule name for the syntaxDecl command wrapper
pub const RULE_SYNTAX_DECL: &str = "syntaxDecl";

/// Rule name for expression commands
pub const RULE_EXPR: &str = "expr";

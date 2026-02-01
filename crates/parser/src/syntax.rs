use bumpalo::{collections::Vec as BumpVec, Bump};
use hashbrown::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Quantifier {
    One,      // exactly one
    Optional, // ?
    Star,     // *
    Plus,     // +
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CharClass {
    Digit,      // \d
    Word,       // \w
    Whitespace, // \s
}

#[derive(Debug, Clone, Copy)]
pub enum Atom<'a> {
    /// Literal string match: "hello"
    Literal(&'a str),

    /// Character set: [a..z], [^0..9]
    CharSet {
        ranges: &'a [(char, char)], // inclusive ranges
        negated: bool,
    },

    /// Character class: \d, \w, \s
    CharClass(CharClass),

    /// Group with alternation: (a | b | c)
    Group {
        alternatives: &'a [&'a [AtomWithQuant<'a>]],
    },

    /// Category reference: Arith
    CategoryRef(&'a str), // interned

    /// Indent anchor: |>
    IndentAnchor,

    /// Strict indent mode: >>
    IndentStrict,

    /// Lax indent mode: >
    IndentLax,

    /// Indent-aware whitespace (for implicit whitespace between elements)
    IndentAwareWs,
}

#[derive(Debug, Clone, Copy)]
pub struct AtomWithQuant<'a> {
    pub atom: Atom<'a>,
    pub quant: Quantifier,
    pub precedence: Option<u32>, // from @N annotation
}

pub struct SyntaxRule<'a> {
    pub name: &'a str,           // interned
    pub category: &'a str,       // interned
    pub pattern: &'a [AtomWithQuant<'a>],
    pub is_left_recursive: bool, // starts with self-category ref
}

/// Rule storage with O(1) amortized insertion.
///
/// Rules are stored in insertion order. `for_category()` returns an iterator
/// in reverse order so the most recently added rule is tried first.
///
/// # Safety Invariant
///
/// Callers must not hold iterators from `for_category()` across calls to `add()`
/// for the same category, as reallocation would invalidate them. In practice
/// this is satisfied: rules are only added between top-level parses, never
/// during recursive parsing.
pub struct RuleTable<'a> {
    arena: &'a Bump,
    rules: HashMap<&'a str, BumpVec<'a, &'a SyntaxRule<'a>>, hashbrown::DefaultHashBuilder, &'a Bump>,
}

impl<'a> RuleTable<'a> {
    pub fn new(arena: &'a Bump) -> Self {
        Self {
            arena,
            rules: HashMap::new_in(arena),
        }
    }

    pub fn add(&mut self, rule: &'a SyntaxRule<'a>) {
        self.rules
            .entry(rule.category)
            .or_insert_with(|| BumpVec::new_in(self.arena))
            .push(rule);
    }

    /// Returns all rules for a category in insertion order.
    /// Caller should iterate in reverse (latest rule = highest priority).
    ///
    /// # Safety
    ///
    /// The returned slice has lifetime `'a` tied to the arena. It is invalidated
    /// if `add()` causes reallocation for this category. See struct-level doc.
    pub fn for_category(&self, category: &str) -> &'a [&'a SyntaxRule<'a>] {
        self.rules
            .get(category)
            .map(|v| {
                // SAFETY: BumpVec data lives in arena with lifetime 'a. Caller
                // must not hold slice across mutations (see struct doc).
                unsafe { std::slice::from_raw_parts(v.as_ptr(), v.len()) }
            })
            .unwrap_or(&[])
    }
}

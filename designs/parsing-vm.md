# Parsing VM Design

A bytecode virtual machine for parsing that compiles syntax rules to instructions, enabling efficient execution and incremental rule updates.

## Motivation

The current parser interprets syntax rules directly during parsing:
- Each pattern match involves traversing `AtomWithQuant` structures
- Rule selection iterates through all rules for a category
- Backtracking requires saving/restoring complex state

A VM approach offers:
- **Faster execution**: Bytecode interpretation is more cache-friendly
- **Incremental updates**: New rules can patch existing bytecode
- **Simpler state management**: VM has explicit state machine semantics
- **Potential for optimization**: Bytecode can be analyzed and optimized

## Architecture Overview

```
┌─────────────────┐     ┌──────────────┐     ┌─────────────┐
│  Syntax Rules   │────▶│   Compiler   │────▶│  Bytecode   │
│  (SyntaxRule)   │     │              │     │  (Vec<Op>)  │
└─────────────────┘     └──────────────┘     └─────────────┘
                                                    │
                                                    ▼
┌─────────────────┐     ┌──────────────┐     ┌─────────────┐
│   Parse Tree    │◀────│      VM      │◀────│   Source    │
│  (SyntaxNode)   │     │              │     │   Input     │
└─────────────────┘     └──────────────┘     └─────────────┘
```

## Bytecode Instructions

### Instruction Encoding

Instructions are 32-bit words to balance density and simplicity:

```
┌────────┬────────────────────────┐
│ opcode │        operand         │
│ 8 bits │        24 bits         │
└────────┴────────────────────────┘
```

Some instructions use the following 32-bit word as an extended operand.

### Core Instructions

#### Input Matching

| Opcode | Operand | Description |
|--------|---------|-------------|
| `LITERAL` | str_id | Match literal string from string table |
| `CHAR_CLASS` | class_id | Match character class (Digit/Word/Whitespace) |
| `CHAR_SET` | charset_id | Match character from charset table |
| `ANY_CHAR` | - | Match any single character |

#### Control Flow

| Opcode | Operand | Description |
|--------|---------|-------------|
| `JUMP` | offset | Unconditional jump |
| `CHOICE` | offset | Push backtrack point, jump on failure |
| `COMMIT` | offset | Pop backtrack point, jump |
| `FAIL` | - | Trigger backtrack or fail parse |
| `END` | - | Successfully end current rule/category |

#### Category/Rule Dispatch

| Opcode | Operand | Description |
|--------|---------|-------------|
| `CALL` | category_id | Parse category (push return address) |
| `RETURN` | - | Return from category parse |
| `DISPATCH` | table_id | Jump to rule based on dispatch table |
| `PRATT_LOOP` | category_id | Enter Pratt parsing infix loop |
| `PRATT_EXTEND` | rule_id | Try to extend with left-recursive rule |

#### Node Building

| Opcode | Operand | Description |
|--------|---------|-------------|
| `BEGIN_NODE` | rule_id | Start building a parse node |
| `END_NODE` | - | Complete parse node, add to parent |
| `CAPTURE_LEAF` | rule_id | Create leaf node from matched text |

#### Quantifier Support

| Opcode | Operand | Description |
|--------|---------|-------------|
| `LOOP_START` | offset | Begin repetition (*, +) |
| `LOOP_END` | offset | End repetition, jump back or continue |
| `OPTIONAL` | offset | Try match, continue either way |

#### Indentation

| Opcode | Operand | Description |
|--------|---------|-------------|
| `INDENT_ANCHOR` | - | Mark pending indent anchor |
| `INDENT_MODE` | mode | Set indent mode (None/Lax/Strict) |
| `INDENT_PUSH` | - | Push current indent to stack |
| `INDENT_POP` | - | Pop indent from stack |
| `SKIP_WS` | flags | Skip whitespace (indent-aware based on flags) |

#### Precedence

| Opcode | Operand | Description |
|--------|---------|-------------|
| `CHECK_PREC` | prec | Fail if current precedence >= operand |
| `SET_PREC` | prec | Set precedence for recursive call |

## VM State

```rust
struct VM<'a> {
    // Bytecode
    code: &'a [u32],
    pc: usize,  // Program counter

    // Input
    source: &'a str,
    pos: usize,
    line: u32,
    col: u32,

    // Call stack (for category calls)
    call_stack: Vec<CallFrame>,

    // Backtrack stack
    backtrack_stack: Vec<BacktrackPoint>,

    // Node building
    node_stack: Vec<NodeBuilder<'a>>,

    // Indentation
    indent_stack: Vec<u32>,
    indent_mode: IndentMode,
    pending_anchor: bool,

    // Pratt parsing
    prec_stack: Vec<u32>,
    left_node: Option<&'a SyntaxNode<'a>>,

    // Error tracking
    furthest_pos: usize,
    furthest_expected: Vec<&'a str>,

    // Arena for allocations
    arena: &'a Bump,
}

struct CallFrame {
    return_pc: usize,
    node_stack_depth: usize,
}

struct BacktrackPoint {
    pc: usize,
    pos: usize,
    line: u32,
    col: u32,
    call_stack_depth: usize,
    node_stack_depth: usize,
    indent_stack_depth: usize,
    indent_mode: IndentMode,
    pending_anchor: bool,
}

struct NodeBuilder<'a> {
    category: &'a str,
    rule: &'a str,
    start: SourceLoc,
    children: BumpVec<'a, &'a SyntaxNode<'a>>,
}
```

## Compilation

### Compiling a Category

Each category compiles to a block of bytecode. The entry point for a category uses `DISPATCH` to select among rules.

```
category_entry:
    DISPATCH dispatch_table_id    ; Try rules in priority order
    FAIL                          ; No rule matched

rule_1:
    BEGIN_NODE rule_1_id
    ... pattern bytecode ...
    END_NODE
    RETURN

rule_2:
    BEGIN_NODE rule_2_id
    ... pattern bytecode ...
    END_NODE
    RETURN
```

### Dispatch Tables

A dispatch table maps category entry to a sequence of rules to try:

```rust
struct DispatchTable {
    category_id: u32,
    // Rules in priority order (reverse insertion order)
    // Each entry: (rule_id, bytecode_offset)
    rules: Vec<(u32, usize)>,
    // For Pratt parsing: separate lists
    prefix_rules: Vec<(u32, usize)>,
    infix_rules: Vec<(u32, usize, u32)>,  // includes precedence
}
```

The VM executes dispatch as:
```
for (rule_id, offset) in table.rules.iter().rev() {
    CHOICE offset        ; Push backtrack, try rule
    JUMP rule_offset     ; Jump to rule code
    COMMIT next          ; Success, skip remaining rules
next:
}
FAIL                     ; No rule matched
```

### Compiling Patterns

#### Literals

```
"hello"  →  LITERAL str_id("hello")
```

#### Character Classes

```
\d  →  CHAR_CLASS Digit
\w  →  CHAR_CLASS Word
\s  →  CHAR_CLASS Whitespace
```

#### Character Sets

```
[a-z]  →  CHAR_SET charset_id
```

#### Category References

```
Expr  →  CALL category_id(Expr)
```

#### Groups (Alternation)

```
("+" | "-")  →
    CHOICE alt2
    LITERAL "+"
    COMMIT done
alt2:
    LITERAL "-"
done:
```

#### Quantifiers

Optional (`?`):
```
Expr?  →
    CHOICE skip
    CALL Expr
skip:
```

Star (`*`):
```
Expr*  →
loop:
    CHOICE done
    CALL Expr
    JUMP loop
done:
```

Plus (`+`):
```
Expr+  →
    CALL Expr
loop:
    CHOICE done
    CALL Expr
    JUMP loop
done:
```

#### Implicit Whitespace

Between non-lexical atoms:
```
Expr "+" Expr  →
    CALL Expr
    SKIP_WS INDENT_AWARE
    LITERAL "+"
    SKIP_WS INDENT_AWARE
    CALL Expr
```

### Compiling Pratt Parsing

Left-recursive rules compile differently. The category entry uses a two-phase approach:

```
Expr_entry:
    ; Phase 1: Parse prefix
    DISPATCH Expr_prefix_table
    FAIL

    ; Phase 2: Pratt loop for infix
pratt_loop:
    PRATT_LOOP Expr_infix_table
    RETURN

Expr_infix_table dispatch:
    ; For each left-recursive rule, try to extend
    ; PRATT_EXTEND checks precedence and tries rule
```

For a rule like `syntax add Expr "+"@10 Expr@10 : Expr`:

```
add_infix:
    ; left node already parsed, stored in VM
    CHECK_PREC 10            ; Fail if current prec >= 10
    BEGIN_NODE add
    ; Insert left node as first child (implicit)
    SKIP_WS INDENT_AWARE
    LITERAL "+"
    SKIP_WS INDENT_AWARE
    SET_PREC 10
    CALL Expr                ; Right operand with precedence
    END_NODE
    ; Node becomes new left, loop continues
```

## Incremental Updates

A key goal is supporting incremental bytecode updates when new rules are added.

### Strategy: Append and Patch

When a new rule is added to category `Cat`:

1. **Append**: Compile the new rule to bytecode, append to code buffer
2. **Patch dispatch table**: Add new rule entry to `Cat`'s dispatch table
3. **No recompilation**: Existing rules remain unchanged

```
Before:
    code: [... Cat_dispatch ... rule1 ... rule2 ...]
    Cat_dispatch_table: [(rule2, off2), (rule1, off1)]

After adding rule3:
    code: [... Cat_dispatch ... rule1 ... rule2 ... rule3 ...]
    Cat_dispatch_table: [(rule3, off3), (rule2, off2), (rule1, off1)]
```

### Dispatch Table Structure

To enable efficient incremental updates, dispatch tables are stored separately from bytecode:

```rust
struct CompiledGrammar {
    // Bytecode buffer (append-only)
    code: Vec<u32>,

    // String table (append-only)
    strings: Vec<&str>,

    // Charset table (append-only)
    charsets: Vec<CharSet>,

    // Category dispatch tables (mutable)
    categories: HashMap<&str, DispatchTable>,

    // Rule metadata
    rules: Vec<RuleInfo>,
}

struct RuleInfo {
    category: u32,
    name: u32,          // string table index
    bytecode_offset: usize,
    is_left_recursive: bool,
    precedence: Option<u32>,
}
```

### Handling Category Reference Updates

When a new rule is added to category `Cat`, any existing `CALL Cat` instructions automatically benefit because:
- `CALL` references the category by ID
- Dispatch happens at runtime via the updated dispatch table
- No patching of call sites required

### Removing Rules

Rule removal is more complex. Options:

1. **Lazy removal**: Mark rule as deleted in dispatch table, skip during dispatch
2. **Compaction**: Periodically recompile to remove dead code
3. **Versioned tables**: Keep old tables for in-flight parses

For skylow, lazy removal is likely sufficient since rule removal is rare.

## Execution Model

### Main Loop

```rust
impl VM {
    fn run(&mut self) -> Result<&'a SyntaxNode<'a>, ParseError> {
        loop {
            let instr = self.fetch();
            match opcode(instr) {
                LITERAL => self.exec_literal(operand(instr))?,
                CHAR_CLASS => self.exec_char_class(operand(instr))?,
                CHAR_SET => self.exec_char_set(operand(instr))?,
                CHOICE => self.exec_choice(operand(instr)),
                COMMIT => self.exec_commit(operand(instr)),
                FAIL => self.exec_fail()?,
                CALL => self.exec_call(operand(instr)),
                RETURN => {
                    if self.call_stack.is_empty() {
                        return Ok(self.finish_root_node());
                    }
                    self.exec_return();
                }
                // ... other instructions
            }
        }
    }
}
```

### Backtracking

```rust
fn exec_choice(&mut self, offset: i32) {
    self.backtrack_stack.push(BacktrackPoint {
        pc: (self.pc as i32 + offset) as usize,
        pos: self.pos,
        line: self.line,
        col: self.col,
        call_stack_depth: self.call_stack.len(),
        node_stack_depth: self.node_stack.len(),
        indent_stack_depth: self.indent_stack.len(),
        indent_mode: self.indent_mode,
        pending_anchor: self.pending_anchor,
    });
}

fn exec_commit(&mut self, offset: i32) {
    self.backtrack_stack.pop();
    self.pc = (self.pc as i32 + offset) as usize;
}

fn exec_fail(&mut self) -> Result<(), ParseError> {
    if let Some(bp) = self.backtrack_stack.pop() {
        self.pc = bp.pc;
        self.pos = bp.pos;
        self.line = bp.line;
        self.col = bp.col;
        self.call_stack.truncate(bp.call_stack_depth);
        self.node_stack.truncate(bp.node_stack_depth);
        self.indent_stack.truncate(bp.indent_stack_depth);
        self.indent_mode = bp.indent_mode;
        self.pending_anchor = bp.pending_anchor;
        Ok(())
    } else {
        Err(self.make_error())
    }
}
```

### Category Calls

```rust
fn exec_call(&mut self, category_id: u32) {
    self.call_stack.push(CallFrame {
        return_pc: self.pc,
        node_stack_depth: self.node_stack.len(),
    });
    self.pc = self.grammar.category_entry(category_id);
}

fn exec_return(&mut self) {
    let frame = self.call_stack.pop().unwrap();
    self.pc = frame.return_pc;
}
```

## Data Tables

### String Table

Stores all literal strings referenced by bytecode:

```rust
struct StringTable<'a> {
    strings: Vec<&'a str>,
    index: HashMap<&'a str, u32>,
}

impl StringTable {
    fn intern(&mut self, s: &'a str) -> u32 {
        if let Some(&id) = self.index.get(s) {
            return id;
        }
        let id = self.strings.len() as u32;
        self.strings.push(s);
        self.index.insert(s, id);
        id
    }
}
```

### Charset Table

Stores compiled character sets:

```rust
struct CharSetTable {
    sets: Vec<CompiledCharSet>,
}

struct CompiledCharSet {
    // Bitmap covering all 256 byte values
    bitmap: [u64; 4],  // 256 bits
    negated: bool,
}
```

## Optimization Opportunities

### Static Dispatch

For categories with few rules, inline the dispatch logic:

```
; Instead of DISPATCH
CHOICE rule2
JUMP rule1
rule1:
    ...
    COMMIT done
rule2:
    ...
done:
```

### First-Set Optimization

Compute FIRST sets for rules and add lookahead checks:

```
; If rule1 starts with "let" and rule2 starts with "fn"
LOOKAHEAD_3              ; Get next 3 chars
SWITCH
    "let" → rule1
    "fn " → rule2
    _     → FAIL
```

### Memoization (Packrat)

Add memo table for expensive category parses:

```rust
struct MemoTable {
    // (category_id, position) → Option<(end_pos, node)>
    cache: HashMap<(u32, usize), Option<(usize, NodeRef)>>,
}
```

Instructions:
- `MEMO_CHECK category_id` - Check memo, skip parse if cached
- `MEMO_STORE` - Store result in memo table

### Tail Call Optimization

Convert `CALL X; RETURN` sequences to `JUMP X_entry`:

```
; Before
CALL Expr
RETURN

; After
JUMP Expr_entry
```

## Memory Layout

### Code Segment

```
┌────────────────────────────────────────┐
│ Bootstrap rules (fixed)                │
├────────────────────────────────────────┤
│ User rule 1                            │
├────────────────────────────────────────┤
│ User rule 2                            │
├────────────────────────────────────────┤
│ ... (grows via append)                 │
└────────────────────────────────────────┘
```

### Runtime Stacks

All stacks use arena allocation for the parse duration:

```
┌─────────────────┐
│ Call Stack      │  Small, bounded by grammar depth
├─────────────────┤
│ Backtrack Stack │  Can grow large for ambiguous grammars
├─────────────────┤
│ Node Stack      │  Bounded by tree depth
├─────────────────┤
│ Indent Stack    │  Push-only, bounded by nesting
└─────────────────┘
```

## Error Handling

### Furthest Position Tracking

Each matching instruction records failure position:

```rust
fn exec_literal(&mut self, str_id: u32) -> Result<(), ()> {
    let s = self.strings.get(str_id);
    if self.source[self.pos..].starts_with(s) {
        self.pos += s.len();
        self.update_line_col(s);
        Ok(())
    } else {
        self.record_expected(s);
        Err(())
    }
}

fn record_expected(&mut self, expected: &'a str) {
    if self.pos > self.furthest_pos {
        self.furthest_pos = self.pos;
        self.furthest_expected.clear();
    }
    if self.pos == self.furthest_pos {
        self.furthest_expected.push(expected);
    }
}
```

### Error Recovery

Optional instruction for error recovery:

```
SYNC_TO delimiter  ; Skip input until delimiter found
```

## Implementation Phases

### Phase 1: Core VM

1. Define instruction set (matching, control flow, calls)
2. Implement VM execution loop
3. Implement backtracking
4. Basic compilation from `SyntaxRule` to bytecode

### Phase 2: Node Building

1. Add node building instructions
2. Implement node stack management
3. Generate proper parse trees

### Phase 3: Pratt Parsing

1. Add precedence instructions
2. Implement Pratt loop in VM
3. Handle left-recursive rule compilation

### Phase 4: Indentation

1. Add indent tracking instructions
2. Implement indent-aware whitespace skipping
3. Compile indent markers to bytecode

### Phase 5: Incremental Updates

1. Implement append-only code buffer
2. Mutable dispatch tables
3. Rule addition without recompilation

### Phase 6: Optimizations

1. First-set lookahead
2. Static dispatch inlining
3. Tail call optimization

## Example: Compiling Arithmetic

Given:
```
syntax_category Arith
syntax nat [0..9]+ : Arith
syntax add Arith "+"@10 Arith@10 : Arith
syntax mul Arith "*"@20 Arith@20 : Arith
```

Compiled bytecode:

```
; String table: ["+", "*"]
; Charset table: [0: digits]

Arith_entry:                ; offset 0
    DISPATCH Arith_prefix   ; Try prefix rules

Arith_pratt:                ; offset 2
    PRATT_LOOP Arith_infix  ; Try infix extensions
    RETURN

nat:                        ; offset 4
    BEGIN_NODE nat
    CHAR_SET 0              ; Match digit
nat_loop:
    CHOICE nat_done
    CHAR_SET 0
    JUMP nat_loop
nat_done:
    CAPTURE_LEAF nat
    RETURN

add_infix:                  ; offset 14
    CHECK_PREC 10
    BEGIN_NODE add
    SKIP_WS
    LITERAL 0               ; "+"
    SKIP_WS
    SET_PREC 10
    CALL Arith
    END_NODE
    RETURN

mul_infix:                  ; offset 24
    CHECK_PREC 20
    BEGIN_NODE mul
    SKIP_WS
    LITERAL 1               ; "*"
    SKIP_WS
    SET_PREC 20
    CALL Arith
    END_NODE
    RETURN

; Dispatch tables:
Arith_prefix: [(nat, 4)]
Arith_infix: [(mul, 24, 20), (add, 14, 10)]
```

## Comparison with Current Approach

| Aspect | Current (Interpreted) | VM (Bytecode) |
|--------|----------------------|---------------|
| Rule storage | `Vec<SyntaxRule>` | Bytecode + tables |
| Pattern matching | Recursive descent | Instruction execution |
| Backtracking | Checkpoint structs | Backtrack stack |
| Memory | Per-rule allocations | Flat code buffer |
| Adding rules | Append to Vec | Append + patch table |
| Cache behavior | Poor (pointer chasing) | Good (linear scan) |

## Dispatch Strategy Analysis

When parsing a category, the VM must select which rule to try. This is the "dispatch" problem. The choice of strategy significantly impacts performance, especially for categories with many rules.

### Option 1: Linear Search

Try each rule in priority order until one succeeds.

```
category_entry:
    CHOICE try_rule2
    JUMP rule1
    COMMIT done
try_rule2:
    CHOICE try_rule3
    JUMP rule2
    COMMIT done
try_rule3:
    JUMP rule3
done:
```

**Pros:**
- Simple implementation
- No additional data structures
- Preserves exact priority ordering
- Good for categories with few rules (1-5)

**Cons:**
- O(n) worst case per parse attempt
- Poor cache behavior for many rules
- Backtracking overhead for each failed rule

**Best for:** Categories with ≤5 rules, or where first rule usually matches.

### Option 2: First-Byte Dispatch Table

Use the first byte of input to index into a 256-entry table that lists candidate rules.

```rust
struct FirstByteDispatch {
    // For each byte value, list of rules that could match
    table: [RuleList; 256],
    // Rules that need lookahead (start with category ref, etc.)
    fallback: RuleList,
}

struct RuleList {
    rules: &[u32],  // Rule IDs in priority order
}
```

**Execution:**
```
PEEK_BYTE                    ; Get first byte without consuming
DISPATCH_BYTE table_id       ; Index into table, get candidate list
; Then linear search within candidates
```

**Pros:**
- O(1) initial dispatch for most cases
- Dramatically reduces candidates for keyword-heavy grammars
- Works well with lexical rules (literals, char sets)

**Cons:**
- 256-entry table per category (2KB if entries are 8 bytes)
- Rules starting with category refs go to fallback
- Must rebuild table when rules added

**Best for:** Categories with many rules that start with distinct literals or char sets.

### Option 3: Trie-Based Dispatch

For categories where rules start with string literals, build a trie for multi-byte lookahead.

```rust
struct TrieDispatch {
    nodes: Vec<TrieNode>,
}

struct TrieNode {
    // Byte value → child node index (0 = no child)
    children: [u16; 256],
    // Rules that match at this prefix (in priority order)
    rules: &[u32],
}
```

**Execution:**
```
TRIE_DISPATCH trie_id        ; Walk trie based on input bytes
; Returns best matching rule or falls back to linear search
```

**Pros:**
- Handles common prefixes efficiently ("let" vs "loop")
- Can distinguish keywords in single lookup
- No backtracking for prefix-disjoint rules

**Cons:**
- Higher memory usage for deep tries
- Complex to maintain incrementally
- Only helps rules starting with literals

**Best for:** Keyword-heavy grammars (programming languages with reserved words).

### Option 4: Hybrid Approach (Recommended)

Combine strategies based on category characteristics:

```rust
enum DispatchStrategy {
    // Few rules: just try them in order
    Linear { rules: &[u32] },

    // Many rules with distinct first bytes
    FirstByte { table: &FirstByteDispatch },

    // Keyword-heavy category
    Trie { trie: &TrieDispatch, fallback: &[u32] },
}

struct DispatchTable {
    prefix_strategy: DispatchStrategy,
    infix_rules: &[(u32, u32)],  // (rule_id, precedence) for Pratt
}
```

**Selection heuristics:**
1. ≤5 rules → Linear
2. >50% rules start with distinct single bytes → FirstByte
3. >50% rules start with multi-char literals → Trie
4. Otherwise → Linear (simplicity wins)

**Incremental updates:**
- Linear: append rule to list, re-sort by priority
- FirstByte: add rule to appropriate table entries
- Trie: insert literal prefix, or demote to Linear if complex

### Option 5: Hash-Based Dispatch

Hash the first N bytes of input to select a rule bucket.

```rust
struct HashDispatch {
    // Hash of first N bytes → candidate rules
    buckets: HashMap<u64, RuleList>,
    lookahead_bytes: usize,  // N
    fallback: RuleList,
}
```

**Pros:**
- O(1) average case
- Compact for sparse rule sets

**Cons:**
- Hash collisions require linear fallback
- Fixed lookahead length is inflexible
- Poor for rules starting with category refs

**Best for:** Rarely the best choice; FirstByte is usually better.

### Recommendation

Start with **Linear dispatch** for simplicity. Add **FirstByte dispatch** as an optimization once the VM is working. The implementation path:

1. **Phase 1**: Linear dispatch only
2. **Phase 2**: Add FirstByte for categories with >10 rules
3. **Phase 3**: Consider Trie for keyword-heavy categories if needed

For incremental updates, Linear and FirstByte are straightforward to maintain. Trie requires more care but can fall back to Linear when modifications make the trie stale.

### Dispatch Table Layout

```rust
struct CompiledDispatch {
    // Category ID → dispatch info
    categories: Vec<CategoryDispatch>,
}

struct CategoryDispatch {
    entry_pc: usize,           // Bytecode entry point
    strategy: DispatchStrategy,

    // For Pratt parsing
    has_left_recursive: bool,
    infix_rules: Vec<InfixRule>,
}

struct InfixRule {
    rule_id: u32,
    bytecode_pc: usize,
    precedence: u32,
}
```

---
name: implement-feature
description: Implement a new language feature across all compiler layers. Use when adding new syntax, operators, statements, or other language constructs to SkyLow.
argument-hint: [feature-description]
---

# Implement Language Feature

Implement a new language feature in SkyLow: $ARGUMENTS

New language features must be implemented and tested at every level of the compiler stack. Create separate tasks for each layer, working top-down.

## Step 0: Evaluate Parser Requirements

First, determine if this feature requires changes to the syntax language or parser:

- **Does it introduce new syntax?** (new keywords, operators, statement forms)
- **Can it be expressed with existing syntax rules in `prelude.skyh`?**

If parser changes are needed, start at the parser level. Otherwise, skip to Step 1.

### If Parser Changes Required

1. Update `crates/parser/` with new syntax primitives if needed
2. Add parser tests in `crates/parser/tests/parse_tree/`
3. Verify with `cargo run -p cli -- --emit=parse test.skyl`

## Step 1: Prelude & AST (baselang)

1. Add syntax rules to `crates/baselang/src/prelude.skyh` for new constructs
2. Update AST types in `crates/baselang/src/ast.rs` to represent the feature
3. Update lowering in `crates/baselang/src/lower.rs` to build AST nodes
4. Add tests in `crates/baselang/tests/lower/`
5. Verify with `cargo run -p cli -- --emit=ast test.skyl`

## Step 2: MIR Lowering (mir)

1. Add new MIR instructions in `crates/mir/src/lib.rs` if needed
2. Implement AST-to-MIR lowering in `crates/mir/src/lower.rs`
3. Add tests in `crates/mir/tests/compile/`
4. Verify with `cargo run -p cli -- --emit=mir test.skyl`

## Step 3: Code Generation (codegen)

1. Add AArch64 instruction lowering in `crates/codegen/src/aarch64/lower.rs`
2. Handle any new register allocation requirements
3. Unit test the instruction emission

## Step 4: JIT Execution (jit)

1. Ensure the feature works with JIT compilation
2. Add execution tests in `crates/jit/tests/execute/`
3. Test with `cargo run -p cli -- test test.skyl`

## Step 5: ELF Binary (elf)

1. Verify the feature works in compiled binaries
2. Add tests in `crates/elf/tests/compile/`
3. Test with `cargo run -p cli -- compile test.skyl -o test && ./test`

## Step 6: Debug Support (debuginfo, debug, debugger)

If the feature affects debugging (local variables, control flow, new scopes):

1. Update debug info generation in `crates/debuginfo/`
2. Update debugger support in `crates/debug/` and `crates/debugger/`
3. Add debugger tests in `crates/debugger/tests/debug/`

## Step 7: End-to-End (compiler)

1. Add integration tests in `crates/compiler/tests/e2e/`
2. Test the full pipeline from source to execution

## Debugging Tips

Use `--emit=` flags to inspect intermediate representations at each stage:
- `--emit=parse` - Parsed syntax tree
- `--emit=ast` - BaseLang AST
- `--emit=mir` - MIR instructions

This makes debugging easier since you can verify each transformation is correct before moving to the next layer.

## Completion Checklist

- [ ] All layers implemented with tests
- [ ] `cargo build` passes
- [ ] `cargo cov` shows 100% function coverage and 95%+ line coverage
- [ ] Feature works with JIT (`test` and `run` commands)
- [ ] Feature works with compiled binaries (`compile` command)
- [ ] Debug support updated if applicable

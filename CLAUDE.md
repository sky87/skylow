# SkyLow

Rust port of the SkyHigh parser targeting a low-level language subset (no closures, manual memory management).

## Project Structure

This is a Cargo workspace with twelve crates:

- **common** (`crates/common/`) - Shared utilities (debug logging, string interning, source tracking)
- **parser** (`crates/parser/`) - Core parser library with interpreter and bytecode VM
- **baselang** (`crates/baselang/`) - Typed AST with prelude syntax definitions
- **mir** (`crates/mir/`) - Mid-level IR (register-based intermediate representation)
- **codegen** (`crates/codegen/`) - Shared AArch64 code generation infrastructure
- **debuginfo** (`crates/debuginfo/`) - Debug information types and `.skydbg` serialization
- **debug** (`crates/debug/`) - Debugging support (breakpoints, stepping, inspection)
- **debugger** (`crates/debugger/`) - Interactive debugger (`skydbg` binary)
- **jit** (`crates/jit/`) - JIT compiler targeting AArch64
- **elf** (`crates/elf/`) - ELF binary generation with optional debug sidecar
- **compiler** (`crates/compiler/`) - Compiler library with driver and test runner
- **cli** (`crates/cli/`) - Command-line interface

## Building & Running

```bash
cargo build                              # debug build
cargo build --release                    # release build
cargo test                               # run tests
```

### CLI Commands

```bash
# Parse syntax files (.skyh)
cargo run -p cli -- file.skyh                    # parse and print AST (uses VM parser)
cargo run -p cli -- --parser-interp file.skyh   # use interpreted parser

# Run and test programs (.skyl)
cargo run -p cli -- test file.skyl               # run tests with JIT
cargo run -p cli -- run file.skyl                # run main() with JIT
cargo run -p cli -- compile file.skyl -o out     # compile to ELF binary

# Inspect intermediate representations
cargo run -p cli -- --emit=parse file.skyl       # print parsed syntax tree
cargo run -p cli -- --emit=ast file.skyl         # print BaseLang AST
cargo run -p cli -- --emit=mir file.skyl         # print MIR
```

## Debug Logging

Control via environment variables:

```bash
DEBUG=parser cargo run -p cli -- file.skyh     # enable parser logging
DEBUG=* cargo run -p cli -- file.skyh          # enable all loggers
DEBUG_VERBOSITY=2 DEBUG=parser ...                    # verbosity 0-3 (default 1)
```

### Adding Logging

```rust
use common::debug::create_logger;
use common::{log, log_detail, log_success, log_fail};

let log = create_logger("mymodule");

log!(log, "processing {}", name);
log_detail!(log, "details here");
log_success!(log, "completed");
log_fail!(log, "failed: {}", reason);
log.push_indent();
log.pop_indent();
```

## Testing

**Function coverage must be 100%.** Run `cargo cov` and check the `Functions` column. Every function must be exercised by meaningful tests. This is non-negotiable.

**Line coverage should be 95%+ FOR EACH FILE. Reach for 100% whenever possible.** This can be relaxed with good justification (e.g., unreachable error handling, platform-specific code), but requires explicit reasoning.

### Running Tests with Coverage

```bash
cargo cov                    # run tests with coverage report
cargo cov-html               # generate HTML coverage report
cargo cov-lcov               # generate lcov format for CI integration
```

### Finding Uncovered Functions

```bash
cargo cov-lcov
grep -B1 "^FNDA:0," lcov.info | grep "^FN:" | cut -d',' -f2 | rustfilt
```

This shows demangled names of all uncovered functions. If `rustfilt` isn't installed, omit it to see mangled names.

### Finding Uncovered Lines

```bash
cargo cov-lcov
grep -B1 "^DA:.*,0$" lcov.info | grep "^SF:" | sort -u
```

This shows files with uncovered lines. To see specific line numbers in a file:

```bash
grep -A1000 "SF:.*parser.rs" lcov.info | grep -m1 -B1000 "^end_of_record" | grep "^DA:.*,0$"
```

### Parse Tree Tests

Parse tree tests verify the parser produces correct output for various input files. Each test consists of:

- A `.skyh` input file containing syntax declarations and expressions
- A `.skyh.expected` file containing the expected s-expression output

The test runner parses each `.skyh` file and compares the formatted output against the expected file. Tests use the `datatest_stable` crate for data-driven testing. Tests are located in `crates/parser/tests/`.

**Running tests:**
```bash
cargo test                           # run all tests
cargo test run_test::arith           # run a specific test by name
cargo test -- --nocapture            # show stdout/stderr during tests
```

**Adding a new test:**
1. Create a `.skyh` file in `crates/parser/tests/parse_tree/` with syntax declarations and test expressions
2. Run `cargo run -p cli -- your_test.skyh` to see the actual output
3. Create a `.skyh.expected` file with the expected output
4. Run `cargo test` to verify

**Output format:**
- Leaf nodes: `[Category.rule text]`
- Branch nodes: `(Category.rule child1 child2 ...)`
- Internal nodes (starting with `_`) are inlined/hidden in output

### Debugging Test Failures

When a test fails, the diff shows expected vs actual output. To debug:

1. Run the parser directly on the test file to see output
2. Use `DEBUG=parser` to see parsing trace
3. Check if syntax rules are being registered correctly
4. Verify indent handling for multiline patterns

### File-Based Tests

Each crate has its own file-based tests using `datatest-stable`:

- **parser** (`crates/parser/tests/parse_tree/`) - Parser output tests (`.skyh` → `.skyh.expected`)
- **baselang** (`crates/baselang/tests/lower/`) - AST lowering tests (`.skyl` → `.skyl.expected`)
- **mir** (`crates/mir/tests/compile/`) - MIR compilation tests (`.skyl` → `.skyl.expected`)
- **jit** (`crates/jit/tests/execute/`) - JIT execution tests (`.skyl` → `.skyl.expected`)
- **elf** (`crates/elf/tests/compile/`) - ELF binary tests (`.skyl` → `.skyl.expected`)
- **debugger** (`crates/debugger/tests/debug/`) - Debugger integration tests (`.skyl` → `.skyl.expected`)
- **compiler** (`crates/compiler/tests/e2e/`) - Full pipeline integration tests (`.skyl` → `.skyl.expected`)

**Adding a new test:**
1. Create a `.skyl` (or `.skyh`) file in the appropriate test directory
2. Run the relevant component to see actual output
3. Create a `.skyl.expected` (or `.skyh.expected`) file with expected output
4. Run `cargo test -p <crate-name>` to verify

## After Completing a Task

1. `cargo build` - verify no compile errors
2. `cargo cov` - verify tests pass with 100% function coverage and 95%+ line coverage per file

## Guidelines

- If something looks strange or is hard to understand, ask for clarification

### Running Commands with Timeouts

Always run commands with appropriate timeouts to avoid getting stuck on infinite loops. Use the `timeout` command to limit execution time:

```bash
timeout 30s cargo test                    # 30 second timeout for tests
timeout 60s cargo run -p cli -- file.skyh  # 60 second timeout for parsing
timeout 10s ./target/debug/skylow file.skyh        # 10 second timeout for quick operations
```

This is especially important when:
- Running newly written or modified code that may have bugs
- Executing JIT-compiled code
- Testing parsing of potentially malformed input
- Any operation that could theoretically loop forever

If a command times out, investigate the cause (infinite loop, deadlock, etc.) before retrying.

### Git Commit Messages

- Messages should be prefixed with a lower case word indicating the change type, e.g. `fix:`, `feature:`, `refactor:`, `docs:`, `chore:`, etc...
- Use present tense for verbs (e.g., "add", "fix", "remove")
- Keep messages concise but descriptive
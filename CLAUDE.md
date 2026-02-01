# SkyLow

Rust port of the SkyHigh parser targeting a low-level language subset (no closures, manual memory management).

## Project Structure

This is a Cargo workspace with four crates:

- **skylow-common** (`crates/common/`) - Shared utilities (debug logging, string interning)
- **skylow-parser** (`crates/parser/`) - Core parser library with clean public API
- **skylow** (`crates/skylow/`) - Compiler library with driver orchestration
- **skylow-cli** (`crates/skylow-cli/`) - Command-line interface

## Building & Running

```bash
cargo build                              # debug build
cargo build --release                    # release build
cargo run -p skylow-cli -- file.skyh     # parse a file (uses VM by default)
cargo run -p skylow-cli -- --parser-interp file.skyh  # use interpreted parser
cargo test                               # run tests
```

## Debug Logging

Control via environment variables:

```bash
DEBUG=parser cargo run -p skylow-cli -- file.skyh     # enable parser logging
DEBUG=* cargo run -p skylow-cli -- file.skyh          # enable all loggers
DEBUG_VERBOSITY=2 DEBUG=parser ...                    # verbosity 0-3 (default 1)
```

### Adding Logging

```rust
use skylow_common::debug::create_logger;
use skylow_common::{log, log_detail, log_success, log_fail};

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
2. Run `cargo run -p skylow-cli -- your_test.skyh` to see the actual output
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

## After Completing a Task

1. `cargo build` - verify no compile errors
2. `cargo cov` - verify tests pass with 100% function coverage and 95%+ line coverage per file

## Guidelines

- If something looks strange or is hard to understand, ask for clarification

### Git Commit Messages

- Messages should be prefixed with a lower case word indicating the change type, e.g. `fix:`, `feature:`, `refactor:`, `docs:`, `chore:`, etc...
- Use present tense for verbs (e.g., "add", "fix", "remove")
- Keep messages concise but descriptive
# SkyLow

Rust port of the SkyHigh parser targeting a low-level language subset (no closures, manual memory management).

## Project Structure

This is a Cargo workspace with the following crates:

| Crate | Path | Description |
|-------|------|-------------|
| **common** | `crates/common/` | Shared utilities (debug logging, string interning, source tracking) |
| **parser** | `crates/parser/` | Core parser library with interpreter and bytecode VM |
| **baselang** | `crates/baselang/` | Typed AST with prelude syntax definitions |
| **mir** | `crates/mir/` | Mid-level IR (register-based intermediate representation) |
| **codegen** | `crates/codegen/` | Shared AArch64 code generation infrastructure |
| **debuginfo** | `crates/debuginfo/` | Debug information types and `.skydbg` serialization |
| **debug** | `crates/debug/` | Debugging support (breakpoints, stepping, inspection) |
| **debugger** | `crates/debugger/` | Interactive debugger (`skydbg` binary) |
| **jit** | `crates/jit/` | JIT compiler targeting AArch64 |
| **elf** | `crates/elf/` | ELF binary generation with optional debug sidecar |
| **compiler** | `crates/compiler/` | Compiler library with driver and test runner |
| **cli** | `crates/cli/` | Command-line interface |

## Building

```bash
cargo build                  # Debug build
cargo build --release        # Release build
cargo test                   # Run tests
```

## CLI Usage

### Quick Start

```bash
# Parse a syntax file
cargo run -p cli -- file.skyh

# Run tests in a program
cargo run -p cli -- test program.skyl

# Execute a program's main function
cargo run -p cli -- run program.skyl

# Compile to standalone binary
cargo run -p cli -- compile program.skyl -o output
```

Or after building, use the binary directly:

```bash
skylow file.skyh
skylow test program.skyl
skylow run program.skyl
skylow compile program.skyl -o output
```

### Commands

#### Parse (default)

Parse a `.skyh` syntax file and print the parse tree.

```bash
skylow file.skyh
skylow --parser-interp file.skyh    # Use interpreted parser (for debugging)
```

**Example input** (`arith.skyh`):
```
syntax_category Arith

syntax nat [0..9]+ : Arith
syntax add Arith "+"@10 Arith@10 : Arith
syntax mul Arith "*"@20 Arith@20 : Arith
syntax arith "[Arith|" Arith "]"

[Arith| 1 + 2 * 3 ]
```

**Output:**
```
(Expr.arith (Arith.add [Arith.nat 1] (Arith.mul [Arith.nat 2] [Arith.nat 3])))
```

#### Test

Run assertion-based tests in a `.skyl` file using JIT compilation.

```bash
skylow test program.skyl
```

**Example input** (`tests.skyl`):
```
test arithmetic:
  assert(2 + 2 == 4)
  assert(3 * 4 == 12)
  assert(10 - 3 == 7)

test comparisons:
  assert(5 > 3)
  assert(2 < 10)
```

**Output:**
```
PASS: arithmetic
PASS: comparisons

2 passed, 0 failed
```

Exit code is `1` if any tests fail, `0` if all pass.

#### Run

Execute a program's `main()` function using JIT compilation.

```bash
skylow run program.skyl
```

**Example:**
```
fn main():
  assert(2 + 2 == 4)
  assert(10 - 3 == 7)
```

The exit code is propagated from the executed program.

#### Compile

Compile a `.skyl` file to a standalone ELF binary (AArch64).

```bash
skylow compile program.skyl -o output
```

**Example:**
```bash
$ skylow compile hello.skyl -o hello
Compiled to hello

$ ./hello
$ echo $?
0
```

### Emit Options

Inspect intermediate representations at different compilation stages.

```bash
skylow --emit=parse program.skyl   # Print parsed syntax tree
skylow --emit=ast program.skyl     # Print BaseLang AST
skylow --emit=mir program.skyl     # Print MIR
```

**AST output example:**
```
Test "simple equality"
  Assert
    Cmp(Eq)
      Int(1)
      Int(1)
```

**MIR output example:**
```
fn "simple":
  r0 = load 1
  r1 = load 1
  r2 = eq r0, r1
  assert r2, #0
  ret
```

### Options Reference

| Option | Description |
|--------|-------------|
| `--parser-interp` | Use interpreted parser instead of VM |
| `--emit=parse` | Print parsed syntax tree |
| `--emit=ast` | Print BaseLang AST |
| `--emit=mir` | Print MIR |
| `-o <file>` | Output file (required for `compile` command) |
| `--help`, `-h` | Show help message |

### File Types

| Extension | Description |
|-----------|-------------|
| `.skyh` | Syntax definition files (used by parse command) |
| `.skyl` | SkyLow program files (used by test, run, compile) |

## Debugger

SkyLow includes `skydbg`, a GDB-like debugger for debugging compiled binaries.

### Quick Start

```bash
# Build a binary with debug info
cargo run -p cli -- compile program.skyl -o program

# Start the debugger
cargo run -p debugger -- program

# Or after building
skydbg program
```

### Commands

| Command | Description |
|---------|-------------|
| `break <file>:<line>` | Set breakpoint at source location |
| `break <function>` | Set breakpoint at function entry |
| `delete <n>` | Delete breakpoint |
| `run [args...]` | Start program execution |
| `continue`, `c` | Continue execution |
| `step`, `s` | Step one source line |
| `stepi`, `si` | Step one instruction |
| `next`, `n` | Step over function calls |
| `finish` | Run until current function returns |
| `print <expr>`, `p` | Print expression value |
| `backtrace`, `bt` | Print stack trace |
| `info locals` | Print local variables |
| `info registers` | Print register values |
| `info breakpoints` | List breakpoints |
| `list [file:line]` | Show source context |
| `quit`, `q` | Exit debugger |

### Script Mode

Run debugger commands from a script file:

```bash
skydbg --script test.dbg program
```

Scripts support assertions for automated testing:

```
break main
run
expect stop at program.skyl:5
step
assert x == 42
continue
quit
```

## Debug Logging

Control debug output via environment variables:

```bash
DEBUG=parser skylow file.skyh              # Enable parser logging
DEBUG=* skylow file.skyh                   # Enable all loggers
DEBUG_VERBOSITY=2 DEBUG=parser skylow ...  # Verbosity 0-3 (default 1)
```

## Example Workflow

```bash
# 1. Write a program
cat > program.skyl << 'EOF'
test addition:
  assert(1 + 1 == 2)
  assert(2 + 3 == 5)

fn main():
  assert(10 + 20 == 30)
EOF

# 2. Inspect the AST
skylow --emit=ast program.skyl

# 3. Inspect the MIR
skylow --emit=mir program.skyl

# 4. Run tests
skylow test program.skyl

# 5. Run main
skylow run program.skyl

# 6. Compile to binary
skylow compile program.skyl -o program
./program
```

## License

See [LICENSE](LICENSE) for details.

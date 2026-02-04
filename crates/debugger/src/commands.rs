//! Command parsing for the debugger

/// A parsed debugger command
#[derive(Debug, Clone, PartialEq)]
pub enum Command {
    /// Set a breakpoint
    Break(BreakLocation),
    /// Delete a breakpoint by number
    Delete(u32),
    /// Run the program
    Run(Vec<String>),
    /// Continue execution
    Continue,
    /// Step one source line (step into)
    Step,
    /// Step one instruction
    StepInstruction,
    /// Step over function calls
    Next,
    /// Run until current function returns
    Finish,
    /// Print an expression
    Print(String),
    /// Show backtrace
    Backtrace,
    /// Show info (locals, registers, breakpoints)
    Info(InfoKind),
    /// List source code
    List(Option<SourceLocation>),
    /// Quit the debugger
    Quit,
    /// Assert a condition (for scripts)
    Assert(String),
    /// Expect to stop at a location (for scripts)
    ExpectStop(SourceLocation),
    /// Empty line (repeat last command)
    Empty,
    /// Unknown command
    Unknown(String),
}

/// Breakpoint location specification
#[derive(Debug, Clone, PartialEq)]
pub enum BreakLocation {
    /// Source file and line
    Source { file: String, line: u32 },
    /// Function name
    Function(String),
    /// Address
    Address(u64),
}

/// Source location
#[derive(Debug, Clone, PartialEq)]
pub struct SourceLocation {
    pub file: String,
    pub line: u32,
}

/// Kind of info to display
#[derive(Debug, Clone, PartialEq)]
pub enum InfoKind {
    Locals,
    Registers,
    Breakpoints,
}

/// Parse a command line into a Command
pub fn parse(line: &str) -> Command {
    let line = line.trim();

    // Handle comments
    if line.starts_with('#') || line.is_empty() {
        return Command::Empty;
    }

    let mut parts = line.splitn(2, char::is_whitespace);
    let cmd = parts.next().unwrap_or("");
    let args = parts.next().unwrap_or("").trim();

    match cmd {
        "break" | "b" => parse_break(args),
        "delete" | "d" => parse_delete(args),
        "run" | "r" => parse_run(args),
        "continue" | "c" => Command::Continue,
        "step" | "s" => Command::Step,
        "stepi" | "si" => Command::StepInstruction,
        "next" | "n" => Command::Next,
        "finish" => Command::Finish,
        "print" | "p" => Command::Print(args.to_string()),
        "backtrace" | "bt" => Command::Backtrace,
        "info" | "i" => parse_info(args),
        "list" | "l" => parse_list(args),
        "quit" | "q" => Command::Quit,
        "assert" => Command::Assert(args.to_string()),
        "expect" => parse_expect(args),
        "" => Command::Empty,
        _ => Command::Unknown(cmd.to_string()),
    }
}

fn parse_break(args: &str) -> Command {
    if args.is_empty() {
        return Command::Unknown("break requires an argument".to_string());
    }

    // Check for address (0x...)
    if args.starts_with("0x") || args.starts_with("0X") {
        if let Ok(addr) = u64::from_str_radix(&args[2..], 16) {
            return Command::Break(BreakLocation::Address(addr));
        }
    }

    // Check for file:line
    if let Some(colon_pos) = args.rfind(':') {
        let file = &args[..colon_pos];
        let line_str = &args[colon_pos + 1..];
        if let Ok(line) = line_str.parse::<u32>() {
            return Command::Break(BreakLocation::Source {
                file: file.to_string(),
                line,
            });
        }
    }

    // Otherwise treat as function name
    Command::Break(BreakLocation::Function(args.to_string()))
}

fn parse_delete(args: &str) -> Command {
    match args.parse::<u32>() {
        Ok(n) => Command::Delete(n),
        Err(_) => Command::Unknown("delete requires a breakpoint number".to_string()),
    }
}

fn parse_run(args: &str) -> Command {
    let args: Vec<String> = if args.is_empty() {
        Vec::new()
    } else {
        args.split_whitespace().map(|s| s.to_string()).collect()
    };
    Command::Run(args)
}

fn parse_info(args: &str) -> Command {
    match args {
        "locals" | "local" | "loc" => Command::Info(InfoKind::Locals),
        "registers" | "regs" | "reg" => Command::Info(InfoKind::Registers),
        "breakpoints" | "break" | "b" => Command::Info(InfoKind::Breakpoints),
        _ => Command::Unknown(format!("unknown info command: {}", args)),
    }
}

fn parse_list(args: &str) -> Command {
    if args.is_empty() {
        return Command::List(None);
    }

    if let Some(colon_pos) = args.rfind(':') {
        let file = &args[..colon_pos];
        let line_str = &args[colon_pos + 1..];
        if let Ok(line) = line_str.parse::<u32>() {
            return Command::List(Some(SourceLocation {
                file: file.to_string(),
                line,
            }));
        }
    }

    Command::Unknown(format!("invalid list argument: {}", args))
}

fn parse_expect(args: &str) -> Command {
    // expect stop at <file>:<line>
    let args = args.trim();
    if !args.starts_with("stop at ") {
        return Command::Unknown("expected 'expect stop at <file>:<line>'".to_string());
    }

    let location = &args[8..]; // Skip "stop at "
    if let Some(colon_pos) = location.rfind(':') {
        let file = &location[..colon_pos];
        let line_str = &location[colon_pos + 1..];
        if let Ok(line) = line_str.parse::<u32>() {
            return Command::ExpectStop(SourceLocation {
                file: file.to_string(),
                line,
            });
        }
    }

    Command::Unknown(format!("invalid expect location: {}", location))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_break_source() {
        assert_eq!(
            parse("break main.skyl:10"),
            Command::Break(BreakLocation::Source {
                file: "main.skyl".to_string(),
                line: 10
            })
        );
    }

    #[test]
    fn test_parse_break_function() {
        assert_eq!(
            parse("break main"),
            Command::Break(BreakLocation::Function("main".to_string()))
        );
    }

    #[test]
    fn test_parse_break_address() {
        assert_eq!(
            parse("break 0x1000"),
            Command::Break(BreakLocation::Address(0x1000))
        );
    }

    #[test]
    fn test_parse_delete() {
        assert_eq!(parse("delete 1"), Command::Delete(1));
    }

    #[test]
    fn test_parse_run() {
        assert_eq!(parse("run"), Command::Run(vec![]));
        assert_eq!(
            parse("run arg1 arg2"),
            Command::Run(vec!["arg1".to_string(), "arg2".to_string()])
        );
    }

    #[test]
    fn test_parse_continue() {
        assert_eq!(parse("continue"), Command::Continue);
        assert_eq!(parse("c"), Command::Continue);
    }

    #[test]
    fn test_parse_step() {
        assert_eq!(parse("step"), Command::Step);
        assert_eq!(parse("s"), Command::Step);
        assert_eq!(parse("stepi"), Command::StepInstruction);
        assert_eq!(parse("si"), Command::StepInstruction);
    }

    #[test]
    fn test_parse_next() {
        assert_eq!(parse("next"), Command::Next);
        assert_eq!(parse("n"), Command::Next);
    }

    #[test]
    fn test_parse_finish() {
        assert_eq!(parse("finish"), Command::Finish);
    }

    #[test]
    fn test_parse_print() {
        assert_eq!(parse("print x"), Command::Print("x".to_string()));
        assert_eq!(parse("p x + 1"), Command::Print("x + 1".to_string()));
    }

    #[test]
    fn test_parse_backtrace() {
        assert_eq!(parse("backtrace"), Command::Backtrace);
        assert_eq!(parse("bt"), Command::Backtrace);
    }

    #[test]
    fn test_parse_info() {
        assert_eq!(parse("info locals"), Command::Info(InfoKind::Locals));
        assert_eq!(parse("info registers"), Command::Info(InfoKind::Registers));
        assert_eq!(parse("info breakpoints"), Command::Info(InfoKind::Breakpoints));
        assert_eq!(parse("i loc"), Command::Info(InfoKind::Locals));
        assert_eq!(parse("i regs"), Command::Info(InfoKind::Registers));
    }

    #[test]
    fn test_parse_list() {
        assert_eq!(parse("list"), Command::List(None));
        assert_eq!(
            parse("list main.skyl:10"),
            Command::List(Some(SourceLocation {
                file: "main.skyl".to_string(),
                line: 10
            }))
        );
    }

    #[test]
    fn test_parse_quit() {
        assert_eq!(parse("quit"), Command::Quit);
        assert_eq!(parse("q"), Command::Quit);
    }

    #[test]
    fn test_parse_assert() {
        assert_eq!(parse("assert x == 42"), Command::Assert("x == 42".to_string()));
    }

    #[test]
    fn test_parse_expect() {
        assert_eq!(
            parse("expect stop at main.skyl:15"),
            Command::ExpectStop(SourceLocation {
                file: "main.skyl".to_string(),
                line: 15
            })
        );
    }

    #[test]
    fn test_parse_empty() {
        assert_eq!(parse(""), Command::Empty);
        assert_eq!(parse("# comment"), Command::Empty);
    }

    #[test]
    fn test_parse_unknown() {
        match parse("foobar") {
            Command::Unknown(s) => assert_eq!(s, "foobar"),
            _ => panic!("expected Unknown"),
        }
    }

    #[test]
    fn test_parse_short_aliases() {
        assert_eq!(parse("b main"), Command::Break(BreakLocation::Function("main".to_string())));
        assert_eq!(parse("d 1"), Command::Delete(1));
        assert_eq!(parse("r"), Command::Run(vec![]));
        assert_eq!(parse("l"), Command::List(None));
    }
}

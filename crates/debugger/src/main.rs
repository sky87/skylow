//! SkyDbg - Debugger CLI for SkyLow binaries
//!
//! A GDB-like debugger that can debug ELF binaries with SkyDbg debug information.
//!
//! Usage:
//!   skydbg <binary>              # Start REPL
//!   skydbg --script <script> <binary>  # Run script

mod commands;
mod repl;
mod script;
mod target;

use std::env;
use std::path::PathBuf;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: skydbg [--script <script>] <binary> [args...]");
        process::exit(1);
    }

    let mut script_path: Option<PathBuf> = None;
    let mut binary_path: Option<PathBuf> = None;
    let mut binary_args: Vec<String> = Vec::new();

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "--script" | "-s" => {
                if i + 1 >= args.len() {
                    eprintln!("Error: --script requires an argument");
                    process::exit(1);
                }
                script_path = Some(PathBuf::from(&args[i + 1]));
                i += 2;
            }
            "--help" | "-h" => {
                print_help();
                process::exit(0);
            }
            arg if arg.starts_with('-') => {
                eprintln!("Unknown option: {}", arg);
                process::exit(1);
            }
            _ => {
                if binary_path.is_none() {
                    binary_path = Some(PathBuf::from(&args[i]));
                } else {
                    binary_args.push(args[i].clone());
                }
                i += 1;
            }
        }
    }

    let binary_path = match binary_path {
        Some(p) => p,
        None => {
            eprintln!("Error: no binary specified");
            process::exit(1);
        }
    };

    if !binary_path.exists() {
        eprintln!("Error: binary not found: {}", binary_path.display());
        process::exit(1);
    }

    // Create debugger session
    let mut session = match repl::Session::new(binary_path.clone(), binary_args) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error creating session: {}", e);
            process::exit(1);
        }
    };

    // Run script or start REPL
    if let Some(script) = script_path {
        match script::run_script(&mut session, &script) {
            Ok(()) => process::exit(0),
            Err(e) => {
                eprintln!("Script error: {}", e);
                process::exit(1);
            }
        }
    } else {
        repl::run(&mut session);
    }
}

fn print_help() {
    println!(
        r#"skydbg - SkyLow Debugger

Usage: skydbg [options] <binary> [args...]

Options:
  -s, --script <file>  Run commands from script file
  -h, --help           Show this help message

Commands (in REPL or script):
  break <file>:<line>  Set breakpoint at source location
  break <function>     Set breakpoint at function entry
  delete <n>           Delete breakpoint
  run [args...]        Start program execution
  continue, c          Continue execution
  step, s              Step one source line
  stepi, si            Step one instruction
  next, n              Step over function calls
  finish               Run until current function returns
  print <expr>, p      Print expression value
  backtrace, bt        Print stack trace
  info locals          Print local variables
  info registers       Print register values
  info breakpoints     List breakpoints
  list [file:line]     Show source context
  quit, q              Exit debugger

Script assertions:
  assert <expr>                Check condition
  expect stop at <file>:<line> Assert stop location
"#
    );
}

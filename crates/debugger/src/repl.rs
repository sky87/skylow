//! REPL (Read-Eval-Print-Loop) for the debugger

use debug::commands;
use debug::Session;
use debug::Target;
use std::io::{self, BufRead, Write};

/// Run the REPL
pub fn run<T: Target>(session: &mut Session<T>) {
    println!("SkyDbg debugger");
    println!("Type 'help' for commands, 'quit' to exit.");
    println!();

    if session.debug_info().is_some() {
        println!("Debug info loaded.");
    } else {
        println!("Warning: No debug info available.");
    }
    println!();

    let stdin = io::stdin();
    let mut stdout = io::stdout();

    loop {
        print!("(skydbg) ");
        stdout.flush().unwrap();

        let mut line = String::new();
        match stdin.lock().read_line(&mut line) {
            Ok(0) => {
                println!();
                break;
            }
            Ok(_) => {}
            Err(e) => {
                eprintln!("Error reading input: {}", e);
                break;
            }
        }

        let cmd = commands::parse(&line);
        match session.execute(cmd) {
            Ok(true) => break, // Quit
            Ok(false) => {}
            Err(e) => eprintln!("Error: {}", e),
        }
    }

    println!("Goodbye.");
}

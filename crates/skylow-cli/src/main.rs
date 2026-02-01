use bumpalo::Bump;
use std::env;
use std::fs;
use std::process;

use skylow::{format_node, Driver, TestRunner};

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() > 1 && args[1] == "test" {
        run_test_command(&args[2..]);
    } else {
        run_parse_command(&args[1..]);
    }
}

fn run_test_command(args: &[String]) {
    let file_path = match args.first() {
        Some(p) => p,
        None => {
            eprintln!("Usage: skylow test <file.skyl>");
            process::exit(1);
        }
    };

    let source = match fs::read_to_string(file_path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading {}: {}", file_path, e);
            process::exit(1);
        }
    };

    let runner = TestRunner::new();
    let result = runner.run(&source);

    // Print parse errors
    for err in &result.parse_errors {
        eprintln!("{}:{}", file_path, err);
    }

    // Print lowering errors
    for err in &result.lower_errors {
        eprintln!("{}: {}", file_path, err);
    }

    // Print test results
    for test_result in &result.results {
        if test_result.passed {
            println!("PASS: {}", test_result.name);
        } else {
            println!(
                "FAIL: {} - {}",
                test_result.name,
                test_result.failure_message.as_deref().unwrap_or("unknown error")
            );
        }
    }

    // Print summary
    let passed = result.passed();
    let failed = result.failed();
    println!("{} passed, {} failed", passed, failed);

    if !result.success() {
        process::exit(1);
    }
}

fn run_parse_command(args: &[String]) {
    let mut use_interpreter = false;
    let mut file_path: Option<&str> = None;

    for arg in args {
        match arg.as_str() {
            "--parser-interp" => use_interpreter = true,
            arg if !arg.starts_with('-') => {
                if file_path.is_none() {
                    file_path = Some(arg);
                }
            }
            _ => {
                eprintln!("Unknown option: {}", arg);
                process::exit(1);
            }
        }
    }

    let path = match file_path {
        Some(p) => p,
        None => {
            eprintln!("Usage: skylow [--parser-interp] <file.skyh>");
            eprintln!("       skylow test <file.skyl>");
            eprintln!("Options:");
            eprintln!("  --parser-interp  Use the interpreted parser instead of the VM");
            process::exit(1);
        }
    };

    let source = match fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading {}: {}", path, e);
            process::exit(1);
        }
    };

    let arena = Bump::new();
    let driver = if use_interpreter {
        Driver::with_interpreter(&arena)
    } else {
        Driver::new(&arena)
    };
    let result = driver.process(&source);

    if !result.errors.is_empty() {
        for err in &result.errors {
            eprintln!("{}:{}:{}: {}", path, err.loc.line, err.loc.col, err.msg);
            eprintln!("  {}", err.source_line);
            let spaces = err.loc.col.saturating_sub(1) as usize;
            eprintln!("  {}^", " ".repeat(spaces));
        }
    }

    for node in &result.nodes {
        print!("{}", format_node(node, 0));
    }

    if !result.errors.is_empty() {
        process::exit(1);
    }
}

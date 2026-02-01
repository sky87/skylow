use bumpalo::Bump;
use std::env;
use std::fs;
use std::process;

use skylow::{format_node, Driver};

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut use_interpreter = false;
    let mut file_path: Option<&str> = None;

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "--parser-interp" => use_interpreter = true,
            arg if !arg.starts_with('-') => {
                if file_path.is_none() {
                    file_path = Some(arg);
                }
            }
            _ => {
                eprintln!("Unknown option: {}", args[i]);
                process::exit(1);
            }
        }
        i += 1;
    }

    let path = match file_path {
        Some(p) => p,
        None => {
            eprintln!("Usage: {} [--parser-interp] <file.skyh>", args[0]);
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

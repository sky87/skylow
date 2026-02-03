use bumpalo::Bump;
use std::env;
use std::fs;
use std::io::Write;
use std::process;

use compiler::{format_node, Compiler, Driver, MainRunner, TestRunner};

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        print_usage();
        process::exit(1);
    }

    match args[1].as_str() {
        "test" => run_test_command(&args[2..]),
        "compile" => run_compile_command(&args[2..]),
        "run" => run_run_command(&args[2..]),
        "--help" | "-h" => print_usage(),
        "--parser-interp" => run_parse_command(&args[1..]),
        arg if arg.starts_with("--emit=") => {
            let emit_type = &arg[7..];
            run_emit_command(emit_type, &args[2..]);
        }
        arg if arg.starts_with('-') => {
            eprintln!("Unknown option: {}", arg);
            print_usage();
            process::exit(1);
        }
        _ => run_parse_command(&args[1..]),
    }
}

fn print_usage() {
    eprintln!("Usage: skylow <command> [options] <file>");
    eprintln!();
    eprintln!("Commands:");
    eprintln!("  <file.skyh>              Parse syntax file and print AST");
    eprintln!("  test <file.skyl>         Run tests with JIT compilation");
    eprintln!("  compile <file.skyl> -o <out>  Compile to ELF binary");
    eprintln!("  run <file.skyl>          Run main() with JIT compilation");
    eprintln!();
    eprintln!("Options:");
    eprintln!("  --parser-interp          Use the interpreted parser instead of VM");
    eprintln!("  --emit=parse <file>      Print parsed syntax tree");
    eprintln!("  --emit=ast <file>        Print BaseLang AST");
    eprintln!("  --emit=mir <file>        Print MIR");
    eprintln!("  -o <file>                Output file (for compile command)");
    eprintln!("  --help, -h               Show this help message");
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

fn run_compile_command(args: &[String]) {
    let mut file_path: Option<&str> = None;
    let mut output_path: Option<&str> = None;
    let mut i = 0;

    while i < args.len() {
        match args[i].as_str() {
            "-o" => {
                if i + 1 < args.len() {
                    output_path = Some(&args[i + 1]);
                    i += 2;
                } else {
                    eprintln!("Error: -o requires an output file");
                    process::exit(1);
                }
            }
            arg if !arg.starts_with('-') => {
                if file_path.is_none() {
                    file_path = Some(arg);
                }
                i += 1;
            }
            _ => {
                eprintln!("Unknown option: {}", args[i]);
                process::exit(1);
            }
        }
    }

    let input_path = match file_path {
        Some(p) => p,
        None => {
            eprintln!("Usage: skylow compile <file.skyl> -o <output>");
            process::exit(1);
        }
    };

    let out_path = match output_path {
        Some(p) => p,
        None => {
            eprintln!("Error: output file required (-o <file>)");
            process::exit(1);
        }
    };

    let source = match fs::read_to_string(input_path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading {}: {}", input_path, e);
            process::exit(1);
        }
    };

    let compiler = Compiler::new();
    let filename = std::path::Path::new(input_path)
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or(input_path);

    match compiler.compile(&source, filename) {
        Ok(elf) => {
            let mut file = match fs::File::create(out_path) {
                Ok(f) => f,
                Err(e) => {
                    eprintln!("Error creating {}: {}", out_path, e);
                    process::exit(1);
                }
            };
            if let Err(e) = file.write_all(&elf) {
                eprintln!("Error writing {}: {}", out_path, e);
                process::exit(1);
            }
            // Make executable on Unix
            #[cfg(unix)]
            {
                use std::os::unix::fs::PermissionsExt;
                let mut perms = fs::metadata(out_path).unwrap().permissions();
                perms.set_mode(0o755);
                fs::set_permissions(out_path, perms).unwrap();
            }
            println!("Compiled to {}", out_path);
        }
        Err(e) => {
            eprintln!("{}: {}", input_path, e);
            process::exit(1);
        }
    }
}

fn run_run_command(args: &[String]) {
    let file_path = match args.first() {
        Some(p) => p,
        None => {
            eprintln!("Usage: skylow run <file.skyl>");
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

    let runner = MainRunner::new();
    let result = runner.run(&source);

    // Print parse errors
    for err in &result.parse_errors {
        eprintln!("{}:{}", file_path, err);
    }

    // Print lowering errors
    for err in &result.lower_errors {
        eprintln!("{}: {}", file_path, err);
    }

    // Print failure message
    if let Some(msg) = &result.failure_message {
        eprintln!("{}: {}", file_path, msg);
    }

    process::exit(result.exit_code as i32);
}

fn run_emit_command(emit_type: &str, args: &[String]) {
    let file_path = match args.first() {
        Some(p) => p,
        None => {
            eprintln!("Usage: skylow --emit={} <file.skyl>", emit_type);
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

    match emit_type {
        "parse" => emit_parse(&source, file_path),
        "ast" => emit_ast(&source, file_path),
        "mir" => emit_mir(&source, file_path),
        _ => {
            eprintln!("Unknown emit type: {}", emit_type);
            eprintln!("Valid types: parse, ast, mir");
            process::exit(1);
        }
    }
}

fn emit_parse(source: &str, file_path: &str) {
    use baselang::parse_with_prelude;

    let arena = Bump::new();
    let result = parse_with_prelude(&arena, source);

    if !result.errors.is_empty() {
        for err in &result.errors {
            eprintln!("{}:{}:{}: {}", file_path, err.loc.line, err.loc.col, err.msg);
        }
        process::exit(1);
    }

    for node in &result.nodes {
        print!("{}", format_node(node, 0));
    }
}

fn emit_ast(source: &str, file_path: &str) {
    use baselang::{lower_program, parse_with_prelude};

    let arena = Bump::new();
    let result = parse_with_prelude(&arena, source);

    if !result.errors.is_empty() {
        for err in &result.errors {
            eprintln!("{}:{}:{}: {}", file_path, err.loc.line, err.loc.col, err.msg);
        }
        process::exit(1);
    }

    let program = match lower_program(&arena, &result.nodes) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("{}: {}", file_path, e);
            process::exit(1);
        }
    };

    // Print AST in debug format
    println!("{:#?}", program);
}

fn emit_mir(source: &str, file_path: &str) {
    use baselang::{lower_program, parse_with_prelude};
    use mir::lower_program as lower_to_mir;

    let arena = Bump::new();
    let result = parse_with_prelude(&arena, source);

    if !result.errors.is_empty() {
        for err in &result.errors {
            eprintln!("{}:{}:{}: {}", file_path, err.loc.line, err.loc.col, err.msg);
        }
        process::exit(1);
    }

    let program = match lower_program(&arena, &result.nodes) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("{}: {}", file_path, e);
            process::exit(1);
        }
    };

    let mir = lower_to_mir(&program);

    // Print MIR in debug format
    println!("{:#?}", mir);
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
            print_usage();
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

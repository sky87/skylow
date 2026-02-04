//! Script execution for the debugger

use crate::commands;
use crate::repl::Session;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

/// Run a debugger script file
pub fn run_script(session: &mut Session, script_path: &Path) -> Result<(), String> {
    let file = File::open(script_path)
        .map_err(|e| format!("Failed to open script {}: {}", script_path.display(), e))?;

    let reader = BufReader::new(file);

    for (line_num, line) in reader.lines().enumerate() {
        let line = line.map_err(|e| format!("Error reading line {}: {}", line_num + 1, e))?;

        // Skip empty lines and comments
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with('#') {
            continue;
        }

        // Echo the command
        println!("> {}", trimmed);

        // Parse and execute
        let cmd = commands::parse(trimmed);
        match session.execute(cmd) {
            Ok(true) => {
                // Quit command
                return Ok(());
            }
            Ok(false) => {}
            Err(e) => {
                return Err(format!("Line {}: {}", line_num + 1, e));
            }
        }
    }

    Ok(())
}

/// Run commands from a string (for testing)
#[cfg(test)]
fn run_commands(session: &mut Session, commands: &str) -> Result<(), String> {
    for (line_num, line) in commands.lines().enumerate() {
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with('#') {
            continue;
        }

        let cmd = commands::parse(trimmed);
        match session.execute(cmd) {
            Ok(true) => return Ok(()),
            Ok(false) => {}
            Err(e) => {
                return Err(format!("Line {}: {}", line_num + 1, e));
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn test_run_script_nonexistent() {
        let mut session = Session::new(PathBuf::from("/bin/true"), vec![]).unwrap();
        let result = run_script(&mut session, Path::new("/nonexistent.dbg"));
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Failed to open"));
    }

    #[test]
    fn test_run_commands_empty() {
        let mut session = Session::new(PathBuf::from("/bin/true"), vec![]).unwrap();
        let result = run_commands(&mut session, "");
        assert!(result.is_ok());
    }

    #[test]
    fn test_run_commands_comments() {
        let mut session = Session::new(PathBuf::from("/bin/true"), vec![]).unwrap();
        let result = run_commands(&mut session, "# this is a comment\n\n# another");
        assert!(result.is_ok());
    }

    #[test]
    fn test_run_commands_quit() {
        let mut session = Session::new(PathBuf::from("/bin/true"), vec![]).unwrap();
        let result = run_commands(&mut session, "quit");
        assert!(result.is_ok());
    }
}

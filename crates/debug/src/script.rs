//! Script execution for the debugger

use crate::commands;
use crate::session::Session;
use crate::target::Target;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

/// Run a debugger script file
pub fn run_script<T: Target>(session: &mut Session<T>, script_path: &Path) -> Result<(), String> {
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
pub fn run_commands<T: Target>(session: &mut Session<T>, commands_str: &str) -> Result<(), String> {
    for (line_num, line) in commands_str.lines().enumerate() {
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
    use crate::session::tests::MockTarget;

    #[test]
    fn test_run_script_nonexistent() {
        let target = MockTarget::new();
        let mut session = Session::new(target, None, 0x400078, "test".to_string());
        let result = run_script(&mut session, Path::new("/nonexistent.dbg"));
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Failed to open"));
    }

    #[test]
    fn test_run_commands_empty() {
        let target = MockTarget::new();
        let mut session = Session::new(target, None, 0x400078, "test".to_string());
        let result = run_commands(&mut session, "");
        assert!(result.is_ok());
    }

    #[test]
    fn test_run_commands_comments() {
        let target = MockTarget::new();
        let mut session = Session::new(target, None, 0x400078, "test".to_string());
        let result = run_commands(&mut session, "# this is a comment\n\n# another");
        assert!(result.is_ok());
    }

    #[test]
    fn test_run_commands_quit() {
        let target = MockTarget::new();
        let mut session = Session::new(target, None, 0x400078, "test".to_string());
        let result = run_commands(&mut session, "quit");
        assert!(result.is_ok());
    }

    #[test]
    fn test_run_commands_error() {
        let target = MockTarget::new();
        let mut session = Session::new(target, None, 0x400078, "test".to_string());
        // step before run should error
        let result = run_commands(&mut session, "step");
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Line 1:"));
    }

    #[test]
    fn test_run_script_from_file_with_quit() {
        use std::io::Write;
        let dir = std::env::temp_dir().join("skydbg_script_test");
        std::fs::create_dir_all(&dir).unwrap();
        let script_path = dir.join("test_quit.dbg");
        {
            let mut f = std::fs::File::create(&script_path).unwrap();
            writeln!(f, "# comment").unwrap();
            writeln!(f).unwrap();
            writeln!(f, "info breakpoints").unwrap();
            writeln!(f, "quit").unwrap();
        }

        let target = MockTarget::new();
        let mut session = Session::new(target, None, 0x400078, "test".to_string());
        let result = run_script(&mut session, &script_path);
        assert!(result.is_ok());
        assert!(session.get_output().iter().any(|l| l.contains("No breakpoints")));

        let _ = std::fs::remove_file(&script_path);
    }

    #[test]
    fn test_run_script_from_file_no_quit() {
        use std::io::Write;
        let dir = std::env::temp_dir().join("skydbg_script_test");
        std::fs::create_dir_all(&dir).unwrap();
        let script_path = dir.join("test_noquit.dbg");
        {
            let mut f = std::fs::File::create(&script_path).unwrap();
            writeln!(f, "info breakpoints").unwrap();
        }

        let target = MockTarget::new();
        let mut session = Session::new(target, None, 0x400078, "test".to_string());
        let result = run_script(&mut session, &script_path);
        assert!(result.is_ok());
        assert!(session.get_output().iter().any(|l| l.contains("No breakpoints")));

        let _ = std::fs::remove_file(&script_path);
    }

    #[test]
    fn test_run_script_with_error() {
        use std::io::Write;
        let dir = std::env::temp_dir().join("skydbg_script_test");
        std::fs::create_dir_all(&dir).unwrap();
        let script_path = dir.join("test_err.dbg");
        {
            let mut f = std::fs::File::create(&script_path).unwrap();
            writeln!(f, "step").unwrap(); // step before run = error
        }

        let target = MockTarget::new();
        let mut session = Session::new(target, None, 0x400078, "test".to_string());
        let result = run_script(&mut session, &script_path);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Line 1:"));

        let _ = std::fs::remove_file(&script_path);
    }
}

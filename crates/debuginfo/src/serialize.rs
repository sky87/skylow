//! Serialization of debug information to the SkyDbg binary format

use crate::types::{DebugInfo, FunctionLocals, LineMapping, SourceFile, Symbol, SymbolKind};
use std::io::{self, Write};

/// Magic bytes for SkyDbg format: "SKDB"
pub const MAGIC: &[u8; 4] = b"SKDB";

/// Current format version
pub const VERSION: u16 = 1;

/// Section type identifiers
pub mod section_type {
    pub const SOURCE: u8 = 0x01;
    pub const SYMBOL: u8 = 0x02;
    pub const LINE: u8 = 0x03;
    pub const LOCALS: u8 = 0x04;
}

/// Write debug info to the SkyDbg binary format
pub fn write_skydbg<W: Write>(debug_info: &DebugInfo, writer: &mut W) -> io::Result<()> {
    // Header
    writer.write_all(MAGIC)?;
    writer.write_all(&VERSION.to_le_bytes())?;
    writer.write_all(&0u16.to_le_bytes())?; // Flags (reserved)

    // Count non-empty sections
    let mut section_count = 0u32;
    if !debug_info.sources.is_empty() {
        section_count += 1;
    }
    if !debug_info.symbols.is_empty() {
        section_count += 1;
    }
    if !debug_info.line_map.is_empty() {
        section_count += 1;
    }
    if !debug_info.locals.is_empty() {
        section_count += 1;
    }
    writer.write_all(&section_count.to_le_bytes())?;

    // Write sections
    if !debug_info.sources.is_empty() {
        write_source_section(writer, &debug_info.sources)?;
    }
    if !debug_info.symbols.is_empty() {
        write_symbol_section(writer, &debug_info.symbols)?;
    }
    if !debug_info.line_map.is_empty() {
        write_line_section(writer, &debug_info.line_map)?;
    }
    if !debug_info.locals.is_empty() {
        write_locals_section(writer, &debug_info.locals)?;
    }

    Ok(())
}

fn write_source_section<W: Write>(writer: &mut W, sources: &[SourceFile]) -> io::Result<()> {
    // Section header
    writer.write_all(&[section_type::SOURCE])?;

    // Calculate section size
    let mut section_data = Vec::new();
    section_data.extend_from_slice(&(sources.len() as u32).to_le_bytes());
    for source in sources {
        section_data.extend_from_slice(&source.id.to_le_bytes());
        let path_bytes = source.path.as_bytes();
        section_data.extend_from_slice(&(path_bytes.len() as u32).to_le_bytes());
        section_data.extend_from_slice(path_bytes);
        // Write text presence flag and optional text
        if let Some(text) = &source.text {
            section_data.push(1);
            let text_bytes = text.as_bytes();
            section_data.extend_from_slice(&(text_bytes.len() as u32).to_le_bytes());
            section_data.extend_from_slice(text_bytes);
        } else {
            section_data.push(0);
        }
    }

    writer.write_all(&(section_data.len() as u32).to_le_bytes())?;
    writer.write_all(&section_data)?;
    Ok(())
}

fn write_symbol_section<W: Write>(writer: &mut W, symbols: &[Symbol]) -> io::Result<()> {
    // Section header
    writer.write_all(&[section_type::SYMBOL])?;

    // Calculate section size
    let mut section_data = Vec::new();
    section_data.extend_from_slice(&(symbols.len() as u32).to_le_bytes());
    for symbol in symbols {
        let name_bytes = symbol.name.as_bytes();
        section_data.extend_from_slice(&(name_bytes.len() as u32).to_le_bytes());
        section_data.extend_from_slice(name_bytes);
        section_data.extend_from_slice(&symbol.source_id.to_le_bytes());
        section_data.extend_from_slice(&symbol.line.to_le_bytes());
        section_data.extend_from_slice(&symbol.col.to_le_bytes());
        section_data.extend_from_slice(&symbol.code_offset.to_le_bytes());
        section_data.extend_from_slice(&symbol.code_size.to_le_bytes());
        let kind_byte = match symbol.kind {
            SymbolKind::Function => 0u8,
            SymbolKind::Test => 1u8,
        };
        section_data.push(kind_byte);
    }

    writer.write_all(&(section_data.len() as u32).to_le_bytes())?;
    writer.write_all(&section_data)?;
    Ok(())
}

fn write_line_section<W: Write>(writer: &mut W, line_map: &[LineMapping]) -> io::Result<()> {
    // Section header
    writer.write_all(&[section_type::LINE])?;

    // Calculate section size: 4 (count) + 21 bytes per entry
    // (func_id: 4, code_offset: 4, source_id: 4, line: 4, col: 4, flags: 1)
    let section_size = 4 + line_map.len() * 21;
    writer.write_all(&(section_size as u32).to_le_bytes())?;

    // Entry count
    writer.write_all(&(line_map.len() as u32).to_le_bytes())?;

    // Write entries
    for mapping in line_map {
        writer.write_all(&mapping.func_id.to_le_bytes())?;
        writer.write_all(&mapping.code_offset.to_le_bytes())?;
        writer.write_all(&mapping.source_id.to_le_bytes())?;
        writer.write_all(&mapping.line.to_le_bytes())?;
        writer.write_all(&mapping.col.to_le_bytes())?;
        writer.write_all(&[mapping.flags.bits()])?;
    }

    Ok(())
}

fn write_locals_section<W: Write>(writer: &mut W, locals: &[FunctionLocals]) -> io::Result<()> {
    // Section header
    writer.write_all(&[section_type::LOCALS])?;

    // Calculate section size
    let mut section_data = Vec::new();
    section_data.extend_from_slice(&(locals.len() as u32).to_le_bytes());
    for func_locals in locals {
        section_data.extend_from_slice(&func_locals.func_id.to_le_bytes());
        section_data.extend_from_slice(&(func_locals.locals.len() as u32).to_le_bytes());
        for local in &func_locals.locals {
            let name_bytes = local.name.as_bytes();
            section_data.extend_from_slice(&(name_bytes.len() as u32).to_le_bytes());
            section_data.extend_from_slice(name_bytes);
            section_data.extend_from_slice(&local.register.to_le_bytes());
            section_data.extend_from_slice(&local.live_start.to_le_bytes());
            section_data.extend_from_slice(&local.live_end.to_le_bytes());
        }
    }

    writer.write_all(&(section_data.len() as u32).to_le_bytes())?;
    writer.write_all(&section_data)?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{FunctionLocals, LineMappingFlags, LocalVariable};

    #[test]
    fn test_write_empty_debug_info() {
        let debug_info = DebugInfo::default();
        let mut buf = Vec::new();
        write_skydbg(&debug_info, &mut buf).unwrap();

        // Check header
        assert_eq!(&buf[0..4], MAGIC);
        assert_eq!(u16::from_le_bytes([buf[4], buf[5]]), VERSION);
        assert_eq!(u16::from_le_bytes([buf[6], buf[7]]), 0); // flags
        assert_eq!(u32::from_le_bytes([buf[8], buf[9], buf[10], buf[11]]), 0); // section count
    }

    #[test]
    fn test_write_source_section() {
        let debug_info = DebugInfo {
            sources: vec![
                SourceFile {
                    id: 0,
                    path: "test.skyl".to_string(),
                    text: None,
                },
                SourceFile {
                    id: 1,
                    path: "other.skyl".to_string(),
                    text: Some("source text".to_string()),
                },
            ],
            ..Default::default()
        };

        let mut buf = Vec::new();
        write_skydbg(&debug_info, &mut buf).unwrap();

        // Verify we can at least write without error
        assert!(buf.len() > 12); // Header + some data
        assert_eq!(u32::from_le_bytes([buf[8], buf[9], buf[10], buf[11]]), 1); // 1 section
    }

    #[test]
    fn test_write_symbol_section() {
        let debug_info = DebugInfo {
            symbols: vec![Symbol {
                name: "main".to_string(),
                source_id: 0,
                line: 1,
                col: 1,
                code_offset: 0,
                code_size: 100,
                kind: SymbolKind::Function,
            }],
            ..Default::default()
        };

        let mut buf = Vec::new();
        write_skydbg(&debug_info, &mut buf).unwrap();

        assert!(buf.len() > 12);
        assert_eq!(u32::from_le_bytes([buf[8], buf[9], buf[10], buf[11]]), 1);
    }

    #[test]
    fn test_write_line_section() {
        let debug_info = DebugInfo {
            line_map: vec![
                LineMapping {
                    func_id: 0,
                    code_offset: 0,
                    source_id: 0,
                    line: 1,
                    col: 1,
                    flags: LineMappingFlags::STATEMENT,
                },
                LineMapping {
                    func_id: 0,
                    code_offset: 16,
                    source_id: 0,
                    line: 2,
                    col: 5,
                    flags: LineMappingFlags::empty(),
                },
            ],
            ..Default::default()
        };

        let mut buf = Vec::new();
        write_skydbg(&debug_info, &mut buf).unwrap();

        assert!(buf.len() > 12);
    }

    #[test]
    fn test_write_locals_section() {
        let debug_info = DebugInfo {
            locals: vec![FunctionLocals {
                func_id: 0,
                locals: vec![LocalVariable {
                    name: "x".to_string(),
                    register: 0,
                    live_start: 0,
                    live_end: 100,
                }],
            }],
            ..Default::default()
        };

        let mut buf = Vec::new();
        write_skydbg(&debug_info, &mut buf).unwrap();

        assert!(buf.len() > 12);
    }

    #[test]
    fn test_write_all_sections() {
        let debug_info = DebugInfo {
            sources: vec![SourceFile {
                id: 0,
                path: "test.skyl".to_string(),
                text: None,
            }],
            symbols: vec![Symbol {
                name: "main".to_string(),
                source_id: 0,
                line: 1,
                col: 1,
                code_offset: 0,
                code_size: 100,
                kind: SymbolKind::Function,
            }],
            line_map: vec![LineMapping {
                func_id: 0,
                code_offset: 0,
                source_id: 0,
                line: 1,
                col: 1,
                flags: LineMappingFlags::STATEMENT,
            }],
            locals: vec![FunctionLocals {
                func_id: 0,
                locals: vec![],
            }],
        };

        let mut buf = Vec::new();
        write_skydbg(&debug_info, &mut buf).unwrap();

        // Should have 4 sections
        assert_eq!(u32::from_le_bytes([buf[8], buf[9], buf[10], buf[11]]), 4);
    }
}

//! Deserialization of debug information from the SkyDbg binary format

use crate::serialize::{section_type, MAGIC, VERSION};
use crate::types::{
    DebugInfo, FunctionLocals, LineMapping, LineMappingFlags, LocalVariable, SourceFile, Symbol,
    SymbolKind,
};
use std::io::{self, Read};

/// Errors that can occur during deserialization
#[derive(Debug)]
pub enum ReadError {
    /// IO error while reading
    Io(io::Error),
    /// Invalid magic bytes
    InvalidMagic,
    /// Unsupported version
    UnsupportedVersion(u16),
    /// Invalid section type
    InvalidSectionType(u8),
    /// Invalid UTF-8 string
    InvalidUtf8,
    /// Unexpected end of data
    UnexpectedEof,
    /// Invalid symbol kind
    InvalidSymbolKind(u8),
}

impl std::fmt::Display for ReadError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ReadError::Io(e) => write!(f, "IO error: {}", e),
            ReadError::InvalidMagic => write!(f, "invalid SkyDbg magic bytes"),
            ReadError::UnsupportedVersion(v) => write!(f, "unsupported version: {}", v),
            ReadError::InvalidSectionType(t) => write!(f, "invalid section type: 0x{:02x}", t),
            ReadError::InvalidUtf8 => write!(f, "invalid UTF-8 string"),
            ReadError::UnexpectedEof => write!(f, "unexpected end of file"),
            ReadError::InvalidSymbolKind(k) => write!(f, "invalid symbol kind: {}", k),
        }
    }
}

impl std::error::Error for ReadError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            ReadError::Io(e) => Some(e),
            _ => None,
        }
    }
}

impl From<io::Error> for ReadError {
    fn from(e: io::Error) -> Self {
        ReadError::Io(e)
    }
}

/// Read debug info from the SkyDbg binary format
pub fn read_skydbg<R: Read>(reader: &mut R) -> Result<DebugInfo, ReadError> {
    // Read and validate header
    let mut magic = [0u8; 4];
    reader.read_exact(&mut magic)?;
    if magic != *MAGIC {
        return Err(ReadError::InvalidMagic);
    }

    let mut version_bytes = [0u8; 2];
    reader.read_exact(&mut version_bytes)?;
    let version = u16::from_le_bytes(version_bytes);
    if version != VERSION {
        return Err(ReadError::UnsupportedVersion(version));
    }

    // Skip flags
    let mut flags_bytes = [0u8; 2];
    reader.read_exact(&mut flags_bytes)?;

    // Read section count
    let mut section_count_bytes = [0u8; 4];
    reader.read_exact(&mut section_count_bytes)?;
    let section_count = u32::from_le_bytes(section_count_bytes);

    let mut debug_info = DebugInfo::default();

    // Read sections
    for _ in 0..section_count {
        let mut section_type_byte = [0u8; 1];
        reader.read_exact(&mut section_type_byte)?;

        let mut section_size_bytes = [0u8; 4];
        reader.read_exact(&mut section_size_bytes)?;
        let section_size = u32::from_le_bytes(section_size_bytes) as usize;

        let mut section_data = vec![0u8; section_size];
        reader.read_exact(&mut section_data)?;

        match section_type_byte[0] {
            section_type::SOURCE => {
                debug_info.sources = read_source_section(&section_data)?;
            }
            section_type::SYMBOL => {
                debug_info.symbols = read_symbol_section(&section_data)?;
            }
            section_type::LINE => {
                debug_info.line_map = read_line_section(&section_data)?;
            }
            section_type::LOCALS => {
                debug_info.locals = read_locals_section(&section_data)?;
            }
            t => return Err(ReadError::InvalidSectionType(t)),
        }
    }

    Ok(debug_info)
}

fn read_u32(data: &[u8], offset: &mut usize) -> Result<u32, ReadError> {
    if *offset + 4 > data.len() {
        return Err(ReadError::UnexpectedEof);
    }
    let value = u32::from_le_bytes([
        data[*offset],
        data[*offset + 1],
        data[*offset + 2],
        data[*offset + 3],
    ]);
    *offset += 4;
    Ok(value)
}

fn read_string(data: &[u8], offset: &mut usize) -> Result<String, ReadError> {
    let len = read_u32(data, offset)? as usize;
    if *offset + len > data.len() {
        return Err(ReadError::UnexpectedEof);
    }
    let s = std::str::from_utf8(&data[*offset..*offset + len])
        .map_err(|_| ReadError::InvalidUtf8)?
        .to_string();
    *offset += len;
    Ok(s)
}

fn read_source_section(data: &[u8]) -> Result<Vec<SourceFile>, ReadError> {
    let mut offset = 0;
    let count = read_u32(data, &mut offset)? as usize;
    let mut sources = Vec::with_capacity(count);

    for _ in 0..count {
        let id = read_u32(data, &mut offset)?;
        let path = read_string(data, &mut offset)?;

        // Read text presence flag
        if offset >= data.len() {
            return Err(ReadError::UnexpectedEof);
        }
        let has_text = data[offset] != 0;
        offset += 1;

        let text = if has_text {
            Some(read_string(data, &mut offset)?)
        } else {
            None
        };

        sources.push(SourceFile { id, path, text });
    }

    Ok(sources)
}

fn read_symbol_section(data: &[u8]) -> Result<Vec<Symbol>, ReadError> {
    let mut offset = 0;
    let count = read_u32(data, &mut offset)? as usize;
    let mut symbols = Vec::with_capacity(count);

    for _ in 0..count {
        let name = read_string(data, &mut offset)?;
        let source_id = read_u32(data, &mut offset)?;
        let line = read_u32(data, &mut offset)?;
        let col = read_u32(data, &mut offset)?;
        let code_offset = read_u32(data, &mut offset)?;
        let code_size = read_u32(data, &mut offset)?;

        if offset >= data.len() {
            return Err(ReadError::UnexpectedEof);
        }
        let kind_byte = data[offset];
        offset += 1;

        let kind = match kind_byte {
            0 => SymbolKind::Function,
            1 => SymbolKind::Test,
            k => return Err(ReadError::InvalidSymbolKind(k)),
        };

        symbols.push(Symbol {
            name,
            source_id,
            line,
            col,
            code_offset,
            code_size,
            kind,
        });
    }

    Ok(symbols)
}

fn read_line_section(data: &[u8]) -> Result<Vec<LineMapping>, ReadError> {
    let mut offset = 0;
    let count = read_u32(data, &mut offset)? as usize;
    let mut line_map = Vec::with_capacity(count);

    for _ in 0..count {
        let func_id = read_u32(data, &mut offset)?;
        let code_offset = read_u32(data, &mut offset)?;
        let source_id = read_u32(data, &mut offset)?;
        let line = read_u32(data, &mut offset)?;
        let col = read_u32(data, &mut offset)?;

        if offset >= data.len() {
            return Err(ReadError::UnexpectedEof);
        }
        let flags_bits = data[offset];
        offset += 1;

        let flags =
            LineMappingFlags::from_bits(flags_bits).unwrap_or(LineMappingFlags::empty());

        line_map.push(LineMapping {
            func_id,
            code_offset,
            source_id,
            line,
            col,
            flags,
        });
    }

    Ok(line_map)
}

fn read_locals_section(data: &[u8]) -> Result<Vec<FunctionLocals>, ReadError> {
    let mut offset = 0;
    let count = read_u32(data, &mut offset)? as usize;
    let mut locals_list = Vec::with_capacity(count);

    for _ in 0..count {
        let func_id = read_u32(data, &mut offset)?;
        let local_count = read_u32(data, &mut offset)? as usize;
        let mut locals = Vec::with_capacity(local_count);

        for _ in 0..local_count {
            let name = read_string(data, &mut offset)?;
            let register = read_u32(data, &mut offset)?;
            let live_start = read_u32(data, &mut offset)?;
            let live_end = read_u32(data, &mut offset)?;

            locals.push(LocalVariable {
                name,
                register,
                live_start,
                live_end,
            });
        }

        locals_list.push(FunctionLocals { func_id, locals });
    }

    Ok(locals_list)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::serialize::write_skydbg;
    use std::error::Error;

    #[test]
    fn test_roundtrip_empty() {
        let original = DebugInfo::default();
        let mut buf = Vec::new();
        write_skydbg(&original, &mut buf).unwrap();

        let read_back = read_skydbg(&mut &buf[..]).unwrap();
        assert_eq!(original, read_back);
    }

    #[test]
    fn test_roundtrip_sources() {
        let original = DebugInfo {
            sources: vec![
                SourceFile {
                    id: 0,
                    path: "test.skyl".to_string(),
                    text: None,
                },
                SourceFile {
                    id: 1,
                    path: "other.skyl".to_string(),
                    text: Some("fn main():".to_string()),
                },
            ],
            ..Default::default()
        };

        let mut buf = Vec::new();
        write_skydbg(&original, &mut buf).unwrap();

        let read_back = read_skydbg(&mut &buf[..]).unwrap();
        assert_eq!(original, read_back);
    }

    #[test]
    fn test_roundtrip_symbols() {
        let original = DebugInfo {
            symbols: vec![
                Symbol {
                    name: "main".to_string(),
                    source_id: 0,
                    line: 1,
                    col: 1,
                    code_offset: 0,
                    code_size: 100,
                    kind: SymbolKind::Function,
                },
                Symbol {
                    name: "test_foo".to_string(),
                    source_id: 0,
                    line: 10,
                    col: 1,
                    code_offset: 100,
                    code_size: 50,
                    kind: SymbolKind::Test,
                },
            ],
            ..Default::default()
        };

        let mut buf = Vec::new();
        write_skydbg(&original, &mut buf).unwrap();

        let read_back = read_skydbg(&mut &buf[..]).unwrap();
        assert_eq!(original, read_back);
    }

    #[test]
    fn test_roundtrip_line_map() {
        let original = DebugInfo {
            line_map: vec![
                LineMapping {
                    func_id: 0,
                    code_offset: 0,
                    source_id: 0,
                    line: 1,
                    col: 1,
                    flags: LineMappingFlags::STATEMENT | LineMappingFlags::PROLOGUE,
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
        write_skydbg(&original, &mut buf).unwrap();

        let read_back = read_skydbg(&mut &buf[..]).unwrap();
        assert_eq!(original, read_back);
    }

    #[test]
    fn test_roundtrip_locals() {
        let original = DebugInfo {
            locals: vec![FunctionLocals {
                func_id: 0,
                locals: vec![
                    LocalVariable {
                        name: "x".to_string(),
                        register: 0,
                        live_start: 0,
                        live_end: 100,
                    },
                    LocalVariable {
                        name: "y".to_string(),
                        register: 1,
                        live_start: 20,
                        live_end: 80,
                    },
                ],
            }],
            ..Default::default()
        };

        let mut buf = Vec::new();
        write_skydbg(&original, &mut buf).unwrap();

        let read_back = read_skydbg(&mut &buf[..]).unwrap();
        assert_eq!(original, read_back);
    }

    #[test]
    fn test_roundtrip_complete() {
        let original = DebugInfo {
            sources: vec![SourceFile {
                id: 0,
                path: "main.skyl".to_string(),
                text: Some("fn main():\n  assert(1 == 1)".to_string()),
            }],
            symbols: vec![Symbol {
                name: "main".to_string(),
                source_id: 0,
                line: 1,
                col: 1,
                code_offset: 0,
                code_size: 64,
                kind: SymbolKind::Function,
            }],
            line_map: vec![
                LineMapping {
                    func_id: 0,
                    code_offset: 0,
                    source_id: 0,
                    line: 1,
                    col: 1,
                    flags: LineMappingFlags::PROLOGUE,
                },
                LineMapping {
                    func_id: 0,
                    code_offset: 16,
                    source_id: 0,
                    line: 2,
                    col: 3,
                    flags: LineMappingFlags::STATEMENT,
                },
            ],
            locals: vec![FunctionLocals {
                func_id: 0,
                locals: vec![LocalVariable {
                    name: "r0".to_string(),
                    register: 0,
                    live_start: 16,
                    live_end: 48,
                }],
            }],
        };

        let mut buf = Vec::new();
        write_skydbg(&original, &mut buf).unwrap();

        let read_back = read_skydbg(&mut &buf[..]).unwrap();
        assert_eq!(original, read_back);
    }

    #[test]
    fn test_invalid_magic() {
        let data = b"XXXX\x01\x00\x00\x00\x00\x00\x00\x00";
        let result = read_skydbg(&mut &data[..]);
        assert!(matches!(result, Err(ReadError::InvalidMagic)));
    }

    #[test]
    fn test_unsupported_version() {
        let data = b"SKDB\x02\x00\x00\x00\x00\x00\x00\x00";
        let result = read_skydbg(&mut &data[..]);
        assert!(matches!(result, Err(ReadError::UnsupportedVersion(2))));
    }

    #[test]
    fn test_invalid_section_type() {
        // Header with 1 section, then invalid section type 0xFF
        let data = b"SKDB\x01\x00\x00\x00\x01\x00\x00\x00\xFF\x00\x00\x00\x00";
        let result = read_skydbg(&mut &data[..]);
        assert!(matches!(result, Err(ReadError::InvalidSectionType(0xFF))));
    }

    #[test]
    fn test_read_error_display() {
        let e = ReadError::InvalidMagic;
        assert_eq!(e.to_string(), "invalid SkyDbg magic bytes");

        let e = ReadError::UnsupportedVersion(99);
        assert_eq!(e.to_string(), "unsupported version: 99");

        let e = ReadError::InvalidSectionType(0xFF);
        assert_eq!(e.to_string(), "invalid section type: 0xff");

        let e = ReadError::InvalidUtf8;
        assert_eq!(e.to_string(), "invalid UTF-8 string");

        let e = ReadError::UnexpectedEof;
        assert_eq!(e.to_string(), "unexpected end of file");

        let e = ReadError::InvalidSymbolKind(99);
        assert_eq!(e.to_string(), "invalid symbol kind: 99");
    }

    #[test]
    fn test_read_error_source() {
        let io_err = io::Error::new(io::ErrorKind::Other, "test");
        let e = ReadError::Io(io_err);
        assert!(e.source().is_some());

        let e = ReadError::InvalidMagic;
        assert!(e.source().is_none());
    }
}

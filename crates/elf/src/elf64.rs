//! ELF64 binary format generation for Linux/AArch64
//!
//! This module generates minimal static ELF executables without external dependencies.

/// ELF64 header size
pub const ELF64_EHDR_SIZE: usize = 64;
/// ELF64 program header size
pub const ELF64_PHDR_SIZE: usize = 56;

/// ELF magic number
const ELF_MAGIC: [u8; 4] = [0x7f, b'E', b'L', b'F'];
/// ELF class: 64-bit
const ELFCLASS64: u8 = 2;
/// ELF data encoding: little endian
const ELFDATA2LSB: u8 = 1;
/// ELF version
const EV_CURRENT: u8 = 1;
/// ELF OS/ABI: Linux
const ELFOSABI_LINUX: u8 = 3;
/// ELF type: executable
const ET_EXEC: u16 = 2;
/// Machine type: AArch64
const EM_AARCH64: u16 = 183;

/// Program header type: loadable segment
const PT_LOAD: u32 = 1;
/// Segment flags: readable
const PF_R: u32 = 0x4;
/// Segment flags: executable
const PF_X: u32 = 0x1;

/// Default base address for the executable
pub const BASE_ADDR: u64 = 0x400000;

/// Generate an ELF64 header
pub fn generate_elf_header(
    entry_point: u64,
    phdr_count: u16,
) -> [u8; ELF64_EHDR_SIZE] {
    let mut header = [0u8; ELF64_EHDR_SIZE];

    // e_ident (16 bytes)
    header[0..4].copy_from_slice(&ELF_MAGIC);
    header[4] = ELFCLASS64;
    header[5] = ELFDATA2LSB;
    header[6] = EV_CURRENT;
    header[7] = ELFOSABI_LINUX;
    // Padding (8 bytes, already zero)

    // e_type (2 bytes) - ET_EXEC
    header[16..18].copy_from_slice(&ET_EXEC.to_le_bytes());
    // e_machine (2 bytes) - EM_AARCH64
    header[18..20].copy_from_slice(&EM_AARCH64.to_le_bytes());
    // e_version (4 bytes)
    header[20..24].copy_from_slice(&1u32.to_le_bytes());
    // e_entry (8 bytes) - entry point
    header[24..32].copy_from_slice(&entry_point.to_le_bytes());
    // e_phoff (8 bytes) - program header offset (immediately after ELF header)
    header[32..40].copy_from_slice(&(ELF64_EHDR_SIZE as u64).to_le_bytes());
    // e_shoff (8 bytes) - section header offset (0 = none)
    header[40..48].copy_from_slice(&0u64.to_le_bytes());
    // e_flags (4 bytes)
    header[48..52].copy_from_slice(&0u32.to_le_bytes());
    // e_ehsize (2 bytes)
    header[52..54].copy_from_slice(&(ELF64_EHDR_SIZE as u16).to_le_bytes());
    // e_phentsize (2 bytes)
    header[54..56].copy_from_slice(&(ELF64_PHDR_SIZE as u16).to_le_bytes());
    // e_phnum (2 bytes)
    header[56..58].copy_from_slice(&phdr_count.to_le_bytes());
    // e_shentsize (2 bytes)
    header[58..60].copy_from_slice(&0u16.to_le_bytes());
    // e_shnum (2 bytes)
    header[60..62].copy_from_slice(&0u16.to_le_bytes());
    // e_shstrndx (2 bytes)
    header[62..64].copy_from_slice(&0u16.to_le_bytes());

    header
}

/// Generate an ELF64 program header for a loadable segment
pub fn generate_program_header(
    offset: u64,      // File offset
    vaddr: u64,       // Virtual address
    file_size: u64,   // Size in file
    mem_size: u64,    // Size in memory
    flags: u32,       // Segment flags (PF_R, PF_X, PF_W)
    align: u64,       // Alignment
) -> [u8; ELF64_PHDR_SIZE] {
    let mut phdr = [0u8; ELF64_PHDR_SIZE];

    // p_type (4 bytes) - PT_LOAD
    phdr[0..4].copy_from_slice(&PT_LOAD.to_le_bytes());
    // p_flags (4 bytes)
    phdr[4..8].copy_from_slice(&flags.to_le_bytes());
    // p_offset (8 bytes) - file offset
    phdr[8..16].copy_from_slice(&offset.to_le_bytes());
    // p_vaddr (8 bytes) - virtual address
    phdr[16..24].copy_from_slice(&vaddr.to_le_bytes());
    // p_paddr (8 bytes) - physical address (same as vaddr)
    phdr[24..32].copy_from_slice(&vaddr.to_le_bytes());
    // p_filesz (8 bytes) - size in file
    phdr[32..40].copy_from_slice(&file_size.to_le_bytes());
    // p_memsz (8 bytes) - size in memory
    phdr[40..48].copy_from_slice(&mem_size.to_le_bytes());
    // p_align (8 bytes) - alignment
    phdr[48..56].copy_from_slice(&align.to_le_bytes());

    phdr
}

/// Segment flags for read+execute (code)
pub const SEGMENT_RX: u32 = PF_R | PF_X;
/// Segment flags for read-only (data)
pub const SEGMENT_R: u32 = PF_R;

/// Calculate the header size (ELF header + program headers)
pub fn header_size(phdr_count: u16) -> usize {
    ELF64_EHDR_SIZE + (ELF64_PHDR_SIZE * phdr_count as usize)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_elf_header_generation() {
        let header = generate_elf_header(BASE_ADDR + 0x78, 1);
        assert_eq!(header.len(), ELF64_EHDR_SIZE);
        // Check magic number
        assert_eq!(&header[0..4], &ELF_MAGIC);
        // Check class
        assert_eq!(header[4], ELFCLASS64);
    }

    #[test]
    fn test_program_header_generation() {
        let phdr = generate_program_header(0, BASE_ADDR, 100, 100, SEGMENT_RX, 0x1000);
        assert_eq!(phdr.len(), ELF64_PHDR_SIZE);
    }

    #[test]
    fn test_header_size() {
        assert_eq!(header_size(1), ELF64_EHDR_SIZE + ELF64_PHDR_SIZE);
        assert_eq!(header_size(2), ELF64_EHDR_SIZE + 2 * ELF64_PHDR_SIZE);
    }
}

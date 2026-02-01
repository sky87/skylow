//! Executable memory management
//!
//! Uses mmap to allocate memory with PROT_EXEC for JIT code execution.

use std::ptr;

/// Executable memory region for JIT-compiled code
pub struct ExecutableMemory {
    ptr: *mut u8,
    len: usize,
}

impl ExecutableMemory {
    /// Allocate executable memory and copy code into it
    pub fn new(code: &[u8]) -> Result<Self, String> {
        if code.is_empty() {
            return Err("cannot allocate empty code".to_string());
        }

        // Round up to page size
        let page_size = unsafe { libc::sysconf(libc::_SC_PAGESIZE) as usize };
        let len = (code.len() + page_size - 1) & !(page_size - 1);

        let ptr = unsafe {
            libc::mmap(
                ptr::null_mut(),
                len,
                libc::PROT_READ | libc::PROT_WRITE,
                libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
                -1,
                0,
            )
        };

        if ptr == libc::MAP_FAILED {
            return Err("mmap failed".to_string());
        }

        let ptr = ptr as *mut u8;

        // Copy code into the memory
        unsafe {
            ptr::copy_nonoverlapping(code.as_ptr(), ptr, code.len());
        }

        // Make it executable (and read-only)
        let result = unsafe {
            libc::mprotect(ptr as *mut libc::c_void, len, libc::PROT_READ | libc::PROT_EXEC)
        };

        if result != 0 {
            unsafe {
                libc::munmap(ptr as *mut libc::c_void, len);
            }
            return Err("mprotect failed".to_string());
        }

        Ok(Self { ptr, len })
    }

    /// Get a function pointer to the compiled code
    ///
    /// # Safety
    /// The caller must ensure the code is valid for the function signature.
    pub unsafe fn as_fn<F>(&self) -> F
    where
        F: Copy,
    {
        std::mem::transmute_copy(&self.ptr)
    }

    /// Get the raw pointer to the code
    pub fn as_ptr(&self) -> *const u8 {
        self.ptr
    }
}

impl Drop for ExecutableMemory {
    fn drop(&mut self) {
        unsafe {
            libc::munmap(self.ptr as *mut libc::c_void, self.len);
        }
    }
}

// ExecutableMemory is Send because the memory it owns is not shared
unsafe impl Send for ExecutableMemory {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_executable_memory() {
        // AArch64: mov w0, #42; ret
        let code: &[u8] = &[
            0x40, 0x05, 0x80, 0x52, // mov w0, #42
            0xc0, 0x03, 0x5f, 0xd6, // ret
        ];

        let mem = ExecutableMemory::new(code).expect("allocation failed");
        let func: extern "C" fn() -> i32 = unsafe { mem.as_fn() };
        assert_eq!(func(), 42);
    }

    #[test]
    fn test_empty_code_error() {
        let result = ExecutableMemory::new(&[]);
        assert!(result.is_err());
    }

    #[test]
    fn test_as_ptr() {
        // AArch64: mov w0, #42; ret
        let code: &[u8] = &[
            0x40, 0x05, 0x80, 0x52, // mov w0, #42
            0xc0, 0x03, 0x5f, 0xd6, // ret
        ];
        let mem = ExecutableMemory::new(code).expect("allocation failed");
        let ptr = mem.as_ptr();
        assert!(!ptr.is_null());
    }
}

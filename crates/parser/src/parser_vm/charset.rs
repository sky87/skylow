//! Compiled character set using bitmap for fast matching.

/// Compiled character set using bitmap for fast matching
#[derive(Clone)]
pub struct CompiledCharSet {
    /// Bitmap for ASCII characters (256 bits = 32 bytes)
    bitmap: [u64; 4],
    negated: bool,
}

impl CompiledCharSet {
    pub fn new(ranges: &[(char, char)], negated: bool) -> Self {
        let mut bitmap = [0u64; 4];
        for &(lo, hi) in ranges {
            for c in (lo as u32)..=(hi as u32) {
                if c < 256 {
                    let idx = (c / 64) as usize;
                    let bit = c % 64;
                    bitmap[idx] |= 1u64 << bit;
                }
            }
        }
        Self { bitmap, negated }
    }

    #[inline]
    pub fn matches(&self, c: char) -> bool {
        let code = c as u32;
        if code >= 256 {
            return self.negated;
        }
        let idx = (code / 64) as usize;
        let bit = code % 64;
        let in_set = (self.bitmap[idx] & (1u64 << bit)) != 0;
        if self.negated { !in_set } else { in_set }
    }

    /// Check if this charset is negated
    pub fn is_negated(&self) -> bool {
        self.negated
    }
}


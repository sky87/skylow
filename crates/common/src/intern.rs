use bumpalo::Bump;
use std::collections::HashMap;

pub struct StringInterner<'a> {
    arena: &'a Bump,
    map: HashMap<&'a str, ()>,
}

impl<'a> StringInterner<'a> {
    pub fn new(arena: &'a Bump) -> Self {
        Self {
            arena,
            map: HashMap::new(),
        }
    }

    pub fn intern(&mut self, s: &str) -> &'a str {
        if let Some((&existing, _)) = self.map.get_key_value(s) {
            existing
        } else {
            let interned = self.arena.alloc_str(s);
            self.map.insert(interned, ());
            interned
        }
    }
}

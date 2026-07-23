// CONTROL for the all-bindings-must-agree rule. `HashMap` is bound twice: once
// behind `#[cfg(feature = "std")]` at the crate root, and once *unconditionally*
// in a submodule. The ungated one keeps the name std-resolving with the feature
// off, so nothing may be excused — not the bare uses, and not `Entry` derived
// through `hash_map` either, since `hash_map` shares the ungated import.

#![no_std]

#[cfg(feature = "std")]
pub use std::collections::{HashMap, hash_map};

pub mod always_std {
    // No `#[cfg]` — this binding is real in every configuration.
    pub use std::collections::{HashMap, hash_map};
}

pub mod switch {
    use super::HashMap;

    pub fn make() {
        let _ = HashMap::new();
    }
}

pub mod frontend {
    pub fn insert() {
        use crate::hash_map::Entry;
        let _ = Entry::Occupied;
    }
}

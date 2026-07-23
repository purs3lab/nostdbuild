// The alexcrichton-cranelift-frontend 0.53.0 shape (KI-7). The crate offers
// hashbrown as the no_std replacement for std's maps, picks between them with a
// `#[cfg(feature = "std")]` pair at the crate root, and then re-exports the
// chosen binding through the module tree.
//
// Nothing below carries a `#[cfg]` of its own except the two root imports, so
// every use site reaches the driver with no gate at all — which is why they were
// short-circuited to `StillStd` without a single compile. The gate exists; it
// just lives on the import that bound the name.
//
// Not meant to compile (there is no hashbrown dependency); `ModCollector` only
// parses it.

#![no_std]

#[cfg(not(feature = "std"))]
pub use hashbrown::{HashMap, hash_map};
#[cfg(feature = "std")]
pub use std::collections::{HashMap, hash_map};

// A rename: the *bound* name is `Map`, and that is what use sites reference.
#[cfg(feature = "std")]
pub use std::collections::BTreeMap as Map;

pub mod switch {
    // Emits no plugin record whatsoever — the binding graph for this hop exists
    // only in the syn tree.
    use super::HashMap;

    pub struct Switch {
        cases: HashMap<u32, u32>,
    }

    impl Switch {
        pub fn new() -> Self {
            Switch {
                cases: HashMap::new(),
            }
        }
    }
}

pub mod frontend {
    pub fn insert() {
        // Two-hop: `hash_map` is itself a std-rooted binding from the root brace
        // import, so `Entry` is std-bound through it.
        use crate::hash_map::Entry;

        let _ = Entry::Occupied;
        let _ = Entry::Vacant;
    }

    pub fn renamed() {
        let _ = Map::new();
    }
}

pub mod nostd_only {
    // CONTROL, and a sharp one: ungated, binds `HashMap`, but is not std-rooted.
    // Only std-rooted bindings may seed the table — if the root check were
    // dropped this ungated binding would clear `HashMap`'s all-gated flag and
    // every excuse in this fixture would disappear.
    pub use hashbrown::HashMap;
}

pub mod globbed {
    // Deliberately out of scope: a glob binds names syn cannot enumerate, so the
    // fixpoint refuses to follow it and anything it binds stays unexcused.
    use super::hash_map::*;
}

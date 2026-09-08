// KI-7 fixture gap: the alexcrichton-cranelift-frontend 0.53.0 shape, but with
// a dependency that actually builds no_std instead of an uncompiled fixture.
//
// Every corpus clearance measured for KI-7 so far (cranelift-frontend itself)
// went through `ProbeDecision::CompileFailed` — the whole dependency tree dies
// inside cranelift-codegen, unrelated to the hashbrown swap. No crate has ever
// shown a brace/routed clearance via a probe that actually compiled. `mapshim`
// stands in for hashbrown so the ¬std covering run here (`--no-default-
// features`) builds clean — proof the recovered gate is correct, not just
// quieter, in at least one real (if minimal) case.
//
// NOT `#![no_std]`: that would give the crate its own no_std condition, which
// triggers an unconditional baseline ¬std covering run
// (`driver.rs`'s "When the crate has cfg_attr(condition, no_std), always do a
// baseline no_std run") for reasons unrelated to KI-7 at all. Even without it,
// see `tests/routed_std_import_tests.rs`'s section comment: the crate-ROOT
// cfg swap below is itself an item the general covering-set search finds and
// compiles on its own, so this fixture ends up NOT actually gated on the
// KI-7 gate-recovery call sites either — it is evidence a real clearance
// exists, not a regression guard for that mechanism specifically.

#[cfg(not(feature = "std"))]
pub use mapshim::{HashMap, hash_map};
#[cfg(feature = "std")]
pub use std::collections::{HashMap, hash_map};

pub mod switch {
    // Emits no plugin record at all — this hop exists only in the syn tree
    // (KI-7's `use super::HashMap;` trap).
    use super::HashMap;

    pub fn make() -> HashMap<u8, u8> {
        HashMap::new()
    }
}

pub mod frontend {
    pub fn state() -> crate::hash_map::RandomState {
        // Two-hop re-export: `hash_map` is itself std-rooted via the root
        // brace import, so `RandomState` is std-bound through it — routed,
        // with no plugin record of its own (KI-7's other trap).
        use crate::hash_map::RandomState;

        RandomState::new()
    }
}

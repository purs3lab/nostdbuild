// agnostic-lite-0.5.5 in miniature — the shape behind R34-12 / KI-26.
//
// The crate is built to be no_std and there is exactly one std span, behind the
// one gate a probe negates. What decides the verdict is not that span: negating
// `std` deletes `Inner`'s only constructor, `Inner` becomes dead, and the
// crate's own `deny(warnings)` makes that a hard error. The probe fails on
//
//     error: struct `Inner` is never constructed
//
// which says nothing about std, and the span comes back unproven. agnostic-lite
// runs this 74 times over 114 spans and emits nothing.
//
// A plugin pass is an *analysis* compile — it collects HIR records, and lints do
// not affect the name resolution those records are read for — so it caps lints.
// The verification build in `compiler.rs` does not, and must not.
#![cfg_attr(not(feature = "std"), no_std)]
#![deny(warnings)]

use core::fmt;

pub struct E;

impl fmt::Display for E {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "e")
    }
}

impl fmt::Debug for E {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "E")
    }
}

// The span under test. std in every covering run, so it is probed by negating
// its only gate — and nothing here actually needs std, so the probe must clear
// it and the crate must be reported no_std-able with `std` off.
#[cfg(feature = "std")]
impl std::error::Error for E {}

// The trap, and the whole point of the fixture. `Inner` is private and is
// constructed on the std path only, so the configuration the probe compiles is
// exactly the configuration in which it is dead.
struct Inner {
    _n: u32,
}

#[cfg(feature = "std")]
pub fn make() -> E {
    let _ = Inner { _n: 0 };
    E
}

//! A `compile_error!` nested inside a `#[cfg]`-gated inline module.
//!
//! cfg stripping is outside-in: with `enable` off the whole of `gated` is
//! removed before the inner `#[cfg]` is ever looked at, so the crate is saying
//!
//!     ¬(enable ∧ (cannot ∨ wasm))
//!
//! and nothing at all about `cannot`/`wasm` on their own. Both collection sites
//! read only the item's own attributes and emitted the fragment `¬(cannot ∨
//! wasm)` unconditionally — O-15's defect on the nesting axis instead of the
//! stacking one, and the same price: a hard constraint here is the seed veto, so
//! a crate whose no_std solve never wanted `enable` lost every std-off run to a
//! constraint it did not write. (`cannot` is deliberately a feature the no_std
//! condition names, which is what keeps `excluded_compile_error_eqs` from
//! withholding the equation and hiding the defect.)
//!
//! Noted as an unfixed caveat of F18 in ALL_TARGET_FAILURES.md: "A
//! `compile_error!` inside a `#[cfg]`-ed-out module is read as though the module
//! always compiled."
#![cfg_attr(not(feature = "cannot"), no_std)]

#[cfg(feature = "enable")]
mod gated {
    // `wasm`, not `test`: a bare `cfg(test)` is skipped by `should_skip` and
    // would make this fixture prove nothing.
    #[cfg(any(feature = "cannot", feature = "wasm"))]
    compile_error!("`cannot` and `wasm` cannot be used with `enable`");

    pub fn nothing() {}
}

pub fn top() {}

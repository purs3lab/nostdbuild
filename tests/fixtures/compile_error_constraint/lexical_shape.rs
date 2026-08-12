//! lexical-util 1.0.6 shape: an *implication* rather than a bare disjunction —
//! a feature the crate accepts only in company. The emitted set
//! `--no-default-features --features floats` satisfies the antecedent and
//! neither disjunct of the consequent, so every target dies on the macro:
//!
//! ```text
//! error: Do not use the `floats` feature directly. Use `write-floats` and/or
//!        `parse-floats` instead.
//! ```
//!
//! The repair has to *add* — `write-floats` or `parse-floats` — because turning
//! `floats` back off is the feature solve's decision to make, not this pass's.
#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(all(
    feature = "floats",
    not(any(feature = "write-floats", feature = "parse-floats"))
))]
compile_error!(
    "Do not use the `floats` feature directly. Use `write-floats` and/or `parse-floats` instead."
);

pub fn nothing() {}

//! spo-rhai 1.17.2 / rhai 1.21.0 shape (src/lib.rs:506-508): a stacked
//! `compile_error!` where one of the two attributes names an atom policy G
//! erases.
//!
//! O-1's rule says such a constraint cannot be modelled soundly and must be
//! dropped — an erased atom is UNKNOWN and this position asserts the cfg's
//! *negation*, which admits no truth value for it. Applied per attribute the
//! rule dropped the `target_family` half and kept `¬no_std` **alone**: a
//! fragment of a constraint the crate never stated, asserted where the rule says
//! say nothing. That is rhai's third `(not no_std)`.
#![cfg_attr(feature = "no_std", no_std)]

#[cfg(target_family = "wasm")]
#[cfg(feature = "no_std")]
compile_error!("`no_std` cannot be used for WASM target");

pub fn nothing() {}

//! spo-rhai 1.17.2 / rhai 1.21.0 shape (src/lib.rs:498-504): two `#[cfg]`s
//! stacked on one `compile_error!`.
//!
//! rustc ANDs them, so the crate is saying `¬(no_std ∧ wasm-bindgen)`. Read one
//! attribute at a time it became `¬no_std ∧ ¬wasm-bindgen`, and `¬no_std` is the
//! negation of this crate's own no_std condition — every std-off covering seed
//! came back "unsatisfiable with hard constraints" (300 of them in spo-rhai),
//! the enabler search was skipped, and all 273 spans landed `AlwaysStd` for want
//! of a run that was never attempted.
#![cfg_attr(feature = "no_std", no_std)]

#[cfg(feature = "no_std")]
#[cfg(feature = "wasm-bindgen")]
compile_error!("`wasm-bindgen` cannot be used with `no-std`");

pub fn nothing() {}

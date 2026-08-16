//! mtxgroup 0.1.1 shape: a `compile_error!` whose cfg nests two groups as
//! siblings under `any(...)` — "exactly one of `std` and `spin`".
//!
//! The two operands of the outer `any` are themselves groups (`not(any(…))`
//! and `all(…)`), which is the case `parse_token_stream` used to combine with
//! AND regardless of the operator that owned them.
#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(any(
    not(any(feature = "std", feature = "spin")),
    all(feature = "std", feature = "spin")
))]
compile_error!("Exactly one of the features `std` and `spin` must be enabled");

pub fn nothing() {}

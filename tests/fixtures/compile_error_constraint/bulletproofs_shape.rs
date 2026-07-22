//! bulletproofs-bls shape: a two-way `compile_error!` disjunction over features
//! that share nothing with the crate's no_std condition.
#![cfg_attr(not(feature = "std"), no_std)]

extern crate alloc;

#[cfg(all(not(feature = "rust"), not(feature = "blst")))]
compile_error!("At least `rust` or `blst` must be selected");

pub fn nothing() {}

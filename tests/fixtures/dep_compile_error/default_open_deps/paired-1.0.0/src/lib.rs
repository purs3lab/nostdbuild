//! spin 0.9's shape: `default` alone answers the dependency's own
//! `compile_error!`, and nothing about the edge as authored says whether
//! `default` survives.
#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(not(any(feature = "std", feature = "libm")))]
compile_error!("paired requires either the `std` or `libm` feature");

pub fn nothing() {}

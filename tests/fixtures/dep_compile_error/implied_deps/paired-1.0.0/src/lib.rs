//! color 0.2.3's shape: the dependency refuses to build unless the consumer
//! picks one of two features.
#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(not(any(feature = "std", feature = "libm")))]
compile_error!("paired requires either the `std` or `libm` feature");

pub fn nothing() {}

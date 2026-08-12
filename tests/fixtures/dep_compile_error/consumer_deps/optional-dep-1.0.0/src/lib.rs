#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(not(feature = "std"))]
compile_error!("optional-dep requires the `std` feature");

pub fn nothing() {}

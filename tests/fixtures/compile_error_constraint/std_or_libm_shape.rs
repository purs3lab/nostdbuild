//! color 0.2.3's shape: a two-way disjunction where one disjunct is the very
//! feature the no_std verdict turned off. Used here to pin the `forbidden`
//! rule — `std` is never an acceptable repair, `libm` is.
#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(not(any(feature = "std", feature = "libm")))]
compile_error!("color requires either the `std` or `libm` feature");

pub fn nothing() {}

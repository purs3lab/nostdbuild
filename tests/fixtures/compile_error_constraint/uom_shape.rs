//! uom shape: a wide `any(...)` disjunction ("at least one storage type").
#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(not(any(feature = "u32", feature = "i32", feature = "f32", feature = "f64")))]
compile_error!("A least one underlying storage type must be enabled.");

pub fn nothing() {}

//! bulletproofs-bls-4.0.0's `src/lib.rs`, shapes 12/16/24.
#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(all(not(feature = "rust"), not(feature = "blst")))]
compile_error!("At least `rust` or `blst` must be selected");

#[cfg(not(feature = "blst"))]
pub use rust_backend::via_rust as backend;

#[cfg(feature = "blst")]
pub use blst_backend::via_blst as backend;

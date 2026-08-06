#![no_std]

//! serde_json's shape: a module whose file is deliberately not valid Rust, so
//! that reaching it *is* the compile error. The tool reaches it exactly when its
//! own model has both features off — the configuration it most wants to analyse.

#[cfg(not(any(feature = "std", feature = "alloc")))]
mod features_check;

mod good;

pub use good::marker;

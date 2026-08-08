//! miden-thiserror 1.0.59 shape (src/provide.rs:5): a `compile_error!` whose cfg
//! mixes a feature with a non-feature atom inside `any(...)`.
//!
//! `error_in_core` is set by the crate's build script on a nightly compiler, so
//! the compile_error can never fire — the crate builds clean with
//! `--no-default-features --target aarch64-unknown-none`. Policy G erases
//! `error_in_core` out of the `or`, collapsing the cfg to `not(feature = "std")`
//! and the emitted constraint to `(not (not (or std)))` — "std is mandatory".
#![no_std]

#[cfg(not(any(feature = "std", error_in_core)))]
compile_error!("cannot compile this feature without the 'std' feature or a nightly compiler");

pub fn nothing() {}

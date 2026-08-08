//! midenc-hir-symbol 0.0.8 shape (src/sync.rs:19): the `all(...)` mirror of
//! `erased_any_shape.rs`.
//!
//! `not(target_family = "wasm")` erases to nothing inside the `and`, i.e. reads
//! as true, so the cfg collapses to `and(not std)` and the constraint to
//! `(not (and (not std)))` — "std is mandatory". The crate builds clean with
//! `--no-default-features --target wasm32v1-none`, which is in `TARGET_LIST`.
#![no_std]

#[cfg(all(not(feature = "std"), not(target_family = "wasm")))]
compile_error!("no_std builds of this crate are only supported on wasm targets");

pub fn nothing() {}

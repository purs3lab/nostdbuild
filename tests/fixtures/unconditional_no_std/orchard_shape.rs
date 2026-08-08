//! orchard 0.11.0 / sapling-crypto 0.5.0 shape (src/lib.rs:10 and :23): the
//! crate is `#![no_std]` outright, so there is no `#![cfg_attr(<cond>, no_std)]`
//! to read a no_std condition off — but it links std through a gated crate-root
//! `extern crate std`, which says the same thing.
//!
//! Without a condition the driver runs no baseline no_std pass and
//! `covering_set_modes` makes no std/no_std split, so every covering run had
//! `std` on and every std span classified `AlwaysStd`. orchard failed on four
//! `core2::io` spans while `--no-default-features --target aarch64-unknown-none`
//! compiled clean.
#![no_std]

extern crate alloc;

#[cfg(feature = "std")]
extern crate std;

pub fn nothing() {}

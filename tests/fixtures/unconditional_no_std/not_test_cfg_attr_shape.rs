//! splay-safe-rs 0.8.3, blas-array2 0.3.0 (O-14): the crate is `#![no_std]`
//! through `#![cfg_attr(not(test), no_std)]`. `test` is not a feature, so it is
//! erased and the attribute yields no equation at all — the crate got neither a
//! `cfg_attr` condition nor the unconditional-`#![no_std]` inference, even
//! though it is no_std in every configuration cargo builds here.
//!
//! Its `extern crate std` is gated, so the condition is that gate's negation,
//! exactly as for a bare `#![no_std]`. Without it splay-safe-rs had one covering
//! seed, `(or std)`, one run, `[std]`, and every std span landed `AlwaysStd`.
#![cfg_attr(not(test), no_std)]

#[cfg(feature = "std")]
extern crate std;

extern crate alloc;

pub fn nothing() {}

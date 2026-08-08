//! Control: the crate states its own no_std condition, *and* has a gated
//! crate-root `extern crate std` naming a different feature. The author's
//! `cfg_attr` must win — the inference exists only for its absence.
#![cfg_attr(not(feature = "no-std"), no_std)]

#[cfg(feature = "std")]
extern crate std;

pub fn nothing() {}

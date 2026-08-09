//! nate-common 0.1.10 / tinywasm 0.8.0 shape: the crate is `#![no_std]` and its
//! std facade is a module of its own, reached unconditionally. The crate root
//! carries no `extern crate std` at all, so O-3's crate-root-only inference
//! found nothing and the crate got no no_std condition — no baseline no_std run,
//! no std/no_std covering split, every std span `AlwaysStd`.
//!
//! `extern crate std` in `details` links std for the whole crate exactly as one
//! in the root would, and the module is compiled in every configuration, so its
//! gate is the crate's statement of when it links std.
#![no_std]

pub mod details;

pub fn nothing() {}

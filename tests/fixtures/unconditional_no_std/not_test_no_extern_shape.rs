//! ckc-rs 0.1.15: `#![cfg_attr(not(test), no_std)]` and no `extern crate std`
//! anywhere. Both halves of O-14 at once — the attribute is an unconditional
//! `#![no_std]`, and with nothing linking std the condition is `true`.
#![cfg_attr(not(test), no_std)]

extern crate alloc;

pub fn nothing() {}

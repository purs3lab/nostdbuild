//! Control: `#![no_std]` and no `extern crate std` at all — winter-crypto,
//! watchface. Nothing links std under any feature, so there is no condition to
//! find and no std/no_std split to make.
#![no_std]

extern crate alloc;

pub fn nothing() {}

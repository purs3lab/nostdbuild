//! Control: an ungated crate-root `extern crate std` alongside a gated one.
//! std is linked whatever the features do, so negating the gated one is not the
//! no_std condition — the ungated declaration vetoes the inference.
#![no_std]

extern crate std;

#[cfg(feature = "std")]
extern crate std as std_again;

pub fn nothing() {}

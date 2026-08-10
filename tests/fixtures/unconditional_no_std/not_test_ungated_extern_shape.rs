//! Control: the `not(test)` attribute reaches the *same* inference a bare
//! `#![no_std]` does, vetoes included. std is linked whatever the features do,
//! so there is no condition to find and nothing to negate.
#![cfg_attr(not(test), no_std)]

extern crate std;

pub fn nothing() {}

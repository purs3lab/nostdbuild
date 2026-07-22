//! Control: a crate with no `compile_error!` at all.
#![cfg_attr(not(feature = "std"), no_std)]

pub fn nothing() {}

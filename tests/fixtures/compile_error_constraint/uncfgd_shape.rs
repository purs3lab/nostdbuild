//! Control: a `compile_error!` carrying attributes none of which is a `#[cfg]`.
//!
//! There is no condition to negate, so it must contribute nothing — and must not
//! panic. `attrs.iter().find(|a| a.path().is_ident("cfg")).unwrap()` did.
#![no_std]

#[allow(unused)]
compile_error!("this crate cannot be built");

pub fn nothing() {}

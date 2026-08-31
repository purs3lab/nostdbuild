//! The same shape with the gate on an out-of-line `mod`, which is where the
//! ambient lives for most real crates: `gated.rs` carries no `#[cfg]` of its
//! own, and the only record that its contents are conditional is this
//! declaration. `ModCollector` walks the file under an inherited condition, so
//! it can see this; the flat `Attributes` walk cannot.
#![cfg_attr(not(feature = "cannot"), no_std)]

#[cfg(feature = "enable")]
mod gated;

pub fn top() {}

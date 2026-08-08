//! Control: the gate mixes a non-feature atom into `all(...)`, where erasing it
//! means assuming it *true*. `not(feature = "std")` would then forbid `std` on
//! every target, including the ones where `target_os` makes the whole gate
//! false and `std` is free. Nothing is claimed for this shape.
#![no_std]

#[cfg(all(target_os = "linux", feature = "std"))]
extern crate std;

pub fn nothing() {}

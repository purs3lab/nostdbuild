#![no_std]

/// Stands in for hashbrown / core_io / libm: the type the crate uses when `std`
/// is off. `Default` so the call site is identical in both halves.
#[derive(Default)]
pub struct Thing;

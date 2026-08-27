#![no_std]

/// Ungated, like nalgebra's `norm_squared`: the item the consumer names is not
/// the item the build is missing.
pub fn norm_squared<T: deep::ComplexField>(v: T) -> T {
    v.norm_squared()
}

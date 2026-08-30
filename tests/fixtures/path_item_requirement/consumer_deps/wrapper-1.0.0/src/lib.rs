#![no_std]

/// Ungated, like nalgebra's `norm_squared`: the item the consumer names is not
/// the item the build is missing.
pub fn helper() -> u64 {
    mathdep::now()
}

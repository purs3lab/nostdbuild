#![no_std]

pub fn wipe(v: &mut Vec<u8>) {
    // Ungated: this call is in the build whatever the feature set, so the impl
    // it resolves to is a real requirement.
    v.zeroize();
}

#[cfg(feature = "std")]
pub fn wipe_with_std(v: &mut Vec<u8>) {
    // Only compiled when `std` is on — which is the one configuration the run
    // exists to avoid. Whatever this call resolves to demands nothing of the
    // build being produced.
    v.zeroize();
}

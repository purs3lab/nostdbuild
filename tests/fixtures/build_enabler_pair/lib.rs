// proptest 1.6.0 in miniature: the bare-metal build needs TWO features at once,
// and a third candidate drags in a std-only crate.
//
// The configurations, all of them load bearing:
//
//   * `std` ON   → compiles on the HOST only (`extern crate std` resolves
//                  nowhere else), so no covering run is ever bare metal;
//   * `[]`       → no `Vec` and no `mathshim::sqrt`;
//   * `[alloc]`  → still no `sqrt`;
//   * `[nostd_math]` → still no `Vec`;
//   * `[alloc, nostd_math]` → COMPILES for a bare-metal target — the answer,
//                  and unreachable by a search that tries one feature at a time;
//   * all candidates on → `hostdep` links `hostonly`, which is plain std, so
//                  the all-on trial fails for a reason unrelated to the pair.

#![no_std]

#[cfg(feature = "std")]
extern crate std;

#[cfg(feature = "alloc")]
extern crate alloc;

// proptest's `multiplex_alloc!` shape, written out: the same name comes from
// `alloc` or from `std`, and neither exists in the empty configuration.
#[cfg(all(feature = "alloc", not(feature = "std")))]
use alloc::vec::Vec;
#[cfg(feature = "std")]
use std::vec::Vec;

#[cfg(feature = "std")]
pub fn label() -> std::string::String {
    std::string::String::new()
}

pub fn buffer() -> Vec<u8> {
    Vec::new()
}

/// Ungated, so it is in every configuration — this is what makes the *second*
/// half of the pair load bearing without it appearing in any `#[cfg]`.
pub fn root(x: f64) -> f64 {
    mathshim::sqrt(x)
}

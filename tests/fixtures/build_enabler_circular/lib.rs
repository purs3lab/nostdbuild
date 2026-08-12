// proptest 1.6.0's circularity in miniature.
//
// `mod facade` is proptest's `#[cfg(any(feature = "std", feature = "alloc"))]
// mod _alloc;`. The only configuration that compiles anywhere is `std`, and
// only on the host, so `Thing` there resolves to `std::cell::Cell` and — with
// no std-off run to contradict it — is `AlwaysStd`. Negating that span's gate,
// `std ∨ alloc`, yields `¬std ∧ ¬alloc`, which struck `alloc` off the enabler
// candidate list. `alloc` is half of what makes this crate build for a
// bare-metal target, so the search was forbidden from proposing what it was
// looking for.
//
// The configurations, all load bearing:
//
//   * `[std]`                  → host only (`extern crate std` resolves nowhere else);
//   * `[]`                     → no `make`, and no `mathshim::sqrt`;
//   * `[alloc]`                → `make` exists, still no `sqrt` — this is the
//                                covering run the tree's own cfgs produce, and
//                                it must FAIL, or the search never runs;
//   * `[nostd_math]`           → `sqrt` exists, but the ungated call to `make` does not;
//   * `[alloc, nostd_math]`    → COMPILES for a bare-metal target.

#![no_std]

#[cfg(feature = "std")]
extern crate std;

#[cfg(any(feature = "std", feature = "alloc"))]
mod facade {
    // The multiplex: the same name is std's or core's depending on the build.
    #[cfg(feature = "std")]
    pub use std::cell::Cell as Thing;
    #[cfg(not(feature = "std"))]
    pub use core::cell::Cell as Thing;

    /// Ungated *inside* the gated module, so the only gate this span carries is
    /// the module's `any(std, alloc)` — exactly proptest's `_alloc` spans.
    pub fn make() -> Thing<u8> {
        Thing::new(0)
    }
}

#[cfg(any(feature = "std", feature = "alloc"))]
pub use facade::make;

/// Ungated, and it needs both halves: `make` exists only under `std ∨ alloc`,
/// and `mathshim::sqrt` only once a backend is selected.
pub fn root(x: f64) -> f64 {
    let cell = make();
    mathshim::sqrt(x) + f64::from(cell.get())
}

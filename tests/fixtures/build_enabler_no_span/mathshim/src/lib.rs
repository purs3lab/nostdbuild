#![no_std]

/// `num-traits` in miniature: `sqrt` exists only once a backend is selected, so
/// a dependent that turns neither on fails to resolve the call. No
/// `compile_error!` — the parent's analysis must not be able to see the rule,
/// which is the whole point of the fixture.
#[cfg(any(feature = "std", feature = "libm"))]
pub fn sqrt(x: f64) -> f64 {
    x
}

#![no_std]

/// `micromath::F32Ext` in miniature: the trait exists only once a backend is
/// selected, so a dependent that turns neither on cannot even resolve the
/// import. No `compile_error!` — the parent's analysis must not be able to see
/// the rule, which is the whole point of the fixture.
#[cfg(any(feature = "std", feature = "libm"))]
pub trait F32Ext {
    fn round(self) -> f32;
}

#[cfg(any(feature = "std", feature = "libm"))]
impl F32Ext for f32 {
    fn round(self) -> f32 {
        self
    }
}

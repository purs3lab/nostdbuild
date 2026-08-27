#![no_std]

pub trait Zeroize {
    fn zeroize(&mut self);
}

pub trait DefaultIsZeroes: Default {}

// The impl the consumer's call needs, and the arm a std-on pass selects.
// Nothing names it: the method the consumer writes (`zeroize`) is gated by
// nothing at all, and this block has no identifier to match — the KI-27 shape.
#[cfg(feature = "alloc")]
impl<Z> Zeroize for Vec<Z> {
    fn zeroize(&mut self) {}
}

// The same `(trait, self type)` behind a different feature. A requirement that
// asserted the gate of the impl the compiler happened to select would rule this
// arm out; the disjunction is what lets the solve reach it.
#[cfg(all(feature = "portable", not(feature = "alloc")))]
impl<Z> Zeroize for Vec<Z> {
    fn zeroize(&mut self) {}
}

// A blanket impl of the same trait, ungated. Coherence says it cannot also apply
// to `Vec` — one of the two would be an overlap — so it must not be read as
// making the requirement above free.
impl<Z: DefaultIsZeroes> Zeroize for Z {
    fn zeroize(&mut self) {}
}

// A different self type behind a different feature: `simd` must never be
// proposed for a `Vec` obligation.
#[cfg(feature = "simd")]
impl Zeroize for u128 {
    fn zeroize(&mut self) {}
}

// Unconditional: a call resolving here needs no feature at all.
impl Zeroize for Slice {
    fn zeroize(&mut self) {}
}

// Not a trait impl. An inherent block has no trait to key on and must not turn
// up among the alternatives.
impl Slice {
    #[cfg(feature = "simd")]
    pub fn wide(&self) {}
}

pub struct Slice;
pub struct CString;

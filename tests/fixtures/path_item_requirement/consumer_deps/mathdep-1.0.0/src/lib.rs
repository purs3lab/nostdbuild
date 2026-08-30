#![no_std]

pub mod float {
    // The earcut case, and num-traits' own shape: the trait the consumer
    // imports exists only under the disjunction of two features. A covering run
    // with `std` on resolves the import here — a run with neither on does not
    // compile at all, which is why the record can only ever come from a pass
    // that selected the arm the no_std build must not take.
    #[cfg(any(feature = "std", feature = "libm"))]
    pub trait Float {
        fn sqrt(self) -> Self;
    }

    // Ungated. Naming this requires nothing, and a requirement invented for it
    // would fix a feature the solve was free to choose.
    pub trait NumCast {
        fn cast(self) -> Self;
    }
}

// Two complementary arms of one name. One of them is always there, so naming it
// requires nothing — the item-level counterpart of an unconditional impl.
#[cfg(feature = "std")]
pub fn now() -> u64 {
    0
}

#[cfg(not(feature = "std"))]
pub fn now() -> u64 {
    1
}

// A single arm with no alternative: the requirement is exactly this feature.
#[cfg(feature = "serde")]
pub struct Serializer;

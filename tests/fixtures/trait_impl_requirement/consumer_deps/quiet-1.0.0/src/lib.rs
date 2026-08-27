#![no_std]

pub trait Zeroize {
    fn zeroize(&mut self);
}

// The same trait name and the same self type as `gated`, gated on this crate's
// own `alloc`. A record naming `gated` must not pick this up.
#[cfg(feature = "alloc")]
impl<Z> Zeroize for Vec<Z> {
    fn zeroize(&mut self) {}
}

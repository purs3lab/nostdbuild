// core2 in miniature: an io module that is the real `std::io` when the shim's
// own `std` feature is on, and a core-only stand-in when it is off.
#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(feature = "std")]
pub use std::io;

#[cfg(not(feature = "std"))]
pub mod io {
    /// The no_std stand-in. Never reached in this fixture: nothing turns the
    /// shim's default `std` feature off, which is the whole point.
    pub struct Error;

    pub type Result<T> = core::result::Result<T, Error>;

    pub trait Write {
        fn flush(&mut self) -> Result<()>;
    }
}

// bitwrap_extra-2.0.6 in miniature — the shape that filled `PROBE_SET_INFEASIBLE`.
//
// `impl std::error::Error for E` is std in every covering run, so it is probed by
// negating its only gate, `std`. There is exactly one configuration that negates
// it (`{}`), and that configuration does not compile: `pack` returns a `Vec`,
// which without std is in neither the prelude nor any import the crate makes.
//
// So the probe returns no evidence and the crate is reported unproven. That is the
// right verdict; the point of the test is that it must arrive with the compiler's
// reason attached, because the reason is what says this is the crate's problem and
// not a feature set the tool chose badly.
#![cfg_attr(not(feature = "std"), no_std)]

use core::fmt;

pub struct E;

impl fmt::Display for E {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "e")
    }
}

impl fmt::Debug for E {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "E")
    }
}

#[cfg(feature = "std")]
impl std::error::Error for E {}

// The author's omission: no `extern crate alloc` and no `use alloc::vec::Vec`, so
// this line only resolves while std supplies the prelude.
pub fn pack() -> Result<Vec<u8>, E> {
    Ok(Vec::new())
}

fn main() {}

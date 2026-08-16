// euclid 0.22.11 in miniature — R31-5's larger half.
//
// The crate is `#![no_std]` outright and names `std` nowhere, so:
//
//   * its no_std condition is `true` and the classifier finds not one std span;
//   * with `libm` off it still compiles NOWHERE — `mathshim::sqrt` does not
//     exist until one of the shim's backends is selected.
//
// So there is nothing to prove and nothing that builds, which is precisely the
// combination the enabler search used to be gated out of: no `AlwaysStd` span
// meant "nothing to gain", and the crate shipped `--no-default-features` with
// `libm = ["mathshim/libm"]` sitting unused in its own manifest.

#![no_std]

/// Ungated, so it is in every configuration: this is what makes `libm` load
/// bearing without `libm` ever appearing in a `#[cfg]`.
pub fn root(x: f64) -> f64 {
    mathshim::sqrt(x)
}

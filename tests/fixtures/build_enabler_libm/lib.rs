// bevy_input / totsu_core in miniature.
//
// Two facts have to hold at once for this to reproduce bucket T2:
//
//   * with `std` ON the crate compiles on the HOST and nowhere else — the
//     `extern crate std` below has nothing to resolve against on bare metal;
//   * with `std` OFF and `libm` OFF it compiles NOWHERE — `mathshim::sqrt` does
//     not exist until one of the shim's backends is selected.
//
// So no covering run ever compiles for a bare-metal target, the only records
// come from the host, and `String` is std in every surviving run. Every probe
// that negates `feature = "std"` then fails to compile as well, and the span
// leaves as `unproven` — unless the enabler search finds `libm` first.

#![no_std]

#[cfg(feature = "std")]
extern crate std;

#[cfg(feature = "std")]
pub fn label() -> std::string::String {
    std::string::String::new()
}

/// Ungated, so it is in every configuration: this is what makes `libm` load
/// bearing without `libm` ever appearing in a `#[cfg]`.
pub fn root(x: f64) -> f64 {
    mathshim::sqrt(x)
}

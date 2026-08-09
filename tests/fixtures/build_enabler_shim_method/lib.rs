// xmrs 0.9.9 in miniature.
//
// Three facts have to hold at once:
//
//   * with `std` ON the crate compiles on the HOST and nowhere else — the
//     `extern crate std` below has nothing to resolve against on bare metal;
//   * with `std` OFF and `libm` OFF it compiles NOWHERE — `f32shim::F32Ext`
//     does not exist until one of the shim's backends is selected;
//   * with `std` OFF and `libm` ON it compiles for a bare-metal target, and
//     `nearest` binds the shim's trait method instead of std's inherent one.
//
// So the only covering run that survives has `std` on, `nearest` is std in
// every run, and — being ungated — `initial_ungated_results` short-circuits its
// probe to `StillStd` without compiling anything. The third configuration is
// the answer and only the enabler search ever compiles it.

#![no_std]

#[cfg(feature = "std")]
extern crate std;

/// A gated std span. Its purpose is to give the enabler search a `¬std` gate to
/// avoid: without one, `std` is a perfectly good answer to "what makes this
/// crate compile" and the search would offer it.
#[cfg(feature = "std")]
pub fn label() -> std::string::String {
    std::string::String::new()
}

#[allow(unused_imports)]
use f32shim::F32Ext;

/// The span that needs the run, not a probe. No `#[cfg]` anywhere on it: with
/// std linked, `round` binds std's *inherent* `f32::round` (inherent beats
/// trait) and resolves to std; with std off and the shim on, the same
/// expression binds `F32Ext::round` and resolves to `f32shim`.
pub fn nearest(x: f32) -> f32 {
    x.round()
}

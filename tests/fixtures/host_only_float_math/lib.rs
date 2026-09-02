// R34-3 class B: `f64::sqrt` is a method call with nothing binding `f64` — "the
// owner is a primitive, nothing binds `f32`, so there is no import whose cfg
// the record could inherit" (see `MethodResolver::record`'s comment). Ungated
// by construction, so it is short-circuited to `StillStd` on any run
// regardless of feature — the unused `shim` dependency exists only to make
// every bare-metal target fail before this crate is compiled (its own default
// `std` feature is never turned off), so the only evidence for this span is a
// host build. Confirmed against real rustc (`tuit 0.2.1`): with std off this
// fails `E0599: no method named 'sqrt' found for type 'f32'`, not a
// resolution into `core` — real evidence the item needs std here, and the
// most this crate's own probing can honestly say is "unproven".
#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(feature = "std")]
extern crate std;

pub fn root(x: f64) -> f64 {
    x.sqrt()
}

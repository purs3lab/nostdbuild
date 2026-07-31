// The crate has two halves: with `std` on it uses `std::string::String`, with
// `std` off it uses a type from `shim`, an OPTIONAL dependency.
//
// Nothing in `[features]` connects the absence of `std` to `shim`, so the solver
// used to answer the "¬std" covering set with `--no-default-features` and nothing
// else. Cargo accepts that set and rustc then rejects the crate
// (`E0432: unresolved import shim`), so the run is thrown away and the std run is
// the only survivor — which is what makes the `String` import look unavoidable.
//
// With the cfg⇒optional-dep edge the ¬std set becomes
// `--no-default-features --features shim`, which compiles, and the crate has a
// working no_std configuration again.

#[cfg(not(feature = "std"))]
use shim::Thing;

#[cfg(feature = "std")]
use std::string::String as Thing;

fn main() {
    let _t: Thing = Default::default();
}

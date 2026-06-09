// This fixture exists to exercise the CEGAR retry loop in probe_one_target.
//
// The `std` feature gates the use of std::error::Error (always-std import).
// The `nightly` feature is declared incompatible with no-std via compile_error!,
// mimicking the real-world pattern where enabling one feature pulls in std
// at compile time even when the `std` feature flag is off.
//
// Probing behaviour:
//   When negating the `std` gate, Z3 greedily picks `{nightly=on, std=off}` first
//   (because `nightly` appears in all_constraints and is compatible with ¬std in Z3).
//   That compile attempt hits compile_error! → fail.
//   CEGAR blocks `{nightly=on, std=off}`, retries → Z3 finds `{nightly=off, std=off}`.
//   That compile succeeds and shows the Error impl absent → NonStd verdict.

// Simulates an incompatible feature combination (nightly requires std at the Cargo level).
#[cfg(all(feature = "nightly", not(feature = "std")))]
compile_error!("nightly feature requires std to be enabled");

#[cfg(feature = "std")]
use std::error::Error;

pub struct ParseError;

#[cfg(feature = "std")]
impl Error for ParseError {}

// This function's cfg gate (`nightly`) must appear in the AST so that
// `all_constraints` includes `(or nightly)` and Z3 can pick nightly=on.
#[cfg(feature = "nightly")]
pub fn nightly_helper() {}

fn main() {}

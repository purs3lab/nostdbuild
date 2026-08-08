//! lyon_tessellation 1.0.15 shape (src/lib.rs:6 and :188): the same, with the
//! gate mixing `test` into an `any(...)`.
//!
//! Policy G erases `test` by dropping the operand, which under `any(...)` is
//! assuming it false — true of every build the tool asks cargo for, so the gate
//! is `feature = "std"` exactly and its negation is the no_std condition. This
//! is bucket 3C's argument (346f239) in the negated position.
#![no_std]

#[cfg(any(test, feature = "std"))]
extern crate std;

pub fn nothing() {}

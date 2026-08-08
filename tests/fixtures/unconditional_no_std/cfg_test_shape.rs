//! Control: elfloader 0.16.0's shape — `extern crate std` for the test harness
//! only. `should_skip` drops `#[cfg(test)]` items before they are ever recorded,
//! so no gate is seen and nothing is inferred. The crate is no_std in every
//! configuration cargo will build here.
#![no_std]

#[cfg(test)]
#[macro_use]
extern crate std;

pub fn nothing() {}

//! Control: the `extern crate std` lives inside an *inline* `mod` block, which
//! is that module's own binding and not the file's. Only a file's top level
//! counts (`condition_stack.len() == 1`).
//!
//! ⚠ This is no longer tinywasm 0.8.0's shape, despite what this comment used to
//! say. tinywasm's declaration is in `src/std.rs`, a *file* module reached
//! unconditionally, and that one does yield a condition — see
//! `submodule_facade`.
#![no_std]

pub mod shim {
    #[cfg(feature = "std")]
    extern crate std;
}

pub fn nothing() {}

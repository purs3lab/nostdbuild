//! Control: tinywasm 0.8.0's shape, moved into one file — the `extern crate
//! std` lives inside a module, not at the crate root. An `extern crate` binds
//! the name in the module that declares it, so this one says nothing about
//! whether the crate as a whole links std.
#![no_std]

pub mod shim {
    #[cfg(feature = "std")]
    extern crate std;
}

pub fn nothing() {}

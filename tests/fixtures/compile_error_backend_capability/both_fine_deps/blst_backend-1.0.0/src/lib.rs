//! Same shape as `blst_backend`, but this one also supports no_std — the
//! negative control: nothing should be forbidden when every named backend
//! can build no_std.
#![no_std]

pub fn via_blst() {}

//! curve25519-dalek's shape: the crate root says nothing, and the
//! `compile_error!` that decides whether the build starts at all sits in a
//! submodule.
#![no_std]

pub(crate) mod backend;

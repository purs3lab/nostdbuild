#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(all(feature = "libm", not(feature = "std")))]
mod nostd_float;
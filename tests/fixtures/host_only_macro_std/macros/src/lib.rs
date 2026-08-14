// bp-runtime in miniature. Two jobs, both of them the real crate's:
//
//  1. it fails on every bare-metal target, because its own default `std` feature
//     is on and nothing in the dependent's manifest turns it off — so the
//     dependent never gets compiled for one, and every covering run it has is a
//     host build (`std_inconclusive`);
//  2. it exports a macro that writes `std::result::Result` into the *caller's*
//     source, which is what `decl_bridge_finality_runtime_apis!` does.
#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(feature = "std")]
pub use std::io;

/// `decl_bridge_finality_runtime_apis!(wococo, grandpa)`, boiled down to the one
/// thing that matters here: the expansion spells `std` and the caller does not.
/// The record lands at the invocation span carrying
/// `expansion_crate: Some("macros")`.
#[macro_export]
macro_rules! declare_api {
    ($name:ident) => {
        pub fn $name(value: u8) -> std::result::Result<u8, ()> {
            Ok(value)
        }
    };
}

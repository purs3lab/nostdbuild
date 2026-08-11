// bebytes_derive's own header. With `std` off the crate is no_std and its own
// `use std::…` no longer resolves — `E0433: use of unresolved module or unlinked
// crate std` at bebytes_derive/src/bit_validation.rs:5 is exactly this.
#![cfg_attr(not(feature = "std"), no_std)]

extern crate proc_macro;

use proc_macro::TokenStream;
use std::string::String;

/// Nothing std is injected: the expansion is core-only, so the consumer has no
/// reason to touch this macro's features at all.
#[proc_macro_derive(Plain)]
pub fn plain(_input: TokenStream) -> TokenStream {
    let expansion = String::from("pub const fn injected() -> u8 { 0 }");
    expansion.parse().expect("the injected tokens parse")
}

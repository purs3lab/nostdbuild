#![cfg_attr(not(feature = "std"), no_std)]

extern crate proc_macro;

use proc_macro::TokenStream;
use std::string::String;

/// The expansion puts std in the consumer, so the attribution half of the
/// evidence fires — and the build half then refuses the only parking available,
/// because with `std` off this macro does not compile at all.
#[proc_macro_derive(Inject)]
pub fn inject(_input: TokenStream) -> TokenStream {
    let expansion = String::from(
        "extern crate std;
         trait PathToNeedsStd {
             fn injected(&self) -> std::string::String;
         }",
    );
    expansion.parse().expect("the injected tokens parse")
}

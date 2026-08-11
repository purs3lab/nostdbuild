extern crate proc_macro;

use proc_macro::TokenStream;

/// displaydoc's `specialization()` in miniature: with the `std` feature on, the
/// expansion carries `extern crate std` and a std path *into the consumer*; with
/// it off it emits nothing. `cfg!` rather than `#[cfg]` on purpose — this macro
/// compiles with the feature either way, so what the parking trial measures is
/// the expansion, not the macro's own build.
#[proc_macro_derive(Inject)]
pub fn inject(_input: TokenStream) -> TokenStream {
    if cfg!(feature = "std") {
        "extern crate std;
         trait PathToInjector {
             fn injected(&self) -> std::string::String;
         }"
        .parse()
        .expect("the injected tokens parse")
    } else {
        TokenStream::new()
    }
}

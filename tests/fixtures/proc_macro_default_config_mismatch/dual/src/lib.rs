extern crate proc_macro;

use proc_macro::TokenStream;

/// `std` on: the consumer gets std. `std` off: the consumer gets the no_std
/// flavour — which, exactly like packed_struct_codegen's `::core::result::Result`
/// in an edition-2015 std crate, is not something the consumer's *std* build can
/// necessarily compile. The macro is fine either way; the question the trial fails
/// to answer is about a configuration nobody asked about.
#[proc_macro_derive(Dual)]
pub fn dual(_input: TokenStream) -> TokenStream {
    if cfg!(feature = "std") {
        "extern crate std;
         trait PathToDual {
             fn dual(&self) -> std::string::String;
         }"
        .parse()
        .expect("the injected tokens parse")
    } else {
        "pub fn dual_receipt() -> crate::AltReceipt { crate::AltReceipt }"
            .parse()
            .expect("the injected tokens parse")
    }
}

// Exercises the prefix-match logic in resolve_local_facade_gateways.
// `extern crate std` is declared three levels down (crate::a::b::c).
// Usage of `crate::a::b::c::SomeType` must be recognised as going through
// the `std` facade, not as a local-only path — but only when the `std`
// feature is enabled.

pub(crate) mod a {
    pub(crate) mod b {
        pub(crate) mod c {
            #[cfg(feature = "std")]
            extern crate std;
            #[cfg(feature = "std")]
            pub(crate) use std::error::Error;
        }
    }
}

struct ParseError;

#[cfg(feature = "std")]
impl crate::a::b::c::Error for ParseError {}

fn main() {}

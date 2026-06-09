// Mimics the tinywasm pattern: a local `mod std_shim` wraps the real `std`
// crate behind explicit `extern crate` declarations in nested submodules.
// The HIR resolver sees `crate::std_shim::error` as a local module and
// resolves `Error` to its canonical definition in `core`.  The post-processing
// step in `resolve_local_facade_gateways` must recover `std` as the gateway.

pub(crate) mod std_shim {
    #[cfg(feature = "std")]
    extern crate std;
    #[cfg(feature = "std")]
    pub(crate) use std::*;

    pub(crate) mod error {
        #[cfg(feature = "std")]
        extern crate std;
        #[cfg(feature = "std")]
        pub(crate) use std::error::Error;
    }
}

struct ParseError;

#[cfg(feature = "std")]
impl crate::std_shim::error::Error for ParseError {}

fn main() {}

#[cfg(not(feature = "use_std"))]
extern crate core as core;
#[cfg(feature = "use_std")]
extern crate std as core;

fn main() {
    let err: core::error::Error;
}

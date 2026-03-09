#[cfg(feature = "make_conflicting")]
pub use std::error::Error;
#[cfg(not(feature = "make_conflicting"))]
pub struct Error;

fn main() {


}
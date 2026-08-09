//! The facade, as nate-common writes it: `std` names the real std when the
//! feature is on and `core` when it is off, so every bare `std::` path in the
//! crate is core under `¬std`.

#[cfg(feature = "std")]
pub extern crate std;

#[cfg(not(feature = "std"))]
pub extern crate core as std;

pub fn nothing() {}

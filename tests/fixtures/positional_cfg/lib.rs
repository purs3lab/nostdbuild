// Fixture: a bare positional `cfg(...)` macro argument gates the invocation's
// generated output (among-0.1.7 `impl_specific_ref_and_mut!` pattern). The std
// type and its `cfg(...)` guard are sibling positional args; the guard is NOT a
// leading `#[cfg(...)]` attribute, so it must be recognised as a positional cfg.

macro_rules! impl_specific {
    ($t:ty, $($attr:meta),* ) => {
        $(#[$attr])*
        impl AsThing for $t {}
    };
}

pub trait AsThing {}

// std type gated by a positional `cfg(feature = "std")` sibling arg.
impl_specific!(::std::path::Path, cfg(feature = "std"), doc = "requires std");

// Control: a std type in an invocation with NO cfg arg must stay ungated.
impl_specific!(::std::sync::Arc<u8>,);

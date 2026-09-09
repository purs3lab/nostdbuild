// KI-36 in miniature. `custom` and `real` are both arms that make `Text`
// exist off the host — `custom` in the shape `clock_source-0.2.4` actually
// ships (a self-registration hook only a final binary could really complete;
// this fixture does not need to model the extern hook itself, only that it
// compiles as a lib unconditionally), `real` as an ordinary no_std-safe
// definition. An enabler search that still considered `custom` would find it
// first — it sorts before `real` — so the fix has to remove it from the
// candidate pool entirely, not just deprioritize it.

#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(feature = "std")]
extern crate std;

#[cfg(feature = "std")]
pub type Text = std::string::String;

#[cfg(all(feature = "custom", not(feature = "std")))]
pub type Text = &'static str;

#[cfg(all(feature = "real", not(feature = "std")))]
pub type Text = &'static str;

/// Ungated, so it is in every configuration — and it names a type that
/// exists only under `std`, `custom`, or `real`. With all three off, `Text`
/// is not a name at all (`E0412`), which is the configuration the no_std
/// solve emits, because nothing in this crate's own `#[cfg]`s mentions
/// either bare-metal arm.
pub fn label(t: Text) -> Text {
    t
}

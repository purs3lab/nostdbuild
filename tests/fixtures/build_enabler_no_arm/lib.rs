// KI-30 in miniature.
//
// Three facts have to hold at once:
//
//   * with `std` ON the crate compiles on the HOST and nowhere else;
//   * with `embedded` ON it compiles for a bare-metal target — so the crate
//     *has* reached bare metal and `CRATE_REACHED_BARE_METAL` is true, which is
//     what closes the enabler search's gate during analysis;
//   * with BOTH OFF it compiles nowhere: `Text` is not a name at all
//     (`E0412 cannot find type Text`), which is the configuration the no_std
//     solve then emits, because nothing in it mentions `embedded`.
//
// mavlink-core 0.13.1 is the real one: `Read`/`Write` come from `std::io` under
// `feature = "std"` and from its own `embedded` module under
// `feature = "embedded"` / `"embedded-hal-02"`, the uses are ungated, and the
// emitted config has none of the three.

#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(feature = "std")]
extern crate std;

#[cfg(feature = "std")]
pub type Text = std::string::String;

#[cfg(all(feature = "embedded", not(feature = "std")))]
mod embedded;
#[cfg(all(feature = "embedded", not(feature = "std")))]
pub use embedded::Text;

/// Ungated, so it is in every configuration — and it names a type that exists
/// only under an arm. This is what the analysis cannot see: no `#[cfg]` of this
/// crate mentions `embedded`, so no covering-set or probe solve has an opinion
/// about it, and every model leaves it off.
pub fn label(t: Text) -> Text {
    t
}

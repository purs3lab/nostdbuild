// Fixture: `#[cfg(...)] include!("path")` — the cfg on the include! statement must
// gate everything the included file contributes, and the included file's spans must
// be reachable at all (they live in a different file than the including node).
// See bigdecimal-0.4.8: `#[cfg(feature = "std")] include!("./with_std.rs");`

#[cfg(feature = "std")]
include!("./inc_gated.rs");

include!("./inc_ungated.rs");

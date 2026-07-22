#![no_std]

// A module whose name is a keyword, declared with a raw identifier. Rust resolves
// this to the file `move/mod.rs` (the *unraw* name), not `r#move/mod.rs`. syn's
// `Ident::to_string()` keeps the `r#`, so without stripping it the whole subtree
// is never walked and any std usage inside it looks unguarded. (ref_kind's
// `mod r#move;`.)
#[cfg(feature = "std")]
extern crate std as std_crate;

mod r#move;

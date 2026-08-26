#![feature(rustc_private)]

//! `filter_equations` decides which of a crate's `#[cfg]` conditions constrain
//! the no_std solve. It used to decide by asking whether the Z3 AST's *rendered
//! text* contained the feature name, which is a substring test, and feature
//! names are not substring-free.
//!
//! `bitcoin 0.32` carries
//! `#[cfg(all(feature = "secp-recovery", feature = "base64", feature = "rand-std"))]`.
//! It names no `std` feature. But `"rand-std".contains("std")` held, so the
//! equation was kept for a crate whose no_std condition is `not(feature = "std")`,
//! `find_possible_equations` found it compatible, and `solve` asserted it — which
//! made all three features *entailed true*. The run's own log said
//! `entailed true=["rand-std", "secp-recovery", "base64"]`, `rand-std` was written
//! onto the dependency edge as a no_std requirement, and it links `std`: every
//! target failed with `E0463 can't find crate for std`.
//!
//! Corpus-wide, 106 manifest writes and 68 "requires feature X for no_std" claims
//! name a feature that contains `std` without being `std`: `hashes-std`,
//! `rustc-dep-of-std`, `spin_no_std`, `derive_clone_dyn_use_std`.
//!
//! The fix carries the parsed feature atoms out of `parse_attributes` alongside
//! each equation and tests membership. These tests pin that: a feature only
//! answers to its own name.

use nostd::Attributes;
use nostd::parser::{filter_equations, parse_attributes};
use syn::visit::Visit;

/// Collect the crate-level `#[cfg]` attributes the solver would see.
fn attrs_of(src: &str) -> Attributes {
    let file: syn::File = syn::parse_str(src).expect("fixture source parses");
    let mut attrs = Attributes::default();
    attrs.visit_file(&file);
    attrs
}

/// The equations kept for a crate whose no_std condition names `main_features`.
fn kept(src: &str, main_features: &[&str]) -> Vec<String> {
    let ctx = z3::Context::new(&z3::Config::new());
    let equations = parse_attributes(&attrs_of(src), &ctx);
    let feats: Vec<String> = main_features.iter().map(|s| s.to_string()).collect();
    filter_equations(&equations, &feats)
        .iter()
        .map(|e| e.to_string())
        .collect()
}

#[test]
fn a_feature_whose_name_merely_contains_std_is_not_std() {
    // bitcoin 0.32's gate, reduced. Nothing here is the `std` feature.
    let src = r#"
        #[cfg(all(feature = "secp-recovery", feature = "base64", feature = "rand-std"))]
        pub fn sign() {}
    "#;
    assert!(
        kept(src, &["std"]).is_empty(),
        "an equation naming only `rand-std`/`base64`/`secp-recovery` must not be \
         kept for a crate gated on `std` — asserting it makes `rand-std` a no_std \
         requirement, and `rand-std` links std"
    );
}

#[test]
fn the_std_feature_itself_is_still_matched() {
    // The guard on over-correcting: a gate that really does name `std` must
    // still constrain the solve, or the no_std condition stops being read at all.
    let src = r#"
        #[cfg(all(feature = "encoding", feature = "std"))]
        pub fn encode() {}
    "#;
    assert_eq!(
        kept(src, &["std"]).len(),
        1,
        "a gate naming the `std` feature must still be kept"
    );
}

#[test]
fn each_of_the_corpus_std_lookalikes_is_rejected() {
    // The names the corpus scan turned up, each in a gate that names no `std`.
    for lookalike in [
        "hashes-std",
        "rustc-dep-of-std",
        "spin_no_std",
        "derive_clone_dyn_use_std",
        "bitcoin-hashes-std",
        "no-std",
    ] {
        let src = format!(
            r#"
            #[cfg(all(feature = "alloc", feature = "{lookalike}"))]
            pub fn f() {{}}
            "#
        );
        assert!(
            kept(&src, &["std"]).is_empty(),
            "`{lookalike}` must not answer to `std`"
        );
    }
}

#[test]
fn a_lookalike_alongside_the_real_feature_is_still_kept() {
    // Membership is per-atom, not per-equation-shape: `rand-std` being present
    // is not a reason to drop a gate that *also* names `std`.
    let src = r#"
        #[cfg(all(feature = "rand-std", feature = "std"))]
        pub fn f() {}
    "#;
    assert_eq!(
        kept(src, &["std"]).len(),
        1,
        "a gate naming both `rand-std` and `std` names `std`, and must be kept"
    );
}

#[test]
fn matching_is_per_feature_not_per_prefix() {
    // The mirror of the `rand-std` case: a crate gated on `alloc` must not pick
    // up `alloc_cow`, and one gated on `use_alloc` must not pick up `alloc`.
    let src = r#"
        #[cfg(all(feature = "alloc_cow", feature = "serde"))]
        pub fn f() {}
    "#;
    assert!(
        kept(src, &["alloc"]).is_empty(),
        "`alloc_cow` must not answer to `alloc`"
    );

    let src = r#"
        #[cfg(all(feature = "alloc", feature = "serde"))]
        pub fn f() {}
    "#;
    assert!(
        kept(src, &["use_alloc"]).is_empty(),
        "`alloc` must not answer to `use_alloc`"
    );
}

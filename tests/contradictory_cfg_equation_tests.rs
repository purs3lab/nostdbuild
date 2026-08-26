#![feature(rustc_private)]

//! A `#[cfg]` states *when code exists*. `solver::solve` asserts every equation it
//! is handed, which reads the same attribute as *what the configuration must
//! satisfy* — and under a `not(feature = "std")` hard constraint the ubiquitous
//! `#[cfg(any(feature = "std", feature = "X"))]` collapses to "X must be on".
//!
//! Most of the time that is merely over-eager: `alloc` and `libm` usually *are*
//! what the no_std path wants, and `driver::discover_build_enablers` is the thing
//! that proves it by compiling. One class is never right, though — an assertion
//! that turns on a feature the crate's own no_std condition forbids. It states
//! that the crate is no_std exactly when it enables std.
//!
//! `bitcoin 0.32` is the case:
//! `#[cfg(all(feature = "secp-recovery", feature = "base64", feature = "rand-std"))]`
//! made all three *entailed true* (the run log said so verbatim), and
//! `rand-std = ["std", ...]`. `rand-std` was then written onto the dependency edge
//! as a no_std requirement and every target died on
//! `E0463 can't find crate for std`. 87 such writes corpus-wide, 57 of them crates
//! that build nothing at all.
//!
//! These tests pin `solver::features_forced_true`, the entailment query the drop
//! is built on, and its composition with `parser::reaches_forbidden_feature`.

use nostd::parser::reaches_forbidden_feature;
use nostd::solver::features_forced_true;
use nostd::{CrateInfo, types::TupleVec};
use z3::ast::Bool;

fn feat(name: &str, values: &[(&str, &str)]) -> (String, TupleVec) {
    (
        name.to_string(),
        values
            .iter()
            .map(|(d, f)| (d.to_string(), f.to_string()))
            .collect(),
    )
}

/// A crate whose `[features]` table is `rand-std = ["std", ...]`, like bitcoin's.
fn bitcoin_like() -> CrateInfo {
    CrateInfo {
        name: "bitcoin".to_string(),
        version: "0.32.102".to_string(),
        features: vec![
            feat("std", &[]),
            // bitcoin's real entry is `rand-std = ["secp256k1/rand-std", "std"]`.
            // `downloader::read_local_features` splits each value on `/`, so a
            // forwarded value becomes `(dep, feat)` and a *bare local* one becomes
            // `(name, name)` — which is exactly the `dep_name == qualifier` test
            // `close_over_local_features` uses to recognise it.
            feat("rand-std", &[("secp256k1", "rand-std"), ("std", "std")]),
            feat("secp-recovery", &[]),
            feat("base64", &[]),
            feat("alloc", &[]),
        ],
        ..Default::default()
    }
}

#[test]
fn an_and_over_three_features_forces_all_three() {
    // The exact shape of bitcoin's gate. Asserting it entails every conjunct.
    let ctx = z3::Context::new(&z3::Config::new());
    let (a, b, c) = (
        Bool::new_const(&ctx, "secp-recovery"),
        Bool::new_const(&ctx, "base64"),
        Bool::new_const(&ctx, "rand-std"),
    );
    let gate = Bool::and(&ctx, &[&a, &b, &c]);
    let not_std = Bool::new_const(&ctx, "std").not();

    let declared: Vec<String> = bitcoin_like()
        .features
        .iter()
        .map(|(n, _)| n.clone())
        .collect();
    let mut forced = features_forced_true(&ctx, &[not_std], &gate, &declared);
    forced.sort();
    assert_eq!(
        forced,
        vec![
            "base64".to_string(),
            "rand-std".to_string(),
            "secp-recovery".to_string()
        ],
        "asserting an `all(..)` gate must entail each feature it names"
    );
}

#[test]
fn a_forced_feature_that_enables_std_is_recognised_as_contradictory() {
    // The composition the drop is built on: `rand-std` reaches `std` through the
    // crate's own table, and `std` is what the no_std condition forbids.
    let info = bitcoin_like();
    let forbidden = vec!["std".to_string()];
    assert!(
        reaches_forbidden_feature(&info, "rand-std", &forbidden),
        "`rand-std = [\"std\"]` reaches the forbidden `std`"
    );
    assert!(
        !reaches_forbidden_feature(&info, "secp-recovery", &forbidden),
        "`secp-recovery` gates no std and must not be judged contradictory — the \
         drop has to be narrow or it takes the innocent conjuncts with it"
    );
}

#[test]
fn an_any_gate_with_std_forces_the_other_arm() {
    // num-complex 0.4.6's `#[cfg(any(feature = "std", feature = "libm"))]`, which
    // is how a crate that is `#![no_std]` unconditionally came to "require libm".
    // Kept deliberately: `libm` reaches nothing forbidden, so it survives the drop
    // and only `discover_build_enablers` / usage can settle whether it is wanted.
    let ctx = z3::Context::new(&z3::Config::new());
    let gate = Bool::or(
        &ctx,
        &[
            &Bool::new_const(&ctx, "std"),
            &Bool::new_const(&ctx, "libm"),
        ],
    );
    let not_std = Bool::new_const(&ctx, "std").not();
    let declared = vec!["std".to_string(), "libm".to_string()];

    assert_eq!(
        features_forced_true(&ctx, &[not_std], &gate, &declared),
        vec!["libm".to_string()],
        "`any(std, X)` under `not(std)` entails X — this is the mechanism, and the \
         reason `entailed_true` alone cannot tell a requirement from an artefact"
    );

    let info = CrateInfo {
        features: vec![feat("std", &[]), feat("libm", &[])],
        ..Default::default()
    };
    assert!(
        !reaches_forbidden_feature(&info, "libm", &["std".to_string()]),
        "`libm` enables no std, so the contradictory-equation drop must leave it \
         alone rather than silently removing a feature the crate may need"
    );
}

#[test]
fn nothing_is_forced_by_an_unsatisfiable_assertion() {
    // `all(std, encoding)` cannot hold under `not(std)`. `find_possible_equations`
    // drops these anyway; the helper must not report the whole declared set as
    // "forced" out of vacuous truth on the way there.
    let ctx = z3::Context::new(&z3::Config::new());
    let gate = Bool::and(
        &ctx,
        &[
            &Bool::new_const(&ctx, "std"),
            &Bool::new_const(&ctx, "encoding"),
        ],
    );
    let not_std = Bool::new_const(&ctx, "std").not();
    let declared = vec!["std".to_string(), "encoding".to_string()];
    assert!(
        features_forced_true(&ctx, &[not_std], &gate, &declared).is_empty(),
        "an assertion that cannot hold entails nothing actionable"
    );
}

#[test]
fn a_feature_the_gate_leaves_free_is_not_forced() {
    // The guard against reporting don't-cares as forced: `any(a, b)` entails
    // neither arm on its own.
    let ctx = z3::Context::new(&z3::Config::new());
    let gate = Bool::or(
        &ctx,
        &[&Bool::new_const(&ctx, "a"), &Bool::new_const(&ctx, "b")],
    );
    let declared = vec!["a".to_string(), "b".to_string()];
    assert!(
        features_forced_true(&ctx, &[], &gate, &declared).is_empty(),
        "`any(a, b)` forces neither a nor b"
    );
}

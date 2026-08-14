#![feature(rustc_private)]

//! Regression tests for `compile_error!` constraint enforcement.
//!
//! A `#[cfg(...)] compile_error!(...)` states a feature combination the crate
//! refuses to build under. When the constraint's features are disjoint from the
//! crate's no_std condition the equation is withheld from the solver (see
//! `excluded_compile_error_eqs` in parser.rs — probing uom showed 5 of its 21
//! storage-type disjuncts make a no_std build impossible, so letting Z3 pick
//! freely is genuinely unsafe). The stage-2 check that was supposed to catch a
//! resulting violation asserted only the *enabled* features as true and left
//! every other feature free, which makes any disjunction trivially satisfiable —
//! it never fired. bulletproofs-bls shipped `--no-default-features` with neither
//! `rust` nor `blst` and the telemetry recorded no violation.
//!
//! `violated_compile_error_constraints` closes the world instead: a feature the
//! build will not pass to cargo is asserted false.

//!
//! The second half of this file covers the opposite failure (O-1): a
//! `compile_error!` whose cfg names a non-feature atom. Policy G erases such an
//! atom out of its combinator, which reads as *false* inside `any(…)` and *true*
//! inside `all(…)` — both directions make the cfg more likely to fire. That is
//! the safe side wherever a cfg gates code, and the wrong side here, because
//! this position emits the cfg's *negation*. miden-thiserror and
//! midenc-hir-symbol both ended up with a hard constraint saying "std is
//! mandatory" for crates that build clean with `--no-default-features`, and
//! every std-off covering seed was skipped as unsatisfiable.

use std::path::{Path, PathBuf};

use nostd::visitor::ModCollector;
use nostd::{Attributes, CrateInfo, parser};

fn fixture(name: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/compile_error_constraint")
        .join(name)
}

/// Drive the real `Attributes` visitor over a fixture file, exactly as
/// `parse_crate` is called for a main crate.
fn attrs_for(fixture_name: &str) -> Attributes {
    parser::parse_crate(
        "compile-error-fixture:0.0.0",
        false,
        None,
        &[],
        Some(&[fixture(fixture_name)]),
    )
}

fn crate_info(features: &[(&str, &[&str])]) -> CrateInfo {
    CrateInfo {
        name: "compile-error-fixture".to_string(),
        version: "0.0.0".to_string(),
        features: features
            .iter()
            .map(|(name, deps)| {
                (
                    name.to_string(),
                    deps.iter().map(|d| (d.to_string(), d.to_string())).collect(),
                )
            })
            .collect(),
        default_features: true,
        ..Default::default()
    }
}

fn violations(
    fixture_name: &str,
    info: &CrateInfo,
    enabled: &[&str],
    default_features_on: bool,
) -> Vec<String> {
    let ctx = z3::Context::new(&z3::Config::new());
    let attrs = attrs_for(fixture_name);
    let enabled: Vec<String> = enabled.iter().map(|s| s.to_string()).collect();
    parser::violated_compile_error_constraints(
        &ctx,
        &attrs,
        info,
        &enabled,
        default_features_on,
    )
}

// ---------------------------------------------------------------------------
// bulletproofs-bls shape — the crate that exposed the bug
// ---------------------------------------------------------------------------

/// The exact shipped configuration: `--no-default-features` with neither
/// disjunct. Must be reported.
#[test]
fn empty_feature_set_violates_a_two_way_disjunction() {
    let info = crate_info(&[("default", &["blst", "std"]), ("blst", &[]), ("rust", &[])]);
    let v = violations("bulletproofs_shape.rs", &info, &[], false);
    assert_eq!(v.len(), 1, "expected the constraint to be reported, got {:?}", v);
}

#[test]
fn enabling_blst_satisfies_the_disjunction() {
    let info = crate_info(&[("default", &["blst", "std"]), ("blst", &[]), ("rust", &[])]);
    let v = violations("bulletproofs_shape.rs", &info, &["blst"], false);
    assert!(v.is_empty(), "blst should satisfy the constraint, got {:?}", v);
}

/// The other disjunct must work too — the check must not be hardcoded to one arm.
#[test]
fn enabling_rust_satisfies_the_disjunction() {
    let info = crate_info(&[("default", &["blst", "std"]), ("blst", &[]), ("rust", &[])]);
    let v = violations("bulletproofs_shape.rs", &info, &["rust"], false);
    assert!(v.is_empty(), "rust should satisfy the constraint, got {:?}", v);
}

/// The vacuous-check regression guard. Under the old code every feature not in
/// `enable` stayed free, so `rust ∨ blst` was satisfiable no matter what was on.
/// An unrelated feature must not be mistaken for satisfying the constraint.
#[test]
fn an_unrelated_enabled_feature_does_not_satisfy_the_disjunction() {
    let info = crate_info(&[("default", &["blst", "std"]), ("blst", &[]), ("rust", &[])]);
    let v = violations("bulletproofs_shape.rs", &info, &["std"], false);
    assert_eq!(
        v.len(),
        1,
        "`std` satisfies neither disjunct; expected a violation, got {:?}",
        v
    );
}

/// When defaults are left on, `default = ["blst", "std"]` supplies `blst`
/// transitively. Exercises `close_over_local_features` through `default`.
#[test]
fn default_features_supply_the_disjunct() {
    let info = crate_info(&[("default", &["blst", "std"]), ("blst", &[]), ("rust", &[])]);
    let v = violations("bulletproofs_shape.rs", &info, &[], true);
    assert!(
        v.is_empty(),
        "default features enable blst; expected no violation, got {:?}",
        v
    );
}

/// A feature reached through a chain of local features counts as enabled.
#[test]
fn transitively_enabled_feature_satisfies_the_disjunction() {
    let info = crate_info(&[
        ("default", &["std"]),
        ("bundle", &["blst"]),
        ("blst", &[]),
        ("rust", &[]),
    ]);
    let v = violations("bulletproofs_shape.rs", &info, &["bundle"], false);
    assert!(
        v.is_empty(),
        "bundle enables blst transitively; expected no violation, got {:?}",
        v
    );
}

// ---------------------------------------------------------------------------
// uom shape — the wide disjunction the overlap filter exists to protect
// ---------------------------------------------------------------------------

/// uom's real outcome: `f32`/`f64` arrive from `[features] default` and the
/// build is fine. Must stay quiet — this is the no-regression case.
#[test]
fn one_storage_type_satisfies_the_wide_disjunction() {
    let info = crate_info(&[
        ("default", &["f32", "f64", "std"]),
        ("u32", &[]),
        ("i32", &[]),
        ("f32", &[]),
        ("f64", &[]),
    ]);
    let v = violations("uom_shape.rs", &info, &["f32", "f64"], false);
    assert!(v.is_empty(), "f32 satisfies the constraint, got {:?}", v);
}

/// The wide disjunction is exactly the shape the old check could never fail on.
#[test]
fn no_storage_type_violates_the_wide_disjunction() {
    let info = crate_info(&[
        ("default", &["f32", "f64", "std"]),
        ("u32", &[]),
        ("i32", &[]),
        ("f32", &[]),
        ("f64", &[]),
    ]);
    let v = violations("uom_shape.rs", &info, &["std"], false);
    assert_eq!(
        v.len(),
        1,
        "no storage type is on; expected a violation, got {:?}",
        v
    );
}

// ---------------------------------------------------------------------------
// The repair — the features that make a violated constraint satisfiable
// ---------------------------------------------------------------------------

fn repair(
    fixture_name: &str,
    info: &CrateInfo,
    enabled: &[&str],
    default_features_on: bool,
    forbidden: &[&str],
) -> Vec<String> {
    let ctx = z3::Context::new(&z3::Config::new());
    let attrs = attrs_for(fixture_name);
    let enabled: Vec<String> = enabled.iter().map(|s| s.to_string()).collect();
    let forbidden: Vec<String> = forbidden.iter().map(|s| s.to_string()).collect();
    parser::compile_error_repair_features(
        &ctx,
        &attrs,
        info,
        &enabled,
        default_features_on,
        &forbidden,
    )
}

fn lexical_info() -> CrateInfo {
    crate_info(&[
        ("default", &["std"]),
        ("std", &[]),
        ("floats", &[]),
        ("write", &[]),
        ("parse", &[]),
        ("write-floats", &["write", "floats"]),
        ("parse-floats", &["parse", "floats"]),
    ])
}

/// lexical-util's shipped set. One feature is enough and one is what must come
/// back — a repair that switched on both disjuncts would be correct and not
/// minimal.
#[test]
fn a_violated_implication_is_repaired_by_one_added_feature() {
    let add = repair("lexical_shape.rs", &lexical_info(), &["floats"], false, &["std"]);
    assert_eq!(
        add.len(),
        1,
        "expected exactly one added feature, got {:?}",
        add
    );
    assert!(
        add[0] == "write-floats" || add[0] == "parse-floats",
        "expected one of the two disjuncts, got {:?}",
        add
    );
}

/// The repair adds; it never takes the feature solve's choice away. Dropping
/// `floats` would satisfy the constraint too, and is not this pass's call.
#[test]
fn the_repair_never_removes_an_enabled_feature() {
    let add = repair("lexical_shape.rs", &lexical_info(), &["floats"], false, &["std"]);
    assert!(
        !add.contains(&"floats".to_string()),
        "the repair must not name an already-enabled feature, got {:?}",
        add
    );
}

/// The repaired set has to pass the check that reported the violation. Guards
/// the wiring between the two functions, not just the model.
#[test]
fn the_repaired_feature_set_no_longer_violates() {
    let info = lexical_info();
    let add = repair("lexical_shape.rs", &info, &["floats"], false, &["std"]);
    let mut enabled = vec!["floats".to_string()];
    enabled.extend(add);
    let ctx = z3::Context::new(&z3::Config::new());
    let v = parser::violated_compile_error_constraints(
        &ctx,
        &attrs_for("lexical_shape.rs"),
        &info,
        &enabled,
        false,
    );
    assert!(v.is_empty(), "repaired set still violates: {:?}", v);
}

/// uom's 21-way disjunction: the wide shape must not produce a wide repair.
#[test]
fn a_wide_disjunction_is_repaired_minimally() {
    let info = crate_info(&[
        ("default", &["f32", "f64", "std"]),
        ("u32", &[]),
        ("i32", &[]),
        ("f32", &[]),
        ("f64", &[]),
        ("std", &[]),
    ]);
    let add = repair("uom_shape.rs", &info, &["std"], false, &["std"]);
    assert_eq!(add.len(), 1, "expected a single storage type, got {:?}", add);
}

/// Nothing violated, nothing to repair — the case every crate that builds today
/// is in.
#[test]
fn a_satisfied_constraint_yields_no_repair() {
    let add = repair(
        "lexical_shape.rs",
        &lexical_info(),
        &["floats", "write-floats"],
        false,
        &["std"],
    );
    assert!(add.is_empty(), "expected no repair, got {:?}", add);
}

/// A repair that would have to re-enable a feature the no_std verdict turned off
/// is no repair: the build is not worth the property the run exists to
/// establish.
#[test]
fn a_forbidden_feature_is_never_offered_as_a_repair() {
    let info = crate_info(&[("default", &["std"]), ("std", &[]), ("libm", &[])]);
    let add = repair("std_or_libm_shape.rs", &info, &[], false, &["std", "libm"]);
    assert!(
        add.is_empty(),
        "both disjuncts are forbidden; expected no repair, got {:?}",
        add
    );
}

/// The same shape with only `std` forbidden *is* repairable — the control that
/// proves the test above fails for the stated reason and not because the
/// fixture yields nothing.
#[test]
fn the_permitted_disjunct_is_offered() {
    let info = crate_info(&[("default", &["std"]), ("std", &[]), ("libm", &[])]);
    let add = repair("std_or_libm_shape.rs", &info, &[], false, &["std"]);
    assert_eq!(add, vec!["libm".to_string()], "expected libm, got {:?}", add);
}

/// A `compile_error!` may test a cfg a build script emits (bucket I). Passing
/// such a name to `--features` makes cargo error out, so it cannot be a repair.
#[test]
fn an_undeclared_feature_is_never_offered_as_a_repair() {
    let info = crate_info(&[("default", &["std"]), ("std", &[])]);
    let add = repair("bulletproofs_shape.rs", &info, &[], false, &["std"]);
    assert!(
        add.is_empty(),
        "neither disjunct is declared in [features]; expected no repair, got {:?}",
        add
    );
}

/// Declared, and the repair appears — the control for the test above.
#[test]
fn a_declared_disjunct_is_offered() {
    let info = crate_info(&[
        ("default", &["std"]),
        ("std", &[]),
        ("blst", &[]),
        ("rust", &[]),
    ]);
    let add = repair("bulletproofs_shape.rs", &info, &[], false, &["std"]);
    assert_eq!(add.len(), 1, "expected one disjunct, got {:?}", add);
}

/// A crate with no `compile_error!` at all never reaches the solver.
#[test]
fn no_constraint_yields_no_repair() {
    let info = crate_info(&[("default", &["std"])]);
    assert!(repair("no_constraint.rs", &info, &[], false, &[]).is_empty());
}

// ---------------------------------------------------------------------------
// Controls
// ---------------------------------------------------------------------------

/// A crate declaring no `compile_error!` must never be reported, whatever the
/// feature set.
#[test]
fn crate_without_compile_error_reports_nothing() {
    let info = crate_info(&[("default", &["std"])]);
    assert!(violations("no_constraint.rs", &info, &[], false).is_empty());
    assert!(violations("no_constraint.rs", &info, &["std"], true).is_empty());
}

/// The visitor must actually be collecting the constraint — if `compile_error_attrs`
/// came back empty every other assertion here would pass vacuously.
#[test]
fn the_fixture_constraint_is_actually_collected() {
    let ctx = z3::Context::new(&z3::Config::new());
    let names = parser::compile_error_feature_names(&attrs_for("bulletproofs_shape.rs"), &ctx);
    assert!(
        names.contains("blst") && names.contains("rust"),
        "expected blst and rust among the compile_error features, got {:?}",
        names
    );
}

// ---------------------------------------------------------------------------
// O-1 — a cfg naming an atom policy G erases emits no constraint at all
// ---------------------------------------------------------------------------

/// The negated attribute as the visitor builds it: `#[cfg(not(<orig tokens>))]`.
fn negated(src: &str) -> syn::Attribute {
    let item: syn::ItemStruct = syn::parse_str(&format!("{src} struct S;")).unwrap();
    let tokens = match &item.attrs[0].meta {
        syn::Meta::List(l) => l.tokens.clone(),
        other => panic!("fixture attribute is not a meta list: {other:?}"),
    };
    syn::parse_quote!(#[cfg(not(#tokens))])
}

fn constraint_of(src: &str) -> Option<String> {
    let ctx = z3::Context::new(&z3::Config::new());
    parser::compile_error_constraint(&negated(src), &ctx, None).map(|eq| eq.to_string())
}

/// miden-thiserror: `error_in_core` erased out of the `or` leaves
/// `(not (not (or std)))` — "std is mandatory" — for a crate that builds clean
/// on aarch64-unknown-none with no features at all.
#[test]
fn erased_atom_inside_any_emits_no_constraint() {
    assert_eq!(
        constraint_of(r#"#[cfg(not(any(feature = "std", error_in_core)))]"#),
        None
    );
}

/// midenc-hir-symbol: the `all(…)` mirror. `not(target_family = "wasm")` erases
/// to true, leaving `(not (and (not std)))` — same verdict, other combinator.
#[test]
fn erased_atom_inside_all_emits_no_constraint() {
    assert_eq!(
        constraint_of(r#"#[cfg(all(not(feature = "std"), not(target_family = "wasm")))]"#),
        None
    );
}

/// Control: an all-feature cfg has nothing erased and must still constrain.
/// These are the three real shapes the fix must not touch — parley, vls-core,
/// bulletproofs-bls. If this passed vacuously the fix would be a no-op wrecker.
#[test]
fn all_feature_cfgs_still_emit_their_constraint() {
    for src in [
        r#"#[cfg(not(any(feature = "std", feature = "libm")))]"#, // parley
        r#"#[cfg(not(any(feature = "std", feature = "no-std")))]"#, // vls-core
        r#"#[cfg(all(not(feature = "rust"), not(feature = "blst")))]"#, // bulletproofs-bls
        r#"#[cfg(feature = "std")]"#,
    ] {
        assert!(
            constraint_of(src).is_some(),
            "{src} names only features; its constraint must survive"
        );
    }
}

/// A `feature = "X"` Cargo cannot enable (bucket I) is erased by the same
/// mechanism, so it must be dropped here too — but only when the caller supplies
/// the declared set. With `None` every `feature = …` stays a solver variable.
#[test]
fn undeclared_feature_is_erased_only_when_the_declared_set_is_supplied() {
    let ctx = z3::Context::new(&z3::Config::new());
    let attr = negated(r#"#[cfg(not(any(feature = "std", feature = "nightly")))]"#);
    let known: std::collections::HashSet<String> = ["std".to_string()].into_iter().collect();

    assert!(
        parser::compile_error_constraint(&attr, &ctx, None).is_some(),
        "without a declared set both atoms are solver variables"
    );
    assert!(
        parser::compile_error_constraint(&attr, &ctx, Some(&known)).is_none(),
        "`nightly` is not declared, so it erases and the negation is unsound"
    );
}

// --- the wiring: ModCollector is what actually vetoes the covering seeds ---

/// The real entry point. `ModCollector::hard_constraints` is what
/// `find_feature_combs_for_all_code` feeds to the solver, and a bad constraint
/// there is what made every `(and (not std))` seed "unsatisfiable with hard
/// constraints". A direct call to `compile_error_constraint` would not prove
/// this path uses it.
#[test]
fn mod_collector_drops_the_erased_atom_constraint() {
    for name in ["erased_any_shape.rs", "erased_all_shape.rs"] {
        let ctx = z3::Context::new(&z3::Config::new());
        let mut collector = ModCollector::new(&ctx);
        collector.collect(&fixture(name), "lib");
        assert!(
            collector.hard_constraints.is_empty(),
            "{name}: expected no hard constraint, got {:?}",
            collector.hard_constraints
        );
    }
}

/// Control for the above: the same walk over an all-feature `compile_error!`
/// must still produce one. Without this the collector test passes even if
/// `visit_item_macro` stopped collecting `compile_error!` entirely.
#[test]
fn mod_collector_keeps_an_all_feature_constraint() {
    let ctx = z3::Context::new(&z3::Config::new());
    let mut collector = ModCollector::new(&ctx);
    collector.collect(&fixture("bulletproofs_shape.rs"), "lib");
    assert_eq!(
        collector.hard_constraints.len(),
        1,
        "expected the rust/blst constraint, got {:?}",
        collector.hard_constraints
    );
}

/// The closed-world final check must not report a violation that is an artefact
/// of the erasure. With no features on, the over-strong `(not (not (or std)))`
/// is unsatisfiable and would be reported — but miden-thiserror's build with no
/// features is exactly the one that compiles.
#[test]
fn erased_atom_constraint_is_not_reported_as_violated() {
    let info = crate_info(&[("std", &[])]);
    for name in ["erased_any_shape.rs", "erased_all_shape.rs"] {
        let v = violations(name, &info, &[], false);
        assert!(
            v.is_empty(),
            "{name}: the constraint is unsound in this position and must not be checked, got {:?}",
            v
        );
    }
}

// ---------------------------------------------------------------------------
// O-15 — stacked `#[cfg]`s are ANDed by rustc, so they are ONE constraint
// ---------------------------------------------------------------------------
//
// `#[cfg(A)] #[cfg(B)] compile_error!(..)` fires only when both hold, so the
// crate is saying `¬(A ∧ B)`. Both collection sites read the attributes one at a
// time: `ModCollector` emitted a negation per attribute (`¬A ∧ ¬B`, strictly
// stronger than anything the crate wrote) and `Attributes` took only the first.
// In spo-rhai 1.17.2 and rhai 1.21.0 the `¬no_std` that falls out is the
// negation of the crate's own no_std condition.

/// Assert `constraint ∧ (each named feature at its given polarity)` is SAT.
fn constraint_admits<'a>(
    ctx: &'a z3::Context,
    constraints: &[z3::ast::Bool<'a>],
    assignment: &[(&str, bool)],
) -> bool {
    let solver = z3::Solver::new(ctx);
    for c in constraints {
        solver.assert(c);
    }
    for (feat, on) in assignment {
        let var = z3::ast::Bool::new_const(ctx, *feat);
        solver.assert(&if *on { var } else { var.not() });
    }
    solver.check() == z3::SatResult::Sat
}

/// The bug, at the entry point that fails crates. One constraint, not two — and
/// crucially one that leaves the crate's no_std condition satisfiable.
#[test]
fn stacked_cfgs_yield_one_conjoined_constraint() {
    let ctx = z3::Context::new(&z3::Config::new());
    let mut collector = ModCollector::new(&ctx);
    collector.collect(&fixture("stacked_shape.rs"), "lib");
    assert_eq!(
        collector.hard_constraints.len(),
        1,
        "the item states one constraint, got {:?}",
        collector.hard_constraints
    );
    assert!(
        constraint_admits(&ctx, &collector.hard_constraints, &[("no_std", true)]),
        "`no_std` alone is not what the crate forbids; the std-off seeds must \
         survive this constraint, got {:?}",
        collector.hard_constraints
    );
}

/// ...and it still forbids what the crate actually forbids. Without this the
/// test above passes on a fix that simply threw the constraint away.
#[test]
fn the_conjoined_constraint_still_forbids_both_features_together() {
    let ctx = z3::Context::new(&z3::Config::new());
    let mut collector = ModCollector::new(&ctx);
    collector.collect(&fixture("stacked_shape.rs"), "lib");
    assert!(
        !constraint_admits(
            &ctx,
            &collector.hard_constraints,
            &[("no_std", true), ("wasm-bindgen", true)]
        ),
        "the crate refuses to build with both; got {:?}",
        collector.hard_constraints
    );
}

/// O-1's rule applies to the *fold*: an erased atom in any one of the stacked
/// attributes drops the whole constraint. Per attribute it dropped the
/// `target_family` half and left `¬no_std` asserted on its own.
#[test]
fn an_erased_atom_in_a_stacked_cfg_drops_the_whole_constraint() {
    let ctx = z3::Context::new(&z3::Config::new());
    let mut collector = ModCollector::new(&ctx);
    collector.collect(&fixture("stacked_erased_shape.rs"), "lib");
    assert!(
        collector.hard_constraints.is_empty(),
        "a negation over an erased atom cannot be modelled soundly, and a \
         fragment of it is not a weaker version of it, got {:?}",
        collector.hard_constraints
    );
}

/// The other half of the tool, at its own entry point. `compile_error_attrs` is
/// what `process_crate`, the violation check, the repair and the dependency-side
/// check all read; `Attributes::visit_item_macro` took the first `#[cfg]` and
/// dropped the rest, so it stated a different constraint from the one above
/// about the same declaration.
#[test]
fn the_attributes_half_collects_the_same_conjoined_constraint() {
    let ctx = z3::Context::new(&z3::Config::new());
    let attrs = attrs_for("stacked_shape.rs");
    let names = parser::compile_error_feature_names(&attrs, &ctx);
    assert!(
        names.contains("no_std") && names.contains("wasm-bindgen"),
        "both stacked attributes name a feature of the constraint, got {names:?}"
    );

    let info = crate_info(&[("no_std", &[]), ("wasm-bindgen", &[]), ("std", &[])]);
    let v = violations("stacked_shape.rs", &info, &["no_std"], false);
    assert!(
        v.is_empty(),
        "`no_std` on its own is a configuration this crate supports; reporting \
         it violated is what stopped the build, got {:?}",
        v
    );
}

/// Control for the above: the configuration the crate really refuses is still
/// reported. `no_std` and `wasm-bindgen` together is the error it wrote.
#[test]
fn the_forbidden_combination_is_still_reported_as_violated() {
    let info = crate_info(&[("no_std", &[]), ("wasm-bindgen", &[]), ("std", &[])]);
    let v = violations("stacked_shape.rs", &info, &["no_std", "wasm-bindgen"], false);
    assert_eq!(
        v.len(),
        1,
        "expected the stacked constraint to be reported, got {:?}",
        v
    );
}

/// A `compile_error!` whose attributes contain no `#[cfg]` states no condition.
/// Reaching for the first one and unwrapping panicked the whole parse.
#[test]
fn a_compile_error_without_a_cfg_contributes_nothing() {
    let ctx = z3::Context::new(&z3::Config::new());
    let mut collector = ModCollector::new(&ctx);
    collector.collect(&fixture("uncfgd_shape.rs"), "lib");
    assert!(
        collector.hard_constraints.is_empty(),
        "nothing to negate, got {:?}",
        collector.hard_constraints
    );
    assert!(
        parser::compile_error_feature_names(&attrs_for("uncfgd_shape.rs"), &ctx).is_empty(),
        "an unconditional compile_error! names no features"
    );
}

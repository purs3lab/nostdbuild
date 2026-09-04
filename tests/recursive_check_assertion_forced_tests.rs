#![feature(rustc_private)]

//! KI-21: two crates cleared by the O-7 neighbourhood (`vls-core`, `bp-runtime`)
//! still exited at the recursive requirement check after `3454905` fixed the
//! original "Z3 coin flip read as requirement" bug. Re-probing both showed every
//! remaining violation traced to the same run's own `[equations]` diagnostic:
//! `serde:1.0.174`'s `rc`, `bitcoin:0.30.2`'s `no-std`, `sp-arithmetic:26.1.0`'s
//! `serde`, etc. were each logged as "required only because a cfg condition was
//! asserted (the no_std condition itself does not need them)" — the exact
//! `assertion_forced` set `process_crate` already computes and records in
//! `features_forced_by_cfg_assertion`, but never subtracted from `entailed_true`
//! before `audit_dependency_requirement`'s Direction 1 read it as proof.
//!
//! `entailed_true` alone cannot make this distinction (both are UNSAT-to-negate);
//! the caller — `recursive_dep_requirement_check` — now subtracts
//! `assertion_forced` before use. These tests fix `audit_dependency_requirement`'s
//! half of the contract: given the *filtered* `entailed_true` a correct caller
//! passes, an unjustified feature is reported (not silently dropped) but does not
//! become a fatal violation; a genuinely proven one still does.

use std::collections::{HashMap, HashSet};

use nostd::CrateInfo;
use nostd::parser::audit_dependency_requirement;

/// `main` depends on `dep`, declaring no extra features on the edge — the shape
/// of `lightning` → `serde_with` → `serde`, `frame-support` → `sp-arithmetic`.
fn main_with_bare_edge_to(dep_name: &str) -> CrateInfo {
    CrateInfo {
        name: "main".to_string(),
        deps_and_features: vec![(
            CrateInfo {
                name: dep_name.to_string(),
                ..Default::default()
            },
            vec![],
        )],
        ..Default::default()
    }
}

#[test]
fn entailed_true_still_flags_a_genuine_requirement() {
    // `rc` proven required (a correct caller's filtered `entailed_true`, or a
    // feature the dependency's own no_std condition names directly): not on the
    // edge, no [features] entry maps to it, nothing else can excuse it — this is
    // exactly the shape a real dependency-edge gap has to keep producing.
    let main_info = main_with_bare_edge_to("serde");
    let dep_info = CrateInfo::default();
    let (violations, unjustified) = audit_dependency_requirement(
        &main_info,
        &dep_info,
        "serde",
        "main:0.1.0",
        "serde:1.0.174",
        &["rc".to_string()],
        &[],
        &["rc".to_string()],
        &[],
        &HashSet::new(),
        &HashMap::new(),
    );
    assert!(
        unjustified.is_empty(),
        "a proven feature is not unjustified: {unjustified:?}"
    );
    assert_eq!(
        violations.len(),
        1,
        "a proven, undeclared, unmappable feature must still be a violation: {violations:?}"
    );
    assert!(violations[0].contains("rc"));
}

#[test]
fn a_feature_only_forced_by_cfg_assertion_is_unjustified_not_a_violation() {
    // The KI-21 shape: `rc` is in `enable` (it was in the dependency's raw solve
    // result) but a correct caller has already subtracted it from `entailed_true`
    // because `process_crate` recorded it as `assertion_forced` — some unrelated
    // `#[cfg(feature = "rc")]` gate in serde's own source got asserted, not
    // anything serde's no_std condition itself names. Direction 1 must not
    // escalate this to a fatal "dependency edge is broken" violation.
    let main_info = main_with_bare_edge_to("serde");
    let dep_info = CrateInfo::default();
    let (violations, unjustified) = audit_dependency_requirement(
        &main_info,
        &dep_info,
        "serde",
        "main:0.1.0",
        "serde:1.0.174",
        &["rc".to_string()],
        &[],
        &[], // entailed_true: caller already dropped "rc" as assertion_forced
        &[],
        &HashSet::new(),
        &HashMap::new(),
    );
    assert!(
        violations.is_empty(),
        "an assertion-forced feature must not become a fatal violation: {violations:?}"
    );
    assert_eq!(
        unjustified,
        vec!["rc".to_string()],
        "it must still be recorded, just not escalated"
    );
}

#[test]
fn a_feature_reaching_a_cross_crate_item_is_justified_without_entailed_true() {
    // The other half of Direction 1's OR: even with `entailed_true` empty (as a
    // correct caller now often produces once assertion-forced atoms are
    // subtracted), a feature the parent's own code demonstrably reaches through
    // is still real evidence and must still be flagged.
    let main_info = main_with_bare_edge_to("serde");
    let dep_info = CrateInfo::default();
    let mut feature_to_items = HashMap::new();
    feature_to_items.insert("rc".to_string(), HashSet::from(["Rc".to_string()]));
    let mut parent_valid_cross_crate_items = HashSet::new();
    parent_valid_cross_crate_items.insert(("serde".to_string(), "Rc".to_string()));

    let (violations, unjustified) = audit_dependency_requirement(
        &main_info,
        &dep_info,
        "serde",
        "main:0.1.0",
        "serde:1.0.174",
        &["rc".to_string()],
        &[],
        &[],
        &[],
        &parent_valid_cross_crate_items,
        &feature_to_items,
    );
    assert!(
        unjustified.is_empty(),
        "item-reachability alone must justify without entailed_true: {unjustified:?}"
    );
    assert_eq!(violations.len(), 1);
}

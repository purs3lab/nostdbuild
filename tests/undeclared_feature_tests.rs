#![feature(rustc_private)]

//! Regression tests for `#[cfg(feature = "X")]` where X is a feature Cargo
//! cannot enable.
//!
//! blst declares no `std` feature, yet gates most of its std usage on
//! `#[cfg(feature = "std")]` — its build script emits
//! `cargo:rustc-cfg=feature="std"` for every non-embedded target. Modelling X as
//! a solver variable fails twice: negating it never removes the code (so the
//! gate is reported unguarded), and X leaks into `--features`, where cargo
//! rejects it outright.
//!
//! The fix erases such an atom exactly as bucket G erases `target_os = "…"`, so
//! a gate naming only undeclared features is *externally gated* and a mixed gate
//! projects onto the features that remain.

use std::collections::HashSet;
use std::path::{Path, PathBuf};

use nostd::types::ReadableSpan;
use nostd::visitor::{ModCollector, ancestors_for_span, externally_gated_for_span};

fn fixture() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/undeclared_feature/lib.rs")
}

/// Mirrors what `visitor::declared_features` returns for the fixture manifest:
/// its `[features]` table lists only `declared`.
fn declared() -> HashSet<String> {
    ["declared".to_string()].into_iter().collect()
}

fn span_of(content: &str, needle: &str) -> ReadableSpan {
    let idx = content
        .find(needle)
        .unwrap_or_else(|| panic!("substring {needle:?} not found in fixture"));
    let before = &content[..idx];
    let line = before.matches('\n').count() + 1;
    let col = idx - before.rfind('\n').map(|p| p + 1).unwrap_or(0);
    ReadableSpan {
        file: "lib.rs".to_string(),
        start_line: line,
        start_col: col,
        end_line: line,
        end_col: col + needle.len(),
        usage_crate: Some("std".to_string()),
    }
}

/// Build the tree with the undeclared-feature check active.
fn with_filtered_tree<F: FnOnce(&nostd::visitor::ModNode<'_>, &str)>(f: F) {
    let root = fixture();
    let content = std::fs::read_to_string(&root).unwrap();
    let cfg = z3::Config::new();
    let ctx = z3::Context::new(&cfg);
    let mut collector = ModCollector::with_known_features(&ctx, declared());
    let tree = collector.collect(&root, "lib");
    f(&tree, &content);
}

/// Build the tree with the check disabled, so `feature = "forced"` stays a Bool.
fn with_unfiltered_tree<F: FnOnce(&nostd::visitor::ModNode<'_>, &str)>(f: F) {
    let root = fixture();
    let content = std::fs::read_to_string(&root).unwrap();
    let cfg = z3::Config::new();
    let ctx = z3::Context::new(&cfg);
    let mut collector = ModCollector::new(&ctx);
    let tree = collector.collect(&root, "lib");
    f(&tree, &content);
}

#[test]
fn undeclared_feature_gate_is_externally_gated() {
    with_filtered_tree(|tree, content| {
        let span = span_of(content, "std::sync::Mutex");
        assert!(
            externally_gated_for_span(tree, &span),
            "`#[cfg(feature = \"forced\")]` names no feature cargo can enable, \
             so it must be treated as an external gate"
        );
    });
}

#[test]
fn undeclared_feature_never_becomes_a_solver_variable() {
    with_filtered_tree(|tree, content| {
        let span = span_of(content, "std::sync::Mutex");
        let ancestors = ancestors_for_span(tree, &span);
        let rendered = format!("{ancestors:?}");
        assert!(
            !rendered.contains("forced"),
            "`forced` must not appear as a Z3 variable — it would leak into the \
             --features list cargo is handed, which rejects it. Got: {rendered}"
        );
    });
}

#[test]
fn mixed_gate_projects_onto_the_declared_feature() {
    with_filtered_tree(|tree, content| {
        // `all(not(feature = "declared"), feature = "forced")` — erasing the
        // undeclared atom must leave `not(declared)`, which is still solvable,
        // rather than collapsing the whole gate away.
        let span = span_of(content, "std::sync::Once");
        let ancestors = ancestors_for_span(tree, &span);
        let rendered = format!("{ancestors:?}");
        assert!(
            rendered.contains("declared"),
            "the declared half of a mixed gate must survive erasure, got: {rendered}"
        );
        assert!(
            !rendered.contains("forced"),
            "the undeclared half must be erased, got: {rendered}"
        );
    });
}

// ---------------------------------------------------------------------------
// Controls — these must hold regardless, or the fix is over-reaching.
// ---------------------------------------------------------------------------

#[test]
fn control_declared_feature_stays_a_solver_variable() {
    with_filtered_tree(|tree, content| {
        let span = span_of(content, "std::collections::BTreeMap");
        let ancestors = ancestors_for_span(tree, &span);
        let rendered = format!("{ancestors:?}");
        assert!(
            rendered.contains("declared"),
            "a real feature must still be solved over, got: {rendered}"
        );
        assert!(
            !externally_gated_for_span(tree, &span),
            "a real feature gate must not be excused as external"
        );
    });
}

#[test]
fn control_ungated_std_stays_hard() {
    with_filtered_tree(|tree, content| {
        let span = span_of(content, "std::fs::File");
        assert!(
            !externally_gated_for_span(tree, &span),
            "std usage under no cfg at all must never be excused"
        );
        assert!(
            ancestors_for_span(tree, &span).is_none_or(|a| a.is_empty()),
            "ungated std must have no gate"
        );
    });
}

#[test]
fn control_without_known_features_undeclared_stays_a_variable() {
    // Guards the `None` path every fixture test and the dependency-parsing code
    // relies on: filtering a dependency's features against the main crate's list
    // would erase real, controllable features.
    with_unfiltered_tree(|tree, content| {
        let span = span_of(content, "std::sync::Mutex");
        let ancestors = ancestors_for_span(tree, &span);
        let rendered = format!("{ancestors:?}");
        assert!(
            rendered.contains("forced"),
            "with no known-feature set supplied, every `feature = \"…\"` must \
             remain a solver variable, got: {rendered}"
        );
    });
}


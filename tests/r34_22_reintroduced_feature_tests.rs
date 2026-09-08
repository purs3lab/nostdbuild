#![feature(rustc_private)]
//! R34-22 sizing instrumentation: `solver::reintroduced_main_disabled_features`
//! is the pure predicate behind `Telemetry::dep_pass_reintroduced_main_disabled`
//! — measurement only, no behavior change. See `ALL_TARGET_FAILURES.md`'s R34-22
//! entry: `bin/main.rs`'s `process_dep_crate_wrapper` filters a dependency
//! pass's `temp_flexible` against `previously_disabled` and against
//! `parser::reaches_forbidden_feature`, but never against the main crate's own
//! initial `to_disable` — so a feature the main solve just turned off can still
//! be silently re-added to `main_features` by the next dependency's pass. This
//! function is what a sizing pass reads to find that shape without changing it.

use std::collections::HashSet;

use nostd::solver::reintroduced_main_disabled_features;

fn set(names: &[&str]) -> HashSet<String> {
    names.iter().map(|s| s.to_string()).collect()
}

fn list(names: &[&str]) -> Vec<String> {
    names.iter().map(|s| s.to_string()).collect()
}

#[test]
fn flags_a_feature_the_main_solve_already_disabled() {
    // bevy_transform's own solve puts `bevy_reflect` in `to_disable`; bevy_tasks'
    // dependency pass hands it back in `temp_flexible` anyway.
    let temp_flexible = list(&["bevy-support", "bevy_reflect"]);
    let main_to_disable = set(&["bevy_reflect", "smol_str"]);
    let mut got = reintroduced_main_disabled_features(&temp_flexible, &main_to_disable);
    got.sort();
    assert_eq!(got, vec!["bevy_reflect".to_string()]);
}

#[test]
fn empty_when_nothing_overlaps() {
    let temp_flexible = list(&["builtin_devices", "std"]);
    let main_to_disable = set(&["smol_str"]);
    assert!(reintroduced_main_disabled_features(&temp_flexible, &main_to_disable).is_empty());
}

#[test]
fn empty_temp_flexible_never_reports_anything() {
    let main_to_disable = set(&["smol_str", "bevy_reflect"]);
    assert!(reintroduced_main_disabled_features(&[], &main_to_disable).is_empty());
}

#[test]
fn every_overlapping_feature_is_reported_not_just_the_first() {
    let temp_flexible = list(&["a", "b", "c"]);
    let main_to_disable = set(&["a", "c"]);
    let mut got = reintroduced_main_disabled_features(&temp_flexible, &main_to_disable);
    got.sort();
    assert_eq!(got, vec!["a".to_string(), "c".to_string()]);
}

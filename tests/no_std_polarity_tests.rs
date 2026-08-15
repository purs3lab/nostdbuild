#![feature(rustc_private)]

//! R31-3 — the emitted feature selection has to satisfy the crate's own
//! `#![cfg_attr(<cond>, no_std)]`, not merely have satisfied it at solve time.
//!
//! `NO_STD_ATTR_NOT_ENABLED`, 17 crates: rustc says `std is required by <crate>
//! because it does not declare #![no_std]` because the configuration the tool
//! finally passed to cargo leaves the crate root evaluating to std. Two shapes,
//! both here:
//!
//! * the solve chose `¬std` and a later pass handed `std` back — nuuid 0.5.0,
//!   whose first dependency pass recomputes "the default features this
//!   dependency does not disable" from the published `default = ["getrandom",
//!   "std"]` and knows nothing of the main solve;
//! * the solve turned the feature that *carries* `no_std` on and `minimize`
//!   dropped it as a feature whose whole subtree only links optional deps —
//!   `robust`'s `no_std = ["ieee754"]`, `utm`'s `no_std = ["num"]`, `lasso`'s
//!   `no-std`.
//!
//! `solver::no_std_forced_features` reads what the condition entails;
//! `parser::enforce_no_std_polarity` applies it to the final selection.

use nostd::types::TupleVec;
use nostd::{CrateInfo, parser, solver};
use z3::ast::Bool;

/// The condition as `process_crate` has it: parsed straight off the crate-root
/// attribute.
fn condition<'a>(ctx: &'a z3::Context, attr_src: &str) -> Bool<'a> {
    let file: syn::File = syn::parse_str(attr_src).expect("attribute parses");
    let attr = file.attrs.first().expect("one inner attribute").clone();
    parser::parse_main_attributes_direct(&attr, ctx)
        .0
        .expect("the attribute names at least one feature")
}

fn feature(name: &str, values: &[&str]) -> (String, TupleVec) {
    (
        name.to_string(),
        values
            .iter()
            .map(|v| (v.to_string(), v.to_string()))
            .collect(),
    )
}

fn crate_info(features: Vec<(String, TupleVec)>) -> CrateInfo {
    CrateInfo {
        name: "subject".to_string(),
        version: "0.1.0".to_string(),
        features,
        ..Default::default()
    }
}

fn strings(v: &[&str]) -> Vec<String> {
    v.iter().map(|s| s.to_string()).collect()
}

// ---------------------------------------------------------------------------
// What the condition entails
// ---------------------------------------------------------------------------

/// nuuid 0.5.0: `#![cfg_attr(not(any(test, feature = "std")), no_std)]`. `test`
/// is erased (it is not a feature), so what is left forbids `std` and requires
/// nothing.
#[test]
fn negated_std_forbids_std() {
    let ctx = z3::Context::new(&z3::Config::new());
    let cond = condition(&ctx, "#![cfg_attr(not(any(test, feature = \"std\")), no_std)]");
    let info = crate_info(vec![
        feature("default", &["getrandom", "std"]),
        feature("std", &[]),
        feature("getrandom", &[]),
    ]);

    let (required, forbidden) = solver::no_std_forced_features(&ctx, &cond, &info);
    assert!(required.is_empty(), "nothing is required: {required:?}");
    assert_eq!(forbidden, strings(&["std"]));
}

/// lasso 0.7.3 / robust 1.1.0 / utm 0.1.6: the crate is no_std only when the
/// named feature is *on*.
#[test]
fn opt_in_feature_is_required() {
    let ctx = z3::Context::new(&z3::Config::new());
    let cond = condition(&ctx, "#![cfg_attr(feature = \"no-std\", no_std)]");
    let info = crate_info(vec![feature("no-std", &[])]);

    let (required, forbidden) = solver::no_std_forced_features(&ctx, &cond, &info);
    assert_eq!(required, strings(&["no-std"]));
    assert!(forbidden.is_empty(), "nothing is forbidden: {forbidden:?}");
}

/// A choice the condition leaves open stays open. Entailment is the test, not
/// the model's assignment — blaming a feature the solve was entitled to pick
/// either way is what cost `wg` its whole feature list on the probe side, and
/// this must not reintroduce it here.
#[test]
fn a_free_choice_is_neither_required_nor_forbidden() {
    let ctx = z3::Context::new(&z3::Config::new());
    let cond = condition(
        &ctx,
        "#![cfg_attr(any(feature = \"alloc\", feature = \"spin\"), no_std)]",
    );
    let info = crate_info(vec![feature("alloc", &[]), feature("spin", &[])]);

    let (required, forbidden) = solver::no_std_forced_features(&ctx, &cond, &info);
    assert!(required.is_empty(), "{required:?}");
    assert!(forbidden.is_empty(), "{forbidden:?}");
}

/// `integer_or_float-0.3.2` writes `#![cfg_attr(no_std, no_std)]` — a bare cfg
/// flag, not a feature. No manifest edit can set it, and naming it in
/// `--features` makes cargo reject the invocation, so it must not be reported
/// as something to enable.
#[test]
fn an_atom_that_is_not_a_declared_feature_is_ignored() {
    let ctx = z3::Context::new(&z3::Config::new());
    // `feature = "no_std"` builds the same Bool constant the bare `no_std` cfg
    // would if it were modelled; the point is that the crate declares no such
    // feature.
    let cond = condition(&ctx, "#![cfg_attr(feature = \"no_std\", no_std)]");
    let info = crate_info(vec![feature("alloc", &[])]);

    let (required, forbidden) = solver::no_std_forced_features(&ctx, &cond, &info);
    assert!(required.is_empty(), "{required:?}");
    assert!(forbidden.is_empty(), "{forbidden:?}");
}

// ---------------------------------------------------------------------------
// Applying it to the final selection
// ---------------------------------------------------------------------------

/// nuuid's failure, from the other end: the selection about to reach cargo is
/// `--features std`, which is the one thing the crate's condition rules out.
/// Dropping it from the list is not enough — `std` is in `default`, so the
/// defaults have to come off and the rest of `default` be restored by name.
#[test]
fn a_forbidden_default_feature_turns_the_defaults_off() {
    let info = crate_info(vec![
        feature("default", &["getrandom", "std"]),
        feature("std", &[]),
        feature("getrandom", &[]),
    ]);
    let mut main_features = strings(&["std"]);
    let mut enable = strings(&["std"]);
    let mut disable_default = false;

    let (added, removed) = parser::enforce_no_std_polarity(
        &info,
        &mut main_features,
        &mut enable,
        &mut disable_default,
        &[],
        &strings(&["std"]),
    );

    assert!(added.is_empty(), "{added:?}");
    assert_eq!(removed, strings(&["std"]));
    assert!(disable_default, "defaults must come off");
    assert!(!main_features.contains(&"std".to_string()));
    assert!(!enable.contains(&"std".to_string()));
    assert!(
        main_features.contains(&"getrandom".to_string()),
        "the rest of `default` was on and is not forbidden: {main_features:?}"
    );
}

/// robust 1.1.0: `minimize` dropped `no_std` as a feature whose subtree only
/// links `ieee754`. It is the feature that carries the attribute.
#[test]
fn a_required_feature_is_put_back() {
    let info = crate_info(vec![feature("no_std", &["ieee754"])]);
    let mut main_features: Vec<String> = Vec::new();
    let mut enable: Vec<String> = Vec::new();
    let mut disable_default = false;

    let (added, removed) = parser::enforce_no_std_polarity(
        &info,
        &mut main_features,
        &mut enable,
        &mut disable_default,
        &strings(&["no_std"]),
        &[],
    );

    assert_eq!(added, strings(&["no_std"]));
    assert!(removed.is_empty(), "{removed:?}");
    assert_eq!(main_features, strings(&["no_std"]));
    assert_eq!(enable, strings(&["no_std"]));
    assert!(!disable_default, "nothing here says to drop the defaults");
}

/// The normal case, and the one every crate that builds today is in: the
/// selection already agrees with the condition, so nothing moves.
#[test]
fn a_consistent_selection_is_left_alone() {
    let info = crate_info(vec![
        feature("default", &["alloc"]),
        feature("alloc", &[]),
        feature("std", &["alloc"]),
    ]);
    let mut main_features = strings(&["alloc"]);
    let mut enable = strings(&["alloc"]);
    let mut disable_default = false;

    let (added, removed) = parser::enforce_no_std_polarity(
        &info,
        &mut main_features,
        &mut enable,
        &mut disable_default,
        &[],
        &strings(&["std"]),
    );

    assert!(added.is_empty(), "{added:?}");
    assert!(removed.is_empty(), "{removed:?}");
    assert!(!disable_default);
    assert_eq!(main_features, strings(&["alloc"]));
    assert_eq!(enable, strings(&["alloc"]));
}

/// dlopen-rs 0.7.3 never passes `std` on the command line: it passes `debug`
/// and `tls`, and its own table says `debug = ["std"]`, `tls = ["std"]`. A
/// feature that turns `std` on is `std` as far as the crate root is concerned,
/// so membership is not the test — reachability is. `libgcc` reaches nothing
/// forbidden and stays.
#[test]
fn a_feature_that_enables_a_forbidden_one_goes_too() {
    let info = crate_info(vec![
        feature("default", &["tls", "libgcc", "debug"]),
        feature("debug", &["std"]),
        feature("tls", &["std"]),
        feature("libgcc", &[]),
        feature("std", &[]),
    ]);
    let mut main_features = strings(&["debug", "libgcc", "tls"]);
    let mut enable = strings(&["tls"]);
    let mut disable_default = true;

    let (added, removed) = parser::enforce_no_std_polarity(
        &info,
        &mut main_features,
        &mut enable,
        &mut disable_default,
        &[],
        &strings(&["std"]),
    );

    assert!(added.is_empty(), "{added:?}");
    assert_eq!(removed, strings(&["debug", "tls"]));
    assert_eq!(main_features, strings(&["libgcc"]));
    assert!(enable.is_empty(), "{enable:?}");
}

/// The tarfs control, and the reason the dependency-pass repair subtracts rather
/// than widens. tarfs 0.2.7 is `default = ["std", "builtin_devices"]` under
/// `#![cfg_attr(not(feature = "std"), no_std)]`, and `builtin_devices` gates a
/// module that uses std unconditionally — the solve turns it off, and nothing
/// here may turn it back on. Only `std` is forbidden by the *condition*;
/// `builtin_devices` is a decision the solve made for another reason, and this
/// function must not have an opinion about it either way.
#[test]
fn a_feature_the_condition_says_nothing_about_is_left_alone() {
    let info = crate_info(vec![
        feature("default", &["std", "builtin_devices"]),
        feature("std", &[]),
        feature("builtin_devices", &[]),
    ]);

    assert!(parser::reaches_forbidden_feature(
        &info,
        "std",
        &strings(&["std"])
    ));
    assert!(
        !parser::reaches_forbidden_feature(&info, "builtin_devices", &strings(&["std"])),
        "`builtin_devices` does not enable `std`; the no_std condition says nothing about it"
    );

    // And the final guard leaves an already-`--no-default-features` selection
    // with nothing in it exactly as it found it.
    let mut main_features: Vec<String> = Vec::new();
    let mut enable: Vec<String> = Vec::new();
    let mut disable_default = true;
    let (added, removed) = parser::enforce_no_std_polarity(
        &info,
        &mut main_features,
        &mut enable,
        &mut disable_default,
        &[],
        &strings(&["std"]),
    );
    assert!(added.is_empty() && removed.is_empty(), "{added:?} {removed:?}");
    assert!(main_features.is_empty(), "{main_features:?}");
}

/// A forbidden feature that `default` reaches *indirectly* counts too: cargo
/// enables the whole closure, so leaving the defaults on leaves `std` on.
#[test]
fn an_indirectly_defaulted_forbidden_feature_also_turns_the_defaults_off() {
    let info = crate_info(vec![
        feature("default", &["full"]),
        feature("full", &["std", "extra"]),
        feature("std", &[]),
        feature("extra", &[]),
    ]);
    let mut main_features: Vec<String> = Vec::new();
    let mut enable: Vec<String> = Vec::new();
    let mut disable_default = false;

    let (_, removed) = parser::enforce_no_std_polarity(
        &info,
        &mut main_features,
        &mut enable,
        &mut disable_default,
        &[],
        &strings(&["std"]),
    );

    assert!(disable_default, "defaults must come off");
    assert_eq!(removed, strings(&["std"]));
    assert!(
        !main_features.contains(&"full".to_string()),
        "`full` reaches `std`, so it cannot be restored by name: {main_features:?}"
    );
}

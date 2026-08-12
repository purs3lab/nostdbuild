#![feature(rustc_private)]

//! KI-22 — a proc macro that injects `std` into a crate whose manifest has no
//! edge to it.
//!
//! `driver::park_injecting_proc_macros` turns an injected `std` off by writing
//! `default-features = false` on an edge of the consumer's own manifest. A macro
//! reached through another dependency — `sp-debug-derive` below `sp-core`,
//! `hidden-macro` below `reexporter` here — is not such an edge, so it keeps its
//! `std` feature and keeps injecting `extern crate std` at every invocation
//! site. Same wall the ordinary dependency case hits: the tool cannot rewrite a
//! manifest it does not emit.
//!
//! Reported rather than left to fail a bare-metal build with `E0463 can't find
//! crate for std` at a span the crate never wrote.
//!
//! ⚠ KI-22 says "every corpus instance is Substrate", which is **wrong** and was
//! measured: 39 distinct proc-macro crates with a `std`/`alloc` feature are
//! reached non-directly, and `num-enum-derive` (191 consumers) and `displaydoc`
//! (93 non-Substrate consumers) are nothing to do with Substrate.

use std::path::{Path, PathBuf};

use nostd::{Telemetry, driver, types::FeatureRunOutput};

fn manifest() -> String {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/unreachable_proc_macro/consumer/Cargo.toml")
        .display()
        .to_string()
}

/// One record, shaped exactly as the plugin emits it. Built from JSON so the
/// test does not have to track every field of `PathRecord`.
fn output(records: &[(&str, &str)]) -> FeatureRunOutput {
    let records: Vec<String> = records
        .iter()
        .map(|(usage_crate, expansion_crate)| {
            format!(
                r#"{{
                    "path_text": "std::string::String",
                    "definition_crate": "std",
                    "context": "ImportDeclaration",
                    "span": {{
                        "file": "src/lib.rs",
                        "start_line": 1, "start_col": 1,
                        "end_line": 1, "end_col": 2,
                        "usage_crate": "{usage_crate}"
                    }},
                    "local_route": null,
                    "defining_module": null,
                    "expansion_crate": "{expansion_crate}"
                }}"#
            )
        })
        .collect();
    serde_json::from_str(&format!(
        r#"{{ "records": [{}], "macro_module_imports": [] }}"#,
        records.join(",")
    ))
    .expect("fixture record JSON must match PathRecord")
}

fn report(records: &[(&str, &str)]) -> Telemetry {
    let mut telemetry = Telemetry::default();
    driver::report_unreachable_proc_macro_injectors(
        &output(records),
        &manifest(),
        "consumer:0.1.0",
        &mut telemetry,
    );
    telemetry
}

/// The case itself: a proc macro two levels down, injecting std.
#[test]
fn a_proc_macro_without_an_edge_is_reported() {
    let telemetry = report(&[("std", "hidden_macro")]);
    assert_eq!(
        telemetry.proc_macro_std_unreachable_injectors.len(),
        1,
        "expected hidden-macro to be reported, got {:?}",
        telemetry.proc_macro_std_unreachable_injectors
    );
    let entry = &telemetry.proc_macro_std_unreachable_injectors[0];
    assert_eq!(entry.macro_crate, "hidden_macro");
    assert_eq!(entry.records, 1);
}

/// The report has to say how the macro got in, or it names a crate the user
/// cannot act on.
#[test]
fn the_report_names_the_dependency_it_came_through() {
    let telemetry = report(&[("std", "hidden_macro")]);
    let entry = &telemetry.proc_macro_std_unreachable_injectors[0];
    assert!(
        entry.parents.iter().any(|p| p.starts_with("reexporter")),
        "expected reexporter among the parents, got {:?}",
        entry.parents
    );
}

/// An edge the manifest owns is the parking's territory, whether or not the
/// parking succeeded. Reporting it here would double-report every direct
/// injector.
#[test]
fn a_direct_proc_macro_edge_is_not_reported() {
    let telemetry = report(&[("std", "direct_macro")]);
    assert!(
        telemetry.proc_macro_std_unreachable_injectors.is_empty(),
        "direct-macro is an edge of the manifest, got {:?}",
        telemetry.proc_macro_std_unreachable_injectors
    );
}

/// A `macro_rules!` in an ordinary dependency has no feature of its own to turn
/// off, so it is not this bucket — the report would name something with no
/// action behind it.
#[test]
fn a_non_proc_macro_crate_is_not_reported() {
    let telemetry = report(&[("std", "plain_helper")]);
    assert!(
        telemetry.proc_macro_std_unreachable_injectors.is_empty(),
        "plain-helper is not a proc macro, got {:?}",
        telemetry.proc_macro_std_unreachable_injectors
    );
}

/// `std::println!` expands from std. That is the crate's own std usage, not an
/// injection by a dependency.
#[test]
fn an_expansion_from_the_sysroot_is_not_reported() {
    for expansion in ["std", "core", "alloc", "LOCAL"] {
        let telemetry = report(&[("std", expansion)]);
        assert!(
            telemetry.proc_macro_std_unreachable_injectors.is_empty(),
            "{} is not an injecting dependency, got {:?}",
            expansion,
            telemetry.proc_macro_std_unreachable_injectors
        );
    }
}

/// Only std records count. A macro that expands to `core::fmt` is doing nothing
/// this crate needs to hear about.
#[test]
fn a_non_std_record_is_not_reported() {
    let telemetry = report(&[("core", "hidden_macro")]);
    assert!(
        telemetry.proc_macro_std_unreachable_injectors.is_empty(),
        "a core record is not injected std, got {:?}",
        telemetry.proc_macro_std_unreachable_injectors
    );
}

/// Records are counted, not just detected — the count is what says how much of
/// the crate's reported std belongs to the macro.
#[test]
fn every_injected_record_is_counted() {
    let telemetry = report(&[
        ("std", "hidden_macro"),
        ("std", "hidden_macro"),
        ("core", "hidden_macro"),
    ]);
    assert_eq!(telemetry.proc_macro_std_unreachable_injectors[0].records, 2);
}

/// The lookup is by crate name with `-`/`_` folded: `hidden-macro` the package
/// is `hidden_macro` the crate, and a record only ever carries the latter.
#[test]
fn the_package_and_crate_spellings_are_the_same_lookup() {
    let dir = nostd::parser::find_sibling_crate_dir(&manifest(), "hidden_macro");
    assert!(dir.is_some(), "underscore spelling must resolve");
    assert_eq!(
        dir,
        nostd::parser::find_sibling_crate_dir(&manifest(), "hidden-macro"),
        "both spellings must resolve to the same directory"
    );
}

/// The fixture has to be a proc macro for any of this to mean anything.
#[test]
fn the_fixture_macro_is_actually_a_proc_macro() {
    let dir: PathBuf = nostd::parser::find_sibling_crate_dir(&manifest(), "hidden_macro").unwrap();
    assert!(nostd::parser::crate_dir_is_proc_macro(&dir));
    let plain: PathBuf = nostd::parser::find_sibling_crate_dir(&manifest(), "plain_helper").unwrap();
    assert!(!nostd::parser::crate_dir_is_proc_macro(&plain));
}

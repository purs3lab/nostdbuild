#![feature(rustc_private)]

//! Regression tests for `#[cfg_attr(pred, some_attr(..))]` on an unconditional
//! item. A conditionally-applied proc-macro (derive or attribute macro) generates
//! code that rustc maps back to a span *inside* the cfg_attr tokens; that region
//! must be gated by `pred`, or the generated std usage looks unguarded.
//! See base58_monero-2.0.0 (derive(Error)) and accesskit-0.18.0 (pyo3 pyclass).

use std::path::{Path, PathBuf};

use nostd::types::ReadableSpan;
use nostd::visitor::{ModCollector, ancestors_for_span};

fn fixture() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/cfg_attr_applied/lib.rs")
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

#[test]
fn cfg_attr_applied_attribute_region_is_gated() {
    let root = fixture();
    let content = std::fs::read_to_string(&root).unwrap();

    let ctx = z3::Context::new(&z3::Config::new());
    let mut collector = ModCollector::new(&ctx);
    let node = collector.collect(&root, "cfg_attr_applied");

    // Derive form: span of the derive name inside `cfg_attr(feature="std", ...)`.
    let derive_target = span_of(&content, "MarkerGatedDerive");
    assert!(
        ancestors_for_span(&node, &derive_target).is_some(),
        "span inside cfg_attr(feature=\"std\", derive(..)) should be gated, got None"
    );

    // Attribute-macro form, multi-line cfg_attr (accesskit shape): a token deep
    // inside the applied attribute's argument list.
    let attr_target = span_of(&content, "marker_gated_eq");
    assert!(
        ancestors_for_span(&node, &attr_target).is_some(),
        "span inside multi-line cfg_attr(feature=\"pyo3\", pyclass(..)) should be gated, got None"
    );
}

/// An inner `#![cfg_attr(..)]` is crate-level config (e.g. `no_std`), not a
/// proc-macro — it generates no code, so it must not record a gate. Recording one
/// adds a bogus item to the covering-set pool / `all_constraints` and perturbs the
/// solver; that regressed tarfs-0.2.7, where the lost `not(builtin_devices)`
/// constraint let a std-using module be enabled in a no_std build.
#[test]
fn inner_cfg_attr_does_not_record_a_gate() {
    let root = fixture();
    let content = std::fs::read_to_string(&root).unwrap();

    let ctx = z3::Context::new(&z3::Config::new());
    let mut collector = ModCollector::new(&ctx);
    let node = collector.collect(&root, "cfg_attr_applied");

    // A span sitting inside the inner `#![cfg_attr(not(feature = "std"), no_std)]`
    // must not resolve to a gate.
    let target = span_of(&content, "not(feature = \"std\"), no_std");
    let anc = ancestors_for_span(&node, &target);
    assert!(
        anc.is_none(),
        "inner #![cfg_attr(..)] must not record a gate item, got {anc:?}"
    );
}

#[test]
fn plain_derive_without_cfg_attr_stays_ungated() {
    let root = fixture();
    let content = std::fs::read_to_string(&root).unwrap();

    let ctx = z3::Context::new(&z3::Config::new());
    let mut collector = ModCollector::new(&ctx);
    let node = collector.collect(&root, "cfg_attr_applied");

    // Control: a plain `#[derive(..)]` carries no cfg_attr predicate.
    let target = span_of(&content, "MarkerUngatedDerive");
    let anc = ancestors_for_span(&node, &target);
    assert!(
        anc.is_none(),
        "plain derive (no cfg_attr) must stay ungated, got {anc:?}"
    );
}

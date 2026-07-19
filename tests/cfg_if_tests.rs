#![feature(rustc_private)]

//! Regression tests for `cfg_if::cfg_if!` gating. The `#[cfg(..)]` predicate in a
//! cfg_if arm sits after `if` and gates a following brace group, so the visitor
//! parses the arm grammar and gates each arm body by span containment.
//! See all_is_cubes, backtrace, bevy_platform.

use std::path::{Path, PathBuf};

use nostd::types::ReadableSpan;
use nostd::visitor::{ModCollector, ancestors_for_span};

fn fixture() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/cfg_if_macro/lib.rs")
}

/// Build a `ReadableSpan` pointing at the `needle` substring in the fixture.
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
fn cfg_if_arms_gate_std_paths() {
    let root = fixture();
    let content = std::fs::read_to_string(&root).unwrap();

    let ctx = z3::Context::new(&z3::Config::new());
    let mut collector = ModCollector::new(&ctx);
    let node = collector.collect(&root, "cfg_if_macro");

    // std paths inside a `if #[cfg(..)] { .. }` / `else { .. }` arm — each must
    // resolve to a gate. Includes else-arm, qualified-path form, and a std path
    // inside a nested cfg_if.
    for needle in [
        "std::path::PathPlain",            // if #[cfg(feature="std")] arm
        "core::fmt::CoreFallback",         // trailing else arm
        "std::collections::HashMapNested", // qualified cfg_if::cfg_if! form
        "std::os::windows::NestedWin",     // nested cfg_if inside std arm
        "std::os::unix::NestedUnix",       // nested else inside std arm
        "std::time::ElseIfStd",            // std path in an `else if` arm
    ] {
        let target = span_of(&content, needle);
        let anc = ancestors_for_span(&node, &target);
        assert!(
            anc.is_some(),
            "{needle} should be gated by its cfg_if arm predicate, got None"
        );
    }
}

#[test]
fn std_path_outside_cfg_if_is_ungated() {
    let root = fixture();
    let content = std::fs::read_to_string(&root).unwrap();

    let ctx = z3::Context::new(&z3::Config::new());
    let mut collector = ModCollector::new(&ctx);
    let node = collector.collect(&root, "cfg_if_macro");

    // `std::sync::UngatedArc` sits outside any cfg_if and must NOT be gated.
    let target = span_of(&content, "std::sync::UngatedArc");
    let anc = ancestors_for_span(&node, &target);
    assert!(
        anc.is_none(),
        "std path outside cfg_if must not be gated, got {anc:?}"
    );
}

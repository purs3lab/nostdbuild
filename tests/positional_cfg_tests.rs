#![feature(rustc_private)]

//! Regression tests for a bare positional `cfg(...)` macro argument gating an
//! invocation's std usage (among-0.1.7 `impl_specific_ref_and_mut!` pattern).
//! Unlike `leading_cfg_condition` (the `#[cfg(...)]` attribute form), the guard
//! here is a plain positional arg sibling to the std type. It is a
//! probe-validated *candidate* gate: the visitor proposes it over the whole
//! invocation and the probe confirms by negating it.

use std::path::{Path, PathBuf};

use nostd::types::ReadableSpan;
use nostd::visitor::{ModCollector, ancestors_for_span};

fn fixture() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/positional_cfg/lib.rs")
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
fn positional_cfg_arg_gates_sibling_std_path() {
    let root = fixture();
    let content = std::fs::read_to_string(&root).unwrap();

    let ctx = z3::Context::new(&z3::Config::new());
    let mut collector = ModCollector::new(&ctx);
    let node = collector.collect(&root, "positional_cfg");

    // `::std::path::Path` sits in the invocation carrying `cfg(feature = "std")`.
    let target = span_of(&content, "::std::path::Path");
    let anc = ancestors_for_span(&node, &target);
    assert!(
        anc.is_some(),
        "std path should be gated by the positional cfg(feature=\"std\") sibling arg, got None"
    );
}

#[test]
fn std_path_without_positional_cfg_stays_ungated() {
    let root = fixture();
    let content = std::fs::read_to_string(&root).unwrap();

    let ctx = z3::Context::new(&z3::Config::new());
    let mut collector = ModCollector::new(&ctx);
    let node = collector.collect(&root, "positional_cfg");

    // `::std::sync::Arc` is in an invocation with NO cfg arg — must not be gated.
    let target = span_of(&content, "::std::sync::Arc<u8>");
    let anc = ancestors_for_span(&node, &target);
    assert!(
        anc.is_none(),
        "std path in an invocation without a positional cfg must stay ungated, got {anc:?}"
    );
}

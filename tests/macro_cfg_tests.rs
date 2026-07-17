#![feature(rustc_private)]

//! Regression tests for `#[cfg(...)]` gates that live at a macro *invocation*
//! site (passed as `$(#[$attrs:meta])*`). syn cannot see inside macro
//! invocations, so the visitor parses the invocation tokens to recognise these
//! gates by span containment. See valuable-0.1.1 `value!` / `collection!`.

use std::path::{Path, PathBuf};

use nostd::types::ReadableSpan;
use nostd::visitor::{ModCollector, ancestors_for_span};

fn fixture() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/macro_cfg_invocation/lib.rs")
}

/// Build a `ReadableSpan` pointing at the `needle` substring in the fixture, as
/// the HIR pass would (1-based line, 0-based column). Panics if not found.
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
fn invocation_cfg_gates_std_paths() {
    let root = fixture();
    let content = std::fs::read_to_string(&root).unwrap();

    let ctx = z3::Context::new(&z3::Config::new());
    let mut collector = ModCollector::new(&ctx);
    let node = collector.collect(&root, "macro_cfg_invocation");

    // Each of these std paths sits under a `#[cfg(feature = "std")]` at the
    // invocation site and must resolve to a gate.
    for needle in [
        "std::path::Path",
        "std::error::Error",
        // Spans the end of a generic-arg list — the comma inside `<T, H>` must
        // not truncate the segment.
        "std::collections::HashSet<T, H>",
    ] {
        let target = span_of(&content, needle);
        let anc = ancestors_for_span(&node, &target);
        assert!(
            anc.is_some(),
            "{needle} should be gated by the invocation-site #[cfg(feature=\"std\")], got None"
        );
    }
}

#[test]
fn ungated_macro_entries_are_not_gated() {
    let root = fixture();
    let content = std::fs::read_to_string(&root).unwrap();

    let ctx = z3::Context::new(&z3::Config::new());
    let mut collector = ModCollector::new(&ctx);
    let node = collector.collect(&root, "macro_cfg_invocation");

    // `UngatedWrapper<T>` appears as an ungated `collection!` entry right after a
    // std-gated one. Its region must NOT inherit the previous entry's cfg.
    let target = span_of(&content, "UngatedWrapper<T>,");
    let anc = ancestors_for_span(&node, &target);
    assert!(
        anc.is_none(),
        "ungated macro entry must not inherit a neighbouring cfg gate, got {anc:?}"
    );
}

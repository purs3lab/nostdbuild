#![feature(rustc_private)]

//! Regression tests for `#[cfg(...)] include!("path")`. Two things must hold:
//!  * the cfg on the `include!` statement gates everything the included file
//!    contributes (previously the early return in `visit_item_macro` dropped it), and
//!  * the included file's spans are reachable at all — `find_ancestors_for_span`
//!    scans a node's items only when the node's `source_file` matches the target
//!    span's file, so content visited *inline* from another file could never be
//!    found. The included file is therefore registered as its own gated child node.
//! See bigdecimal-0.4.8 (`#[cfg(feature = "std")] include!("./with_std.rs");`).

use std::path::{Path, PathBuf};

use nostd::types::ReadableSpan;
use nostd::visitor::{ModCollector, ancestors_for_span};

fn root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/include_cfg/lib.rs")
}

/// Span of `needle` inside the included file `rel`, reported the way rustc does:
/// the path as written at the include site (note the `./`).
fn span_of(rel: &str, needle: &str) -> ReadableSpan {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/include_cfg")
        .join(rel);
    let content = std::fs::read_to_string(&path).unwrap();
    let idx = content
        .find(needle)
        .unwrap_or_else(|| panic!("substring {needle:?} not found in {rel}"));
    let before = &content[..idx];
    let line = before.matches('\n').count() + 1;
    let col = idx - before.rfind('\n').map(|p| p + 1).unwrap_or(0);
    ReadableSpan {
        file: format!("./{rel}"),
        start_line: line,
        start_col: col,
        end_line: line,
        end_col: col + needle.len(),
        usage_crate: Some("std".to_string()),
    }
}

#[test]
fn cfg_on_include_gates_the_included_file() {
    let ctx = z3::Context::new(&z3::Config::new());
    let mut collector = ModCollector::new(&ctx);
    let node = collector.collect(&root(), "include_cfg");

    // std usage inside a file pulled in by `#[cfg(feature = "std")] include!(..)`.
    let target = span_of("inc_gated.rs", "std::string::MarkerGatedInclude");
    let anc = ancestors_for_span(&node, &target);
    assert!(
        anc.is_some(),
        "std usage in a cfg-gated include! should be gated by the include-site cfg, got None"
    );
}

#[test]
fn ungated_include_stays_ungated() {
    let ctx = z3::Context::new(&z3::Config::new());
    let mut collector = ModCollector::new(&ctx);
    let node = collector.collect(&root(), "include_cfg");

    // Control: no cfg on this include!, so its std usage must remain ungated.
    let target = span_of("inc_ungated.rs", "std::string::MarkerUngatedInclude");
    let anc = ancestors_for_span(&node, &target);
    assert!(
        anc.is_none(),
        "std usage in an ungated include! must stay ungated, got {anc:?}"
    );
}

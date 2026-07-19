#![feature(rustc_private)]

//! Regression tests for modules declared from inside a `macro_rules!` invocation
//! as passthrough `$item` args (agnostic_lite `cfg_time!` /
//! `cfg_time_with_docsrs!` pattern). syn can't expand the macro, and the plugin's
//! `macro_module_imports` skips these because a passthrough mod's span is not
//! `from_expansion()`. So the syn visitor scans invocation tokens for `mod X;`
//! and registers the module so its file is walked and its std usage is gated.

use std::path::{Path, PathBuf};

use nostd::types::ReadableSpan;
use nostd::visitor::{ModCollector, ancestors_for_span};

fn root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/macro_mod_decl/lib.rs")
}

/// `ReadableSpan` for `needle` in `file` (path relative to the fixture dir).
fn span_of(rel: &str, needle: &str) -> ReadableSpan {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/macro_mod_decl")
        .join(rel);
    let content = std::fs::read_to_string(&path).unwrap();
    let idx = content
        .find(needle)
        .unwrap_or_else(|| panic!("substring {needle:?} not found in {rel}"));
    let before = &content[..idx];
    let line = before.matches('\n').count() + 1;
    let col = idx - before.rfind('\n').map(|p| p + 1).unwrap_or(0);
    ReadableSpan {
        file: rel.to_string(),
        start_line: line,
        start_col: col,
        end_line: line,
        end_col: col + needle.len(),
        usage_crate: Some("std".to_string()),
    }
}

#[test]
fn macro_declared_module_file_is_walked_and_gated() {
    let ctx = z3::Context::new(&z3::Config::new());
    let mut c = ModCollector::new(&ctx);
    let node = c.collect(&root(), "macro_mod_decl");

    // leaf.rs: `mod leaf;` came from `cfg_time! { .. }`. Its std usage is gated by
    // an inner `#[cfg(feature = "std")]` — only reachable if the file was walked.
    let leaf = span_of("leaf.rs", "std::string::String");
    assert!(
        ancestors_for_span(&node, &leaf).is_some(),
        "leaf.rs std usage should be gated (module declared via macro must be walked)"
    );

    // grandchild.rs: `mod grandchild;` came from a macro inside `child.rs`, which
    // is itself declared `#[cfg(feature = "parent")]`. The unconditional std
    // usage must inherit `feature = "parent"` through the macro-declared modules.
    let gc = span_of("child/grandchild.rs", "std::vec::Vec");
    assert!(
        ancestors_for_span(&node, &gc).is_some(),
        "grandchild std usage should inherit the parent chain gate through the macro"
    );
}

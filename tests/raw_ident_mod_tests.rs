#![feature(rustc_private)]

//! Regression test for raw-identifier module declarations (`mod r#move;`).
//!
//! Rust resolves `mod r#move;` to the file named by the *unraw* identifier —
//! `move/mod.rs` — because `r#` is only a lexer escape, not part of the name.
//! syn's `Ident::to_string()` keeps the `r#`, so without stripping it the whole
//! subtree resolves to a nonexistent `r#move/mod.rs`, is never walked, and every
//! std usage inside it looks unguarded (ref_kind's `mod r#move;`).

use std::path::{Path, PathBuf};

use nostd::types::ReadableSpan;
use nostd::visitor::{ModCollector, ancestors_for_span};

fn root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/raw_ident_mod/lib.rs")
}

/// `ReadableSpan` for `needle` in `file` (path relative to the fixture dir).
fn span_of(rel: &str, needle: &str) -> ReadableSpan {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/raw_ident_mod")
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
fn raw_identifier_module_file_is_walked_and_gated() {
    let ctx = z3::Context::new(&z3::Config::new());
    let mut c = ModCollector::new(&ctx);
    let node = c.collect(&root(), "raw_ident_mod");

    // `mod r#move;` must resolve to `move/mod.rs`. The std usage there is gated by
    // `#[cfg(feature = "std")]` on its impl — `ancestors_for_span` returns `Some`
    // only if the raw-ident module was resolved to the unraw file and walked.
    let s = span_of("move/mod.rs", "std::string::String");
    assert!(
        ancestors_for_span(&node, &s).is_some(),
        "raw-ident module `mod r#move;` must resolve to move/mod.rs and be walked"
    );
}

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

/// The `#[cfg]` on the *invocation* gates what the invocation declares.
///
/// serde_json's `#[cfg(not(any(feature = "std", feature = "alloc")))]
/// hide_from_rustfmt! { mod error; }` used to register `error.rs` with no gate:
/// the scan received only the ambient condition stack ANDed with the macro
/// *definition*'s gate, and the item's own attribute was parsed for the
/// `LocalItem` and then dropped. Everything in a file reached that way then read
/// as unconditional.
#[test]
fn module_declared_by_a_gated_macro_invocation_inherits_the_invocation_gate() {
    let ctx = z3::Context::new(&z3::Config::new());
    let mut c = ModCollector::new(&ctx);
    let node = c.collect(&root(), "macro_mod_decl");

    // invoked.rs holds an ungated std usage, so the ONLY gate that can reach it
    // is `#[cfg(feature = "invocation")]` on the `passthrough! { mod invoked; }`
    // invocation.
    let invoked = span_of("invoked.rs", "std::string::String");
    assert!(
        ancestors_for_span(&node, &invoked).is_some(),
        "std usage in a module declared by a #[cfg]-gated macro invocation must \
         carry that gate"
    );

    // Control: same macro, no attribute on the invocation, nothing gating the
    // usage — the fix must not manufacture a gate where none exists.
    let ungated = span_of("ungated.rs", "std::string::String");
    assert!(
        ancestors_for_span(&node, &ungated).is_none(),
        "an ungated macro invocation must leave its module ungated"
    );
}

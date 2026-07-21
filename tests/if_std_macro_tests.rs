#![feature(rustc_private)]

//! Regression tests for a `macro_rules!` that applies one uniform `#[cfg(...)]`
//! to every item it expands — futures-preview's `if_std!`:
//!
//! ```ignore
//! macro_rules! if_std {
//!     ($($i:item)*) => ($( #[cfg(feature = "std")] $i )*)
//! }
//!
//! if_std! {          // <- no attribute here
//!     mod mpsc;      //    yet everything expands under `feature = "std"`
//!     use std::sync::Mutex;
//! }
//! ```
//!
//! The gate is in the *definition*, so the invocation looks unconditional. The
//! plugin's `macro_body_cfgs` cannot supply it: that attaches a definition's
//! cfgs only to paths whose spans are `from_expansion()`, and tokens passed
//! through an `$i:item` matcher keep their original call-site spans. It handles
//! the mirror-image shape, where the std path is written in the definition too.
//!
//! Both sub-shapes are covered: items expanded inline (futures-sink-preview) and
//! `mod X;` declarations (futures-channel-preview). The latter only became a
//! *reachable* false positive after bucket H — before that the file was invisible
//! rather than visible-but-ungated.

use std::path::{Path, PathBuf};

use nostd::types::ReadableSpan;
use nostd::visitor::{ModCollector, ancestors_for_span, externally_gated_for_span};

fn fixture() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/if_std_macro/lib.rs")
}

fn span_in(file: &str, content: &str, needle: &str) -> ReadableSpan {
    let idx = content
        .find(needle)
        .unwrap_or_else(|| panic!("substring {needle:?} not found in fixture"));
    let before = &content[..idx];
    let line = before.matches('\n').count() + 1;
    let col = idx - before.rfind('\n').map(|p| p + 1).unwrap_or(0);
    ReadableSpan {
        file: file.to_string(),
        start_line: line,
        start_col: col,
        end_line: line,
        end_col: col + needle.len(),
        usage_crate: Some("std".to_string()),
    }
}

fn with_tree<F: FnOnce(&nostd::visitor::ModNode<'_>, &str, &str)>(f: F) {
    let root = fixture();
    let lib = std::fs::read_to_string(&root).unwrap();
    let gated = std::fs::read_to_string(root.parent().unwrap().join("gated_mod.rs")).unwrap();
    let cfg = z3::Config::new();
    let ctx = z3::Context::new(&cfg);
    let mut collector = ModCollector::new(&ctx);
    let tree = collector.collect(&root, "lib");
    f(&tree, &lib, &gated);
}

#[test]
fn inline_item_inside_if_std_is_gated() {
    // futures-sink-preview's shape: `impl Sink for ::std::vec::Vec<T>` and
    // friends sit directly in the `if_std!` body.
    with_tree(|tree, lib, _| {
        let span = span_in("lib.rs", lib, "std::sync::Mutex");
        let rendered = format!("{:?}", ancestors_for_span(tree, &span));
        assert!(
            rendered.contains("std"),
            "an item expanded by `if_std!` must inherit the definition's \
             `feature = \"std\"` gate, got: {rendered}"
        );
    });
}

#[test]
fn mod_decl_inside_if_std_is_gated() {
    // futures-channel-preview's shape: `if_std! { mod lock; pub mod mpsc; }`.
    // Bucket H made these files visible; without the gate every span in them
    // reads as unguarded std (26 spans in that crate).
    with_tree(|tree, _, gated| {
        let span = span_in("gated_mod.rs", gated, "std::collections::HashMap");
        let rendered = format!("{:?}", ancestors_for_span(tree, &span));
        assert!(
            rendered.contains("std"),
            "a module declared inside `if_std!` must have its file gated by the \
             definition's cfg, got: {rendered}"
        );
    });
}

// ---------------------------------------------------------------------------
// Controls — a spurious gate would excuse real std usage, so the extraction is
// deliberately strict. These pin that strictness.
// ---------------------------------------------------------------------------

#[test]
fn control_macro_without_a_cfg_gates_nothing() {
    with_tree(|tree, lib, _| {
        let span = span_in("lib.rs", lib, "std::fs::File");
        let ancestors = ancestors_for_span(tree, &span);
        assert!(
            ancestors.is_none_or(|a| a.is_empty()),
            "a passthrough macro applies no cfg, so its body must stay ungated"
        );
        assert!(
            !externally_gated_for_span(tree, &span),
            "a passthrough macro's body must not be excused as externally gated"
        );
    });
}

#[test]
fn control_rules_disagreeing_on_the_gate_are_rejected() {
    // Only one arm matches at expansion, and we cannot tell which from the
    // invocation alone. Naming either gate could excuse genuinely hard std, so
    // the macro must contribute nothing.
    with_tree(|tree, lib, _| {
        let span = span_in("lib.rs", lib, "std::net::TcpStream");
        let ancestors = ancestors_for_span(tree, &span);
        assert!(
            ancestors.is_none_or(|a| a.is_empty()),
            "rules that disagree on their cfg must yield no gate"
        );
    });
}

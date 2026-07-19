#![feature(rustc_private)]

//! Regression tests for `#[cfg(...)]` on a struct *literal* field
//! (`Self { #[cfg(feature = "std")] backtrace: <std expr> }`). These fields are
//! `syn::FieldValue`, distinct from the struct *definition* fields (`syn::Field`)
//! handled by `visit_field`, so they need their own visit handler.
//! See array_section-0.2.3 and bevy_ecs-0.16.0.

use std::path::{Path, PathBuf};

use nostd::types::ReadableSpan;
use nostd::visitor::{ModCollector, ancestors_for_span};

fn fixture() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/field_value_cfg/lib.rs")
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
fn cfg_on_struct_literal_field_gates_its_value() {
    let root = fixture();
    let content = std::fs::read_to_string(&root).unwrap();

    let ctx = z3::Context::new(&z3::Config::new());
    let mut collector = ModCollector::new(&ctx);
    let node = collector.collect(&root, "field_value_cfg");

    // The std expression is the value of a `#[cfg(feature = "std")]` field in a
    // struct literal — it must resolve to a gate.
    let target = span_of(&content, "std::backtrace::GatedCapture::capture()");
    let anc = ancestors_for_span(&node, &target);
    assert!(
        anc.is_some(),
        "std expr in a cfg-gated struct-literal field should be gated, got None"
    );
}

#[test]
fn ungated_struct_literal_field_stays_ungated() {
    let root = fixture();
    let content = std::fs::read_to_string(&root).unwrap();

    let ctx = z3::Context::new(&z3::Config::new());
    let mut collector = ModCollector::new(&ctx);
    let node = collector.collect(&root, "field_value_cfg");

    // Control: a struct-literal field with no cfg must not pick up a gate.
    let target = span_of(&content, "std::mem::UngatedSize::of()");
    let anc = ancestors_for_span(&node, &target);
    assert!(
        anc.is_none(),
        "std expr in an ungated struct-literal field must stay ungated, got {anc:?}"
    );
}

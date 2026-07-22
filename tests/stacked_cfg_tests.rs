#![feature(rustc_private)]

//! Regression test for multiple `#[cfg(..)]` attributes stacked on one item.
//!
//! Stacked cfgs AND together in Rust — the item is present only if every one
//! holds. `parse_cfg_gate` used to take just the first, dropping the rest, so
//! `#[cfg(feature = "block-padding")] #[cfg(feature = "std")] impl std::error::Error`
//! came out gated on `block-padding` alone. Negating that gate then re-enabled
//! std and manufactured a false positive (inout).

use std::path::{Path, PathBuf};

use nostd::types::ReadableSpan;
use nostd::visitor::{ModCollector, ancestors_for_span};

fn root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/stacked_cfg/lib.rs")
}

fn span_of(rel: &str, needle: &str) -> ReadableSpan {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/stacked_cfg")
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
fn stacked_cfg_attributes_are_anded() {
    let ctx = z3::Context::new(&z3::Config::new());
    let mut c = ModCollector::new(&ctx);
    let node = c.collect(&root(), "stacked_cfg");

    let s = span_of("lib.rs", "std::error::Error");
    let gate =
        ancestors_for_span(&node, &s).expect("the stacked-cfg impl must be recorded with a gate");
    let text = gate
        .iter()
        .map(|b| b.to_string())
        .collect::<Vec<_>>()
        .join(" ");

    // The two assertions are mutual controls: `alpha` proves the first cfg is
    // still there (the fix did not merely swap gates), `std` proves the stacked
    // second cfg is now ANDed in (before the fix it was dropped → this fails).
    assert!(
        text.contains("alpha"),
        "gate should carry the first cfg (alpha); got {text:?}"
    );
    assert!(
        text.contains("std"),
        "gate should also carry the stacked `#[cfg(feature = \"std\")]`; got {text:?}"
    );
}

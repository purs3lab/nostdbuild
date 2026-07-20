#![feature(rustc_private)]

//! Regression tests for `mod X;` declared inside a `cfg_if!` arm.
//!
//! syn hands macro tokens over opaquely, and `visit_item_macro` early-returned
//! for cfg_if before reaching `scan_macro_mod_decls`, so these modules were
//! never registered as child `ModNode`s at all. Their files were never walked,
//! so every std usage inside them had no covering node — hence no gate, hence
//! hard std. That is backtrace's `src/symbolize/gimli/*` (67 spans) and
//! hpm_rt's `src/host/*` (25 spans).
//!
//! The bug is not target-specific: a `#[cfg(feature = "std")]` arm was equally
//! invisible, which is why `feature_gated_arm_mod_is_registered` is here.

use std::path::{Path, PathBuf};

use nostd::visitor::{ModCollector, ModNode, externally_gated_for_span};
use nostd::types::ReadableSpan;

fn fixture() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/cfg_if_mod_decl/lib.rs")
}

fn with_tree<F: FnOnce(&ModNode<'_>)>(f: F) {
    let root = fixture();
    let cfg = z3::Config::new();
    let ctx = z3::Context::new(&cfg);
    let mut collector = ModCollector::new(&ctx);
    let tree = collector.collect(&root, "lib");
    f(&tree);
}

fn children_named<'t>(tree: &'t ModNode<'t>, name: &str) -> Vec<&'t ModNode<'t>> {
    tree.children.iter().filter(|c| c.name == name).collect()
}

#[test]
fn path_attr_arms_each_register_their_own_file() {
    with_tree(|tree| {
        let mmaps = children_named(tree, "mmap");
        assert_eq!(
            mmaps.len(),
            2,
            "each cfg_if arm declaring `mod mmap;` must produce its own child node"
        );
        let mut files: Vec<String> = mmaps
            .iter()
            .map(|c| c.source_file.file_name().unwrap().to_string_lossy().into_owned())
            .collect();
        files.sort();
        assert_eq!(
            files,
            vec!["mmap_unix.rs", "mmap_windows.rs"],
            "#[path] must be honoured; without it both arms collapse onto a nonexistent mmap.rs"
        );
    });
}

#[test]
fn path_attr_arm_files_are_actually_walked() {
    with_tree(|tree| {
        // Resolution is only useful if `resolve_child` then walks the file.
        let walked = children_named(tree, "mmap")
            .iter()
            .any(|c| !c.local_items.is_empty());
        assert!(
            walked,
            "a registered #[path] module must have its file walked into local_items"
        );
    });
}

#[test]
fn feature_gated_arm_mod_is_registered() {
    with_tree(|tree| {
        let featmods = children_named(tree, "featmod");
        assert_eq!(featmods.len(), 1, "`mod featmod;` in a feature-gated arm must be registered");
        assert!(
            featmods[0].entry_condition.is_some(),
            "a feature-gated arm must pass its condition to the module it declares"
        );
    });
}

#[test]
fn default_resolution_arm_mod_is_registered() {
    with_tree(|tree| {
        let hosts = children_named(tree, "host");
        assert_eq!(hosts.len(), 1, "`mod host;` with no #[path] must resolve to host/mod.rs");
        assert!(
            hosts[0].source_file.ends_with("host/mod.rs"),
            "expected host/mod.rs, got {:?}",
            hosts[0].source_file
        );
    });
}

#[test]
fn target_gated_arm_mod_is_externally_gated() {
    with_tree(|tree| {
        // The end-to-end chain this bucket exists to enable: once the module is
        // registered, bucket G's rule must excuse the std usage inside it.
        let span = ReadableSpan {
            file: "host/mod.rs".to_string(),
            start_line: 1,
            start_col: 8,
            end_line: 1,
            end_col: 21,
            usage_crate: Some("std".to_string()),
        };
        assert!(
            externally_gated_for_span(tree, &span),
            "std inside a target-gated cfg_if arm's module must be externally gated"
        );
    });
}

#[test]
fn cfg_if_arms_are_recorded_once() {
    with_tree(|tree| {
        // `visit_item_macro` used to record the arms and then descend into
        // `visit_macro`, which recorded them again.
        let mut spans: Vec<(usize, usize)> = tree
            .local_items
            .iter()
            .map(|i| (i.span.start_line, i.span.end_line))
            .collect();
        let before = spans.len();
        spans.sort();
        spans.dedup();
        assert_eq!(
            before,
            spans.len(),
            "each cfg_if arm must produce exactly one LocalItem, found duplicates: {spans:?}"
        );
    });
}

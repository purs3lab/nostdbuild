#![feature(rustc_private)]

//! Regression tests for KI-19: a file `syn` rejects, or a manifest `cargo
//! metadata` refuses, used to end the whole process. 338 of the 344 panicked
//! runs in the run30 corpus died on the first, and every dependent of
//! `secp256k1-sys` on the second.
//!
//! Both are facts about one input, not reasons to abort: the module contributes
//! no items, the crate still gets an analysis, and the miss is recorded in
//! `telemetry.files_syn_failed` / `telemetry.cargo_metadata_failed`.

use std::fs;
use std::path::{Path, PathBuf};

use nostd::visitor::{
    ModCollector, cargo_metadata_failures, collect_named_items_with_conditions, declared_features,
    find_entrypoints, syn_failed_files,
};

fn fixture() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/unparseable_file/lib.rs")
}

#[test]
fn unparseable_module_does_not_kill_the_walk() {
    let root = fixture();
    let ctx = z3::Context::new(&z3::Config::new());
    let mut collector = ModCollector::new(&ctx);

    // The panic this replaces happened here.
    let node = collector.collect(&root, "unparseable_file");

    let names: Vec<String> = collect_named_items_with_conditions(&node, &ctx)
        .into_iter()
        .map(|(name, _)| name)
        .collect();

    // The rest of the crate is still analysed…
    assert!(
        names.iter().any(|n| n == "marker"),
        "sibling module's items should survive an unparseable module, got {names:?}"
    );
    // …and the unparseable module contributes nothing rather than everything.
    assert!(
        !names.iter().any(|n| n == "features_check_item"),
        "unparseable module must contribute no items, got {names:?}"
    );

    assert!(
        syn_failed_files()
            .iter()
            .any(|f| f.ends_with("features_check.rs")),
        "the skipped file must be recorded, got {:?}",
        syn_failed_files()
    );
}

/// `secp256k1-sys` 0.8–0.10: vendored C, `links` only, and a published manifest
/// cargo rejects with `no targets specified`. Every dependent used to panic.
fn write_targetless_crate(dir: &Path) -> PathBuf {
    let _ = fs::remove_dir_all(dir);
    fs::create_dir_all(dir).unwrap();
    let manifest = dir.join("Cargo.toml");
    fs::write(
        &manifest,
        r#"[package]
name = "targetless"
version = "0.1.0"
edition = "2018"
links = "targetless"

[features]
std = []
alloc = []
"#,
    )
    .unwrap();
    manifest
}

#[test]
fn manifest_cargo_refuses_yields_no_entrypoints() {
    let tmp = std::env::temp_dir().join("nostd_targetless_entrypoints");
    let manifest = write_targetless_crate(&tmp);
    let manifest = manifest.to_str().unwrap();

    let mut known_modules = Vec::new();
    let root = find_entrypoints(manifest, &mut known_modules);

    assert_eq!(root, tmp);
    assert!(
        known_modules.is_empty(),
        "a crate with no sources has no entrypoints, got {known_modules:?}"
    );
    assert!(
        cargo_metadata_failures().iter().any(|m| m == manifest),
        "the refused manifest must be recorded, got {:?}",
        cargo_metadata_failures()
    );

    fs::remove_dir_all(&tmp).unwrap();
}

/// Falling back to an *empty* feature set would erase every `feature = "X"` atom
/// in the crate (see `ModCollector::with_known_features`), turning gated code
/// into unconditional code. Read them off the manifest instead.
#[test]
fn features_survive_a_refused_manifest() {
    let tmp = std::env::temp_dir().join("nostd_targetless_features");
    let manifest = write_targetless_crate(&tmp);

    let feats = declared_features(manifest.to_str().unwrap());

    assert!(feats.contains("std"), "got {feats:?}");
    assert!(feats.contains("alloc"), "got {feats:?}");

    fs::remove_dir_all(&tmp).unwrap();
}

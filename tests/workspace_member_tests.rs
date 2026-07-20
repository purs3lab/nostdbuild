#![feature(rustc_private)]

//! Tests for the `cargo metadata` fallback in `find_entrypoints`.
//!
//! Published crates.io tarballs sometimes keep a `[workspace]` section whose
//! members were stripped out of the archive (old `libc` referencing
//! `libc-test/`). Cargo refuses to read such a manifest at all, so entrypoint
//! discovery has to retry with the member lists emptied.

use std::fs;
use std::path::PathBuf;

use nostd::visitor::find_entrypoints;

/// Write a crate whose `[workspace]` lists a member directory that is absent,
/// mirroring what crates.io does to `libc-0.2.21`.
fn write_crate_with_missing_member(dir: &std::path::Path) -> PathBuf {
    fs::create_dir_all(dir.join("src")).unwrap();
    fs::write(dir.join("src/lib.rs"), "pub fn f() {}\n").unwrap();

    let manifest = dir.join("Cargo.toml");
    fs::write(
        &manifest,
        r#"[package]
name = "ghostws"
version = "0.1.0"
edition = "2018"

[lib]
name = "ghostws"
path = "src/lib.rs"

[workspace]
members = ["absent-test", "absent-test/generate-files"]
"#,
    )
    .unwrap();
    manifest
}

#[test]
fn finds_entrypoint_despite_missing_workspace_member() {
    let tmp = std::env::temp_dir().join("nostd_ghostws_found");
    let _ = fs::remove_dir_all(&tmp);
    let manifest = write_crate_with_missing_member(&tmp);

    let mut known_modules = Vec::new();
    let root = find_entrypoints(manifest.to_str().unwrap(), &mut known_modules);

    assert_eq!(root, tmp);
    assert_eq!(known_modules, vec![tmp.join("src/lib.rs")]);

    fs::remove_dir_all(&tmp).unwrap();
}

#[test]
fn restores_the_manifest_it_patched() {
    let tmp = std::env::temp_dir().join("nostd_ghostws_restored");
    let _ = fs::remove_dir_all(&tmp);
    let manifest = write_crate_with_missing_member(&tmp);
    let before = fs::read_to_string(&manifest).unwrap();

    find_entrypoints(manifest.to_str().unwrap(), &mut Vec::new());

    // The patched manifest is scratch state: later passes re-read this file and
    // must still see the crate's real workspace declaration.
    assert_eq!(fs::read_to_string(&manifest).unwrap(), before);

    fs::remove_dir_all(&tmp).unwrap();
}

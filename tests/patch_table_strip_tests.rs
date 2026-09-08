#![feature(rustc_private)]

//! KI-31: `[patch]`/`[patch.<registry>]`/`[replace]` tables are read by nothing
//! downstream — the syn tree, HIR records and emitted `--features` would describe
//! the patched source while the verification build compiles whatever cargo
//! resolves the unpatched edge to. `downloader::gather_crate_info`'s strip block
//! already removes `workspace`, `lints`, `dev-dependencies` and `target`; this
//! covers the same removal for `patch`/`replace`, plus the telemetry flag that
//! records a patched manifest was seen (only reachable in practice via `--url`,
//! since crates.io strips `[patch]` on publish).

use std::fs;
use std::path::PathBuf;

use nostd::downloader::gather_crate_info;
use nostd::{Telemetry, consts};

/// A crate directory under `consts::DOWNLOAD_PATH`, which is where
/// `parser::determine_manifest_file` looks for the main crate's manifest.
struct Fixture {
    name_with_version: String,
    dir: PathBuf,
}

impl Fixture {
    fn new(slug: &str, manifest: &str) -> Self {
        let dir = PathBuf::from(consts::DOWNLOAD_PATH).join(format!("{slug}-0.0.0"));
        fs::create_dir_all(&dir).expect("failed to create fixture crate dir");
        fs::write(dir.join("Cargo.toml"), manifest).expect("failed to write fixture manifest");
        Self {
            name_with_version: format!("{slug}:0.0.0"),
            dir,
        }
    }

    fn manifest(&self) -> toml::Value {
        fs::read_to_string(self.dir.join("Cargo.toml"))
            .expect("fixture manifest reread")
            .parse()
            .expect("fixture manifest parses")
    }
}

impl Drop for Fixture {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.dir);
    }
}

/// A working-tree manifest as `--url` would hand it to the pipeline: a
/// `[patch.crates-io]` table redirecting `serde` to a local path, plus an
/// ordinary dependency and a `[workspace]` table to confirm the existing
/// strips still fire alongside the new one.
const PATCHED: &str = r#"
[package]
name = "patched-demo"
version = "0.0.0"

[workspace]

[dependencies]
libm = "0.2"

[patch.crates-io]
serde = { path = "../local-serde" }
"#;

const UNPATCHED: &str = r#"
[package]
name = "unpatched-demo"
version = "0.0.0"

[dependencies]
libm = "0.2"
"#;

#[test]
fn a_patch_table_is_stripped_and_recorded() {
    let fixture = Fixture::new("ki31-patch-strip", PATCHED);
    let mut telemetry = Telemetry::default();

    gather_crate_info(
        &fixture.name_with_version,
        false,
        None,
        Some(&mut telemetry),
    )
    .expect("gather_crate_info should succeed on a manifest with a patch table");

    let manifest = fixture.manifest();
    assert!(
        manifest.get("patch").is_none(),
        "the [patch] table must not survive into the emitted manifest, got {manifest:?}"
    );
    assert!(
        manifest.get("workspace").is_none(),
        "the existing [workspace] strip must still fire"
    );
    assert!(
        manifest.get("dependencies").is_some(),
        "unrelated tables must survive"
    );
    assert!(
        telemetry.manifest_had_patch_table,
        "a manifest with [patch] must set the KI-31 telemetry flag"
    );
}

#[test]
fn a_replace_table_is_stripped_and_recorded() {
    let manifest_src = r#"
[package]
name = "replaced-demo"
version = "0.0.0"

[dependencies]
libm = "0.2"

[replace]
"serde:1.0.0" = { path = "../local-serde" }
"#;
    let fixture = Fixture::new("ki31-replace-strip", manifest_src);
    let mut telemetry = Telemetry::default();

    gather_crate_info(
        &fixture.name_with_version,
        false,
        None,
        Some(&mut telemetry),
    )
    .expect("gather_crate_info should succeed on a manifest with a replace table");

    let manifest = fixture.manifest();
    assert!(
        manifest.get("replace").is_none(),
        "the [replace] table must not survive into the emitted manifest, got {manifest:?}"
    );
    assert!(
        telemetry.manifest_had_patch_table,
        "a manifest with [replace] must set the KI-31 telemetry flag"
    );
}

#[test]
fn no_patch_table_leaves_the_flag_unset() {
    let fixture = Fixture::new("ki31-no-patch", UNPATCHED);
    let mut telemetry = Telemetry::default();

    gather_crate_info(
        &fixture.name_with_version,
        false,
        None,
        Some(&mut telemetry),
    )
    .expect("gather_crate_info should succeed on an ordinary manifest");

    assert!(
        !telemetry.manifest_had_patch_table,
        "an ordinary manifest without [patch]/[replace] must leave the flag false"
    );
}

#[test]
fn only_gather_reads_do_not_require_telemetry() {
    // The 4 read-only dependency-gather call sites in `parser.rs` always pass
    // `only_gather: true` and `telemetry: None` — this is that shape. It must
    // not touch the manifest on disk at all (no `.bak`, no strip), so passing
    // `None` for telemetry is never a lost signal.
    let fixture = Fixture::new("ki31-only-gather", PATCHED);

    gather_crate_info(&fixture.name_with_version, true, None, None)
        .expect("gather_crate_info should succeed in only_gather mode");

    let manifest = fixture.manifest();
    assert!(
        manifest.get("patch").is_some(),
        "only_gather must not modify the manifest at all, patch table included"
    );
}

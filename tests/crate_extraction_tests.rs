#![feature(rustc_private)]

//! Regression tests for the download/extract step of `clone_from_crates`.
//!
//! A `.crate` tarball used to be staged at a *relative* `./<name>.crate` — the
//! one working directory every eval worker shares. Two workers wanting the same
//! crate raced: one read the tarball while the other was still writing it, hit
//! EOF at the writer's offset, and `unpack` stopped part-way. The truncated
//! directory then outlived the failure, and the "already downloaded?" check
//! (any `.rs` file present) accepted it on every later run, so the crate was
//! analysed with files silently missing. 13 such directories were found on disk.

use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};

use nostd::downloader::{
    EXTRACT_MARKER, PRISTINE_MANIFEST, PristineOutcome, ensure_pristine_manifest,
    extract_crate_checked, extraction_is_complete,
};

/// Build a `<name>-<version>/` crate tarball containing `src/lib.rs` and a
/// `Cargo.toml`, returning its path. `padding` inflates the archive so a
/// truncation lands mid-stream.
fn write_tarball(dir: &Path, name: &str, padding: usize) -> PathBuf {
    let path = dir.join(format!("{name}.crate"));
    let file = fs::File::create(&path).unwrap();
    let enc = flate2::write::GzEncoder::new(file, flate2::Compression::default());
    {
        let mut tar = tar::Builder::new(enc);
        let body = format!("// {}\npub fn f() {{}}\n", "x".repeat(padding));
        let mut header = tar::Header::new_gnu();
        header.set_size(body.len() as u64);
        header.set_mode(0o644);
        header.set_cksum();
        tar.append_data(&mut header, format!("{name}/src/lib.rs"), body.as_bytes())
            .unwrap();
        let manifest = PUBLISHED_MANIFEST.as_bytes();
        let mut header = tar::Header::new_gnu();
        header.set_size(manifest.len() as u64);
        header.set_mode(0o644);
        header.set_cksum();
        tar.append_data(&mut header, format!("{name}/Cargo.toml"), manifest)
            .unwrap();
        tar.into_inner().unwrap().finish().unwrap();
    }
    path
}

/// The manifest as the crate author published it: `graphics` still carries the
/// entry that links the optional dependency (`ab1024-ega-0.3.0`, shortened).
const PUBLISHED_MANIFEST: &str = "\
[package]
name = \"demo\"
version = \"1.0.0\"

[features]
default = [\"graphics\"]
graphics = [\"dep:embedded-graphics-core\"]
";

/// The same manifest after a run: the entry has been moved into the tool's own
/// synthetic feature, so `graphics` is enabled and links nothing.
const TOOL_OUTPUT_MANIFEST: &str = "\
[package]
name = \"demo\"
version = \"1.0.0\"

[features]
custom_default_features = [\"dep:embedded-graphics-core\"]
default = [\"graphics\"]
graphics = []
";

fn scratch(tag: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!("nostd_extract_{tag}_{}", std::process::id()));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).unwrap();
    dir
}

#[test]
fn a_successful_extraction_is_marked_complete() {
    let root = scratch("ok");
    let tarball = write_tarball(&root, "demo-1.0.0", 16);
    let dest = root.join("out");
    fs::create_dir_all(&dest).unwrap();
    let crate_path = dest.join("demo-1.0.0");

    extract_crate_checked(tarball.to_str().unwrap(), &dest, &crate_path).unwrap();

    assert!(crate_path.join("src/lib.rs").exists());
    assert!(
        crate_path.join(EXTRACT_MARKER).exists(),
        "a completed extraction must record that it completed"
    );
    assert!(extraction_is_complete(&crate_path));
    let _ = fs::remove_dir_all(&root);
}

#[test]
fn a_truncated_tarball_leaves_no_directory_behind() {
    let root = scratch("torn");
    // A tarball truncated mid-stream is exactly what a worker read when another
    // worker was still writing the shared staging file.
    let tarball = write_tarball(&root, "demo-1.0.0", 200_000);
    let full = fs::read(&tarball).unwrap();
    let mut torn = fs::File::create(&tarball).unwrap();
    torn.write_all(&full[..full.len() / 2]).unwrap();
    drop(torn);

    let dest = root.join("out");
    fs::create_dir_all(&dest).unwrap();
    let crate_path = dest.join("demo-1.0.0");

    let result = extract_crate_checked(tarball.to_str().unwrap(), &dest, &crate_path);

    assert!(result.is_err(), "a torn tarball must not report success");
    assert!(
        !crate_path.exists(),
        "the partial extraction must be removed, or the next run accepts it as \
         a complete crate forever"
    );
    let _ = fs::remove_dir_all(&root);
}

#[test]
fn an_unmarked_directory_with_sources_is_still_accepted() {
    // Every directory downloaded before the marker existed lacks one. They keep
    // the historical acceptance rule; the repair pass is what finds the
    // truncated ones among them.
    let root = scratch("legacy");
    let crate_path = root.join("legacy-1.0.0/src");
    fs::create_dir_all(&crate_path).unwrap();
    fs::write(crate_path.join("lib.rs"), b"pub fn f() {}\n").unwrap();

    assert!(extraction_is_complete(&root.join("legacy-1.0.0")));
    let _ = fs::remove_dir_all(&root);
}

/// Second half of this file: the *manifest* half of "is this directory usable".
///
/// A run that dies before `AllStats::dump` leaves the rewritten `Cargo.toml` on
/// disk, and the next run's `gather_crate_info` backs *that* up as the original —
/// so the tool's own output becomes the starting manifest and compounds. 348 crate
/// directories were in that state; `ab1024-ega-0.3.0` went from 0/26 targets to
/// 26/26 on the restore alone, because the entry linking its optional dependency
/// had been moved out of `graphics` by a run that never got to put it back.
#[test]
fn extraction_snapshots_the_published_manifest() {
    let root = scratch("pristine_snapshot");
    let tarball = write_tarball(&root, "demo-1.0.0", 16);
    let dest = root.join("out");
    fs::create_dir_all(&dest).unwrap();
    let crate_path = dest.join("demo-1.0.0");

    extract_crate_checked(tarball.to_str().unwrap(), &dest, &crate_path).unwrap();

    assert_eq!(
        fs::read_to_string(crate_path.join(PRISTINE_MANIFEST)).unwrap(),
        PUBLISHED_MANIFEST,
        "the copy every later run restores from is taken while the manifest is \
         still the published one"
    );
    let _ = fs::remove_dir_all(&root);
}

#[test]
fn a_leftover_manifest_is_restored_from_the_pristine_copy() {
    let root = scratch("pristine_restore");
    let crate_path = root.join("demo-1.0.0");
    fs::create_dir_all(&crate_path).unwrap();
    fs::write(crate_path.join(PRISTINE_MANIFEST), PUBLISHED_MANIFEST).unwrap();
    // What a run killed before `dump` leaves behind.
    fs::write(crate_path.join("Cargo.toml"), TOOL_OUTPUT_MANIFEST).unwrap();

    assert_eq!(
        ensure_pristine_manifest(&crate_path),
        PristineOutcome::Restored
    );
    assert_eq!(
        fs::read_to_string(crate_path.join("Cargo.toml")).unwrap(),
        PUBLISHED_MANIFEST,
        "the run must start from the crate's own manifest, not the last run's output"
    );
    let _ = fs::remove_dir_all(&root);
}

#[test]
fn a_clean_manifest_without_a_copy_becomes_the_copy() {
    // Every directory downloaded before the snapshot existed is in this state.
    let root = scratch("pristine_adopt");
    let crate_path = root.join("demo-1.0.0");
    fs::create_dir_all(&crate_path).unwrap();
    fs::write(crate_path.join("Cargo.toml"), PUBLISHED_MANIFEST).unwrap();

    assert_eq!(
        ensure_pristine_manifest(&crate_path),
        PristineOutcome::Snapshotted
    );
    assert_eq!(
        fs::read_to_string(crate_path.join(PRISTINE_MANIFEST)).unwrap(),
        PUBLISHED_MANIFEST
    );
    let _ = fs::remove_dir_all(&root);
}

#[test]
fn a_leftover_manifest_with_no_copy_condemns_the_directory() {
    // The 348 already on disk: nothing local says what the crate published, so
    // the directory is discarded and fetched again rather than analysed.
    let root = scratch("pristine_condemn");
    let crate_path = root.join("demo-1.0.0");
    fs::create_dir_all(&crate_path).unwrap();
    fs::write(crate_path.join("Cargo.toml"), TOOL_OUTPUT_MANIFEST).unwrap();

    assert_eq!(
        ensure_pristine_manifest(&crate_path),
        PristineOutcome::Unrecoverable
    );
    let _ = fs::remove_dir_all(&root);
}

#[test]
fn a_second_call_leaves_the_running_edits_alone() {
    // The manifest is rewritten as the run proceeds. Restoring twice would undo
    // the pass that made those edits.
    let root = scratch("pristine_once");
    let crate_path = root.join("demo-1.0.0");
    fs::create_dir_all(&crate_path).unwrap();
    fs::write(crate_path.join(PRISTINE_MANIFEST), PUBLISHED_MANIFEST).unwrap();
    fs::write(crate_path.join("Cargo.toml"), PUBLISHED_MANIFEST).unwrap();

    assert_eq!(
        ensure_pristine_manifest(&crate_path),
        PristineOutcome::AlreadyPristine
    );
    fs::write(crate_path.join("Cargo.toml"), TOOL_OUTPUT_MANIFEST).unwrap();

    assert_eq!(
        ensure_pristine_manifest(&crate_path),
        PristineOutcome::AlreadyPristine,
        "the second call must not touch the manifest"
    );
    assert_eq!(
        fs::read_to_string(crate_path.join("Cargo.toml")).unwrap(),
        TOOL_OUTPUT_MANIFEST,
        "this run's own edits survive"
    );
    let _ = fs::remove_dir_all(&root);
}

#[test]
fn an_empty_directory_is_not_accepted() {
    let root = scratch("empty");
    let crate_path = root.join("empty-1.0.0");
    fs::create_dir_all(&crate_path).unwrap();

    assert!(
        !extraction_is_complete(&crate_path),
        "a directory with no sources and no marker is not a usable extraction"
    );
    let _ = fs::remove_dir_all(&root);
}

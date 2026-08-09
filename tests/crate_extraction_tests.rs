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

use nostd::downloader::{EXTRACT_MARKER, extract_crate_checked, extraction_is_complete};

/// Build a `<name>-<version>/` crate tarball containing `src/lib.rs`, returning
/// its path. `padding` inflates the archive so a truncation lands mid-stream.
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
        tar.append_data(
            &mut header,
            format!("{name}/src/lib.rs"),
            body.as_bytes(),
        )
        .unwrap();
        tar.into_inner().unwrap().finish().unwrap();
    }
    path
}

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

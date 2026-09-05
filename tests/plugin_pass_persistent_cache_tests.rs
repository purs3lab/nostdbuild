#![feature(rustc_private)]

//! Guard for the cross-process (L2) companion to the in-process `cargo hir`
//! cache in `driver.rs` — see `plugin_pass_cache_tests.rs` for the L1 half.
//!
//! `run_cargo_hir_cached` now writes a compiled result to a directory on
//! disk (`NO_STD_TOOL_TEST_CARGO_HIR_CACHE_DIR` here; the real shared
//! directory in production) so a *different process* can reuse it instead of
//! paying for the same compile again. This exists because hundreds of
//! Substrate/Polkadot-SDK-family roots pull in a shared dependency
//! (`frame-support`, `sp-*`) at an identical pinned version — measured
//! directly, 205 different roots' download trees hold a byte-identical copy
//! of `frame-support-40.1.0`'s `Cargo.toml`, since the tool never rewrites a
//! shared dependency's own manifest, only the root's — so the manifest-hash
//! half of the cache key is already equal across every one of them. See
//! KNOWN_ISSUES.md's "Perf — a dependency shared by many roots..." entry.
//!
//! A second *process* is simulated here by clearing only the in-process (L1)
//! cache between calls — [`clear_in_process_cargo_hir_cache_for_test`] — and
//! leaving the disk and the L2 counters alone, which is exactly what an L1
//! miss looks like from a real second process's point of view.

use std::fs;
use std::path::Path;
use std::sync::Mutex;

use cargo_test_support::{Project, cargo_test, project};

use nostd::driver::{
    cargo_hir_persistent_cache_hits_for_test, cargo_hir_persistent_cache_writes_for_test,
    clear_cargo_hir_cache_for_test, clear_in_process_cargo_hir_cache_for_test,
    run_rustc_plugin_pass_with,
};

/// Both the L1 cache and the `NO_STD_TOOL_TEST_*` env vars that steer L2 are
/// process-global, and `cargo test` runs a file's `#[test]`s on separate
/// threads by default — same reasoning as `plugin_pass_cache_tests.rs`.
static TEST_SERIAL: Mutex<()> = Mutex::new(());

fn load_fixture(name: &str) -> (Project, String) {
    let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures")
        .join(name);

    let mut files: Vec<(String, String)> = Vec::new();
    for entry in
        fs::read_dir(&fixture_path).unwrap_or_else(|e| panic!("reading {fixture_path:?}: {e}"))
    {
        let path = entry.expect("dir entry").path();
        let rel = path.strip_prefix(&fixture_path).expect("under root");
        files.push((
            rel.to_string_lossy().to_string(),
            fs::read_to_string(&path).unwrap_or_else(|e| panic!("reading {path:?}: {e}")),
        ));
    }
    assert!(!files.is_empty(), "fixture {name} has no files");

    let mut builder = project().at(name);
    for (rel, contents) in &files {
        builder = builder.file(rel, contents);
    }
    let p = builder.build();
    let manifest = p.root().join("Cargo.toml").to_str().unwrap().to_string();
    (p, manifest)
}

/// Fresh, unique persistent-cache directory per test so tests never see each
/// other's disk entries despite sharing one process (and therefore one
/// `NO_STD_TOOL_TEST_CARGO_HIR_CACHE_DIR` env var, serialized by
/// `TEST_SERIAL`).
fn fresh_cache_dir(label: &str) -> std::path::PathBuf {
    std::env::temp_dir().join(format!(
        "nostd_persistent_cache_test_{label}_{}",
        uuid::Uuid::new_v4()
    ))
}

/// A fake plugin-install directory: two files `plugin_version_stamp` reads
/// `len()`/`modified()` from. Content is irrelevant — neither file is ever
/// executed, only stat'd — so this stands in for the real
/// `cargo-hir`/`hir-driver` install without touching them.
fn fake_plugin_bin_dir(label: &str, content: &str) -> std::path::PathBuf {
    let dir = std::env::temp_dir().join(format!(
        "nostd_fake_plugin_bin_{label}_{}",
        uuid::Uuid::new_v4()
    ));
    fs::create_dir_all(&dir).unwrap();
    fs::write(dir.join("cargo-hir"), content).unwrap();
    fs::write(dir.join("hir-driver"), content).unwrap();
    dir
}

/// The main guard: a call from what looks like a second process (L1 cold,
/// nothing shared but the disk) must reuse the first call's disk entry
/// rather than recompiling.
///
/// Fails without the fix by construction: `cargo_hir_persistent_cache_hits_for_test`
/// and the cache it reads do not exist on the unfixed code.
#[cargo_test]
fn a_second_process_reuses_the_first_ones_disk_entry() {
    let _guard = TEST_SERIAL.lock().unwrap_or_else(|e| e.into_inner());
    let (_p, manifest) = load_fixture("plugin_pass_cache");
    clear_cargo_hir_cache_for_test();

    let cache_dir = fresh_cache_dir("reuse");
    let bin_dir = fake_plugin_bin_dir("reuse", "v1");
    unsafe {
        std::env::set_var("NO_STD_TOOL_TEST_CARGO_HIR_CACHE_DIR", &cache_dir);
        std::env::set_var("NO_STD_TOOL_TEST_PLUGIN_BIN_DIR", &bin_dir);
    }

    let enable: Vec<String> = Vec::new();
    let target = Some("aarch64-unknown-none");

    let first =
        run_rustc_plugin_pass_with(&manifest, "plugin_pass_cache", &enable, None, false, target);
    assert!(first.is_success(), "first call must compile: {:?}", first);
    assert_eq!(
        cargo_hir_persistent_cache_writes_for_test(),
        1,
        "the first call has nothing to reuse — it must be the writer"
    );
    assert_eq!(
        cargo_hir_persistent_cache_hits_for_test(),
        0,
        "nothing was on disk yet for the first call to hit"
    );

    // Simulate a second process: forget everything L1 knows, but leave the
    // disk (and the L2 counters, which is what we assert on) untouched.
    clear_in_process_cargo_hir_cache_for_test();

    let second =
        run_rustc_plugin_pass_with(&manifest, "plugin_pass_cache", &enable, None, false, target);
    assert!(
        second.is_success(),
        "the cached outcome must reproduce the real one"
    );
    assert_eq!(
        cargo_hir_persistent_cache_hits_for_test(),
        1,
        "an L1-cold call must be served from the disk entry the first call wrote"
    );
    assert_eq!(
        cargo_hir_persistent_cache_writes_for_test(),
        1,
        "a disk hit must not trigger a second compile"
    );
}

/// The version-stamp guard: "rebuilding the plugin" (simulated by changing
/// the stat of the two files `plugin_version_stamp` reads) must make the old
/// entry's key unreachable rather than serving a stale answer — the
/// `db.bin` staleness failure mode this design exists to avoid.
#[cargo_test]
fn a_plugin_rebuild_invalidates_the_disk_entry() {
    let _guard = TEST_SERIAL.lock().unwrap_or_else(|e| e.into_inner());
    let (_p, manifest) = load_fixture("plugin_pass_cache");
    clear_cargo_hir_cache_for_test();

    let cache_dir = fresh_cache_dir("rebuild");
    let bin_dir = fake_plugin_bin_dir("rebuild", "v1");
    unsafe {
        std::env::set_var("NO_STD_TOOL_TEST_CARGO_HIR_CACHE_DIR", &cache_dir);
        std::env::set_var("NO_STD_TOOL_TEST_PLUGIN_BIN_DIR", &bin_dir);
    }

    let enable: Vec<String> = Vec::new();
    let target = Some("aarch64-unknown-none");

    let first =
        run_rustc_plugin_pass_with(&manifest, "plugin_pass_cache", &enable, None, false, target);
    assert!(first.is_success());
    assert_eq!(cargo_hir_persistent_cache_writes_for_test(), 1);

    // "Rebuild" the plugin. A short sleep first: some filesystems have
    // coarse mtime resolution, and content alone changing `len()` is not
    // guaranteed portable — both together make the stamp reliably differ.
    std::thread::sleep(std::time::Duration::from_millis(20));
    fs::write(bin_dir.join("cargo-hir"), "v2 - a different length").unwrap();

    clear_in_process_cargo_hir_cache_for_test();

    let second =
        run_rustc_plugin_pass_with(&manifest, "plugin_pass_cache", &enable, None, false, target);
    assert!(second.is_success());
    assert_eq!(
        cargo_hir_persistent_cache_hits_for_test(),
        0,
        "the old entry's key no longer matches the new version stamp — it must not be read"
    );
    assert_eq!(
        cargo_hir_persistent_cache_writes_for_test(),
        2,
        "a stale entry must not stop the second call from compiling and writing its own"
    );
}

/// The concurrency guard this design exists for: two callers racing on the
/// identical cold key must not both pay for the compile — one becomes the
/// writer, the other waits on its lock and reads the answer back.
#[cargo_test]
fn two_racing_callers_on_the_same_cold_key_only_one_computes() {
    let _guard = TEST_SERIAL.lock().unwrap_or_else(|e| e.into_inner());
    let (_p, manifest) = load_fixture("plugin_pass_cache");
    clear_cargo_hir_cache_for_test();

    let cache_dir = fresh_cache_dir("race");
    let bin_dir = fake_plugin_bin_dir("race", "v1");
    unsafe {
        std::env::set_var("NO_STD_TOOL_TEST_CARGO_HIR_CACHE_DIR", &cache_dir);
        std::env::set_var("NO_STD_TOOL_TEST_PLUGIN_BIN_DIR", &bin_dir);
    }

    let enable: Vec<String> = Vec::new();
    let target = Some("aarch64-unknown-none");
    let manifest_ref = manifest.as_str();

    std::thread::scope(|scope| {
        let a = scope.spawn(|| {
            run_rustc_plugin_pass_with(
                manifest_ref,
                "plugin_pass_cache",
                &enable,
                None,
                false,
                target,
            )
        });
        let b = scope.spawn(|| {
            run_rustc_plugin_pass_with(
                manifest_ref,
                "plugin_pass_cache",
                &enable,
                None,
                false,
                target,
            )
        });
        let (ra, rb) = (a.join().unwrap(), b.join().unwrap());
        assert!(ra.is_success(), "{:?}", ra);
        assert!(rb.is_success(), "{:?}", rb);
    });

    assert_eq!(
        cargo_hir_persistent_cache_writes_for_test(),
        1,
        "two racing callers on one cold key must not both compile it"
    );
}

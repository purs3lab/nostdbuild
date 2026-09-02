#![feature(rustc_private)]

//! Guard for the `cargo hir` compile cache in `driver.rs`.
//!
//! Measured on `bridge-runtime-common-0.21.0`: two independent search
//! mechanisms — the CEGAR covering-set loop and `phases.rs`'s per-span
//! satisfiability check — routinely re-derive the same `(dependency, feature
//! set, target)` question and each pays a fresh multi-minute `cargo hir`
//! compile for it. 393 of 1150 plugin-pass calls in that one crate's analysis
//! (57%) were exact repeats of an earlier call in the same run.
//!
//! `run_cargo_hir_cached` memoizes the compile itself — not the callers'
//! decision logic — keyed by a content hash of `Cargo.toml` plus the sorted
//! feature set, target and `--lib` flag: exactly what a `cargo hir`
//! invocation's own arguments are built from, so a second call with an equal
//! key runs the identical command against byte-identical inputs. This test
//! asserts the second of two identical calls is served from the cache rather
//! than re-compiled.

use std::fs;
use std::path::Path;
use std::sync::Mutex;

use cargo_test_support::{Project, cargo_test, project};

use nostd::driver::{
    cargo_hir_cache_hits_for_test, cargo_hir_cache_len_for_test, clear_cargo_hir_cache_for_test,
    run_rustc_plugin_pass_with,
};

/// The cache under test is a process-global `static`, and `cargo test` runs a
/// file's `#[test]` functions on separate threads within one process by
/// default — so without this, the two tests below race on the same cache and
/// each can observe the other's entries. Real production use never has this
/// problem (one `main` process, one crate's coverage search at a time); it is
/// purely an artifact of two tests sharing what is deliberately global state.
static TEST_SERIAL: Mutex<()> = Mutex::new(());

fn load_fixture(name: &str) -> (Project, String) {
    let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures")
        .join(name);

    let mut files: Vec<(String, String)> = Vec::new();
    for entry in fs::read_dir(&fixture_path).unwrap_or_else(|e| panic!("reading {fixture_path:?}: {e}")) {
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

/// The guard: two calls, identical in every way that feeds the cache key,
/// pinned to one target so this test does not sweep `TARGET_LIST`. The
/// second must be a cache hit and must not grow the cache.
///
/// Fails without the fix by construction — `cargo_hir_cache_hits_for_test`
/// and the cache it reads do not exist on the unfixed code, so this would
/// not even compile there, let alone pass.
#[cargo_test]
fn an_identical_second_call_is_served_from_the_cache() {
    let _guard = TEST_SERIAL.lock().unwrap_or_else(|e| e.into_inner());
    let (_p, manifest) = load_fixture("plugin_pass_cache");
    clear_cargo_hir_cache_for_test();

    let enable: Vec<String> = Vec::new();
    let target = Some("aarch64-unknown-none");

    let first = run_rustc_plugin_pass_with(&manifest, "plugin_pass_cache", &enable, None, false, target);
    assert!(
        first.is_success(),
        "fixture is a trivial #![no_std] crate with no dependencies; it must compile \
         for aarch64-unknown-none, got {:?}",
        first
    );
    assert_eq!(
        cargo_hir_cache_len_for_test(),
        1,
        "one distinct (manifest, features, target) query was made; the cache should hold \
         exactly one entry after it"
    );
    assert_eq!(
        cargo_hir_cache_hits_for_test(),
        0,
        "the first call is necessarily a miss — nothing was cached yet"
    );

    let second = run_rustc_plugin_pass_with(&manifest, "plugin_pass_cache", &enable, None, false, target);
    assert!(second.is_success(), "the cached outcome must reproduce the real one");
    assert_eq!(
        cargo_hir_cache_hits_for_test(),
        1,
        "an identical second call must be served from the cache, not recompiled"
    );
    assert_eq!(
        cargo_hir_cache_len_for_test(),
        1,
        "a cache hit must not insert a second entry for the same key"
    );
}

/// The negative control: change one input the key covers (the feature set)
/// and the two calls must **not** collide — otherwise the cache would be
/// silently answering a different question than the one asked.
#[cargo_test]
fn a_different_feature_set_is_not_a_cache_hit() {
    let _guard = TEST_SERIAL.lock().unwrap_or_else(|e| e.into_inner());
    let (_p, manifest) = load_fixture("plugin_pass_cache");
    clear_cargo_hir_cache_for_test();

    let target = Some("aarch64-unknown-none");
    let empty: Vec<String> = Vec::new();
    let with_default = vec!["default".to_string()];

    let first = run_rustc_plugin_pass_with(&manifest, "plugin_pass_cache", &empty, None, false, target);
    assert!(first.is_success());
    let second =
        run_rustc_plugin_pass_with(&manifest, "plugin_pass_cache", &with_default, None, false, target);
    assert!(second.is_success());

    assert_eq!(
        cargo_hir_cache_hits_for_test(),
        0,
        "two different feature sets must not be treated as the same query"
    );
    assert_eq!(
        cargo_hir_cache_len_for_test(),
        2,
        "each distinct (manifest, features, target) query gets its own cache entry"
    );
}

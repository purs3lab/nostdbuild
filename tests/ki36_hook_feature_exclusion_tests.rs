#![feature(rustc_private)]

//! KI-36 — a feature named exactly `custom` is, by ecosystem convention
//! (`getrandom`, and confirmed here on the real `clock_source-0.2.4`: a `mod
//! custom;` declaring an `extern "C" fn __RUST_CRATE_CLOCK_SOURCE()` with no
//! definition, for the *final binary* to supply), a self-registration hook
//! rather than an ordinary cfg-gated path. Every build this tool runs is
//! `cargo build --lib`, which never reaches the link step that would catch a
//! missing definition, so a lib-only success with `custom` enabled "passes"
//! on every target regardless of whether any real backend exists.
//!
//! `parser::add_synthetic_dependency`'s caller (KI-34) already excludes the
//! literal name `custom` from its own candidate search — but outright,
//! wherever it names a transitive package with an alternative feature to
//! try instead. `dep_edge_retry_candidates` (R34-16) and
//! `enablers_for_selection` (KI-30) cannot borrow that shape unmodified:
//! `clock_source-0.2.5` — the crate this exclusion is sized from — declares
//! no feature besides `default` and `custom`, so banning `custom` outright
//! here left `test_etime`'s golden fixture (which predates this entry) with
//! no candidate ever again. Both functions instead try every ordinary
//! candidate first and reach for `custom` only when none of them compiled —
//! last resort, not refused, so a real alternative still wins the way
//! KI-34 verified for `getrandom`'s `rdrand`, and a crate with no
//! alternative still gets the one candidate that can fix it.
//!
//! Sizing before this fix (grepping `dep_edge_enabler_features`/
//! `emitted_set_enabler_features` — the telemetry fields recording which
//! candidate *won* — across every already-run corpus result) found `custom`
//! is the only hook-shaped name either mechanism has ever selected, twice,
//! both tracing to `clock_source`; every other winning name (`libm`, `alloc`,
//! `heapless`, `embassy`, `checked-overflow`, `no-std`, `default`) is an
//! ordinary ecosystem feature.

use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};

use cargo_test_support::{Project, cargo_test, project};

use nostd::consts;
use nostd::driver::{enablers_for_selection, reset_target_cache};
use nostd::parser::dep_edge_retry_candidates;

// ---------------------------------------------------------------------------
// `dep_edge_retry_candidates` — pure, no compile
// ---------------------------------------------------------------------------

/// A dependency directory laid out exactly the way `dep_edge_retry_candidates`
/// and `determine_manifest_file` expect it: `<DOWNLOAD_PATH>/<main>_deps/<dep>-<version>/Cargo.toml`.
struct DepFixture {
    dep_name_with_version: String,
    main_name: String,
    dir: PathBuf,
}

impl DepFixture {
    fn new(main_slug: &str, dep_slug: &str, manifest: &str) -> Self {
        let main_name = format!("{main_slug}:0.0.0");
        let dep_name_with_version = format!("{dep_slug}:9.9.9");
        let dir = PathBuf::from(consts::DOWNLOAD_PATH)
            .join(format!("{main_slug}-0.0.0_deps"))
            .join(format!("{dep_slug}-9.9.9"));
        fs::create_dir_all(&dir).expect("failed to create fixture dep dir");
        fs::write(dir.join("Cargo.toml"), manifest).expect("failed to write fixture manifest");
        Self {
            dep_name_with_version,
            main_name,
            dir,
        }
    }
}

impl Drop for DepFixture {
    fn drop(&mut self) {
        // `dep_edge_retry_candidates` only checks the dep dir exists, so the
        // parent `_deps` dir does not need cleaning up too.
        let _ = fs::remove_dir_all(&self.dir);
    }
}

const DEP_WITH_HOOK_FEATURE: &str = r#"
[package]
name = "ki36-hook-dep"
version = "9.9.9"

[features]
default = []
custom = []
rdrand = []
"#;

#[test]
fn dep_edge_retry_candidates_tries_custom_last_when_an_alternative_exists() {
    let dep = DepFixture::new(
        "ki36-dep-edge-main",
        "ki36-hook-dep",
        DEP_WITH_HOOK_FEATURE,
    );

    let candidates = dep_edge_retry_candidates(&dep.dep_name_with_version, &dep.main_name, &[]);

    assert_eq!(
        candidates,
        vec![
            "ki36-hook-dep/rdrand".to_string(),
            "ki36-hook-dep/custom".to_string()
        ],
        "`default` is excluded on its own terms, `rdrand` — an ordinary \
         declared feature — must still come through, and `custom` must sort \
         after it: the caller tries candidates in order and stops at the \
         first that builds, so a real alternative always gets first crack \
         over the hook-shaped name: {candidates:?}"
    );
}

#[test]
fn dep_edge_retry_candidates_falls_back_to_a_hook_feature_when_it_is_the_only_one() {
    let dep = DepFixture::new(
        "ki36-dep-edge-only-hook-main",
        "ki36-hook-only-dep",
        r#"
[package]
name = "ki36-hook-only-dep"
version = "9.9.9"

[features]
default = []
custom = []
"#,
    );

    let candidates = dep_edge_retry_candidates(&dep.dep_name_with_version, &dep.main_name, &[]);
    assert_eq!(
        candidates,
        vec!["ki36-hook-only-dep/custom".to_string()],
        "with no non-hook feature declared, `custom` is this edge's only \
         path and must still be offered — banning it outright is what left \
         `clock_source-0.2.5` (this fixture's shape) with no candidate at \
         all: {candidates:?}"
    );
}

// ---------------------------------------------------------------------------
// `enablers_for_selection` — the search itself, against a real fixture
// ---------------------------------------------------------------------------

/// Serialises the tests and clears the caches `enablers_for_selection`'s own
/// search shares through process globals (`LAST_GOOD_TARGET`) — the same
/// discipline `build_enabler_tests.rs` uses, and for the same reason: a
/// fixture that compiled bare metal in an earlier test otherwise leaves the
/// cache warm and later trials silently skip the target sweep.
static SERIAL: std::sync::Mutex<()> = std::sync::Mutex::new(());

fn isolated() -> std::sync::MutexGuard<'static, ()> {
    let guard = SERIAL.lock().unwrap_or_else(|e| e.into_inner());
    reset_target_cache();
    guard
}

fn load_fixture(name: &str) -> (Project, String) {
    let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures")
        .join(name);

    let mut files: Vec<(String, String)> = Vec::new();
    collect_files(&fixture_path, &fixture_path, &mut files);
    assert!(!files.is_empty(), "fixture {name} has no files");

    let mut builder = project().at(name);
    for (rel, contents) in &files {
        builder = builder.file(rel, contents);
    }
    let p = builder.build();
    let manifest = p.root().join("Cargo.toml").to_str().unwrap().to_string();
    (p, manifest)
}

fn collect_files(root: &Path, dir: &Path, out: &mut Vec<(String, String)>) {
    for entry in fs::read_dir(dir).unwrap_or_else(|e| panic!("reading {dir:?}: {e}")) {
        let path = entry.expect("dir entry").path();
        if path.is_dir() {
            collect_files(root, &path, out);
        } else {
            let rel = path.strip_prefix(root).expect("under root");
            out.push((
                rel.to_string_lossy().to_string(),
                fs::read_to_string(&path).unwrap_or_else(|e| panic!("reading {path:?}: {e}")),
            ));
        }
    }
}

/// `custom` sorts alphabetically before `real` in the fixture's declared
/// features, so before this fix a blind search would try — and find —
/// `custom` first, exactly the alphabetical-ordering shape that made
/// getrandom's own `custom` win KI-34's search before that mechanism's
/// exclusion landed. With the fix, `custom` never enters the candidate pool
/// at all, so the search must still find the genuine `real` arm instead of
/// reporting nothing.
#[cargo_test]
fn enablers_for_selection_skips_the_hook_and_still_finds_the_real_arm() {
    let _serial = isolated();
    let (_p, manifest) = load_fixture("ki36_hook_feature");

    let emitted: HashSet<String> = HashSet::new();
    let exclude: HashSet<String> = HashSet::from(["std".to_string(), "default".to_string()]);
    let found = enablers_for_selection(&manifest, "ki36_hook_feature", &emitted, &[], &exclude);

    assert_eq!(
        found,
        vec!["real".to_string()],
        "the search must skip `custom` (a self-registration hook that would \
         also compile) and land on `real`, the genuine bare-metal arm: {found:?}"
    );
}

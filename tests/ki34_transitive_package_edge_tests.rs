#![feature(rustc_private)]

//! KI-34: every repair before this one only ever reaches a feature one hop
//! from the manifest this tool edits (`<direct-dep>/<feat>`), because that is
//! the only shape cargo's `--features` flag accepts. `getrandom`'s own
//! `compile_error!("target is not supported")` fires two hops behind
//! `rand_core` in crates like `crypto-bigint-0.6.1`, and no direct edge names
//! it — so nothing above this point could turn its `rdrand` backend on.
//!
//! `parser::implicated_transitive_package` reads the failing build's own
//! compiler diagnostic (a `--> <path>/src/<file>.rs:LINE:COL` line) to find
//! the transitive package and its manifest directly, without needing a
//! separate lockfile parse — verified live against `crypto-bigint-0.6.1`
//! (`getrandom-0.2.15`) before this test was written. `parser::
//! add_synthetic_dependency` / `restore_manifest_text` are the write/undo
//! pair that promotes it to a synthetic direct dependency so Cargo's own
//! feature unification reaches the real, deeper edge.

use std::collections::HashSet;
use std::fs;
use std::path::PathBuf;

use nostd::consts;
use nostd::parser::{add_synthetic_dependency, implicated_transitive_package, restore_manifest_text};

/// A crate directory under `consts::DOWNLOAD_PATH`, which is where
/// `parser::determine_manifest_file` looks for the main crate's manifest —
/// used both for the "main crate" side of a test and, with a distinct slug,
/// to stand in for a transitive package's own on-disk directory (all
/// `implicated_transitive_package` needs is a real path containing `/src/`
/// with a readable `Cargo.toml` one level up, which is exactly what a
/// registry-cache checkout looks like).
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

    fn manifest_text(&self) -> String {
        fs::read_to_string(self.dir.join("Cargo.toml")).expect("fixture manifest reread")
    }

    fn manifest(&self) -> toml::Value {
        self.manifest_text().parse().expect("fixture manifest parses")
    }

    /// A fabricated rustc diagnostic pointing into this fixture's own
    /// directory, in the same shape `compiler::errors_since` hands
    /// `implicated_transitive_package` in production: the tool's own
    /// "Cargo failed..." prefix, then rustc's `--> <path>:LINE:COL`.
    fn error_pointing_here(&self, message: &str) -> String {
        format!(
            "Cargo failed with status code: 101 and message:    Compiling fake v0.0.0\n\
             error: {message}\n  --> {}/src/lib.rs:347:9\n   |\n347 | compile_error!(...);\n",
            self.dir.display()
        )
    }
}

impl Drop for Fixture {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.dir);
    }
}

const FAKE_BACKEND_CRATE: &str = r#"
[package]
name = "fake-backend-crate"
version = "9.9.9"

[features]
rdrand = []
std = []
"#;

#[test]
fn finds_the_transitive_package_named_by_the_diagnostic() {
    let transitive = Fixture::new("ki34-transitive-hit", FAKE_BACKEND_CRATE);
    let err = transitive.error_pointing_here("target is not supported, see docs");

    let found = implicated_transitive_package(&[err], &HashSet::new())
        .expect("diagnostic names an addressable transitive package");

    assert_eq!(found.0, "fake-backend-crate");
    assert_eq!(found.1, transitive.dir.join("Cargo.toml"));
    assert_eq!(found.2, "9.9.9");
}

#[test]
fn skips_a_package_already_a_direct_dependency() {
    let transitive = Fixture::new("ki34-transitive-already-direct", FAKE_BACKEND_CRATE);
    let err = transitive.error_pointing_here("target is not supported, see docs");

    let mut existing = HashSet::new();
    existing.insert("fake-backend-crate".to_string());

    assert!(
        implicated_transitive_package(&[err], &existing).is_none(),
        "a package `dep_edge_retry_candidates` can already reach should not be re-proposed here"
    );
}

#[test]
fn keeps_scanning_past_a_line_with_no_readable_manifest() {
    let genuinely_transitive = Fixture::new("ki34-transitive-second-hit", FAKE_BACKEND_CRATE);

    // The first `--> ` line points at a directory with no `Cargo.toml` at all
    // (a bare-file diagnostic, or one for a package this run never
    // downloaded) -- the scan must not stop there.
    let err = format!(
        "error: something else\n  --> /nonexistent/does-not-exist/src/lib.rs:1:1\n{}",
        genuinely_transitive.error_pointing_here("target is not supported, see docs"),
    );

    let found = implicated_transitive_package(&[err], &HashSet::new())
        .expect("must keep scanning past an unreadable manifest to a later hit");
    assert_eq!(found.1, genuinely_transitive.dir.join("Cargo.toml"));
}

#[test]
fn ignores_an_error_with_no_diagnostic_location() {
    let err = "Cargo failed with status code: 101 and message: some unrelated linker error"
        .to_string();
    assert!(implicated_transitive_package(&[err], &HashSet::new()).is_none());
}

#[test]
fn ignores_a_path_with_no_src_directory() {
    // A `--> ` line that does not follow the `<pkg-dir>/src/<file>.rs` shape
    // (e.g. it names a file at the crate root) has nothing this mechanism can
    // resolve back to a package manifest.
    let err = "error: whatever\n  --> /tmp/not-a-package/build.rs:1:1\n".to_string();
    assert!(implicated_transitive_package(&[err], &HashSet::new()).is_none());
}

const MAIN_CRATE_WITH_ONE_DEP: &str = r#"
[package]
name = "ki34-main"
version = "0.0.0"

[dependencies.serde]
version = "1"
"#;

#[test]
fn add_synthetic_dependency_writes_and_restores_byte_for_byte() {
    let main = Fixture::new("ki34-main-add-restore", MAIN_CRATE_WITH_ONE_DEP);
    let original = main.manifest_text();

    let returned = add_synthetic_dependency(
        &main.name_with_version,
        "fake-backend-crate",
        "9.9.9",
        &["rdrand".to_string()],
    );
    assert_eq!(returned, original, "must hand back the pre-edit manifest text verbatim");

    let mutated = main.manifest();
    let dep = mutated
        .get("dependencies")
        .and_then(|d| d.get("fake-backend-crate"))
        .expect("synthetic dependency entry was written");
    assert_eq!(dep.get("version").and_then(|v| v.as_str()), Some("=9.9.9"));
    assert_eq!(dep.get("default-features").and_then(|v| v.as_bool()), Some(false));
    assert_eq!(
        dep.get("features")
            .and_then(|v| v.as_array())
            .map(|a| a.iter().filter_map(|v| v.as_str()).collect::<Vec<_>>()),
        Some(vec!["rdrand"])
    );
    // The pre-existing dependency is untouched.
    assert!(mutated.get("dependencies").and_then(|d| d.get("serde")).is_some());

    restore_manifest_text(&main.name_with_version, &original);
    assert_eq!(main.manifest_text(), original, "restore must be byte-for-byte");
}

#[test]
#[should_panic(expected = "already present")]
fn add_synthetic_dependency_panics_on_an_existing_key() {
    let main = Fixture::new("ki34-main-existing-key", MAIN_CRATE_WITH_ONE_DEP);
    add_synthetic_dependency(&main.name_with_version, "serde", "1.0.0", &["derive".to_string()]);
}

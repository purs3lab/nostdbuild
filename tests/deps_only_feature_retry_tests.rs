#![feature(rustc_private)]

//! The candidate set behind the KI-11 build retry.
//!
//! lazy-exclusive 1.0.5 emits `--no-default-features --features use-locks` and
//! dies on `libc::pthread_mutex_*`, which do not exist on a bare-metal target.
//! `libc` is a perfectly no_std-capable crate and compiles for that target on its
//! own, so no dependency-level check can predict this — the evidence only exists
//! once the crate itself fails to build. `deps_only_enable_features` names the
//! features that may be dropped and retried at that point: those whose sole
//! effect is linking an optional dependency.
//!
//! Two details of the lazy-exclusive shape matter here. Both optional deps are
//! declared under `[target.'cfg(…)'.dependencies]`, which `gather_crate_info`
//! never reads — so `crate_info.deps_and_features` is *empty* and the candidate
//! set has to go back to the manifest. And `use-locks` is reachable from
//! `default`, so it is only droppable because the emitted config already passes
//! `--no-default-features`.

use std::collections::HashSet;
use std::fs;
use std::path::PathBuf;

use nostd::CrateInfo;
use nostd::consts;
use nostd::parser::deps_only_enable_features;

/// `deps_only_enable_features` reads the manifest off disk through
/// `determine_manifest_file`, so a fixture needs a real file at the path its
/// name-with-version derives. Each test uses its own directory so the parallel
/// test threads cannot collide.
struct Fixture {
    name_with_version: String,
    dir: PathBuf,
}

impl Fixture {
    fn new(slug: &str, manifest: &str) -> Self {
        let name_with_version = format!("{slug}:0.0.0");
        let dir = PathBuf::from(consts::DOWNLOAD_PATH).join(format!("{slug}-0.0.0"));
        fs::create_dir_all(&dir).expect("failed to create fixture crate dir");
        fs::write(dir.join("Cargo.toml"), manifest).expect("failed to write fixture manifest");
        Self {
            name_with_version,
            dir,
        }
    }

    fn lazy_exclusive_like(slug: &str) -> Self {
        Self::new(
            slug,
            "[package]\nname = \"fixture\"\nversion = \"0.0.0\"\n\n\
             [features]\ndefault = [\"use-locks\", \"std\"]\nstd = []\n\
             use-locks = [\"windows-link\", \"libc\"]\n\n\
             [target.'cfg(not(target_os = \"windows\"))'.dependencies.libc]\n\
             version = \"0.2.171\"\noptional = true\n\n\
             [target.'cfg(target_os = \"windows\")'.dependencies.windows-link]\n\
             version = \"0.1.1\"\noptional = true\n",
        )
    }
}

impl Drop for Fixture {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.dir);
    }
}

/// The feature table the tool builds for the manifest above. `deps_and_features`
/// is deliberately empty: `gather_crate_info` only walks `[dependencies]`, so
/// target-scoped optional deps never reach it.
fn lazy_exclusive_info() -> CrateInfo {
    CrateInfo {
        name: "fixture".to_string(),
        version: "0.0.0".to_string(),
        deps_and_features: Vec::new(),
        features: vec![
            (
                "default".to_string(),
                vec![
                    ("use-locks".to_string(), "use-locks".to_string()),
                    ("std".to_string(), "std".to_string()),
                ],
            ),
            ("std".to_string(), vec![]),
            (
                "use-locks".to_string(),
                vec![
                    ("windows-link".to_string(), "windows-link".to_string()),
                    ("libc".to_string(), "libc".to_string()),
                ],
            ),
        ],
        ..Default::default()
    }
}

#[test]
fn target_scoped_optional_dep_enabler_is_a_candidate() {
    let fixture = Fixture::lazy_exclusive_like("deps-only-target-scoped");

    let droppable = deps_only_enable_features(
        &fixture.name_with_version,
        &lazy_exclusive_info(),
        &["use-locks".to_string(), "std".to_string()],
        &HashSet::new(),
        false,
    );

    assert_eq!(
        droppable,
        vec!["use-locks".to_string()],
        "`use-locks` exists only to link libc/windows-link and must be droppable"
    );
}

#[test]
fn marker_feature_is_not_a_candidate() {
    let fixture = Fixture::lazy_exclusive_like("deps-only-marker");

    let droppable = deps_only_enable_features(
        &fixture.name_with_version,
        &lazy_exclusive_info(),
        &["std".to_string()],
        &HashSet::new(),
        false,
    );

    // `std = []` links nothing, so it gates code and dropping it is not a
    // question of optional dependencies at all. Without this the candidate set
    // would sweep up every empty feature in the enable list.
    assert!(
        droppable.is_empty(),
        "`std` enables no optional dep and must not be droppable, got {droppable:?}"
    );
}

#[test]
fn feature_reachable_from_default_is_not_a_candidate() {
    let fixture = Fixture::lazy_exclusive_like("deps-only-via-default");

    let droppable = deps_only_enable_features(
        &fixture.name_with_version,
        &lazy_exclusive_info(),
        &["use-locks".to_string()],
        &HashSet::new(),
        true,
    );

    // With defaults on, `default = ["use-locks", …]` turns the feature back on
    // no matter what the command line says, so retrying without it would just
    // burn a build on an identical configuration.
    assert!(
        droppable.is_empty(),
        "`use-locks` comes back via `default` and must not be droppable, got {droppable:?}"
    );
}

#[test]
fn non_minimalizable_feature_is_not_a_candidate() {
    let fixture = Fixture::lazy_exclusive_like("deps-only-pinned");

    let droppable = deps_only_enable_features(
        &fixture.name_with_version,
        &lazy_exclusive_info(),
        &["use-locks".to_string()],
        &HashSet::from(["use-locks".to_string()]),
        false,
    );

    // A feature pinned by a `compile_error!` constraint or because the solve
    // needs the dependency linked is load-bearing — the retry must not take it.
    assert!(
        droppable.is_empty(),
        "a pinned feature must never be droppable, got {droppable:?}"
    );
}

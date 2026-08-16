#![feature(rustc_private)]

//! R31-6 residue: severing a dependency that is enabled by Cargo's *implicit*
//! feature.
//!
//! `should_skip_dep` decides a dependency cannot be no_std and severs it by
//! taking the dependency out of the `[features]` entry that enables it, leaving
//! that feature on. `remove_feats_enabling_dep` does the cutting, and it can only
//! cut what the `[features]` table declares. An optional dependency that nothing
//! in `[features]` mentions has no entry at all — Cargo synthesises one — so the
//! cut found nothing, `should_skip_dep` returned "skip" anyway, and the feature
//! went out on the command line still linking the dependency.
//!
//! `insecure-time-0.1.0` is the measured shape: `default = ["std", "clap"]`, no
//! `[features] clap`, and `clap` optional. The solve turns `std` off, which
//! disables `default`; `final_feature_list_main` re-adds `clap` as the surviving
//! member of `default`; the sever is a no-op; and the emitted
//! `--no-default-features --features clap` links std on all 26 targets
//! (`E0463: can't find crate for std`).
//!
//! There is nothing to park in that case — the entry does not exist — so the
//! whole edit is to drop the feature from the build, which is what
//! `features_forced_off` is for.

use std::collections::HashSet;
use std::fs;
use std::path::PathBuf;

use nostd::consts::DOWNLOAD_PATH;
use nostd::parser::should_skip_dep;
use nostd::types::TupleVec;
use nostd::{CrateInfo, DataExchange};

/// A main crate plus one dependency, laid out where `determine_manifest_file`
/// looks for each: the main crate at `<DOWNLOAD_PATH>/<slug>-0.0.0/` and its
/// dependencies at `<DOWNLOAD_PATH>/<slug>-0.0.0_deps/<dep>-0.0.0/`.
struct Fixture {
    slug: String,
    dir: PathBuf,
}

impl Fixture {
    fn new(slug: &str, main_manifest: &str) -> Self {
        let dir = PathBuf::from(DOWNLOAD_PATH).join(format!("{slug}-0.0.0"));
        let _ = fs::remove_dir_all(&dir);
        let _ = fs::remove_dir_all(PathBuf::from(DOWNLOAD_PATH).join(format!("{slug}-0.0.0_deps")));
        fs::create_dir_all(dir.join("src")).expect("main crate dir");
        fs::write(dir.join("Cargo.toml"), main_manifest).expect("main manifest");
        fs::write(dir.join("src/lib.rs"), "#![no_std]\n").expect("main lib.rs");
        Self {
            slug: slug.to_string(),
            dir,
        }
    }

    /// A dependency whose crate root carries no `no_std` attribute, so
    /// `no_std_evidence` answers `Absent` and `should_skip_dep` takes its
    /// severing branch.
    fn with_std_only_dep(self, dep: &str) -> Self {
        let dep_dir = PathBuf::from(DOWNLOAD_PATH)
            .join(format!("{}-0.0.0_deps", self.slug))
            .join(format!("{dep}-0.0.0"));
        fs::create_dir_all(dep_dir.join("src")).expect("dep dir");
        fs::write(
            dep_dir.join("Cargo.toml"),
            format!(
                "[package]\nname = \"{dep}\"\nversion = \"0.0.0\"\nedition = \"2018\"\n\n\
                 [lib]\nname = \"{dep}\"\npath = \"src/lib.rs\"\n"
            ),
        )
        .expect("dep manifest");
        fs::write(dep_dir.join("src/lib.rs"), "pub fn f() -> u8 { 1 }\n").expect("dep lib.rs");
        self
    }

    fn feature_values(&self, feat: &str) -> Option<Vec<String>> {
        let toml: toml::Value = fs::read_to_string(self.dir.join("Cargo.toml"))
            .expect("manifest reread")
            .parse()
            .expect("manifest parses");
        toml.get("features")
            .and_then(|f| f.get(feat))
            .and_then(|v| v.as_array())
            .map(|arr| {
                arr.iter()
                    .map(|v| v.as_str().unwrap().to_string())
                    .collect()
            })
    }
}

impl Drop for Fixture {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.dir);
        let _ =
            fs::remove_dir_all(PathBuf::from(DOWNLOAD_PATH).join(format!("{}-0.0.0_deps", self.slug)));
    }
}

/// The optional dependency, as `crate_info.deps_and_features` records it.
fn optional_dep(name: &str) -> (CrateInfo, Vec<String>) {
    (
        CrateInfo {
            name: name.to_string(),
            version: "0.0.0".to_string(),
            optional: true,
            ..Default::default()
        },
        Vec::new(),
    )
}

fn exchange_for(slug: &str, dep: &str, features: Vec<(String, TupleVec)>) -> DataExchange {
    DataExchange {
        name_with_version: format!("{slug}:0.0.0"),
        crate_info: CrateInfo {
            name: slug.to_string(),
            version: "0.0.0".to_string(),
            deps_and_features: vec![optional_dep(dep)],
            features,
            ..Default::default()
        },
        ..Default::default()
    }
}

/// insecure-time in miniature. `clap` is optional and appears in `default`, but
/// there is no `[features] clap` entry — it is Cargo's implicit feature. Nothing
/// can be cut out of the manifest, so the feature itself has to come off the
/// build.
#[test]
fn an_implicit_optional_dep_feature_is_turned_off_when_the_dep_is_not_no_std() {
    let slug = "implicit_sever_forced_off";
    let _fixture = Fixture::new(
        slug,
        "[package]\nname = \"implicit_sever_forced_off\"\nversion = \"0.0.0\"\n\n\
         [dependencies.clap]\nversion = \"4\"\noptional = true\n\n\
         [features]\ndefault = [\"std\", \"clap\"]\nstd = []\n",
    )
    .with_std_only_dep("clap");

    // Only `std` is declared; `clap` is Cargo's implicit feature and is absent.
    let mut exchange = exchange_for(slug, "clap", vec![("std".to_string(), vec![])]);
    // The pair `features_for_optional_deps` produces for an implicit feature.
    let mut deps_and_features: TupleVec = vec![("clap".to_string(), "clap".to_string())];
    let mut forced_off: Vec<String> = Vec::new();

    let skipped = should_skip_dep(
        "clap:0.0.0",
        &mut exchange,
        &mut deps_and_features,
        &["clap".to_string()],
        true,
        false,
        &HashSet::new(),
        &mut forced_off,
    );

    assert!(skipped, "a dependency that is not no_std must be skipped");
    assert_eq!(
        forced_off,
        vec!["clap".to_string()],
        "the implicit feature has no manifest entry to sever, so it must be turned off instead"
    );
    assert!(
        exchange
            .telemetry
            .optional_deps_disabled
            .contains(&"clap".to_string()),
        "the disable has to be recorded like any other sever"
    );
}

/// The control: the same dependency reached through a *declared* feature. Here
/// there is an entry to cut, `remove_feats_enabling_dep` cuts it and parks it,
/// and nothing is forced off the command line — the feature stays on, which is
/// what severing means.
#[test]
fn a_declared_feature_is_severed_in_the_manifest_not_forced_off() {
    let slug = "implicit_sever_declared_control";
    let fixture = Fixture::new(
        slug,
        "[package]\nname = \"implicit_sever_declared_control\"\nversion = \"0.0.0\"\n\n\
         [dependencies.clap]\nversion = \"4\"\noptional = true\n\n\
         [features]\ndefault = [\"cli\"]\ncli = [\"dep:clap\"]\n",
    )
    .with_std_only_dep("clap");

    let mut exchange = exchange_for(
        slug,
        "clap",
        vec![(
            "cli".to_string(),
            vec![("clap".to_string(), "dep:".to_string())],
        )],
    );
    let mut deps_and_features: TupleVec = vec![("clap".to_string(), "cli".to_string())];
    let mut forced_off: Vec<String> = Vec::new();

    let skipped = should_skip_dep(
        "clap:0.0.0",
        &mut exchange,
        &mut deps_and_features,
        &["cli".to_string()],
        true,
        false,
        &HashSet::new(),
        &mut forced_off,
    );

    assert!(skipped, "a dependency that is not no_std must be skipped");
    assert!(
        forced_off.is_empty(),
        "a declared feature is severed in the manifest, not dropped from the build: {forced_off:?}"
    );
    assert_eq!(
        fixture.feature_values("cli"),
        Some(Vec::new()),
        "`dep:clap` must be cut out of the feature that enables it"
    );
}

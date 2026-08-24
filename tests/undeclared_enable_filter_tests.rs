#![feature(rustc_private)]

//! R34-2 / R34-14: a feature the crate's source names but its manifest never
//! declares must not reach the emitted configuration.
//!
//! The solve's model carries one `Bool` per `feature = "…"` atom found in the
//! crate's **source**, and that is a wider set than the `[features]` table.
//! Crates ship cfgs for features they never published — `sp-weights 23.0.0`
//! gates `weight_v2.rs:406` on `feature = "runtime-benchmarks"` while declaring
//! four features, none of them that one; `bitcoin 0.32` dropped `no-std` from
//! the manifest and left the cfgs; the wTools packages ship the whole monorepo,
//! so `impls_index` carries files naming `many` and `meta_former`. Z3 has no
//! reason to set such an atom either way and picks arbitrarily, and a `true`
//! used to be emitted as though the crate had asked for it:
//!
//! * `<dep>/<atom>` in `custom_no_std_feature_enabled`, which cargo refuses
//!   before compiling anything — *"`sp-weights` does not have that feature"*.
//!   17 crates, `kusama-runtime-constants` and the wTools family among them.
//! * a bare feature on the main crate, which `new_feats_to_add` then declares
//!   into existence. `morse-nostd 0.1.2` publishes no features and was emitted
//!   `[features] alloc = []` with `--features alloc`, giving it two
//!   `extern crate alloc` and `E0259`.
//!
//! What is filtered is only the unaskable. The genuine forwarding gap — a
//! feature the dependency really has, that no feature of the main crate
//! reaches — is what `final_feature_list_dep`'s `not_found` exists to repair,
//! and it still travels; `a_real_forwarding_gap_still_travels` is the guard on
//! that.

use std::fs;
use std::path::PathBuf;

use nostd::parser::determine_manifest_file;
use nostd::solver::{
    final_feature_list_dep, new_feats_to_add, retain_selectable_features, selectable_features,
};
use nostd::{CrateInfo, Telemetry, consts};

fn toml_of(s: &str) -> toml::Value {
    toml::from_str(s).expect("fixture manifest parses")
}

fn feature_values(manifest: &toml::Value, feat: &str) -> Vec<String> {
    manifest
        .get("features")
        .and_then(|f| f.get(feat))
        .and_then(|v| v.as_array())
        .map(|arr| {
            arr.iter()
                .map(|v| v.as_str().unwrap().to_string())
                .collect()
        })
        .unwrap_or_default()
}

/// A crate directory under `consts::DOWNLOAD_PATH`, where
/// `parser::determine_manifest_file` looks for a manifest.
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
            name_with_version: format!("{slug}-0.0.0"),
            dir,
        }
    }

    fn manifest(&self) -> toml::Value {
        toml_of(&fs::read_to_string(self.dir.join("Cargo.toml")).expect("fixture manifest reread"))
    }
}

impl Drop for Fixture {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.dir);
    }
}

/// `sp-weights 23.0.0` reduced to its shape: four declared features, and a
/// source that gates an item on a fifth the manifest has never had.
fn sp_weights_shape() -> (CrateInfo, toml::Value) {
    let manifest = toml_of(
        r#"[package]
name = "sp-weights"
version = "23.0.0"

[features]
default = ["std"]
full-metadata-docs = []
serde = []
std = []
"#,
    );
    let crate_info = CrateInfo {
        name: "sp-weights".to_string(),
        version: "23.0.0".to_string(),
        features: vec![
            ("default".to_string(), Vec::new()),
            ("full-metadata-docs".to_string(), Vec::new()),
            ("serde".to_string(), Vec::new()),
            ("std".to_string(), Vec::new()),
        ],
        ..CrateInfo::default()
    };
    (crate_info, manifest)
}

#[test]
fn an_atom_the_manifest_never_declares_is_not_selectable() {
    let (crate_info, manifest) = sp_weights_shape();
    let selectable = selectable_features(&crate_info, &manifest);

    assert!(
        selectable.contains("serde"),
        "a declared feature is selectable"
    );
    assert!(
        !selectable.contains("runtime-benchmarks"),
        "no published sp-weights has ever declared `runtime-benchmarks`; the atom \
         comes from a cfg in weight_v2.rs and cargo cannot turn it on"
    );

    let mut enable = vec!["serde".to_string(), "runtime-benchmarks".to_string()];
    let dropped = retain_selectable_features(&mut enable, &selectable);

    assert_eq!(enable, vec!["serde".to_string()]);
    assert_eq!(
        dropped,
        vec!["runtime-benchmarks".to_string()],
        "dropped atoms are returned, not discarded — a crate whose only no_std \
         switch is one of these has to be triaged, not emitted without it"
    );
}

/// Cargo creates one implicit feature per optional dependency and puts it in no
/// `[features]` table. A selectable set built from the table alone would drop
/// `libm` here — the shape of `glam`, `euclid` and `num-complex`, and the reason
/// `published_features()` is the wrong reader for this check.
#[test]
fn the_implicit_feature_of_an_optional_dependency_is_selectable() {
    let manifest = toml_of(
        r#"[package]
name = "earcut"
version = "0.4.4"

[dependencies.num-traits]
version = "0.2"
default-features = false

[dependencies.libm]
version = "0.2"
optional = true
"#,
    );
    let crate_info = CrateInfo {
        name: "earcut".to_string(),
        version: "0.4.4".to_string(),
        ..CrateInfo::default()
    };

    let selectable = selectable_features(&crate_info, &manifest);
    assert!(
        selectable.contains("libm"),
        "`libm` is declared only as an optional dependency, and cargo's implicit \
         feature of that name is a perfectly askable feature"
    );

    let mut enable = vec!["libm".to_string()];
    assert!(retain_selectable_features(&mut enable, &selectable).is_empty());
    assert_eq!(enable, vec!["libm".to_string()]);
}

/// R34-14 end to end: `morse-nostd` publishes no features, its lib.rs names
/// `alloc`, and the emitted manifest grew an `alloc = []` the crate never had.
#[test]
fn a_feature_the_crate_does_not_publish_is_not_declared_into_existence() {
    let manifest = toml_of(
        r#"[package]
name = "morse-nostd"
version = "0.1.2"
"#,
    );
    let crate_info = CrateInfo {
        name: "morse-nostd".to_string(),
        version: "0.1.2".to_string(),
        ..CrateInfo::default()
    };

    // The bug, stated: handed the model's raw answer, the emission layer invents
    // the feature rather than refusing it.
    let unfiltered = new_feats_to_add(&crate_info, &manifest, &[], &["alloc".to_string()]);
    assert_eq!(
        unfiltered,
        vec![("alloc".to_string(), Vec::new())],
        "this is what shipped: `[features] alloc = []` and `--features alloc`"
    );

    // The fix: the atom never reaches the emission layer.
    let selectable = selectable_features(&crate_info, &manifest);
    let mut enable = vec!["alloc".to_string()];
    let dropped = retain_selectable_features(&mut enable, &selectable);
    assert_eq!(dropped, vec!["alloc".to_string()]);
    assert!(
        new_feats_to_add(&crate_info, &manifest, &[], &enable).is_empty(),
        "with the atom filtered there is nothing left to declare"
    );
}

/// The main crate of the `kusama-runtime-constants` shape: it depends on
/// `sp-weights`, and no feature of its own forwards anything to it.
fn kusama_shape() -> CrateInfo {
    let (dep, _) = sp_weights_shape();
    CrateInfo {
        name: "kusama-runtime-constants".to_string(),
        version: "3.0.0".to_string(),
        deps_and_features: vec![(dep, Vec::new())],
        ..CrateInfo::default()
    }
}

fn main_fixture(slug: &str) -> Fixture {
    Fixture::new(
        slug,
        r#"[package]
name = "kusama-runtime-constants"
version = "3.0.0"

[dependencies.sp-weights]
version = "23.0.0"
default-features = false
"#,
    )
}

/// R34-2 end to end. Unfiltered, the dependency's solve answer is parked as
/// `sp-weights/runtime-benchmarks` and cargo refuses to resolve the manifest.
#[test]
fn an_undeclared_dep_atom_is_not_parked_as_a_dep_feature_pair() {
    let (dep_info, dep_manifest) = sp_weights_shape();
    let crate_info = kusama_shape();

    // Unfiltered — the shipped behaviour, and the reason all 26 targets failed
    // without a single crate being compiled.
    {
        let fixture = main_fixture("undeclared-enable-filter-unfiltered");
        let mut telemetry = Telemetry::default();
        final_feature_list_dep(
            &crate_info_named(&crate_info, &fixture.name_with_version),
            "sp-weights",
            &["runtime-benchmarks".to_string()],
            &[],
            &[],
            &[],
            &mut telemetry,
        );
        assert_eq!(
            feature_values(&fixture.manifest(), consts::CUSTOM_FEATURES_ENABLED),
            vec!["sp-weights/runtime-benchmarks".to_string()],
            "cargo: `sp-weights` does not have that feature"
        );
    }

    // Filtered — the atom is gone before the dependency's answer is consumed, so
    // there is nothing to park.
    {
        let fixture = main_fixture("undeclared-enable-filter-filtered");
        let mut telemetry = Telemetry::default();
        let selectable = selectable_features(&dep_info, &dep_manifest);
        let mut enable = vec!["runtime-benchmarks".to_string()];
        assert_eq!(
            retain_selectable_features(&mut enable, &selectable),
            vec!["runtime-benchmarks".to_string()]
        );
        final_feature_list_dep(
            &crate_info_named(&crate_info, &fixture.name_with_version),
            "sp-weights",
            &enable,
            &[],
            &[],
            &[],
            &mut telemetry,
        );
        assert!(
            feature_values(&fixture.manifest(), consts::CUSTOM_FEATURES_ENABLED).is_empty(),
            "nothing unaskable is parked, and the manifest resolves"
        );
        assert!(
            telemetry.custom_features_added_list.is_empty(),
            "and the run does not record a repair it did not make"
        );
    }
}

/// The guard on the other side: `not_found` exists because a dependency can need
/// a feature the main crate offers no way to enable, and parking it is the
/// repair. `serde` is declared by `sp-weights`, so it is selectable, and it must
/// still be parked exactly as before.
#[test]
fn a_real_forwarding_gap_still_travels() {
    let (dep_info, dep_manifest) = sp_weights_shape();
    let crate_info = kusama_shape();
    let fixture = main_fixture("undeclared-enable-filter-real-gap");
    let mut telemetry = Telemetry::default();

    let selectable = selectable_features(&dep_info, &dep_manifest);
    let mut enable = vec!["serde".to_string(), "runtime-benchmarks".to_string()];
    retain_selectable_features(&mut enable, &selectable);

    final_feature_list_dep(
        &crate_info_named(&crate_info, &fixture.name_with_version),
        "sp-weights",
        &enable,
        &[],
        &[],
        &[],
        &mut telemetry,
    );

    assert_eq!(
        feature_values(&fixture.manifest(), consts::CUSTOM_FEATURES_ENABLED),
        vec!["sp-weights/serde".to_string()],
        "the genuine gap is untouched by the filter"
    );
    assert_eq!(
        telemetry.custom_features_added_list,
        vec![("sp-weights".to_string(), vec!["serde".to_string()])],
    );
}

/// `final_feature_list_dep` builds the manifest path from `crate_info`'s own
/// name and version, so a fixture directory has to be addressed by rewriting
/// those rather than by passing a path.
fn crate_info_named(base: &CrateInfo, name_with_version: &str) -> CrateInfo {
    let (name, version) = name_with_version
        .rsplit_once('-')
        .expect("fixture slug carries a version");
    CrateInfo {
        name: name.to_string(),
        version: version.to_string(),
        deps_and_features: base.deps_and_features.clone(),
        features: base.features.clone(),
        ..CrateInfo::default()
    }
}

#[test]
fn determine_manifest_file_finds_the_fixture() {
    let fixture = main_fixture("undeclared-enable-filter-path");
    let path = determine_manifest_file(&fixture.name_with_version, None);
    assert!(path.ends_with("Cargo.toml"), "{path}");
}

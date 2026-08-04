#![feature(rustc_private)]

//! Bucket T3 (`MANIFEST_EMITTED_BAD`): cargo refuses the manifest the tool wrote, so
//! the analysis behind it is never tested at all — 0/26 targets with no build attempt
//! that means anything.
//!
//! Two independent producers, both fixed here:
//!
//! * **`multiple slashes in feature 'regex/regex-syntax/default'`** — a dependency's
//!   own `default` list may name a feature of *its* dependencies. Prefixing that with
//!   the dep name gives a value cargo has no grammar for. 7 crates: `matchable`,
//!   `binator_nom` (`binator/smallvec/serde`), `ryml` (`cxx/cxxbridge-flags/default`),
//!   `odem-rs`, `mdast_util_to_markdown`, `proper_path_tools`, `pth`.
//! * **`Make sure that 'dep:simd' is included in one of features`** — the tool
//!   declared `simd = []` for what was already cargo's implicit feature for the
//!   optional dependency `simd`, which replaces the implicit feature and leaves the
//!   dependency enabled by nothing. 8 crates: `blake-hash` (`simd` → package
//!   `ppv-lite86`), `digestible`, `informalsystems-prost`, `ceres-executor`,
//!   `inkpad-executor`, `scale-serialization` (all renamed deps), `mfio-rt`,
//!   `iconv-native` (target-gated deps).

use nostd::parser::{
    add_feats_to_custom_feature, features_reference_dep_explicitly, optional_dep_keys,
};
use nostd::solver::new_feats_to_add;
use nostd::{CrateInfo, consts};

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

/// blake-hash's shape: the optional dep is renamed, so its manifest key and its
/// package name differ. `CrateInfo` records only the package.
const BLAKE_HASH: &str = r#"
[package]
name = "blake-hash"
version = "0.4.1"

[dependencies.block-buffer]
version = "0.9"

[dependencies.simd]
version = "0.2.16"
optional = true
package = "ppv-lite86"

[features]
default = ["simd", "std"]
std = []
"#;

/// mfio-rt's shape: the optional deps are target-gated, so `gather_crate_info` — which
/// walks `[dependencies]` only — never sees them.
const MFIO_RT: &str = r#"
[package]
name = "mfio-rt"
version = "0.1.0"

[features]
default = ["mio", "io-uring", "iocp"]
iocp = []

[target."cfg(target_os = \"linux\")".dependencies.io-uring]
version = "0.6"
optional = true

[target."cfg(unix)".dependencies.mio]
version = "0.8"
optional = true
"#;

#[test]
fn optional_dep_keys_uses_the_manifest_key_not_the_package() {
    let keys = optional_dep_keys(&toml_of(BLAKE_HASH));
    assert!(keys.contains("simd"), "got {keys:?}");
    assert!(!keys.contains("ppv-lite86"), "got {keys:?}");
    assert!(!keys.contains("block-buffer"), "non-optional dep leaked");
}

#[test]
fn optional_dep_keys_covers_target_and_build_dependencies() {
    let keys = optional_dep_keys(&toml_of(MFIO_RT));
    assert!(keys.contains("io-uring"), "got {keys:?}");
    assert!(keys.contains("mio"), "got {keys:?}");

    let with_build = toml_of(
        r#"
        [build-dependencies.vcpkg]
        version = "0.2"
        optional = true

        [build-dependencies.cc]
        version = "1"
        "#,
    );
    let keys = optional_dep_keys(&with_build);
    assert!(keys.contains("vcpkg"), "got {keys:?}");
    assert!(!keys.contains("cc"));
}

#[test]
fn implicit_optional_dep_feature_is_not_redeclared() {
    let manifest = toml_of(BLAKE_HASH);
    let mut crate_info = CrateInfo {
        name: "blake-hash".to_string(),
        version: "0.4.1".to_string(),
        ..CrateInfo::default()
    };
    // What `gather_crate_info` records: the package name, never `simd`.
    crate_info.deps_and_features.push((
        CrateInfo {
            name: "ppv-lite86".to_string(),
            optional: true,
            ..CrateInfo::default()
        },
        Vec::new(),
    ));
    crate_info
        .features
        .push(("std".to_string(), Vec::new()));

    let to_declare = new_feats_to_add(
        &crate_info,
        &manifest,
        &["simd".to_string()],
        &["std".to_string()],
    );
    assert!(
        to_declare.is_empty(),
        "`simd` is cargo's implicit feature for the optional dep of that name; \
         declaring it detaches the dep — got {to_declare:?}"
    );
}

#[test]
fn suppressed_implicit_feature_is_declared_with_its_dep_ref() {
    // `dep:simd` anywhere in the table suppresses the implicit `simd` feature, so here
    // the feature really is missing — and its value has to keep the dep attached.
    let manifest = toml_of(
        r#"
        [package]
        name = "blake-hash"
        version = "0.4.1"

        [dependencies.simd]
        version = "0.2.16"
        optional = true
        package = "ppv-lite86"

        [features]
        fast = ["dep:simd"]
        "#,
    );
    assert!(features_reference_dep_explicitly(&manifest, "simd"));
    assert!(!features_reference_dep_explicitly(&toml_of(BLAKE_HASH), "simd"));

    let crate_info = CrateInfo {
        name: "blake-hash".to_string(),
        version: "0.4.1".to_string(),
        features: vec![("fast".to_string(), Vec::new())],
        ..CrateInfo::default()
    };

    let to_declare = new_feats_to_add(&crate_info, &manifest, &[], &["simd".to_string()]);
    assert_eq!(
        to_declare,
        vec![("simd".to_string(), vec!["dep:simd".to_string()])]
    );
}

#[test]
fn features_a_crate_genuinely_lacks_are_still_declared_empty() {
    let manifest = toml_of(MFIO_RT);
    let crate_info = CrateInfo {
        name: "mfio-rt".to_string(),
        version: "0.1.0".to_string(),
        features: vec![("iocp".to_string(), Vec::new())],
        ..CrateInfo::default()
    };

    // `mio` is an optional dep (skipped); `alloc` is a plain name nobody declares.
    let to_declare = new_feats_to_add(
        &crate_info,
        &manifest,
        &[],
        &["mio".to_string(), "alloc".to_string()],
    );
    assert_eq!(
        to_declare,
        vec![("alloc".to_string(), Vec::<String>::new())]
    );
}

#[test]
fn transitive_feature_values_never_reach_the_manifest() {
    let mut manifest = toml_of(
        r#"
        [package]
        name = "matchable"
        version = "0.1.1"

        [features]
        "#,
    );

    add_feats_to_custom_feature(
        &mut manifest,
        consts::CUSTOM_FEATURES_DISABLED,
        &[
            "regex/std".to_string(),
            // What `update_main_crate_default_list` used to build out of regex's own
            // `default = [..., "regex-syntax/default"]`.
            "regex/regex-syntax/default".to_string(),
            // A dep's default may also enable an optional dep of its own.
            "regex/dep:memchr".to_string(),
        ],
    );

    assert_eq!(
        feature_values(&manifest, consts::CUSTOM_FEATURES_DISABLED),
        vec!["regex/std".to_string()],
    );
}

#[test]
fn weak_dep_features_are_still_accepted() {
    let mut manifest = toml_of("[features]\n");
    add_feats_to_custom_feature(
        &mut manifest,
        consts::CUSTOM_FEATURES_ENABLED,
        &["serde?/alloc".to_string(), "dep:serde".to_string()],
    );
    assert_eq!(
        feature_values(&manifest, consts::CUSTOM_FEATURES_ENABLED),
        vec!["serde?/alloc".to_string(), "dep:serde".to_string()],
    );
}

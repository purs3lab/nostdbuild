#![feature(rustc_private)]

//! KI-12: the optional-dep enabler fix was main-crate only.
//!
//! `bin/main.rs` already adds back the optional-dep enabler a chosen no_std
//! feature set makes mandatory (`solver::forced_optional_dep_enablers`) when
//! the crate under analysis is the main crate. `parser::process_dep_crate`
//! computed the exact same thing for a *dependency* one level down
//! (`dep_missing_optional_dep_enablers` telemetry) but never applied it — the
//! dependency's own no_std half could still import an optional dep whose
//! enabler never made it into the emitted feature set.
//!
//! `lazy_static` is a real corpus repro of the shape (seen processing
//! `orchard-0.11.0`, `binver-0.1.1`, `radix_engine_interface-1.3.0` as
//! dependencies): `#[cfg_attr(feature = "spin_no_std", path =
//! "core_lazy.rs")]` selects a module that does `use self::spin::Once;`,
//! where `spin` is an optional dependency only `spin_no_std` enables.
//!
//! These tests exercise `finalize_dep_crate` directly with a precomputed
//! forced-enabler list (the Z3 side that produces that list —
//! `solver::forced_optional_dep_enablers` — already has its own coverage in
//! `tests/optional_dep_backend_tests.rs`); the gap this closes is entirely in
//! what `finalize_dep_crate` does with the result.

use std::collections::HashSet;
use std::fs;
use std::path::PathBuf;

use nostd::consts::DOWNLOAD_PATH;
use nostd::parser::{self, finalize_dep_crate, move_unnecessary_dep_feats};
use nostd::{Attributes, CrateInfo, DataExchange};

/// A main crate plus one direct dependency, laid out where
/// `determine_manifest_file` looks for each — same layout
/// `ki2_dep_link_kept_in_use_tests.rs` uses.
struct Fixture {
    slug: String,
    main_dir: PathBuf,
}

impl Fixture {
    fn new(slug: &str, dep: &str, main_manifest: &str, dep_manifest: &str) -> Self {
        let main_dir = PathBuf::from(DOWNLOAD_PATH).join(format!("{slug}-0.0.0"));
        let deps_dir = PathBuf::from(DOWNLOAD_PATH).join(format!("{slug}-0.0.0_deps"));
        let _ = fs::remove_dir_all(&main_dir);
        let _ = fs::remove_dir_all(&deps_dir);
        fs::create_dir_all(main_dir.join("src")).expect("main crate dir");
        fs::write(main_dir.join("Cargo.toml"), main_manifest).expect("main manifest");
        fs::write(main_dir.join("src/lib.rs"), "#![no_std]\n").expect("main lib.rs");

        let dep_dir = deps_dir.join(format!("{dep}-0.0.0"));
        fs::create_dir_all(dep_dir.join("src")).expect("dep dir");
        fs::write(dep_dir.join("Cargo.toml"), dep_manifest).expect("dep manifest");
        fs::write(dep_dir.join("src/lib.rs"), "// stand-in\n").expect("dep lib.rs");

        Self {
            slug: slug.to_string(),
            main_dir,
        }
    }

    /// A real `Attributes` for the dependency, built the same way
    /// `process_dep_crate` builds one (`parser::parse_crate`) — `Attributes`'
    /// non-`crate_name` fields are private outside this crate, so a struct
    /// literal is not an option here, and `finalize_dep_crate` only ever reads
    /// `crate_name` regardless.
    fn dep_attributes(&self, dep: &str) -> Attributes {
        let dep_lib = PathBuf::from(DOWNLOAD_PATH)
            .join(format!("{}-0.0.0_deps", self.slug))
            .join(format!("{dep}-0.0.0/src/lib.rs"));
        parser::parse_crate(&format!("{dep}:0.0.0"), false, None, &[], Some(&[dep_lib]))
    }

    fn main_toml(&self) -> toml::Value {
        fs::read_to_string(self.main_dir.join("Cargo.toml"))
            .expect("manifest reread")
            .parse()
            .expect("manifest parses")
    }

    fn custom_features_enabled(&self) -> Vec<String> {
        self.main_toml()
            .get("features")
            .and_then(|f| f.get("custom_no_std_feature_enabled"))
            .and_then(|v| v.as_array())
            .map(|arr| {
                arr.iter()
                    .map(|v| v.as_str().unwrap().to_string())
                    .collect()
            })
            .unwrap_or_default()
    }
}

impl Drop for Fixture {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.main_dir);
        let _ = fs::remove_dir_all(
            PathBuf::from(DOWNLOAD_PATH).join(format!("{}-0.0.0_deps", self.slug)),
        );
    }
}

const MAIN_MANIFEST: &str = "[package]\nname = \"{slug}\"\nversion = \"0.0.0\"\n\n\
     [dependencies.lazy_static]\nversion = \"1\"\n";

const LAZY_STATIC_MANIFEST: &str = "[package]\nname = \"lazy_static\"\nversion = \"0.0.0\"\n\n\
     [dependencies.spin]\nversion = \"0.5\"\noptional = true\n\n\
     [features]\nspin_no_std = [\"spin\"]\n";

fn exchange_for(slug: &str) -> DataExchange {
    DataExchange {
        name_with_version: format!("{slug}:0.0.0"),
        crate_info: CrateInfo {
            name: slug.to_string(),
            version: "0.0.0".to_string(),
            deps_and_features: vec![(
                CrateInfo {
                    name: "lazy_static".to_string(),
                    version: "0.0.0".to_string(),
                    optional: false,
                    default_features: true,
                    ..Default::default()
                },
                Vec::new(),
            )],
            ..Default::default()
        },
        ..Default::default()
    }
}

/// The core fix: a forced enabler `finalize_dep_crate` is told about must
/// reach the emitted feature set even though nothing in the dependency's own
/// solve (`enable`/`disable`, both empty here — the shape of a dependency
/// whose isolated solve saw no reason to touch its own optional deps) or its
/// declared `[features]` table routes it there.
#[test]
fn finalize_dep_crate_applies_a_forced_optional_dep_enabler() {
    let slug = "ki12_applies_forced_enabler";
    let fixture = Fixture::new(
        slug,
        "lazy_static",
        &MAIN_MANIFEST.replace("{slug}", slug),
        LAZY_STATIC_MANIFEST,
    );
    let mut exchange = exchange_for(slug);
    let dep = fixture.dep_attributes("lazy_static");

    let (args, _formatted_disable, enable) = finalize_dep_crate(
        &mut exchange,
        &dep,
        Vec::new(),
        Vec::new(),
        None,
        std::collections::HashMap::new(),
        &["spin_no_std".to_string()],
    )
    .expect("finalize_dep_crate");

    assert!(
        enable.contains(&"spin_no_std".to_string()),
        "the forced enabler must reach the dependency's own emitted enable list, got {enable:?}"
    );
    assert!(
        args.contains(&"custom_no_std_feature_enabled".to_string()),
        "with no main [features] entry routing to it, the enabler has to ride the \
         custom-features escape hatch, got args {args:?}"
    );
    assert_eq!(
        fixture.custom_features_enabled(),
        vec!["lazy_static/spin_no_std".to_string()],
        "the main crate's on-disk manifest must gain the dep/feat pair, or cargo never \
         actually turns `spin_no_std` on and lazy_static's `use self::spin::Once` \
         still fails to link"
    );
    assert!(
        exchange
            .protected_dep_features
            .contains(&("lazy_static".to_string(), "spin_no_std".to_string())),
        "the enabler this pass just added must be protected, or a later \
         move_unnecessary_dep_feats pass can delete the very thing this fixed"
    );
}

/// A forced enabler the dependency's own solve already picked (it is already
/// in `enable`) must not be duplicated, but still gets the protection entry —
/// protection does not piggyback on "was this newly added".
#[test]
fn finalize_dep_crate_does_not_duplicate_an_already_enabled_forced_enabler() {
    let slug = "ki12_no_duplicate_forced_enabler";
    let fixture = Fixture::new(
        slug,
        "lazy_static",
        &MAIN_MANIFEST.replace("{slug}", slug),
        LAZY_STATIC_MANIFEST,
    );
    let mut exchange = exchange_for(slug);
    let dep = fixture.dep_attributes("lazy_static");

    let (_args, _formatted_disable, enable) = finalize_dep_crate(
        &mut exchange,
        &dep,
        vec!["spin_no_std".to_string()],
        Vec::new(),
        None,
        std::collections::HashMap::new(),
        &["spin_no_std".to_string()],
    )
    .expect("finalize_dep_crate");

    assert_eq!(
        enable.iter().filter(|f| *f == "spin_no_std").count(),
        1,
        "the dependency's own solve already asked for this; the forced-enabler pass \
         must not add a second copy, got {enable:?}"
    );
    assert!(
        exchange
            .protected_dep_features
            .contains(&("lazy_static".to_string(), "spin_no_std".to_string())),
        "protection must still be recorded even when the value was not newly added"
    );
    let _ = fixture;
}

/// The protection this fix records has to actually stop
/// `move_unnecessary_dep_feats` from deleting the forced enabler in the one
/// case that matters: something else's `dep_forbidden_features` closure
/// disagrees with the Z3 model and calls the same feature name forbidden
/// (KI-12's own note on why this needs `protected_dep_features`, not a bare
/// "was it added" check — the KI-1/KI-5 failure mode).
#[test]
fn move_unnecessary_dep_feats_keeps_a_protected_forced_enabler_even_if_marked_forbidden() {
    let slug = "ki12_protected_survives_forbidden";
    let fixture = Fixture::new(
        slug,
        "lazy_static",
        &format!(
            "[package]\nname = \"{slug}\"\nversion = \"0.0.0\"\n\n\
             [dependencies.lazy_static]\nversion = \"1\"\n\n\
             [features]\ndefault = []\ncustom_no_std_feature_enabled = \
             [\"lazy_static/spin_no_std\"]\n"
        ),
        LAZY_STATIC_MANIFEST,
    );

    let mut telemetry = nostd::Telemetry::default();
    let dep_forbidden: HashSet<String> = ["spin_no_std".to_string()].into_iter().collect();
    let protected: HashSet<(String, String)> =
        [("lazy_static".to_string(), "spin_no_std".to_string())]
            .into_iter()
            .collect();

    let mut fixed_main_args = vec!["custom_no_std_feature_enabled".to_string()];
    let mut flexible_main_args: Vec<String> = Vec::new();

    move_unnecessary_dep_feats(
        &format!("{slug}-0.0.0"),
        &mut fixed_main_args,
        &mut flexible_main_args,
        "lazy_static:0.0.0",
        &["spin_no_std".to_string()],
        &mut telemetry,
        false,
        &protected,
        &dep_forbidden,
    );

    assert_eq!(
        fixture.custom_features_enabled(),
        vec!["lazy_static/spin_no_std".to_string()],
        "protected_dep_features must override dep_forbidden, or the pass this fix relies \
         on to keep the enabler on the command line can immediately delete it again"
    );
}

/// Control for the test above: same forbidden mark, no protection entry —
/// `move_unnecessary_dep_feats` deletes it. Without this, the previous test
/// could pass for the wrong reason (the value surviving regardless of
/// `protected_dep_features`).
#[test]
fn move_unnecessary_dep_feats_removes_an_unprotected_feature_marked_forbidden() {
    let slug = "ki12_unprotected_removed_control";
    let fixture = Fixture::new(
        slug,
        "lazy_static",
        &format!(
            "[package]\nname = \"{slug}\"\nversion = \"0.0.0\"\n\n\
             [dependencies.lazy_static]\nversion = \"1\"\n\n\
             [features]\ndefault = []\ncustom_no_std_feature_enabled = \
             [\"lazy_static/spin_no_std\"]\n"
        ),
        LAZY_STATIC_MANIFEST,
    );

    let mut telemetry = nostd::Telemetry::default();
    let dep_forbidden: HashSet<String> = ["spin_no_std".to_string()].into_iter().collect();

    let mut fixed_main_args = vec!["custom_no_std_feature_enabled".to_string()];
    let mut flexible_main_args: Vec<String> = Vec::new();

    move_unnecessary_dep_feats(
        &format!("{slug}-0.0.0"),
        &mut fixed_main_args,
        &mut flexible_main_args,
        "lazy_static:0.0.0",
        &["spin_no_std".to_string()],
        &mut telemetry,
        false,
        &HashSet::new(),
        &dep_forbidden,
    );

    assert!(
        fixture.custom_features_enabled().is_empty(),
        "without a protection entry, a feature marked forbidden is deleted — this is the \
         behavior the protected case above must escape"
    );
}

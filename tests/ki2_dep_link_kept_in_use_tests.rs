#![feature(rustc_private)]

//! KI-2: severing a dep link the crate's own code still uses.
//!
//! `should_skip_dep` decides a dependency cannot be no_std and severs it by
//! deleting the dependency out of the `[features]` entry that enables it,
//! leaving that feature itself on. R31-2 (`fb813dc`) found that unsound when
//! something under the surviving feature still names the dependency — the
//! `driver::deps_pinned_by_active_use` pin set — and made `should_skip_dep`
//! turn the *feature* off instead of just cutting the edge in that case.
//! `bulletproofs-bls-4.0.0` is the named repro for both R31-2's commit message
//! and KI-2's own writeup: `#[cfg(feature = "blst")] pub use blstrs_plus::{…}`
//! with `blst = ["blstrs_plus"]` and `blstrs_plus` not no_std-capable — sever
//! the edge alone and `blst` stays on, importing a crate cargo no longer links
//! (`E0433`).
//!
//! This is the one case neither existing suite exercises end to end:
//! `dep_pin_tests.rs` checks `deps_pinned_by_active_use` computes the pin set
//! correctly; `implicit_optional_dep_sever_tests.rs` checks `should_skip_dep`'s
//! sever branch only with an *empty* pin set. Here the pin set is non-empty —
//! the `deps_to_keep.contains(&dep_name)` branch — and this is where R31-2's
//! fix actually lives.

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
    /// severing branch — `blstrs_plus-0.8.18` in miniature.
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
        let _ = fs::remove_dir_all(
            PathBuf::from(DOWNLOAD_PATH).join(format!("{}-0.0.0_deps", self.slug)),
        );
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

/// `bulletproofs-bls` in miniature, minus the `compile_error!` (that half is
/// KI-3's, already fixed and covered by `tests/compile_error_backend_capability_tests.rs`
/// — this test isolates just the severing decision). `default = ["blst"]`,
/// `blst = ["blstrs_plus"]`, `blstrs_plus` is not no_std, and the pin set says
/// the dependency is still named under `blst`'s cfg. `should_skip_dep` must
/// turn `blst` itself off rather than only cutting `blstrs_plus` out of
/// `blst`'s own array and leaving `blst` enabled via `default` — the latter is
/// exactly KI-2's `E0433` shape. `default` is still active here
/// (`disable_default = false`), so it is what actually carries `blst` into the
/// build and what the manifest edit has to reach — matching R31-2's own
/// verified case (`eth-blockies-1.1.0`: "`compressed_png` off, `default = []`").
#[test]
fn a_dep_pinned_by_active_use_has_its_leaf_feature_turned_off_not_just_its_edge() {
    let slug = "ki2_sever_pinned";
    let fixture = Fixture::new(
        slug,
        "[package]\nname = \"ki2_sever_pinned\"\nversion = \"0.0.0\"\n\n\
         [dependencies.blstrs_plus]\nversion = \"0.8\"\noptional = true\n\n\
         [features]\ndefault = [\"blst\"]\nblst = [\"blstrs_plus\"]\n",
    )
    .with_std_only_dep("blstrs_plus");

    let mut exchange = exchange_for(
        slug,
        "blstrs_plus",
        vec![
            (
                "default".to_string(),
                vec![("blst".to_string(), "blst".to_string())],
            ),
            (
                "blst".to_string(),
                vec![("blstrs_plus".to_string(), "blstrs_plus".to_string())],
            ),
        ],
    );
    // `features_for_optional_deps_with` checks every declared feature's own
    // transitive reach, so both `default` (reaches it through `blst`) and
    // `blst` (reaches it directly) get a pair here — not just the leaf.
    let mut deps_and_features: TupleVec = vec![
        ("blstrs_plus".to_string(), "default".to_string()),
        ("blstrs_plus".to_string(), "blst".to_string()),
    ];
    let mut forced_off: Vec<String> = Vec::new();
    // The pin set: the main crate's own code still names `blstrs_plus` under a
    // cfg that severing the manifest edge alone would leave true.
    let deps_to_keep: HashSet<String> = ["blstrs_plus".to_string()].into_iter().collect();

    let skipped = should_skip_dep(
        "blstrs_plus:0.0.0",
        &mut exchange,
        &mut deps_and_features,
        // `disable_default = false`: `default` stays active, so it is read at
        // build time and has to be the thing the edit reaches.
        &[],
        false,
        false,
        &deps_to_keep,
        &mut forced_off,
    );

    assert!(skipped, "a dependency that is not no_std must be skipped");
    assert_eq!(
        forced_off,
        vec!["blst".to_string()],
        "the dep is pinned by active use, so the enabling feature must be forced off \
         instead of leaving it on with an emptied edge"
    );
    assert_eq!(
        fixture.feature_values("default"),
        Some(Vec::new()),
        "`blst` must be cut out of `default` — the feature that actually carries it \
         into the build — or cargo re-enables it regardless of `forced_off`"
    );
    assert_eq!(
        fixture.feature_values("blst"),
        Some(vec!["blstrs_plus".to_string()]),
        "`blst`'s own array is not the thing that needed cutting; it is untouched"
    );
    assert!(
        exchange
            .telemetry
            .optional_deps_disabled
            .contains(&"blstrs_plus".to_string()),
        "the sever has to be recorded like any other, pinned or not"
    );
}

/// The other shape the pinned branch has to handle: `blst` reaches the build
/// only through the command line (the caller's `--features blst`, matching
/// kitoken's `multiversion` — see `should_skip_dep`'s own comment), not
/// through any manifest feature that is still active. There is no array
/// entry anywhere to cut, so `forced_off` — dropping it from the command
/// line — has to be the whole edit; asserting an empty `severed` here isn't
/// a weaker check, it is the documented case where a cut would be a no-op.
#[test]
fn a_dep_pinned_only_via_the_command_line_has_nothing_left_to_cut_in_the_manifest() {
    let slug = "ki2_sever_pinned_cli_only";
    let fixture = Fixture::new(
        slug,
        "[package]\nname = \"ki2_sever_pinned_cli_only\"\nversion = \"0.0.0\"\n\n\
         [dependencies.blstrs_plus]\nversion = \"0.8\"\noptional = true\n\n\
         [features]\ndefault = []\nblst = [\"blstrs_plus\"]\n",
    )
    .with_std_only_dep("blstrs_plus");

    let mut exchange = exchange_for(
        slug,
        "blstrs_plus",
        vec![(
            "blst".to_string(),
            vec![("blstrs_plus".to_string(), "blstrs_plus".to_string())],
        )],
    );
    let mut deps_and_features: TupleVec = vec![("blstrs_plus".to_string(), "blst".to_string())];
    let mut forced_off: Vec<String> = Vec::new();
    let deps_to_keep: HashSet<String> = ["blstrs_plus".to_string()].into_iter().collect();

    // `blst` arrives via `enable_features` directly (the command line), and
    // `disable_default = true` keeps `default` (empty anyway here) out of it.
    let skipped = should_skip_dep(
        "blstrs_plus:0.0.0",
        &mut exchange,
        &mut deps_and_features,
        &["blst".to_string()],
        true,
        false,
        &deps_to_keep,
        &mut forced_off,
    );

    assert!(skipped, "a dependency that is not no_std must be skipped");
    assert_eq!(
        forced_off,
        vec!["blst".to_string()],
        "dropping it from the command line is the whole edit here"
    );
    assert_eq!(
        fixture.feature_values("blst"),
        Some(vec!["blstrs_plus".to_string()]),
        "nothing in the manifest enables `blst` from an active feature, so there is \
         nothing for `remove_feats_enabling_dep` to find and cut — correctly a no-op"
    );
}

/// Control: same shape, empty pin set. `should_skip_dep` has no reason to
/// believe the code still uses the dependency, so it only cuts the manifest
/// edge and leaves `blst` on — this is the pre-R31-2 behavior, kept here to
/// show the pinned case above is doing something different, not just what
/// severing always does.
#[test]
fn the_same_shape_without_a_pin_only_severs_the_edge_and_leaves_the_feature_on() {
    let slug = "ki2_sever_unpinned_control";
    let fixture = Fixture::new(
        slug,
        "[package]\nname = \"ki2_sever_unpinned_control\"\nversion = \"0.0.0\"\n\n\
         [dependencies.blstrs_plus]\nversion = \"0.8\"\noptional = true\n\n\
         [features]\ndefault = [\"blst\"]\nblst = [\"blstrs_plus\"]\n",
    )
    .with_std_only_dep("blstrs_plus");

    let mut exchange = exchange_for(
        slug,
        "blstrs_plus",
        vec![(
            "blst".to_string(),
            vec![("blstrs_plus".to_string(), "blstrs_plus".to_string())],
        )],
    );
    let mut deps_and_features: TupleVec = vec![("blstrs_plus".to_string(), "blst".to_string())];
    let mut forced_off: Vec<String> = Vec::new();

    let skipped = should_skip_dep(
        "blstrs_plus:0.0.0",
        &mut exchange,
        &mut deps_and_features,
        &["blst".to_string()],
        true,
        false,
        &HashSet::new(),
        &mut forced_off,
    );

    assert!(skipped, "a dependency that is not no_std must be skipped");
    assert!(
        forced_off.is_empty(),
        "nothing pins this dependency, so nothing should be forced off: {forced_off:?}"
    );
    assert_eq!(
        fixture.feature_values("blst"),
        Some(Vec::new()),
        "the edge is still severed"
    );
}

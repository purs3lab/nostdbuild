#![feature(rustc_private)]

//! R31-5: the third writer that strips a dependency-edge feature.
//!
//! `parser::move_unnecessary_dep_feats` edits the **main** crate's `[features]`
//! table: for each `<dep>/<feat>` a selected feature forwards, it asked *did the
//! dependency's own solve enable `<feat>`?* and deleted the value when the answer
//! was no. That answer is a don't-care, not a refusal — the distinction F4/T4(a)
//! drew for `finalize_dep_crate`'s two removal sites, never applied to this one.
//!
//! mtxgroup 0.1.1 is the measured shape. It writes
//!
//! ```text
//! #[cfg(any(not(any(feature = "std", feature = "spin")),
//!           all(feature = "std", feature = "spin")))]
//! compile_error!("Exactly one of the features `std` and `spin` must be enabled");
//! ```
//!
//! and `spin = ["spin/mutex", "spin/spin_mutex"]`. spin's own solve answers
//! `enable: []` — spin compiles fine without either, and it is *mtxgroup* that
//! needs `spin::Mutex` — so both values were deleted and the feature, whose array
//! was now empty of anything this dep wanted, was dropped from the command line
//! too. The crate then shipped `--no-default-features`, the one configuration its
//! `compile_error!` names. objectpool 0.1.0 is the same sentence with
//! `alloc = ["crossbeam-queue/alloc"]`.
//!
//! The test is now membership of the dependency's forbidden set: its `removable`
//! features (entailed-false where the analysis ran) closed over its own
//! `[features]` table.

use std::collections::HashSet;
use std::fs;
use std::path::PathBuf;

use nostd::parser::{features_that_must_be_off, move_unnecessary_dep_feats};
use nostd::{Telemetry, consts};

/// A crate directory under `consts::DOWNLOAD_PATH`, which is where
/// `parser::determine_manifest_file` looks for the main crate's manifest.
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
        fs::read_to_string(self.dir.join("Cargo.toml"))
            .expect("fixture manifest reread")
            .parse()
            .expect("fixture manifest parses")
    }

    fn values(&self, feat: &str) -> Vec<String> {
        self.manifest()
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

}

impl Drop for Fixture {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.dir);
    }
}

/// mtxgroup's manifest, trimmed to what this pass reads.
const MTXGROUP: &str = r#"
[package]
name = "mtxgroup"
version = "0.0.0"

[dependencies.spin]
version = "0.9.9"
optional = true

[features]
default = ["std"]
std = []
spin = ["spin/mutex", "spin/spin_mutex"]
"#;

fn strip(
    fixture: &Fixture,
    fixed: &mut Vec<String>,
    flexible: &mut Vec<String>,
    dep: &str,
    dep_enable: &[String],
    forbidden: &HashSet<String>,
) {
    move_unnecessary_dep_feats(
        &fixture.name_with_version,
        fixed,
        flexible,
        dep,
        dep_enable,
        &mut Telemetry::default(),
        true,
        &HashSet::new(),
        forbidden,
    );
}

fn set(items: &[&str]) -> HashSet<String> {
    items.iter().map(|s| s.to_string()).collect()
}

/// The regression. spin's solve wants nothing, spin forbids nothing but `std`,
/// and `spin = ["spin/mutex", "spin/spin_mutex"]` is what mtxgroup is built on.
#[test]
fn a_dep_feature_the_solve_merely_did_not_ask_for_survives() {
    let fixture = Fixture::new("dep-edge-strip-dont-care", MTXGROUP);

    let mut fixed = vec!["spin".to_string()];
    let mut flexible = vec!["spin".to_string()];
    strip(
        &fixture,
        &mut fixed,
        &mut flexible,
        "spin:0.9.9",
        &[],
        &set(&["std"]),
    );

    assert_eq!(
        fixture.values("spin"),
        vec![
            "spin/mutex".to_string(),
            "spin/spin_mutex".to_string()
        ],
        "the values the feature exists for were deleted"
    );
    assert_eq!(
        flexible,
        vec!["spin".to_string()],
        "the feature its own compile_error! demands was dropped from the selection"
    );
    // The pass writes both orphan lists unconditionally, empty when it moved
    // nothing; what matters is that neither has anything in it.
    assert!(
        fixture.values(consts::DEP_UNNECESSARY_FEATURES).is_empty(),
        "nothing was refuted, so nothing should have been parked"
    );
    assert!(
        fixture.values(consts::CUSTOM_FEATURES_ENABLED).is_empty(),
        "the feature stayed, so nothing needed re-parking"
    );
}

/// The other side of the same test: a value the dependency genuinely cannot have
/// still goes, and is parked where the manifest records it.
#[test]
fn a_dep_feature_that_must_be_off_is_still_removed() {
    let fixture = Fixture::new(
        "dep-edge-strip-forbidden",
        r#"
[package]
name = "forbidden"
version = "0.0.0"

[dependencies.spin]
version = "0.9.9"

[features]
loud = ["spin/std", "spin/mutex"]
"#,
    );

    let mut fixed = vec!["loud".to_string()];
    let mut flexible: Vec<String> = Vec::new();
    strip(
        &fixture,
        &mut fixed,
        &mut flexible,
        "spin:0.9.9",
        &[],
        &set(&["std"]),
    );

    assert_eq!(
        fixture.values("loud"),
        vec!["spin/mutex".to_string()],
        "only the forbidden value should go"
    );
    assert_eq!(
        fixture.values(consts::DEP_UNNECESSARY_FEATURES),
        vec!["spin/std".to_string()],
    );
}

/// A *flexible* feature that forwards a forbidden value cannot be enabled at all,
/// so it leaves the selection — and the values the dependency did ask for are
/// re-parked under the list the build enables rather than lost with it.
#[test]
fn a_flexible_feature_forwarding_a_forbidden_value_leaves_the_selection() {
    let fixture = Fixture::new(
        "dep-edge-strip-flexible",
        r#"
[package]
name = "flexible"
version = "0.0.0"

[dependencies.spin]
version = "0.9.9"

[features]
loud = ["spin/std", "spin/mutex"]
quiet = ["spin/mutex"]
"#,
    );

    let mut fixed: Vec<String> = Vec::new();
    let mut flexible = vec!["loud".to_string(), "quiet".to_string()];
    strip(
        &fixture,
        &mut fixed,
        &mut flexible,
        "spin:0.9.9",
        &["mutex".to_string()],
        &set(&["std"]),
    );

    assert_eq!(
        flexible,
        vec!["quiet".to_string()],
        "`quiet` forwards nothing forbidden and must stay"
    );
    assert_eq!(
        fixture.values(consts::CUSTOM_FEATURES_ENABLED),
        vec!["spin/mutex".to_string()],
        "the value the dependency asked for went down with `loud` and was not re-parked"
    );
}

/// A dependency `finalize_dep_crate` never reached — a DB-cache miss on the
/// download path, an analysis that bailed — forbids nothing, and this pass is
/// then not entitled to touch the author's manifest.
#[test]
fn an_unanalysed_dependency_costs_the_manifest_nothing() {
    let fixture = Fixture::new("dep-edge-strip-unanalysed", MTXGROUP);

    let mut fixed = vec!["spin".to_string()];
    let mut flexible = vec!["spin".to_string()];
    strip(
        &fixture,
        &mut fixed,
        &mut flexible,
        "spin:0.9.9",
        &[],
        &HashSet::new(),
    );

    assert_eq!(
        fixture.values("spin"),
        vec![
            "spin/mutex".to_string(),
            "spin/spin_mutex".to_string()
        ],
    );
    assert_eq!(flexible, vec!["spin".to_string()]);
}

/// The forbidden set is reachability, not membership: `blocking = ["std"]` is a
/// way of spelling `std`, and forwarding it turns std on just as surely. Same
/// rule `reaches_forbidden_feature` applies to the main crate's own condition.
#[test]
fn a_feature_that_reaches_a_forbidden_one_is_forbidden_too() {
    let features = vec![
        (
            "blocking".to_string(),
            vec![("std".to_string(), "std".to_string())],
        ),
        ("std".to_string(), vec![]),
        (
            "mutex".to_string(),
            vec![("spin_mutex".to_string(), "spin_mutex".to_string())],
        ),
        ("spin_mutex".to_string(), vec![]),
    ];

    let forbidden = features_that_must_be_off(&features, &["std".to_string()]);

    assert!(forbidden.contains("std"));
    assert!(
        forbidden.contains("blocking"),
        "a feature whose subtree turns std on is std"
    );
    assert!(!forbidden.contains("mutex"));
    assert!(!forbidden.contains("spin_mutex"));
}

/// Nothing refuted, nothing forbidden — the shape of every dependency whose
/// no_std-ness constrains none of its features, which is most of them.
#[test]
fn a_dependency_that_refuses_nothing_forbids_nothing() {
    let features = vec![
        ("mutex".to_string(), vec![]),
        (
            "spin_mutex".to_string(),
            vec![("mutex".to_string(), "mutex".to_string())],
        ),
    ];

    assert!(features_that_must_be_off(&features, &[]).is_empty());
}

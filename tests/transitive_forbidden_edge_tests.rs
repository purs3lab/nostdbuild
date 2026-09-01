#![feature(rustc_private)]

//! R34-20: a direct dep edge left at `default = ["std"]` because the crate at
//! the far end of it is clean.
//!
//! `solver::final_feature_list_dep` decided `default-features = false` from the
//! dependency's own `disable` list, and for the crates this repairs that list is
//! empty. sha3 0.10.8 is `#![no_std]`, uses no std and needs no feature to be
//! no_std — so ml-dsa 0.0.4's `sha3` edge kept its defaults, and `sha3/std →
//! digest/std → crypto-common/std` reached a `#[cfg(feature = "std")] extern
//! crate std` two hops down. `E0463` on every target, from a manifest whose
//! `num-traits` edge on the same run *did* get `default-features = false`,
//! because num-traits' own condition names `std` and sha3's names nothing.
//!
//! The fixtures are that shape at three crates: a clean middle dependency whose
//! `default` forwards a feature that a leaf below it links std with.

use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;

use nostd::consts::DOWNLOAD_PATH;
use nostd::parser::transitive_forbidden_dep_features;
use nostd::solver::final_feature_list_dep;
use nostd::{CrateInfo, Telemetry};

/// Each test gets its own root, because the fixtures live at fixed paths under
/// `DOWNLOAD_PATH` and the harness runs tests in one process, in parallel. The
/// dependency names can be shared: they live under `<root>-0.1.0_deps/`.
const MID: &str = "r34_20_mid";
const LEAF: &str = "r34_20_leaf";

/// `<DOWNLOAD_PATH>/<root>-0.1.0/` for the main crate and
/// `<DOWNLOAD_PATH>/<root>-0.1.0_deps/<name>-0.1.0/` for everything below it —
/// where `determine_manifest_file` and `find_sibling_crate_dir` look.
struct Tree {
    root_dir: PathBuf,
    deps_dir: PathBuf,
}

impl Tree {
    fn new(slug: &str) -> Self {
        let root_dir = PathBuf::from(DOWNLOAD_PATH).join(format!("{slug}-0.1.0"));
        let deps_dir = PathBuf::from(DOWNLOAD_PATH).join(format!("{slug}-0.1.0_deps"));
        let _ = fs::remove_dir_all(&root_dir);
        let _ = fs::remove_dir_all(&deps_dir);
        fs::create_dir_all(&deps_dir).unwrap();
        Self { root_dir, deps_dir }
    }

    fn write(&self, dir: &PathBuf, name: &str, manifest_body: &str, lib_rs: &str) {
        fs::create_dir_all(dir.join("src")).unwrap();
        fs::write(dir.join("src/lib.rs"), lib_rs).unwrap();
        fs::write(
            dir.join("Cargo.toml"),
            format!(
                r#"[package]
name = "{name}"
version = "0.1.0"
edition = "2018"

[lib]
name = "{name}"
path = "src/lib.rs"

{manifest_body}"#
            ),
        )
        .unwrap();
    }

    fn root(&self, name: &str, manifest_body: &str, lib_rs: &str) {
        let dir = self.root_dir.clone();
        self.write(&dir, name, manifest_body, lib_rs);
    }

    fn dep(&self, name: &str, manifest_body: &str, lib_rs: &str) {
        let dir = self.deps_dir.join(format!("{name}-0.1.0"));
        self.write(&dir, name, manifest_body, lib_rs);
    }
}

impl Drop for Tree {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.root_dir);
        let _ = fs::remove_dir_all(&self.deps_dir);
    }
}

/// The middle crate: clean, `#![no_std]`, nothing of its own to disable, and a
/// `default` that forwards the leaf's std feature. sha3's shape.
const MID_MANIFEST: &str = r#"[dependencies]
r34_20_leaf = { version = "0.1.0" }

[features]
default = ["std"]
std = ["r34_20_leaf/std"]
"#;

const MID_LIB: &str = "#![no_std]\npub fn mid() -> u8 { 1 }\n";

/// The leaf: `#![no_std]` and still links std under a feature, which is the
/// case a crate-root attribute alone does not catch. crypto-common's shape.
const LEAF_MANIFEST: &str = r#"[features]
std = []
"#;

const LEAF_LIB: &str = r#"#![no_std]

#[cfg(feature = "std")]
extern crate std;

pub fn leaf() -> u8 { 2 }
"#;

const ROOT_MANIFEST: &str = r#"[dependencies]
r34_20_mid = { version = "0.1.0" }
"#;

fn walk_mid(root: &str) -> (Vec<String>, Telemetry) {
    let ctx = z3::Context::new(&z3::Config::new());
    let mut telemetry = Telemetry::default();
    let mut out: Vec<String> = transitive_forbidden_dep_features(
        &format!("{MID}:0.1.0"),
        &format!("{root}:0.1.0"),
        &ctx,
        &mut telemetry,
    )
    .into_iter()
    .collect();
    out.sort();
    (out, telemetry)
}

fn forbidden_for_mid(root: &str) -> Vec<String> {
    walk_mid(root).0
}

#[test]
fn a_leaf_that_links_std_forbids_the_feature_its_parent_forwards() {
    const ROOT: &str = "r34_20_carried_up";
    let tree = Tree::new(ROOT);
    tree.root(ROOT, ROOT_MANIFEST, "#![no_std]\n");
    tree.dep(MID, MID_MANIFEST, MID_LIB);
    tree.dep(LEAF, LEAF_MANIFEST, LEAF_LIB);

    // `std` is a feature of the *middle* crate, named by translating the leaf's
    // own verdict up one edge. Nothing in the middle crate's source says it.
    //
    // `default` comes with it and is kept deliberately: the hop up is closed
    // over the middle crate's own table, and `default = ["std"]` turns `std` on
    // just as surely. It has to survive for the shape that has no named feature
    // in between — `default = ["r34_20_leaf/std"]` — where dropping it as "not a
    // real feature" would leave the set empty and the edge unrepaired.
    assert_eq!(
        forbidden_for_mid(ROOT),
        vec!["default".to_string(), "std".to_string()]
    );
}

#[test]
fn a_leaf_with_no_std_gate_forbids_nothing() {
    const ROOT: &str = "r34_20_clean_leaf";
    let tree = Tree::new(ROOT);
    tree.root(ROOT, ROOT_MANIFEST, "#![no_std]\n");
    tree.dep(MID, MID_MANIFEST, MID_LIB);
    tree.dep(
        LEAF,
        LEAF_MANIFEST,
        "#![no_std]\npub fn leaf() -> u8 { 2 }\n",
    );

    assert!(forbidden_for_mid(ROOT).is_empty());
}

#[test]
fn a_feature_no_ancestor_can_turn_off_is_left_alone() {
    // The leaf links std under `std`, and the middle crate hands it over on a
    // non-optional edge — so no edit to the middle edge can turn it off. That is
    // `DEP_TREE_TRANSITIVE_STD`'s boundary and this must not claim it.
    const ROOT: &str = "r34_20_unreachable";
    let tree = Tree::new(ROOT);
    tree.root(ROOT, ROOT_MANIFEST, "#![no_std]\n");
    tree.dep(
        MID,
        r#"[dependencies]
r34_20_leaf = { version = "0.1.0", features = ["std"] }

[features]
default = []
"#,
        MID_LIB,
    );
    tree.dep(LEAF, LEAF_MANIFEST, LEAF_LIB);

    assert!(forbidden_for_mid(ROOT).is_empty());
}

#[test]
fn an_edge_that_hands_std_over_non_optionally_is_reported() {
    // The same tree, read for what it says about the ecosystem rather than about
    // this build: `features = ["std"]` on the middle crate's own edge is a line
    // in a published manifest, and it is the only thing that could change.
    const ROOT: &str = "r34_20_reported";
    let tree = Tree::new(ROOT);
    tree.root(ROOT, ROOT_MANIFEST, "#![no_std]\n");
    tree.dep(
        MID,
        r#"[dependencies]
r34_20_leaf = { version = "0.1.0", features = ["std"] }

[features]
default = []
"#,
        MID_LIB,
    );
    tree.dep(LEAF, LEAF_MANIFEST, LEAF_LIB);

    let (forbidden, telemetry) = walk_mid(ROOT);
    assert!(forbidden.is_empty());
    assert_eq!(telemetry.unrepairable_std_edges.len(), 1);
    let edge = &telemetry.unrepairable_std_edges[0];
    assert_eq!(edge.crate_name, format!("{LEAF}:0.1.0"));
    assert_eq!(edge.feature, "std");
    assert_eq!(edge.supplier, format!("{MID}:0.1.0"));
    assert_eq!(edge.dep_key, LEAF);
    assert_eq!(edge.via, "features");
    assert_eq!(edge.links_std, format!("{LEAF}:0.1.0"));
    assert_eq!(edge.std_feature, "std");
    assert_eq!(edge.direct_dep, format!("{MID}:0.1.0"));
}

#[test]
fn a_repairable_edge_is_not_reported() {
    // The other direction, and the one that fails if the record is written on
    // every chain that ends: the original fixture reaches `r34_20_leaf/std`
    // through the middle crate's own table, so there is nothing upstream to
    // report — the tool repairs it here.
    const ROOT: &str = "r34_20_not_reported";
    let tree = Tree::new(ROOT);
    tree.root(ROOT, ROOT_MANIFEST, "#![no_std]\n");
    tree.dep(MID, MID_MANIFEST, MID_LIB);
    tree.dep(LEAF, LEAF_MANIFEST, LEAF_LIB);

    let (forbidden, telemetry) = walk_mid(ROOT);
    assert_eq!(forbidden, vec!["default".to_string(), "std".to_string()]);
    assert!(telemetry.unrepairable_std_edges.is_empty());
}

/// The `generic-array 0.14.9` shape: the leaf is reached through an **optional**
/// edge, and R34-20's residual is that the walk did not look at it at all.
const OPTIONAL_MID_MANIFEST: &str = r#"[dependencies]
r34_20_leaf = { version = "0.1.0", optional = true, default-features = false }

[features]
default = []
"#;

#[test]
fn an_optional_edge_that_cannot_reach_the_leafs_std_feature_forbids_nothing() {
    // generic-array declares `zeroize = { optional = true, default-features =
    // false }` and forwards nothing to it, so no feature of generic-array turns
    // `zeroize/std` on and the honest answer is that there is nothing to forbid.
    // The old `parse_top_level_externs` path answers `¬zeroize` here, off the
    // `#[cfg(feature = "zeroize")]` on the extern crate rather than off the hop
    // where std is actually linked.
    //
    // This is the half that fails if walking an optional edge is read as
    // "forbid whatever activates it".
    const ROOT: &str = "r34_20_optional_unreachable";
    let tree = Tree::new(ROOT);
    tree.root(ROOT, ROOT_MANIFEST, "#![no_std]\n");
    tree.dep(MID, OPTIONAL_MID_MANIFEST, MID_LIB);
    tree.dep(LEAF, LEAF_MANIFEST, LEAF_LIB);

    let (forbidden, telemetry) = walk_mid(ROOT);
    assert!(forbidden.is_empty());
    // Nor is an optional edge an upstream problem: it is off unless asked for.
    assert!(telemetry.unrepairable_std_edges.is_empty());
}

#[test]
fn an_optional_edge_whose_defaults_link_std_forbids_what_activates_it() {
    // Same edge with its defaults left on, against a leaf whose `default` list
    // reaches `std`. Now linking the dependency at all links std, so the feature
    // that must go off is the one that puts it in the graph — cargo's implicit
    // `r34_20_leaf` feature of the middle crate.
    const ROOT: &str = "r34_20_optional_defaults";
    let tree = Tree::new(ROOT);
    tree.root(ROOT, ROOT_MANIFEST, "#![no_std]\n");
    tree.dep(
        MID,
        r#"[dependencies]
r34_20_leaf = { version = "0.1.0", optional = true }

[features]
default = []
"#,
        MID_LIB,
    );
    tree.dep(
        LEAF,
        r#"[features]
default = ["std"]
std = []
"#,
        LEAF_LIB,
    );

    let (forbidden, telemetry) = walk_mid(ROOT);
    assert_eq!(forbidden, vec![LEAF.to_string()]);
    assert!(telemetry.unrepairable_std_edges.is_empty());
}

#[test]
fn a_dep_activation_names_the_feature_that_activates_it() {
    // `dep:` spelling: the implicit feature is suppressed, so the name that has
    // to come off is the one the author wrote — and reporting the implicit name
    // here would be a feature the crate does not have (R34-2/R34-14's shape).
    const ROOT: &str = "r34_20_optional_dep_colon";
    let tree = Tree::new(ROOT);
    tree.root(ROOT, ROOT_MANIFEST, "#![no_std]\n");
    tree.dep(
        MID,
        r#"[dependencies]
r34_20_leaf = { version = "0.1.0", optional = true }

[features]
default = []
with_leaf = ["dep:r34_20_leaf"]
"#,
        MID_LIB,
    );
    tree.dep(
        LEAF,
        r#"[features]
default = ["std"]
std = []
"#,
        LEAF_LIB,
    );

    assert_eq!(forbidden_for_mid(ROOT), vec!["with_leaf".to_string()]);
}

/// A `CrateInfo` pair shaped like the root→middle edge: the root declares the
/// dependency at its defaults, and the dependency's own `default` carries the
/// feature the leaf forbids.
fn root_and_mid_info(root: &str) -> CrateInfo {
    let mid = CrateInfo {
        name: MID.to_string(),
        version: "0.1.0".to_string(),
        deps_and_features: Vec::new(),
        features: vec![
            (
                "default".to_string(),
                vec![("std".to_string(), "std".to_string())],
            ),
            (
                "std".to_string(),
                vec![(LEAF.to_string(), "std".to_string())],
            ),
        ],
        default_features: true,
        optional: false,
        git: None,
    };
    CrateInfo {
        name: root.to_string(),
        version: "0.1.0".to_string(),
        deps_and_features: vec![(mid, Vec::new())],
        features: Vec::new(),
        default_features: true,
        optional: false,
        git: None,
    }
}

fn update_default_config_for(root: &str, removable: &[String]) -> bool {
    let tree = Tree::new(root);
    tree.root(root, ROOT_MANIFEST, "#![no_std]\n");
    tree.dep(MID, MID_MANIFEST, MID_LIB);
    tree.dep(LEAF, LEAF_MANIFEST, LEAF_LIB);

    let mut telemetry = Telemetry::default();
    let (_, update_default_config) = final_feature_list_dep(
        &root_and_mid_info(root),
        MID,
        &[],
        // The dependency's own solve says nothing — this is the whole point.
        &[],
        removable,
        &[],
        &mut telemetry,
    );
    let _ = HashMap::<String, String>::new();
    update_default_config
}

#[test]
fn a_verdict_from_below_takes_the_edges_defaults_off() {
    // With the leaf's verdict carried up, `std` is in the dependency's `default`
    // list and the edge has to lose its defaults.
    assert!(update_default_config_for(
        "r34_20_defaults_off",
        &["std".to_string()]
    ));
}

#[test]
fn without_a_verdict_the_edge_is_left_as_the_author_wrote_it() {
    // The other direction: nothing proved, nothing changed. A test that passes
    // either way is not a guard, and this is the half that fails if the new
    // condition is written as an unconditional `true`.
    assert!(!update_default_config_for("r34_20_defaults_kept", &[]));
}

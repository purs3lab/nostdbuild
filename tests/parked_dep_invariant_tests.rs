//! The R34-1 invariant, tested on its own.
//!
//! `common::assert_no_parked_dep_under_a_live_feature` runs at the end of every
//! `main_tests` golden, where a passing run proves nothing about whether the check
//! can fail. These two tests pin it down directly, on bevy_input-0.16.0's recorded
//! state: the published feature table it was fetched with, the manifest the tool
//! emitted, and the argv it emitted alongside.

use std::fs;
use std::path::PathBuf;

mod common;

/// bevy_input-0.16.0 as published: `smol_str` both activates the optional dep and
/// forwards to `bevy_reflect`, and `default` turns it on.
const PUBLISHED: &str = r#"
[package]
name = "bevy_input"
version = "0.16.0"

[dependencies]
smol_str = { version = "0.2", optional = true }

[features]
default = ["std", "bevy_reflect", "smol_str"]
smol_str = ["dep:smol_str", "bevy_reflect/smol_str"]
std = []
"#;

/// What the tool emitted before the fix: `dep:smol_str` deleted out of the
/// `smol_str` feature and parked, with the feature itself left standing.
const EMITTED_PARKED: &str = r#"
[package]
name = "bevy_input"
version = "0.16.0"

[features]
custom_default_features = ["dep:bevy_reflect", "dep:smol_str"]
default = ["std", "bevy_reflect", "smol_str"]
smol_str = ["bevy_reflect/smol_str"]
std = []
"#;

/// What it emits after the fix: the dep stays linked, nothing is parked.
const EMITTED_PINNED: &str = r#"
[package]
name = "bevy_input"
version = "0.16.0"

[features]
custom_default_features = []
default = ["std", "bevy_reflect", "smol_str"]
smol_str = ["dep:smol_str", "bevy_reflect/smol_str"]
std = []
"#;

fn stage(tag: &str, emitted: &str) -> PathBuf {
    let root = std::env::temp_dir().join(format!("nostd_parked_invariant_{tag}"));
    let dir = root.join("fixture-0.0.0");
    let _ = fs::remove_dir_all(&root);
    fs::create_dir_all(&dir).expect("staging dir");
    fs::write(dir.join("Cargo.toml.pristine"), PUBLISHED).expect("published");
    fs::write(dir.join("Cargo.toml"), emitted).expect("emitted");
    root
}

fn args(features: &str) -> Vec<String> {
    ["build", "--no-default-features", "--features", features]
        .iter()
        .map(|s| s.to_string())
        .collect()
}

/// The failing direction. `smol_str` is on the command line and `dep:smol_str` is
/// parked, so the import the feature gates has nothing to resolve against. If this
/// stops panicking the guard has gone vacuous.
#[test]
#[should_panic(expected = "FEATURE_ON_DEP_STRIPPED")]
fn a_parked_dep_under_a_live_feature_is_caught() {
    let root = stage("caught", EMITTED_PARKED);
    common::assert_no_parked_dep_under_a_live_feature(
        root.to_str().unwrap(),
        "custom_default_features",
        "fixture",
        "0.0.0",
        &args("libm,smol_str"),
    );
}

/// The passing direction, and the reason the check is about the *pair* and not
/// about parking as such: with the carrier feature off, the code it gates is not
/// compiled and unlinking the dependency is exactly what should happen. This is
/// `watchface`'s case, which must keep stripping.
#[test]
fn a_parked_dep_under_a_dead_feature_is_fine() {
    let root = stage("dead", EMITTED_PARKED);
    common::assert_no_parked_dep_under_a_live_feature(
        root.to_str().unwrap(),
        "custom_default_features",
        "fixture",
        "0.0.0",
        &args("libm"),
    );
}

/// And the post-fix state passes on the same argv that used to fail.
#[test]
fn nothing_parked_passes_on_the_argv_that_used_to_fail() {
    let root = stage("pinned", EMITTED_PINNED);
    common::assert_no_parked_dep_under_a_live_feature(
        root.to_str().unwrap(),
        "custom_default_features",
        "fixture",
        "0.0.0",
        &args("libm,smol_str"),
    );
}

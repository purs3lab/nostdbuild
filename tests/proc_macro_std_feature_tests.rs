#![feature(rustc_private)]

//! O-9: a proc-macro dependency's `std` feature is the *consumer's* std.
//!
//! Proc-macro crates are exempt from the no_std walk because they are compiled for
//! the host and run there — their own `use std::…` says nothing about the crate being
//! analysed. Their `[features]` are a different matter: they select which tokens the
//! macro injects into the consumer. displaydoc 0.2.6 (`default = ["std"]`) emits
//!
//! ```ignore
//! extern crate std;
//! impl PathToDisplayDoc for std::path::Path { … }
//! ```
//!
//! into every `#[derive(Display)]` site when its `std` feature is on, so a `#![no_std]`
//! consumer got unguarded std at a span it never wrote — and could not build for a
//! bare-metal target either (`E0463 can't find crate for std`). Skipping the dependency
//! everywhere meant nothing ever turned that default off.
//!
//! Only a default feature *named* `std` is parked; everything else the macro's author
//! set stays as it is.

use nostd::consts;
use nostd::parser::park_proc_macro_std_in_manifest;

fn toml_of(s: &str) -> toml::Value {
    toml::from_str(s).expect("fixture manifest parses")
}

/// displaydoc 0.2.6.
const DISPLAYDOC: &str = r#"
[package]
name = "displaydoc"
version = "0.2.6"

[lib]
proc-macro = true

[features]
default = ["std"]
std = []
"#;

/// dfu-core 0.7.0's edge: a plain, non-optional dependency with default features on.
const CONSUMER: &str = r#"
[package]
name = "dfu-core"
version = "0.7.0"

[dependencies.displaydoc]
version = "0.2"

[features]
std = ["dep:thiserror"]
"#;

fn edge<'a>(manifest: &'a toml::Value, key: &str) -> &'a toml::Value {
    manifest
        .get("dependencies")
        .and_then(|d| d.get(key))
        .unwrap_or_else(|| panic!("no dependency edge {key}"))
}

fn edge_features(manifest: &toml::Value, key: &str) -> Vec<String> {
    edge(manifest, key)
        .get("features")
        .and_then(|v| v.as_array())
        .map(|arr| {
            arr.iter()
                .map(|v| v.as_str().unwrap().to_string())
                .collect()
        })
        .unwrap_or_default()
}

fn custom_disabled(manifest: &toml::Value) -> Vec<String> {
    manifest
        .get("features")
        .and_then(|f| f.get(consts::CUSTOM_FEATURES_DISABLED))
        .and_then(|v| v.as_array())
        .map(|arr| {
            arr.iter()
                .map(|v| v.as_str().unwrap().to_string())
                .collect()
        })
        .unwrap_or_default()
}

#[test]
fn a_proc_macros_default_std_is_parked_on_the_edge() {
    let mut main = toml_of(CONSUMER);
    let changed = park_proc_macro_std_in_manifest(&mut main, &toml_of(DISPLAYDOC), "displaydoc");

    assert!(changed, "the displaydoc edge should have been rewritten");
    assert_eq!(
        edge(&main, "displaydoc").get("default-features"),
        Some(&toml::Value::Boolean(false)),
        "default-features must be off, or the macro keeps injecting `extern crate std`"
    );
    assert_eq!(
        custom_disabled(&main),
        vec!["displaydoc/std".to_string()],
        "the parked default belongs in the std-mode restore list"
    );
}

/// The other defaults are the macro author's, not ours: they go back on the edge, so
/// only `std` is actually removed.
#[test]
fn the_macros_other_defaults_are_re_declared_on_the_edge() {
    const MACRO_WITH_MORE: &str = r#"
[package]
name = "displaydoc"
version = "0.2.6"

[lib]
proc-macro = true

[features]
default = ["std", "fancy", "docs"]
std = []
fancy = []
docs = []
"#;
    let mut main = toml_of(CONSUMER);
    assert!(park_proc_macro_std_in_manifest(
        &mut main,
        &toml_of(MACRO_WITH_MORE),
        "displaydoc"
    ));

    let mut feats = edge_features(&main, "displaydoc");
    feats.sort();
    assert_eq!(feats, vec!["docs".to_string(), "fancy".to_string()]);
    assert_eq!(custom_disabled(&main), vec!["displaydoc/std".to_string()]);
}

/// A renamed dependency is keyed by the rename, and the `dep/std` value written into
/// the restore list has to use that same key or cargo rejects the manifest.
#[test]
fn a_renamed_proc_macro_is_found_through_its_package_key() {
    const RENAMED: &str = r#"
[package]
name = "consumer"
version = "0.1.0"

[dependencies.dd]
version = "0.2"
package = "displaydoc"
"#;
    let mut main = toml_of(RENAMED);
    assert!(park_proc_macro_std_in_manifest(
        &mut main,
        &toml_of(DISPLAYDOC),
        "displaydoc"
    ));

    assert_eq!(
        edge(&main, "dd").get("default-features"),
        Some(&toml::Value::Boolean(false))
    );
    assert_eq!(custom_disabled(&main), vec!["dd/std".to_string()]);
}

/// `bf-impl`'s spelling. The name test is a measurement, not a guess: over all 1701
/// distinct proc-macro crates in the corpus, a cfg-gated std-mentioning region whose
/// gate names one of the macro's own defaults is spelled `std` at 343 sites and
/// `use_std` at exactly one. Both are parked.
#[test]
fn a_default_named_use_std_is_parked_too() {
    const USE_STD: &str = r#"
[package]
name = "bf-impl"
version = "0.1.0"

[lib]
proc-macro = true

[features]
default = ["use_std"]
use_std = []
"#;
    const CONSUMES_BF: &str = r#"
[package]
name = "consumer"
version = "0.1.0"

[dependencies.bf-impl]
version = "0.1"
"#;
    let mut main = toml_of(CONSUMES_BF);
    assert!(park_proc_macro_std_in_manifest(
        &mut main,
        &toml_of(USE_STD),
        "bf-impl"
    ));
    assert_eq!(
        edge(&main, "bf-impl").get("default-features"),
        Some(&toml::Value::Boolean(false))
    );
    assert_eq!(custom_disabled(&main), vec!["bf-impl/use_std".to_string()]);
}

// ---------------------------------------------------------------------------
// Controls
// ---------------------------------------------------------------------------

/// `alloc` is deliberately not in the list: a no_std crate can and usually does want
/// it, and a macro emitting `alloc::…` is emitting exactly what no_std wants.
#[test]
fn a_default_named_alloc_is_left_alone() {
    const ALLOC_DEFAULT: &str = r#"
[package]
name = "some_derive"
version = "0.1.0"

[lib]
proc-macro = true

[features]
default = ["alloc"]
alloc = []
"#;
    let before = toml_of(CONSUMER);
    let mut main = before.clone();
    assert!(!park_proc_macro_std_in_manifest(
        &mut main,
        &toml_of(ALLOC_DEFAULT),
        "displaydoc"
    ));
    assert_eq!(main, before);
}

/// Most proc macros have no `std` feature at all (serde_derive, syn-based derives).
/// Nothing about them is this bug, and their edges must not move — turning defaults
/// off there is a change to the macro's own host build with no std to gain.
#[test]
fn a_proc_macro_without_a_default_std_is_left_alone() {
    const NO_STD_FEATURE: &str = r#"
[package]
name = "serde_derive"
version = "1.0.0"

[lib]
proc-macro = true

[features]
default = []
deserialize_in_place = []
"#;
    let before = toml_of(CONSUMER);
    let mut main = before.clone();
    assert!(!park_proc_macro_std_in_manifest(
        &mut main,
        &toml_of(NO_STD_FEATURE),
        "serde_derive"
    ));
    assert_eq!(main, before, "no default `std` means nothing to park");
}

/// A macro that declares `std` but does not have it on by default is already in the
/// state we want. Touching the edge would only risk its other defaults.
#[test]
fn a_std_feature_that_is_not_a_default_is_left_alone() {
    const STD_NOT_DEFAULT: &str = r#"
[package]
name = "some_derive"
version = "0.1.0"

[lib]
proc-macro = true

[features]
default = ["fancy"]
std = []
fancy = []
"#;
    let before = toml_of(CONSUMER);
    let mut main = before.clone();
    assert!(!park_proc_macro_std_in_manifest(
        &mut main,
        &toml_of(STD_NOT_DEFAULT),
        "displaydoc"
    ));
    assert_eq!(main, before);
}

/// `sp-api-proc-macro`'s shape: `std = ["blake2/std"]` is fine, but a *default* entry
/// naming another crate's feature cannot be re-declared on a dependency edge — cargo
/// refuses `multiple slashes in feature` outright. Turning defaults off would silently
/// drop it, so the edge is left alone entirely (the same choice
/// `update_main_crate_default_list` makes for an unreachable default).
#[test]
fn an_unreachable_default_leaves_the_edge_alone() {
    const UNREACHABLE: &str = r#"
[package]
name = "some-proc-macro"
version = "1.0.0"

[lib]
proc-macro = true

[features]
default = ["std", "blake2/std"]
std = []
"#;
    let before = toml_of(CONSUMER);
    let mut main = before.clone();
    assert!(!park_proc_macro_std_in_manifest(
        &mut main,
        &toml_of(UNREACHABLE),
        "displaydoc"
    ));
    assert_eq!(main, before);
}

/// A dependency written as `displaydoc = "0.2"` cannot carry `default-features`, and
/// rewriting the entry into a table is a manifest change with its own blast radius.
/// crates.io normalises published manifests to the table form, so this is the rare
/// case, and it stays untouched.
#[test]
fn a_string_valued_dependency_entry_is_left_alone() {
    const STRING_EDGE: &str = r#"
[package]
name = "consumer"
version = "0.1.0"

[dependencies]
displaydoc = "0.2"
"#;
    let before = toml_of(STRING_EDGE);
    let mut main = before.clone();
    assert!(!park_proc_macro_std_in_manifest(
        &mut main,
        &toml_of(DISPLAYDOC),
        "displaydoc"
    ));
    assert_eq!(main, before);
}

/// The macro is somewhere in the tree but not a direct dependency of this crate: there
/// is no edge here to park it on, and inventing one would add a dependency the crate
/// does not have.
#[test]
fn a_macro_with_no_edge_in_this_manifest_is_left_alone() {
    const OTHER_DEPS: &str = r#"
[package]
name = "consumer"
version = "0.1.0"

[dependencies.log]
version = "0.4"
"#;
    let before = toml_of(OTHER_DEPS);
    let mut main = before.clone();
    assert!(!park_proc_macro_std_in_manifest(
        &mut main,
        &toml_of(DISPLAYDOC),
        "displaydoc"
    ));
    assert_eq!(main, before);
}

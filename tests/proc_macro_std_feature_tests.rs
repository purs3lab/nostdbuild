#![feature(rustc_private)]

//! O-9: a proc-macro dependency's features are the *consumer's*, and which one to
//! turn off is decided by compiling, not by its name.
//!
//! Proc-macro crates are exempt from the no_std walk because they are compiled for
//! the host and run there — their own `use std::…` says nothing about the crate
//! being analysed. Their `[features]` are a different matter: they select which
//! tokens the macro injects into the consumer. displaydoc 0.2.6 (`default =
//! ["std"]`) emits
//!
//! ```ignore
//! extern crate std;
//! impl PathToDisplayDoc for std::path::Path { … }
//! ```
//!
//! into every `#[derive(Display)]` site when its `std` feature is on, so a
//! `#![no_std]` consumer got unguarded std at a span it never wrote — and could not
//! build for a bare-metal target either (`E0463 can't find crate for std`).
//!
//! The rule under test is the evidence, in two halves, both of which the name test
//! it replaced got wrong:
//!
//! * **attribution** — `PathRecord::expansion_crate` is the crate that *defines*
//!   the macro a record came out of, so a std record attributed to `displaydoc` is
//!   the compiler saying displaydoc put it there. No attribution, no parking.
//! * **the build** — a trial is kept only if the crate still compiles with the
//!   default off. Measured over the corpus, 8 of the 115 proc-macro crates with a
//!   `std`-named default use it to guard their own host code; parking it there cost
//!   bebytes 0.7.1 all 26 of its target builds on `E0433 use of unresolved module
//!   std` inside bebytes_derive, a macro that injects no std whatsoever.

use std::fs;
use std::path::Path;

use cargo_test_support::{Project, cargo_test, project};

use nostd::driver::{injected_std_records, park_injecting_proc_macros};
use nostd::parser::park_proc_macro_default_in_manifest;
use nostd::types::{FeatureRunOutput, PathContext, PathRecord, ReadableSpan};
use nostd::{ProcMacroDep, Telemetry, consts};

// ---------------------------------------------------------------------------
// The manifest surgery
// ---------------------------------------------------------------------------

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
fn the_named_default_is_parked_on_the_edge() {
    let mut main = toml_of(CONSUMER);
    let changed =
        park_proc_macro_default_in_manifest(&mut main, &toml_of(DISPLAYDOC), "displaydoc", "std");

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

/// The other defaults are the macro author's, not ours: they go back on the edge,
/// so only the one being tried is actually removed.
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
    assert!(park_proc_macro_default_in_manifest(
        &mut main,
        &toml_of(MACRO_WITH_MORE),
        "displaydoc",
        "std"
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
    assert!(park_proc_macro_default_in_manifest(
        &mut main,
        &toml_of(DISPLAYDOC),
        "displaydoc",
        "std"
    ));

    assert_eq!(
        edge(&main, "dd").get("default-features"),
        Some(&toml::Value::Boolean(false))
    );
    assert_eq!(custom_disabled(&main), vec!["dd/std".to_string()]);
}

/// Nothing about the surgery reads the feature's *name* any more. `bf-impl` spells
/// it `use_std` and one corpus macro could spell it anything at all; the trial that
/// compiles with the injected std gone is what picks it.
#[test]
fn a_default_by_any_other_name_is_parked_the_same_way() {
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
    assert!(park_proc_macro_default_in_manifest(
        &mut main,
        &toml_of(USE_STD),
        "bf-impl",
        "use_std"
    ));
    assert_eq!(
        edge(&main, "bf-impl").get("default-features"),
        Some(&toml::Value::Boolean(false))
    );
    assert_eq!(custom_disabled(&main), vec!["bf-impl/use_std".to_string()]);
}

/// A feature the macro does not have on by default is not this rule's business:
/// it is already off, and turning defaults off would only risk the others.
#[test]
fn a_feature_that_is_not_a_default_is_left_alone() {
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
    assert!(!park_proc_macro_default_in_manifest(
        &mut main,
        &toml_of(STD_NOT_DEFAULT),
        "displaydoc",
        "std"
    ));
    assert_eq!(main, before);
}

/// `sp-api-proc-macro`'s shape: `std = ["blake2/std"]` is fine, but a *default*
/// entry naming another crate's feature cannot be re-declared on a dependency edge
/// — cargo refuses `multiple slashes in feature` outright. Turning defaults off
/// would silently drop it, so the edge is left alone entirely (the same choice
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
    assert!(!park_proc_macro_default_in_manifest(
        &mut main,
        &toml_of(UNREACHABLE),
        "displaydoc",
        "std"
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
    assert!(!park_proc_macro_default_in_manifest(
        &mut main,
        &toml_of(DISPLAYDOC),
        "displaydoc",
        "std"
    ));
    assert_eq!(main, before);
}

/// The macro is somewhere in the tree but not a direct dependency of this crate:
/// there is no edge here to park it on, and inventing one would add a dependency the
/// crate does not have. (KI-22 — a proc macro under a dependency is out of reach.)
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
    assert!(!park_proc_macro_default_in_manifest(
        &mut main,
        &toml_of(DISPLAYDOC),
        "displaydoc",
        "std"
    ));
    assert_eq!(main, before);
}

// ---------------------------------------------------------------------------
// The attribution
// ---------------------------------------------------------------------------

fn record(usage_crate: &str, expansion_crate: Option<&str>) -> PathRecord {
    PathRecord {
        path_text: "std".to_string(),
        definition_crate: usage_crate.to_string(),
        context: PathContext::ImportDeclaration,
        span: ReadableSpan {
            file: "lib.rs".to_string(),
            start_line: 3,
            start_col: 9,
            end_line: 3,
            end_col: 28,
            usage_crate: Some(usage_crate.to_string()),
        },
        local_route: None,
        defining_module: Some("crate".to_string()),
        macro_body_cfgs: vec![],
        expansion_crate: expansion_crate.map(str::to_string),
        is_extern_crate: true,
        gateway_anchor: None,
    }
}

fn output_of(records: Vec<PathRecord>) -> FeatureRunOutput {
    FeatureRunOutput {
        records,
        macro_module_imports: vec![],
        out_dir: None,
    }
}

/// What makes a std record the macro's: it came out of an expansion the macro
/// crate defines. The crate's own std is not, and neither is a non-std record that
/// merely shares the expansion — a `#[derive]` position routinely holds both.
#[test]
fn only_std_records_from_that_macros_expansion_are_counted() {
    let out = output_of(vec![
        record("std", Some("displaydoc")),
        record("std", Some("displaydoc")),
        record("std", None),                 // the crate's own `use std::…`
        record("std", Some("serde_derive")), // a different macro's
        record("core", Some("displaydoc")),  // the macro's, but not std
    ]);
    assert_eq!(injected_std_records(&out, "displaydoc"), 2);
    assert_eq!(injected_std_records(&out, "serde_derive"), 1);
    assert_eq!(injected_std_records(&out, "not_a_dep"), 0);
}

/// The package is `derive-new`; the crate rustc names is `derive_new`.
#[test]
fn the_package_and_crate_spellings_of_a_name_are_the_same_macro() {
    let out = output_of(vec![record("std", Some("derive_new"))]);
    assert_eq!(injected_std_records(&out, "derive-new"), 1);
}

/// …and where they differ by more than that, the `[lib] name` is what the records
/// carry. 5 of the corpus's 1719 proc-macro crates are in this shape
/// (`ethereum_ssz_derive` builds `ssz_derive`).
#[test]
fn a_lib_name_that_is_not_the_package_name_is_what_the_records_carry() {
    let dir = std::env::temp_dir().join(format!("nostd_libname_{}", std::process::id()));
    fs::create_dir_all(&dir).expect("temp dir");
    let manifest = dir.join("Cargo.toml");
    fs::write(
        &manifest,
        r#"
[package]
name = "ethereum_ssz_derive"
version = "0.1.0"

[lib]
name = "ssz_derive"
proc-macro = true
"#,
    )
    .expect("manifest written");

    assert_eq!(
        nostd::parser::dep_crate_name(manifest.to_str().unwrap(), "ethereum_ssz_derive"),
        "ssz_derive"
    );
    // No `[lib] name`, and no manifest at all, both fall back to the package.
    assert_eq!(
        nostd::parser::dep_crate_name("/nonexistent/Cargo.toml", "displaydoc"),
        "displaydoc"
    );
    fs::remove_dir_all(&dir).ok();
}

// ---------------------------------------------------------------------------
// The rule itself, driven end to end — a unit test of the surgery cannot show
// that anything decides to call it, and *deciding* is the whole change.
// ---------------------------------------------------------------------------

/// Copy a whole fixture directory into a cargo test project: these fixtures ship a
/// path dependency, and the rule rewrites the consumer's manifest, so it has to run
/// against a copy.
fn load_fixture(name: &str) -> (Project, String) {
    let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures")
        .join(name);

    let mut files: Vec<(String, String)> = Vec::new();
    collect_files(&fixture_path, &fixture_path, &mut files);
    assert!(!files.is_empty(), "fixture {name} has no files");

    let mut builder = project().at(name);
    for (rel, contents) in &files {
        builder = builder.file(rel, contents);
    }
    let p = builder.build();
    let manifest = p.root().join("Cargo.toml").to_str().unwrap().to_string();
    (p, manifest)
}

fn collect_files(root: &Path, dir: &Path, out: &mut Vec<(String, String)>) {
    for entry in fs::read_dir(dir).unwrap_or_else(|e| panic!("reading {dir:?}: {e}")) {
        let path = entry.expect("dir entry").path();
        if path.is_dir() {
            collect_files(root, &path, out);
        } else {
            let rel = path.strip_prefix(root).expect("under root");
            out.push((
                rel.to_string_lossy().to_string(),
                fs::read_to_string(&path).unwrap_or_else(|e| panic!("reading {path:?}: {e}")),
            ));
        }
    }
}

fn dep_at(project: &Project, dir: &str, package: &str) -> ProcMacroDep {
    ProcMacroDep {
        package: package.to_string(),
        manifest: project
            .root()
            .join(dir)
            .join("Cargo.toml")
            .to_str()
            .unwrap()
            .to_string(),
    }
}

fn manifest_of(path: &str) -> toml::Value {
    toml::from_str(&fs::read_to_string(path).expect("manifest readable")).expect("manifest parses")
}

/// dfu-core's case. The consumer writes no std at all; the derive expansion does,
/// and the compiler says which crate the expansion came from.
#[cargo_test]
fn a_macro_that_injects_std_has_the_responsible_default_parked() {
    let (p, manifest) = load_fixture("proc_macro_injects_std");
    let deps = vec![dep_at(&p, "injector", "injector")];
    let mut telemetry = Telemetry::default();

    park_injecting_proc_macros("proc_macro_injects_std", &manifest, &deps, &mut telemetry);

    assert_eq!(
        telemetry.proc_macro_std_injectors,
        vec!["injector".to_string()],
        "the `extern crate std` in the expansion is attributed to the macro that \
         defines it, so the injection must be seen"
    );
    assert_eq!(
        telemetry.proc_macro_std_parked,
        vec!["injector/std".to_string()],
        "and the default whose removal makes it disappear must be parked"
    );

    let main = manifest_of(&manifest);
    assert_eq!(
        edge(&main, "injector").get("default-features"),
        Some(&toml::Value::Boolean(false)),
        "the parking has to reach the edge cargo reads, not only the telemetry"
    );
    assert_eq!(custom_disabled(&main), vec!["injector/std".to_string()]);
}

/// bebytes 0.7.1's case, and the one the name test failed on. `hostonly` has a
/// `std` default and it means the macro's own build; nothing std ever reaches the
/// consumer. Parking it here compiles nothing and gains nothing.
#[cargo_test]
fn a_macro_that_injects_nothing_keeps_its_defaults() {
    let (p, manifest) = load_fixture("proc_macro_own_std");
    let before = fs::read_to_string(&manifest).expect("manifest readable");
    let deps = vec![dep_at(&p, "hostonly", "hostonly")];
    let mut telemetry = Telemetry::default();

    park_injecting_proc_macros("proc_macro_own_std", &manifest, &deps, &mut telemetry);

    assert!(
        telemetry.proc_macro_std_injectors.is_empty(),
        "no std record in this crate comes out of a `hostonly` expansion, so there \
         is nothing to attribute to it: {:?}",
        telemetry.proc_macro_std_injectors
    );
    assert!(
        telemetry.proc_macro_std_parked.is_empty(),
        "and nothing may be parked on the strength of the feature's name alone: {:?}",
        telemetry.proc_macro_std_parked
    );
    assert_eq!(
        fs::read_to_string(&manifest).expect("manifest readable"),
        before,
        "the manifest must come out byte-identical — `hostonly` needs its `std` to \
         compile at all, and parking it costs the consumer every target build"
    );
}

/// multiwii_serial_protocol_v2 0.1.12's case, and the reason the build check asks
/// *which* package failed rather than whether anything did. `dual` compiles without
/// its `std` default; what does not compile is this crate's **default** (std)
/// configuration, against the no_std flavour of the expansion — a build that was
/// never the question. Rejecting there would throw away a parking the no_std runs
/// want, on the same mistake O-7 and D2 are both about.
#[cargo_test]
fn a_trial_that_only_breaks_the_default_configuration_keeps_the_parking() {
    let (p, manifest) = load_fixture("proc_macro_default_config_mismatch");
    let deps = vec![dep_at(&p, "dual", "dual")];
    let mut telemetry = Telemetry::default();

    park_injecting_proc_macros(
        "proc_macro_default_config_mismatch",
        &manifest,
        &deps,
        &mut telemetry,
    );

    assert_eq!(
        telemetry.proc_macro_std_injectors,
        vec!["dual".to_string()],
        "the std in this crate comes out of a `dual` expansion"
    );
    assert_eq!(
        telemetry.proc_macro_std_parked,
        vec!["dual/std".to_string()],
        "and the parking is kept: `dual` itself built without the feature, so the \
         failure says nothing about the macro"
    );
    assert_eq!(
        telemetry.proc_macro_std_parked_unverified,
        vec!["dual".to_string()],
        "but it is recorded as unverified — no record set could be compared, and \
         saying otherwise would claim evidence that was never gathered"
    );

    let main = manifest_of(&manifest);
    assert_eq!(
        edge(&main, "dual").get("default-features"),
        Some(&toml::Value::Boolean(false))
    );
}

/// The other half of the evidence. This macro really does inject std, so
/// attribution fires — but it also needs its own `std` to compile, so the trial
/// fails to build and is rolled back. The crate keeps the injected std and says so.
#[cargo_test]
fn a_parking_that_does_not_compile_is_rolled_back() {
    let (p, manifest) = load_fixture("proc_macro_unparkable");
    let before = fs::read_to_string(&manifest).expect("manifest readable");
    let deps = vec![dep_at(&p, "needs_std", "needs_std")];
    let mut telemetry = Telemetry::default();

    park_injecting_proc_macros("proc_macro_unparkable", &manifest, &deps, &mut telemetry);

    assert_eq!(
        telemetry.proc_macro_std_injectors,
        vec!["needs_std".to_string()],
        "the injection is real here and must still be attributed"
    );
    assert!(
        telemetry.proc_macro_std_parked.is_empty(),
        "but no parking of it compiles, so none may be kept: {:?}",
        telemetry.proc_macro_std_parked
    );
    assert_eq!(
        telemetry.proc_macro_std_unparkable,
        vec!["needs_std".to_string()],
        "an injector nothing can be done about is reported, not silently dropped"
    );
    assert_eq!(
        fs::read_to_string(&manifest).expect("manifest readable"),
        before,
        "and the manifest must be restored exactly, or the emitted config carries a \
         parking that was already known not to build"
    );
}

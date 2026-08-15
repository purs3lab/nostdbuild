#![feature(rustc_private)]

//! Residual T1 (KI-14): which optional dependencies `minimize` may not unlink.
//!
//! `minimize` unlinks a dependency by deleting its entry out of the feature that
//! names it, leaving that feature enabled. `driver::deps_pinned_by_active_use`
//! decides when that is safe by re-checking every import in the world the edit
//! would create. It used to pin the dependency's own *name* false unconditionally
//! — right for Cargo's synthesised `D = ["dep:D"]`, wrong for a feature the
//! manifest declares itself. bevy_input-0.16.0 declares
//! `smol_str = ["dep:smol_str", "bevy_reflect/smol_str"]` and imports
//! `#[cfg(feature = "smol_str")] use smol_str::SmolStr`, so the gate read as dead,
//! the dep was unlinked, and the emitted `--features libm,smol_str` build died on
//! `E0432: unresolved import smol_str` — the T1 `FEATURE_ON_DEP_STRIPPED`
//! signature all over again.

use std::collections::HashSet;
use std::path::{Path, PathBuf};

use nostd::driver::deps_pinned_by_active_use;
use nostd::visitor::ModCollector;

fn fixture() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/dep_named_feature/lib.rs")
}

const MANIFEST: &str = r#"
[package]
name = "fixture"
version = "0.0.0"

[dependencies]
smol_str = { version = "0.2", optional = true }
chrono = { version = "0.4", optional = true }
embedded-hal-async = { version = "1", optional = true }
critical-section = { version = "1", optional = true }
icu_calendar_data = { version = "1", optional = true }
scale-info = { version = "2", optional = true }
yazi = { version = "0.1", optional = true }
deflate = { version = "1", optional = true }

[features]
default = ["std", "smol_str"]
std = ["chrono"]
smol_str = ["dep:smol_str", "bevy_reflect/smol_str"]
async = ["embedded-hal-async"]
impl-critical-section = ["dep:critical-section"]
compiled_data = ["dep:icu_calendar_data"]
scale = ["dep:scale-info"]
"#;

fn set(names: &[&str]) -> HashSet<String> {
    names.iter().map(|s| s.to_string()).collect()
}

/// Cargo's declared set: the `[features]` entries plus the implicit feature of
/// every optional dep not referenced as `dep:`. `smol_str` appears once — the
/// explicit entry — because `dep:smol_str` suppresses the synthesised one.
fn declared() -> HashSet<String> {
    set(&[
        "default",
        "std",
        "smol_str",
        "async",
        "chrono",
        "embedded-hal-async",
        "impl-critical-section",
        "compiled_data",
        "scale",
        // Implicit: named by no `[features]` entry and by no `dep:` spelling.
        "yazi",
        "deflate",
    ])
}

/// Everything the crate could have on at once. Individual tests turn one thing
/// off rather than restating the whole set.
fn all_on() -> HashSet<String> {
    set(&[
        "default",
        "std",
        "smol_str",
        "async",
        "chrono",
        "impl-critical-section",
        "compiled_data",
        "scale",
        "yazi",
    ])
}

fn pins(active: &HashSet<String>) -> HashSet<String> {
    let ctx = z3::Context::new(&z3::Config::new());
    let mut collector = ModCollector::new(&ctx);
    let root = collector.collect(&fixture(), "dep_named_feature");
    let manifest: toml::Value = MANIFEST.parse().expect("fixture manifest");

    deps_pinned_by_active_use(&ctx, &manifest, &declared(), active, &root, &HashSet::new())
}

#[test]
fn explicitly_declared_feature_of_the_same_name_pins_its_dep() {
    let pinned = pins(&set(&["default", "std", "smol_str", "async", "chrono"]));

    assert!(
        pinned.contains("smol_str"),
        "`smol_str` is a declared feature that survives the unlink, so its gate stays \
         true and the dep must be pinned, got {pinned:?}"
    );
}

/// The case the old predicate was written for and must keep answering the same
/// way: without it `minimize` can never unlink anything and watchface-0.4.0 drags
/// chrono into a no_std build.
#[test]
fn implicit_feature_of_the_same_name_does_not_pin_its_dep() {
    let pinned = pins(&set(&["default", "std", "smol_str", "async", "chrono"]));

    assert!(
        !pinned.contains("chrono"),
        "`chrono` has no declared feature of its own, so deleting the entry turns its \
         gate off and the dep must stay unlinkable, got {pinned:?}"
    );
}

/// A gate naming some *other* feature is untouched by the unlink either way.
#[test]
fn gate_on_an_unrelated_feature_pins_its_dep() {
    let pinned = pins(&set(&["default", "std", "smol_str", "async", "chrono"]));

    assert!(
        pinned.contains("embedded-hal-async"),
        "the `async` gate survives unlinking `embedded-hal-async`, so it must be pinned, \
         got {pinned:?}"
    );
}

/// Control: the pin is a property of the *active* set, not of the feature merely
/// being declared. With `smol_str` off, the import is not compiled and unlinking
/// is safe — otherwise the fix would just pin every same-named dep forever.
#[test]
fn declared_feature_that_is_off_does_not_pin_its_dep() {
    let pinned = pins(&set(&["std", "chrono"]));

    assert!(
        !pinned.contains("smol_str"),
        "with the `smol_str` feature off its import is not compiled, so the dep must \
         stay unlinkable, got {pinned:?}"
    );
}

/// R31-2, the shape that carries the bucket: `mutex-1.0.0` names
/// `critical_section` once, as a call, with no import in the crate. The gate is a
/// declared feature its own `default` turns on, so the unlink left
/// `critical_section::with(…)` compiling against a crate cargo no longer links —
/// and the published manifest builds on all 26 targets untouched.
#[test]
fn a_call_path_pins_its_dep_when_the_gate_survives() {
    let pinned = pins(&all_on());

    assert!(
        pinned.contains("critical-section"),
        "`critical_section::with(…)` is a reference even though nothing imports it, \
         got {pinned:?}"
    );
}

/// The `icu_*` family: the only mention is the macro's own path.
#[test]
fn a_macro_invocation_path_pins_its_dep() {
    let pinned = pins(&all_on());

    assert!(
        pinned.contains("icu_calendar_data"),
        "`icu_calendar_data::make_provider!(Baked)` names the crate, got {pinned:?}"
    );
}

/// The `pallet-*-uapi` family: the only mention is inside a `cfg_attr`, whose
/// body syn never parses into paths.
#[test]
fn a_derive_path_inside_cfg_attr_pins_its_dep() {
    let pinned = pins(&all_on());

    assert!(
        pinned.contains("scale-info"),
        "`#[cfg_attr(feature = \"scale\", derive(scale_info::TypeInfo))]` names the \
         crate, got {pinned:?}"
    );
}

/// Control — the whole point of reading the gate rather than the mention. `yazi`
/// is named by a path, but only under its own implicit feature, which the unlink
/// turns off. Pin this and the pass stops unlinking anything.
#[test]
fn a_path_gated_only_by_the_implicit_feature_does_not_pin_its_dep() {
    let pinned = pins(&all_on());

    assert!(
        !pinned.contains("yazi"),
        "deleting the entry turns `feature = \"yazi\"` off, so the path is not compiled \
         and the dep stays unlinkable, got {pinned:?}"
    );
}

/// Control — a doc comment is a string literal. Resolving spans this way is what
/// pinned watchface's `chrono` once before and cost it its build.
#[test]
fn a_dep_named_only_in_a_doc_comment_is_not_pinned() {
    let pinned = pins(&all_on());

    assert!(
        !pinned.contains("deflate"),
        "prose is not a reference, got {pinned:?}"
    );
}

/// Control — the pin is still a property of the active set: with the gating
/// feature off, the call is not compiled and the unlink is safe.
#[test]
fn a_call_path_under_a_feature_that_is_off_does_not_pin_its_dep() {
    let mut active = all_on();
    active.remove("impl-critical-section");
    let pinned = pins(&active);

    assert!(
        !pinned.contains("critical-section"),
        "with the gating feature off nothing names the dep, got {pinned:?}"
    );
}

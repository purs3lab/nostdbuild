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

[features]
default = ["std", "smol_str"]
std = ["chrono"]
smol_str = ["dep:smol_str", "bevy_reflect/smol_str"]
async = ["embedded-hal-async"]
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

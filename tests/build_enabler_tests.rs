#![feature(rustc_private)]

//! Bucket T2 (`HIR_PROBES_ALL_FAILED`): every std-span probe fails to compile, so
//! classification never finishes and the crate is reported as "std usage could
//! not be proven avoidable" without a single build attempt.
//!
//! The mechanism is not a broken dep tree. It is a feature the crate cannot build
//! without on bare metal that gates *none of its own code* — bevy_input's
//! `libm = ["bevy_math/libm"]`, totsu_core's `libm = ["num-traits/libm"]`. No
//! `#[cfg]` mentions it, so it is never a solver variable, so every model leaves
//! it off, so every compile the prober asks for fails. `bevy_input` is 0/26 in
//! run28 and `--no-default-features --features libm` builds it on all 26.
//!
//! `driver::discover_build_enablers` searches for such a feature and pins it for
//! the probes and for the emitted config.

use std::fs;
use std::path::Path;

use cargo_test_support::{Project, cargo_test, project};

use nostd::Telemetry;
use nostd::driver::{analyze_crate, reset_target_cache};

/// Serialises the tests and clears the caches `analyze_crate` shares through
/// process globals. Without it a fixture that compiles for a bare-metal target
/// leaves `LAST_GOOD_TARGET` set and every later test skips the enabler search
/// silently — the suite then measures thread scheduling, not behaviour.
static SERIAL: std::sync::Mutex<()> = std::sync::Mutex::new(());

fn isolated() -> std::sync::MutexGuard<'static, ()> {
    let guard = SERIAL.lock().unwrap_or_else(|e| e.into_inner());
    reset_target_cache();
    guard
}

/// Copy a whole fixture directory into a cargo test project — the fixture ships a
/// path dependency, so Cargo.toml + lib.rs is not enough.
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

#[cargo_test]
fn enabler_feature_is_found_and_makes_the_std_span_provable() {
    let _serial = isolated();
    let (_p, manifest) = load_fixture("build_enabler_libm");

    let ctx = z3::Context::new(&z3::Config::new());
    let mut telemetry = Telemetry::default();

    let (hard_spans, condition, _coverage, _ce, _root, _records, unproven) =
        analyze_crate(&ctx, &manifest, "build_enabler_libm", &mut telemetry);

    assert_eq!(
        telemetry.build_enabler_features,
        vec!["libm".to_string()],
        "`libm` is the only feature the crate builds on bare metal with; `std` \
         satisfies the very gate the probe negates and must never be offered as \
         the fix, and `default` implies it"
    );

    // The payoff: with `libm` pinned the probe's compile succeeds, the `String`
    // span produces no record under ¬std, and the crate clears instead of
    // reporting an unproven span.
    assert!(
        unproven.is_empty(),
        "no span should be left unproven once the crate can be compiled: {unproven:?}"
    );
    assert!(
        hard_spans.is_empty(),
        "the `String` use is behind `feature = \"std\"` and is avoidable: {hard_spans:?}"
    );
    assert_eq!(telemetry.unproven_std_spans, 0);

    // The emitted config is solved from this condition, so `libm` has to be in it
    // — a probe that only compiled because `libm` was on proves nothing about a
    // build that leaves it off.
    let condition = condition.expect("a no_std condition should have been proven");
    let solver = z3::Solver::new(&ctx);
    solver.assert(&condition);
    solver.assert(&z3::ast::Bool::new_const(&ctx, "libm").not());
    assert_eq!(
        solver.check(),
        z3::SatResult::Unsat,
        "the proven condition must force `libm` on, got {condition}"
    );
}

/// The search compiles the configuration it reports, and those records are the
/// crate's only std-off evidence — so the run is adopted as a covering run
/// rather than thrown away with only the feature name kept.
///
/// xmrs 0.9.9 is the case (O-4). Pinning its enabler is not enough: its eight
/// `f32::{powf,round,…}` calls carry no `#[cfg]`, so the probe short-circuits
/// them to `StillStd` without compiling, and they are `AlwaysStd` only because
/// the one surviving covering run has `std` on. The trial that compiled
/// resolves all eight to `micromath::F32Ext` and holds no std record at all.
#[cargo_test]
fn the_trial_that_compiled_becomes_a_covering_run() {
    let _serial = isolated();
    let (_p, manifest) = load_fixture("build_enabler_shim_method");

    let ctx = z3::Context::new(&z3::Config::new());
    let mut telemetry = Telemetry::default();

    let (hard_spans, _condition, coverage, _ce, _root, _records, unproven) =
        analyze_crate(&ctx, &manifest, "build_enabler_shim_method", &mut telemetry);

    assert_eq!(
        telemetry.build_enabler_features,
        vec!["libm".to_string()],
        "`libm` is the only feature this crate builds on bare metal with"
    );

    // The run set the classification saw. One covering run is the std-on host
    // build; the second is the trial the search compiled, and it exists only if
    // that trial was adopted.
    let coverage = coverage.expect("the default-features pass should have succeeded");
    assert_eq!(
        coverage.num_covering_runs, 2,
        "the compiling std-off trial should have joined the covering runs, got {coverage:?}"
    );

    // The payoff. `nearest` is ungated, so no probe can ever clear it — only a
    // run in which it resolves to the shim can.
    assert!(
        hard_spans.is_empty(),
        "`x.round()` binds the shim's `F32Ext::round` in the configuration the \
         enabler search compiled, so it is not unavoidable std: {hard_spans:?}"
    );
    assert!(
        unproven.is_empty(),
        "no span should be left unproven once the compiling run is counted: {unproven:?}"
    );
    assert_eq!(telemetry.unproven_std_spans, 0);
}

/// The search costs up to 26 builds when it fails, so it must not run for the
/// ordinary crate. `test_extern_std_on_feature` compiles for a bare-metal target
/// in its ¬`use_std` configuration, which is exactly the condition that skips it.
#[cargo_test]
fn a_crate_that_already_builds_bare_metal_runs_no_search() {
    let _serial = isolated();
    let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/test_extern_std_on_feature");

    let p = project()
        .at("build_enabler_skip")
        .file(
            "Cargo.toml",
            &fs::read_to_string(fixture_path.join("Cargo.toml"))
                .expect("Missing Cargo.toml")
                .replace("test_extern_std_on_feature", "build_enabler_skip"),
        )
        .file(
            "main.rs",
            &fs::read_to_string(fixture_path.join("main.rs")).expect("Missing main.rs"),
        )
        .build();

    let manifest = p.root().join("Cargo.toml").to_str().unwrap().to_string();
    let ctx = z3::Context::new(&z3::Config::new());
    let mut telemetry = Telemetry::default();

    let _ = analyze_crate(&ctx, &manifest, "build_enabler_skip", &mut telemetry);

    assert!(
        telemetry.build_enabler_features.is_empty(),
        "no enabler search should run for a crate with a working bare-metal \
         configuration, got {:?}",
        telemetry.build_enabler_features
    );
}

/// The answer can be a *pair*, and a one-at-a-time search can never find one.
///
/// proptest 1.6.0 is the case (O-10's probe): its bare-metal build needs
/// `alloc` **and** `no_std` together — `alloc` alone dies on
/// `num_traits::float::Float`, `no_std` alone on ``cannot find macro `vec` `` —
/// while the all-on trial drags in bit-vec, lazy_static, rusty-fork, tempfile
/// and fnv through five candidates that link optional dependencies. All-on
/// fails, every single fails, and the search reported nothing.
///
/// The discriminator is the manifest, not another compile: a candidate that
/// links an optional dep puts a new crate into the graph that must itself build
/// bare metal; one that cannot do that cannot be the reason all-on failed.
#[cargo_test]
fn a_pair_of_features_is_found_when_all_on_fails_on_an_optional_dep() {
    let _serial = isolated();
    let (_p, manifest) = load_fixture("build_enabler_pair");

    let ctx = z3::Context::new(&z3::Config::new());
    let mut telemetry = Telemetry::default();

    let (hard_spans, _condition, _coverage, _ce, _root, _records, unproven) =
        analyze_crate(&ctx, &manifest, "build_enabler_pair", &mut telemetry);

    let mut found = telemetry.build_enabler_features.clone();
    found.sort();
    assert_eq!(
        found,
        vec!["alloc".to_string(), "nostd_math".to_string()],
        "the crate builds bare metal only with both `alloc` and `nostd_math`; \
         `hostdep` links a std-only crate and must be dropped, not searched \
         around"
    );

    // The payoff, as in the `libm` case: with the pair pinned the probe's
    // compile succeeds and the `String` span behind `feature = "std"` is
    // provably avoidable rather than unproven.
    assert!(
        hard_spans.is_empty(),
        "the `String` use is behind `feature = \"std\"` and is avoidable: {hard_spans:?}"
    );
    assert!(
        unproven.is_empty(),
        "no span should be left unproven once the pair is found: {unproven:?}"
    );
    assert_eq!(telemetry.unproven_std_spans, 0);
}

/// A crate with nothing to prove still has a configuration to find (R31-5).
///
/// The search used to require an `AlwaysStd` span, on the reasoning that a
/// failed probe is the only thing it makes better. euclid 0.22.11 has no such
/// span — `#![cfg_attr(not(test), no_std)]`, no `extern crate std`, no std
/// anywhere — and no bare-metal build either: `num_traits::real` does not exist
/// until `num-traits` gets `std` or `libm`, and euclid's own
/// `libm = ["num-traits/libm"]` gates none of its code, so no covering set ever
/// contains it. It shipped `--no-default-features` and lost all 26 targets while
/// `--no-default-features --features libm` builds it clean. The search's answer
/// is a constraint on the feature solve, not evidence about a span, so it is
/// worth having either way. 45 of R31-5's 48 crates are this shape.
#[cargo_test]
fn a_crate_with_no_std_span_still_gets_the_configuration_that_builds() {
    let _serial = isolated();
    let (_p, manifest) = load_fixture("build_enabler_no_span");

    let ctx = z3::Context::new(&z3::Config::new());
    let mut telemetry = Telemetry::default();

    let (hard_spans, condition, _coverage, _ce, _root, _records, unproven) =
        analyze_crate(&ctx, &manifest, "build_enabler_no_span", &mut telemetry);

    assert_eq!(
        telemetry.build_enabler_features,
        vec!["libm".to_string()],
        "nothing in this crate is std, and `libm` is still the only way it builds \
         for a bare-metal target"
    );

    // Nothing to prove, and nothing claimed either — the search must not invent a
    // verdict about a crate it only compiled.
    assert!(hard_spans.is_empty(), "{hard_spans:?}");
    assert!(unproven.is_empty(), "{unproven:?}");

    // The point of the whole exercise: the emitted selection is solved from this
    // condition, so `libm` has to survive into it.
    let condition = condition.expect("the enabler should have produced a condition");
    let solver = z3::Solver::new(&ctx);
    solver.assert(&condition);
    solver.assert(&z3::ast::Bool::new_const(&ctx, "libm").not());
    assert_eq!(
        solver.check(),
        z3::SatResult::Unsat,
        "the condition must force `libm` on, got {condition}"
    );
}

/// The search must not be constrained by the verdicts it exists to overturn.
///
/// proptest 1.6.0 (O-10's probe). `src/arbitrary/mod.rs:39` is
/// `#[cfg(any(feature = "std", feature = "alloc"))] mod _alloc;`, so every span
/// in `_alloc` carries the gate `std ∨ alloc`. Those spans are `AlwaysStd` for
/// one reason only — no covering run ever compiled std-off — and negating their
/// gate gives `¬std ∧ ¬alloc`, which made `alloc` UNSAT against the constraints
/// and kept it out of the candidate list entirely. `alloc` is half of the pair
/// that builds the crate (`--no-default-features --features alloc,no_std`
/// compiles clean on aarch64-unknown-none), so the search was forbidden from
/// proposing the very thing it was looking for.
///
/// The crate's `no_std_condition` replaces that guard: derived from its own
/// `#![no_std]` / `extern crate std` rather than from a run, and still enough to
/// keep `std` off the table.
#[cargo_test]
fn a_feature_in_a_std_spans_gate_can_still_be_the_enabler() {
    let _serial = isolated();
    let (_p, manifest) = load_fixture("build_enabler_circular");

    let ctx = z3::Context::new(&z3::Config::new());
    let mut telemetry = Telemetry::default();

    let (hard_spans, condition, _coverage, _ce, _root, _records, unproven) =
        analyze_crate(&ctx, &manifest, "build_enabler_circular", &mut telemetry);

    let mut found = telemetry.build_enabler_features.clone();
    found.sort();
    assert_eq!(
        found,
        vec!["alloc".to_string(), "nostd_math".to_string()],
        "`alloc` is a disjunct in the gate of the `AlwaysStd` spans AND half of \
         what the crate builds bare metal with; the first fact must not suppress \
         the second"
    );

    // `std` is still not an acceptable answer — the no_std condition forbids it,
    // and a build that links std cannot succeed for a bare-metal target anyway.
    assert!(
        !telemetry.build_enabler_features.iter().any(|f| f == "std"),
        "`std` must never be offered as the way to make a no_std crate compile"
    );
    let condition = condition.expect("a no_std condition should have been proven");
    let solver = z3::Solver::new(&ctx);
    solver.assert(&condition);
    solver.assert(&z3::ast::Bool::new_const(&ctx, "std"));
    assert_eq!(
        solver.check(),
        z3::SatResult::Unsat,
        "the proven condition must still exclude `std`, got {condition}"
    );

    // The payoff: with `alloc` found the trial compiles, it is adopted as a
    // covering run, and `make()`'s `Thing` resolves to `core::cell::Cell` there
    // — so the span that was `AlwaysStd` is not std at all.
    assert!(
        hard_spans.is_empty(),
        "the facade span resolves to core in the configuration the search \
         compiled: {hard_spans:?}"
    );
    assert!(
        unproven.is_empty(),
        "no span should be left unproven once the enabler is found: {unproven:?}"
    );
}

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
use nostd::driver::analyze_crate;

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

/// The search costs up to 26 builds when it fails, so it must not run for the
/// ordinary crate. `test_extern_std_on_feature` compiles for a bare-metal target
/// in its ¬`use_std` configuration, which is exactly the condition that skips it.
#[cargo_test]
fn a_crate_that_already_builds_bare_metal_runs_no_search() {
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

#![feature(rustc_private)]

use cargo_test_support::{cargo_test, project};
use std::fs;
use std::path::Path;

use nostd::Telemetry;
use nostd::driver::{analyze_crate, find_feature_combs_for_all_code};

/// Regression test for the CEGAR retry loop in probe_one_target.
///
/// The fixture has `std` and `nightly` features. The `std` gate controls
/// `impl Error for ParseError`. A `compile_error!` makes `{nightly=on, std=off}`
/// fail to compile, mimicking a real-world feature incompatibility.
///
/// Without CEGAR the probe gives up on the `std` gate after the first failed compile
/// and marks the Error impl as `CompileFailed` / `StillStd`. With CEGAR it blocks
/// the failing model, finds `{nightly=off, std=off}`, and correctly classifies the
/// span as `NonStd` — so it is absent from the returned hard_spans.
#[cargo_test]
fn test_probe_cegar_retry() {
    let fixture_path =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/test_probe_cegar_retry");

    let p = project()
        .at("test_probe_cegar_retry")
        .file(
            "Cargo.toml",
            &fs::read_to_string(fixture_path.join("Cargo.toml")).expect("Missing Cargo.toml"),
        )
        .file(
            "main.rs",
            &fs::read_to_string(fixture_path.join("main.rs")).expect("Missing main.rs"),
        )
        .build();

    let manifest = p.root().join("Cargo.toml").to_str().unwrap().to_string();
    let ctx = z3::Context::new(&z3::Config::new());
    let mut telemetry = Telemetry::default();

    let (hard_spans, _condition, _coverage, _, _, _) =
        analyze_crate(&ctx, &manifest, "test_probe_cegar_retry", &mut telemetry);

    // The `impl Error for ParseError` span is behind `#[cfg(feature = "std")]`.
    // After CEGAR finds {nightly=off, std=off} compiles successfully with the span
    // absent, it must be classified as NonStd and therefore NOT appear in hard_spans.
    assert!(
        hard_spans.is_empty(),
        "Expected no hard std spans (Error impl should be NonStd), but got: {:?}",
        hard_spans
    );
}

/// Regression test for the greedy-ordering bug: when eq_c_not_b (incompatible with eq_b)
/// is added to the covering set before eq_b, it blocks eq_b from pairing with the seed
/// (eq_a). The resulting set {feat_a, feat_c} fails to compile because a_function calls
/// b_helper (feat_b). CEGAR must re-pair feat_a with feat_b after the first failure.
#[cargo_test]
fn test_cegar_pairing_recovery() {
    let fixture_path =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/test_cegar_ordering");

    let p = project()
        .at("test_cegar_ordering")
        .file(
            "Cargo.toml",
            &fs::read_to_string(fixture_path.join("Cargo.toml")).expect("Missing Cargo.toml"),
        )
        .file(
            "main.rs",
            &fs::read_to_string(fixture_path.join("main.rs")).expect("Missing main.rs"),
        )
        .build();

    let manifest = p.root().join("Cargo.toml").to_str().unwrap().to_string();
    let ctx = z3::Context::new(&z3::Config::new());
    let mut telemetry = Telemetry::default();

    let (_root, covering_runs, _hard, _) =
        find_feature_combs_for_all_code(&ctx, &manifest, "test_cegar_ordering", &mut telemetry);

    // After CEGAR re-pairing, there must be a successful covering run with feat_a enabled.
    // Without CEGAR this fails: the only run containing feat_a also has feat_c and not
    // feat_b, which causes a compile error because a_function calls b_helper (feat_b-gated).
    let feat_a_covered = covering_runs
        .iter()
        .any(|run| run.features.contains(&"feat_a".to_string()));

    assert!(
        feat_a_covered,
        "Expected a successful covering run with feat_a enabled, but covering runs were: {:?}",
        covering_runs
            .iter()
            .map(|r| &r.features)
            .collect::<Vec<_>>()
    );
}

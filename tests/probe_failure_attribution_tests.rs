#![feature(rustc_private)]

//! R31-6, `PROBE_SET_INFEASIBLE`: a probe that never compiled has to say why.
//!
//! When every configuration that negates a span's gate fails to build, the span
//! is neither cleared nor condemned — it is *unproven*, and the run stops with
//! "Std usage in the main crate could not be proven avoidable". Correct, and
//! until now that was the entire output: 40 crates in run31 carried the verdict
//! and a count, with nothing to separate the two cases underneath it.
//!
//! * The crate cannot compile with the gate negated **whatever else is enabled**
//!   — `error[E0412]: cannot find type `Vec` in this scope`, an author's missing
//!   `use alloc::vec::Vec`. Nothing a feature selection can reach; the row is the
//!   crate's, not the tool's.
//! * The particular configuration the probe carried is what failed, and another
//!   one would have compiled. That row *is* the tool's.
//!
//! The compiler answered the question inside the build the probe already ran.
//! `ProbeDecision::CompileFailed` now carries that answer out, the way T5's
//! `dep_not_no_std` exit carries the name of the dependency.

use std::fs;
use std::path::Path;

use cargo_test_support::{cargo_test, project};
use nostd::Telemetry;
use nostd::driver::analyze_crate;

#[cargo_test]
fn an_unproven_span_reports_the_compiler_error_that_left_it_unproven() {
    let fixture_path =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/probe_failure_attribution");

    let p = project()
        .at("probe_failure_attribution")
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

    let (hard_spans, _condition, _coverage, _, _, _, unproven) =
        analyze_crate(&ctx, &manifest, "probe_failure_attribution", &mut telemetry);

    // The verdict itself is unchanged: not proven hard, not proven avoidable.
    assert!(
        hard_spans.is_empty(),
        "the span is unproven, not hard: {hard_spans:?}"
    );
    assert!(
        !unproven.is_empty(),
        "negating `std` cannot compile, so the `impl std::error::Error` span must come back unproven"
    );
    assert_eq!(telemetry.unproven_std_spans, unproven.len());

    // What is new: the reason travels with it.
    let reasons = &telemetry.unproven_std_span_reasons;
    assert!(
        !reasons.is_empty(),
        "an unproven span must record why its probe never compiled"
    );
    assert!(
        reasons.iter().any(|r| r.contains("Vec")),
        "the reason must be the compiler's, naming the item that does not resolve \
         without std; got {reasons:?}"
    );
}

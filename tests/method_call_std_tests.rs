#![feature(rustc_private)]

//! Std usage that only type checking can resolve.
//!
//! The plugin's record pass ran at `after_expansion` over the AST, keyed on the
//! resolver's `partial_res_map`. A method call has no entry there — method
//! resolution is part of type checking — so every dot-syntax call was invisible.
//! `f32::log2` and `f32::round`, which live in `library/std/src/f32.rs` with no
//! `core` counterpart, therefore read as no std usage at all: afe4404 0.2.4 got
//! an emitted manifest instead of a verdict, and failed to build on all 26
//! targets.

use cargo_test_support::{cargo_test, project};
use std::fs;
use std::path::Path;

use nostd::Telemetry;
use nostd::driver::analyze_crate;

fn load_fixture(name: &str) -> (cargo_test_support::Project, String) {
    let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures")
        .join(name);

    let p = project()
        .at(name)
        .file(
            "Cargo.toml",
            &fs::read_to_string(fixture_path.join("Cargo.toml"))
                .unwrap_or_else(|_| panic!("Missing Cargo.toml for fixture {name}")),
        )
        .file(
            "main.rs",
            &fs::read_to_string(fixture_path.join("main.rs"))
                .unwrap_or_else(|_| panic!("Missing main.rs for fixture {name}")),
        )
        .build();

    let manifest = p.root().join("Cargo.toml").to_str().unwrap().to_string();
    (p, manifest)
}

fn run_analyze(manifest: &str, crate_name: &str) -> Vec<nostd::types::ReadableSpan> {
    let ctx = z3::Context::new(&z3::Config::new());
    let mut telemetry = Telemetry::default();
    let (hard_spans, ..) = analyze_crate(&ctx, manifest, crate_name, &mut telemetry);
    hard_spans
}

/// The afe4404 shape. `f32::from(x).log2().round()` is unconditional std usage
/// with no path anywhere in it, and no feature can make it go away.
#[cargo_test]
fn float_method_is_hard_std_usage() {
    let (_p, manifest) = load_fixture("test_float_method_std");
    let hard_spans = run_analyze(&manifest, "test_float_method_std");
    assert!(
        !hard_spans.is_empty(),
        "Expected hard std spans for `f32::log2()`, which is defined in std and has no core counterpart"
    );
}

/// The same call behind `#[cfg(feature = "std")]`. The record must inherit the
/// gate like any other record — a method record whose span escaped the ModNode
/// tree would find no ancestor and sink a crate that converts by turning the
/// feature off.
#[cargo_test]
fn gated_float_method_is_not_hard_std_usage() {
    let (_p, manifest) = load_fixture("test_float_method_gated");
    let hard_spans = run_analyze(&manifest, "test_float_method_gated");
    assert!(
        hard_spans.is_empty(),
        "Expected no hard std spans: the only std-resolving call is gated by `feature = \"std\"`, got {:?}",
        hard_spans
    );
}

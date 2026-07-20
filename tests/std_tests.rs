#![feature(rustc_private)]

use cargo_test_support::{cargo_test, project};
use std::fs;
use std::path::Path;

use nostd::Telemetry;
use nostd::driver::analyze_crate;

/// Load a fixture crate into a temporary cargo project and return the manifest path.
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

// ---------------------------------------------------------------------------
// Unconditional std usage — must appear in hard_spans
// ---------------------------------------------------------------------------

/// `extern crate std as std1` with an `impl crate::std1::error::Error` is unconditional
/// std usage; no feature can make it disappear.
#[cargo_test]
fn test_extern_std_unconditional() {
    let (_p, manifest) = load_fixture("test_extern_std");
    let hard_spans = run_analyze(&manifest, "test_extern_std");
    assert!(
        !hard_spans.is_empty(),
        "Expected hard std spans for unconditional `extern crate std as std1` usage"
    );
}

/// Direct `std::vec::Vec::new()` call is unconditional std usage.
#[cargo_test]
fn test_vec_direct_usage() {
    let (_p, manifest) = load_fixture("test_vec_direct_usage");
    let hard_spans = run_analyze(&manifest, "test_vec_direct_usage");
    assert!(
        !hard_spans.is_empty(),
        "Expected hard std spans for direct std::vec::Vec usage"
    );
}

/// `use std::vec::Vec` import is unconditional std usage.
#[cargo_test]
fn test_vec_used_via_use() {
    let (_p, manifest) = load_fixture("test_vec_used_via_use");
    let hard_spans = run_analyze(&manifest, "test_vec_used_via_use");
    assert!(
        !hard_spans.is_empty(),
        "Expected hard std spans for `use std::vec::Vec` import"
    );
}

// ---------------------------------------------------------------------------
// Feature-gated std — must NOT appear in hard_spans (std is avoidable)
// ---------------------------------------------------------------------------

/// All std usage in this fixture is behind `#[cfg(feature = "std")]`.
/// Without the std feature enabled, no std spans should be hard.
#[cargo_test]
fn test_local_facade_std_is_not_hard() {
    let (_p, manifest) = load_fixture("test_local_facade_std");
    let hard_spans = run_analyze(&manifest, "test_local_facade_std");
    assert!(
        hard_spans.is_empty(),
        "Expected no hard std spans — all std usage is behind #[cfg(feature = \"std\")], but got: {hard_spans:?}"
    );
}

/// std usage is behind `#[cfg(feature = "make_conflicting")]`.
/// CEGAR finds a covering set without that feature → not a hard span.
#[cargo_test]
fn test_feature_causes_std_usage_is_not_hard() {
    let (_p, manifest) = load_fixture("test_feature_causes_std_usage");
    let hard_spans = run_analyze(&manifest, "test_feature_causes_std_usage");
    assert!(
        hard_spans.is_empty(),
        "Expected no hard std spans — std usage is feature-gated, but got: {hard_spans:?}"
    );
}

/// std usage lives inside `#[cfg(feature = "std")]`-gated *provided trait methods*
/// (zerocopy's `read_from_io` / `write_to_io` pattern). The gate is on a
/// `TraitItemFn`, not an `ImplItemFn`; the visitor must record it so the std
/// usage is recognised as feature-gated and avoidable — not hard.
#[cargo_test]
fn test_trait_method_std_gated_is_not_hard() {
    let (_p, manifest) = load_fixture("test_trait_method_std_gated");
    let hard_spans = run_analyze(&manifest, "test_trait_method_std_gated");
    assert!(
        hard_spans.is_empty(),
        "Expected no hard std spans — std usage is behind #[cfg(feature = \"std\")] on provided trait methods, but got: {hard_spans:?}"
    );
}

/// std usage lives only inside a `#[cfg(feature = "std")]`-gated match arm, whose
/// body is an `Expr` (not a `Stmt`) inside a non-gated function. Only `visit_arm`
/// records that gate; without it the usage is misclassified as hard.
#[cargo_test]
fn test_cfg_gated_match_arm_is_not_hard() {
    let (_p, manifest) = load_fixture("test_cfg_gated_match_arm");
    let hard_spans = run_analyze(&manifest, "test_cfg_gated_match_arm");
    assert!(
        hard_spans.is_empty(),
        "Expected no hard std spans — std usage is behind a #[cfg(feature = \"std\")] match arm, but got: {hard_spans:?}"
    );
}

// ---------------------------------------------------------------------------
// alloc-only — alloc usage alone must not produce hard std spans
// ---------------------------------------------------------------------------

/// A crate that uses `alloc::vec::Vec` but never `std::` should have no hard std spans.
#[cargo_test]
fn test_alloc_only_has_no_hard_std_spans() {
    let (_p, manifest) = load_fixture("test_alloc_only");
    let hard_spans = run_analyze(&manifest, "test_alloc_only");
    assert!(
        hard_spans.is_empty(),
        "alloc-only crate should have no hard std spans, but got: {hard_spans:?}"
    );
}

// ---------------------------------------------------------------------------
// Deep-nesting local facade — resolve_local_facade_gateways prefix matching
// ---------------------------------------------------------------------------

/// `extern crate std` declared three levels down (`crate::a::b::c`), item used via
/// `crate::a::b::c::SomeType`. Exercises the prefix-match logic that propagates
/// the facade gateway from a grandchild module upward.
#[cargo_test]
fn test_local_facade_deep_nesting_is_not_hard() {
    let (_p, manifest) = load_fixture("test_local_facade_deep");
    let hard_spans = run_analyze(&manifest, "test_local_facade_deep");
    assert!(
        hard_spans.is_empty(),
        "Expected no hard std spans — std usage in deep-nested facade is feature-gated, but got: {hard_spans:?}"
    );
}

// ---------------------------------------------------------------------------
// cfg(target_os) — non-feature cfg gates are accepted as guards, not reported
// ---------------------------------------------------------------------------

/// `use std::fs` behind `#[cfg(target_os = "linux")]`.
///
/// This test previously asserted the opposite — that such a span is hard std,
/// on the reasoning that the tool controls features but not the target, so no
/// feature combination can remove it. The policy was deliberately reversed:
/// features are the axis *this tool* controls, but the target is the
/// consumer's choice, and `#[cfg(target_os = "linux")]` is the author
/// declaring that this code does not exist on a bare-metal target. A no_std
/// consumer is by definition not on linux, so the gate is accepted as a guard
/// without being proved.
///
/// The probe cannot settle it either way: it compiles for the host, so the
/// target-gated arm is present in every covering run and would classify
/// AlwaysStd unanimously. Compiling for a bare-metal target instead is no help
/// — every std usage becomes a hard error, so nothing is observable.
///
/// Such spans get `ProbeDecision::ExternallyGated`: excluded from `all_hard`
/// (not reported) *and* from `final_condition` (contributing no feature
/// requirement). `Telemetry::externally_gated_spans` counts them so a crate
/// that clears only this way stays separable in the eval from one that clears
/// on a real feature condition.
#[cargo_test]
fn test_cfg_target_os_gates_std_is_excused() {
    let (_p, manifest) = load_fixture("test_cfg_target_os_std");
    let hard_spans = run_analyze(&manifest, "test_cfg_target_os_std");
    assert!(
        hard_spans.is_empty(),
        "Expected no hard std spans — target_os is not the feature axis, so the \
         gate is accepted as a guard, but got: {hard_spans:?}"
    );
}

// ---------------------------------------------------------------------------
// Transitive feature implication — feat_a implies feat_b which gates std
// ---------------------------------------------------------------------------

/// feat_a = ["feat_b"] (cargo implies feat_b when feat_a is set), and feat_b gates
/// `extern crate std`. Enabling only feat_a must still surface std reachability.
/// Since every covering run that includes feat_a also includes feat_b (cargo constraint),
/// the std span is not hard (it disappears when neither feature is enabled),
/// but it should be present in at least one covering run.
#[cargo_test]
fn test_feature_implies_std_transitively_is_not_hard() {
    let (_p, manifest) = load_fixture("test_feature_implies_std_transitively");
    let hard_spans = run_analyze(&manifest, "test_feature_implies_std_transitively");
    assert!(
        hard_spans.is_empty(),
        "Expected no hard std spans — std is transitively feature-gated, but got: {hard_spans:?}"
    );
}

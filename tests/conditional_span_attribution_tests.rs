#![feature(rustc_private)]

//! Which feature a *conditional* span's std-ness is blamed on.
//!
//! The ancestor probe blames the first gate whose negation makes the span stop
//! being std — and deleting the code makes it stop being std, so any feature
//! that merely contains the span qualifies. uom 0.36 is the case that hurts:
//! its float storage lives in `#[cfg(feature = "f32")]` modules and resolves to
//! std only through a `#[cfg(feature = "std")]` re-export, so the probe returns
//! `¬f32` (and `¬si`, `¬f64`), the storage features are stripped off every
//! dependent's edge, and uom's own `compile_error!` fires for having no storage
//! type left.
//!
//! The rule under test: when the covering runs already show the span present
//! and *not* std with some feature off, that feature is the answer — and a
//! feature the span is never observed without is not.

use nostd::phases::{classify_spans, feature_explaining_std};
use nostd::types::{
    CoveringRun, FeatureRunOutput, PathContext, PathRecord, ReadableSpan, SpanAnalysis, SpanVerdict,
};

fn span_at(line: usize, usage_crate: Option<&str>) -> ReadableSpan {
    ReadableSpan {
        file: "src/si/angle.rs".to_string(),
        start_line: line,
        start_col: 0,
        end_line: line,
        end_col: 10,
        usage_crate: usage_crate.map(String::from),
    }
}

fn record(path_text: &str, usage_crate: &str, line: usize) -> PathRecord {
    PathRecord {
        path_text: path_text.to_string(),
        definition_crate: usage_crate.to_string(),
        context: PathContext::Other,
        span: span_at(line, Some(usage_crate)),
        local_route: None,
        defining_module: None,
        macro_body_cfgs: vec![],
        is_extern_crate: false,
        gateway_anchor: None,
    }
}

/// `crate::lib::f64::consts::PI` with uom's `std` feature on: the re-export
/// resolves to std.
fn as_std(line: usize) -> PathRecord {
    record("std::f64::consts::PI", "std", line)
}

/// The same source position with `std` off: the same re-export resolves to core.
fn as_core(line: usize) -> PathRecord {
    record("core::f64::consts::PI", "core", line)
}

fn run(features: &[&str], records: Vec<PathRecord>) -> CoveringRun {
    CoveringRun {
        features: features.iter().map(|s| s.to_string()).collect(),
        output: FeatureRunOutput {
            records,
            macro_module_imports: vec![],
            out_dir: None,
        },
        std_inconclusive: false,
    }
}

fn only(analyses: Vec<SpanAnalysis>) -> SpanAnalysis {
    assert_eq!(analyses.len(), 1, "expected exactly one span");
    analyses.into_iter().next().unwrap()
}

/// The uom shape. The span sits inside `si` and `f32`, so it is absent whenever
/// either is off and present in every run that has both — which is what stops
/// them qualifying. `std` is the only feature the runs show it present without.
#[test]
fn blames_the_feature_the_resolution_flips_on_not_the_one_containing_the_code() {
    let runs = vec![
        run(&["si", "f32", "std"], vec![as_std(38)]),
        run(&["si", "f32"], vec![as_core(38)]),
        // f32 off: the storage module is not compiled, so the span is absent.
        run(&["si", "f64"], vec![]),
    ];

    let a = only(classify_spans(&runs));
    assert!(
        matches!(a.verdict, SpanVerdict::Conditional { .. }),
        "expected a conditional span, got {:?}",
        a.verdict
    );

    assert_eq!(feature_explaining_std(&a), Some("std".to_string()));
}

/// The same span with more storage types enabled around it. `f64` varies across
/// the runs but never with the span non-std, so it must not be picked up; the
/// answer is still `std`.
#[test]
fn a_feature_that_merely_varies_is_not_the_answer() {
    let runs = vec![
        run(&["si", "f32", "f64", "std"], vec![as_std(38)]),
        run(&["si", "f32", "std"], vec![as_std(38)]),
        run(&["si", "f32", "f64"], vec![as_core(38)]),
    ];

    assert_eq!(
        feature_explaining_std(&only(classify_spans(&runs))),
        Some("std".to_string())
    );
}

/// Absence is not evidence. The span never appears outside the std runs, so
/// nothing here says which feature carries the std-ness — the probe, which
/// compiles, keeps the question.
#[test]
fn a_span_only_ever_absent_elsewhere_gets_no_attribution() {
    let runs = vec![
        run(&["si", "f32", "std"], vec![as_std(38)]),
        run(&["si", "f64"], vec![]),
    ];

    let a = only(classify_spans(&runs));
    assert!(
        matches!(a.verdict, SpanVerdict::Conditional { .. }),
        "expected a conditional span, got {:?}",
        a.verdict
    );
    assert_eq!(feature_explaining_std(&a), None);
}

/// Two features are switched together in every run, so the runs cannot say
/// which one carries the std-ness. Falling back to the probe is the point: a
/// coin flip here would strip whichever one it landed on.
#[test]
fn an_ambiguous_pair_falls_back_to_the_probe() {
    let runs = vec![
        run(&["si", "f32", "std", "serde"], vec![as_std(38)]),
        run(&["si", "f32"], vec![as_core(38)]),
    ];

    assert_eq!(feature_explaining_std(&only(classify_spans(&runs))), None);
}

/// wg 0.9.2. `src/sync.rs`'s `Mutex` is `std::sync::Mutex` exactly when
/// `parking_lot` is off — and `triomphe` was on in each of those runs, purely
/// because the covering sets are chosen to cover items rather than to vary one
/// feature at a time. Blaming `triomphe` cost wg its whole emitted feature list
/// (`alloc,parking_lot,triomphe` → nothing) and put `extern crate std` back in
/// the build. The run with both features on is what disqualifies it.
#[test]
fn a_merely_correlated_feature_is_rejected() {
    let runs = vec![
        run(&["triomphe"], vec![as_std(42)]),
        run(&["triomphe", "parking_lot"], vec![record("parking_lot::Mutex", "parking_lot", 42)]),
        run(&["parking_lot"], vec![record("parking_lot::Mutex", "parking_lot", 42)]),
    ];

    let a = only(classify_spans(&runs));
    assert!(
        matches!(a.verdict, SpanVerdict::Conditional { .. }),
        "expected a conditional span, got {:?}",
        a.verdict
    );
    assert_eq!(
        feature_explaining_std(&a),
        None,
        "'triomphe' is on in every std run but also in a non-std one — correlation, not the cause"
    );
}

/// The polarity this rule deliberately cannot express: wg's span is std when
/// `parking_lot` is *off*. The condition emitted is always `¬feature`, so the
/// answer here would have the wrong sign; the probe keeps the question.
#[test]
fn a_feature_whose_absence_brings_std_is_left_to_the_probe() {
    let runs = vec![
        run(&[], vec![as_std(42)]),
        run(&["parking_lot"], vec![record("parking_lot::Mutex", "parking_lot", 42)]),
    ];

    assert_eq!(feature_explaining_std(&only(classify_spans(&runs))), None);
}

/// The honest case for blaming a containing feature is still reachable: here
/// `backend` is what the span is std under, and the runs show it present and
/// core with `backend` off because another arm supplies the item.
#[test]
fn a_feature_that_really_carries_std_is_still_blamed() {
    let runs = vec![
        run(&["backend"], vec![as_std(38)]),
        run(&["fallback"], vec![as_core(38)]),
    ];

    assert_eq!(
        feature_explaining_std(&only(classify_spans(&runs))),
        Some("backend".to_string())
    );
}

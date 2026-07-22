#![feature(rustc_private)]

//! Spans that carry std *and* non-std records at the same source position.
//!
//! A `#[derive(...)]` attribute span collects the whole macro expansion under
//! one position, so one source location routinely emits a std record next to
//! several core ones. `ReadableSpan`'s `PartialEq`/`Hash` ignore `usage_crate`
//! (see `types.rs`), so those records collide onto a single key in
//! `classify_spans`.
//!
//! The rule under test: a co-located non-std record **from the same run** is
//! not evidence that the std-ness is avoidable. Only "there exists a successful
//! run in which this span produced no std record" is.

use nostd::phases::classify_spans;
use nostd::types::{
    CoveringRun, FeatureRunOutput, PathContext, PathRecord, ReadableSpan, SpanVerdict,
};

fn span_at(line: usize, usage_crate: Option<&str>) -> ReadableSpan {
    ReadableSpan {
        file: "src/lib.rs".to_string(),
        start_line: line,
        start_col: 0,
        end_line: line,
        end_col: 10,
        usage_crate: usage_crate.map(String::from),
    }
}

fn record(
    path_text: &str,
    usage_crate: &str,
    line: usize,
    context: PathContext,
    local_route: Option<&str>,
) -> PathRecord {
    PathRecord {
        path_text: path_text.to_string(),
        definition_crate: usage_crate.to_string(),
        context,
        span: span_at(line, Some(usage_crate)),
        local_route: local_route.map(String::from),
        defining_module: None,
        macro_body_cfgs: vec![],
        is_extern_crate: false,
        gateway_anchor: None,
    }
}

/// A std usage record at `line`, in the shape a derive expansion produces.
fn std_usage(line: usize) -> PathRecord {
    record(
        "std::error::Error",
        "std",
        line,
        PathContext::Other,
        None,
    )
}

/// A core record co-located with it — same position, different crate.
fn core_usage(line: usize) -> PathRecord {
    record("core::fmt::Display", "core", line, PathContext::Other, None)
}

fn run(features: &[&str], records: Vec<PathRecord>) -> CoveringRun {
    CoveringRun {
        features: features.iter().map(|s| s.to_string()).collect(),
        output: FeatureRunOutput {
            records,
            macro_module_imports: vec![],
            out_dir: None,
        },
    }
}

fn only(analyses: Vec<nostd::types::SpanAnalysis>) -> nostd::types::SpanAnalysis {
    assert_eq!(analyses.len(), 1, "expected exactly one span");
    analyses.into_iter().next().unwrap()
}

/// `std_configs` holds one entry per *record*, so a single run contributing two
/// std records at one span looks identical to two runs contributing one each.
/// The verdict must key on run identity instead — here run 1 never saw std at
/// this span, so the span is avoidable however many records run 0 piled up.
#[test]
fn std_in_every_run_tracks_run_identity_not_record_count() {
    let runs = vec![
        run(&["std"], vec![std_usage(9), std_usage(9)]),
        run(&["alloc"], vec![core_usage(9)]),
    ];

    let a = only(classify_spans(&runs));

    assert_eq!(a.std_configs.len(), 2, "two std records were seen");
    assert!(
        !a.std_in_every_run,
        "run 1 produced no std record here, so std-ness is avoidable"
    );
    assert!(
        matches!(a.verdict, SpanVerdict::Conditional { .. }),
        "got {:?}",
        a.verdict
    );
}

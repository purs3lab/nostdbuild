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
        std_inconclusive: false,
    }
}

fn only(analyses: Vec<nostd::types::SpanAnalysis>) -> nostd::types::SpanAnalysis {
    assert_eq!(analyses.len(), 1, "expected exactly one span");
    analyses.into_iter().next().unwrap()
}

/// The core case: one run, a std record and a core record at the same position.
/// No run exists in which this span is std-free, so it is unavoidably std —
/// the co-located core record says nothing about that.
#[test]
fn collision_that_is_std_in_every_run_is_always_std() {
    let runs = vec![run(&["std"], vec![core_usage(9), std_usage(9)])];

    let a = only(classify_spans(&runs));

    assert!(a.std_in_every_run);
    assert!(
        matches!(a.verdict, SpanVerdict::AlwaysStd),
        "co-located non-std records must not excuse the span, got {:?}",
        a.verdict
    );
}

/// Control for the above: the same collision, but one covering run produced no
/// std record here. That run *is* a working std-free configuration, so the span
/// stays avoidable and keeps feeding the conditional-probe path.
#[test]
fn collision_with_a_std_free_run_stays_conditional() {
    let runs = vec![
        run(&["std"], vec![core_usage(9), std_usage(9)]),
        run(&["alloc"], vec![core_usage(9)]),
    ];

    let a = only(classify_spans(&runs));

    assert!(!a.std_in_every_run);
    assert_eq!(
        a.verdict,
        SpanVerdict::Conditional {
            alternate_crates: vec!["core".to_string()]
        }
    );
}

/// Alternate crates are deduped and sorted, and std is never among them —
/// pinned because the collection moved from a post-hoc rescan of every record
/// in every run into the indexing loop.
#[test]
fn alternate_crates_are_deduped_and_sorted() {
    let runs = vec![
        run(
            &["std"],
            vec![
                std_usage(9),
                core_usage(9),
                record("hashbrown::HashMap", "hashbrown", 9, PathContext::Other, None),
            ],
        ),
        run(&["alloc"], vec![core_usage(9)]),
    ];

    let a = only(classify_spans(&runs));

    assert_eq!(
        a.verdict,
        SpanVerdict::Conditional {
            alternate_crates: vec!["core".to_string(), "hashbrown".to_string()]
        }
    );
}

/// The exemplar routes the span in phase 3: its `context` decides whether the
/// span is probed as an import (under a `PathContext::ImportDeclaration`
/// filter) or as a plain usage, and `is_local_reexport` reads its
/// `local_route`. Picking whichever record happened to land first would send a
/// std usage down the import path, where the filtered candidate list can never
/// contain it — and it would be excused as "the span disappeared". So on a
/// collision the exemplar must be a std record, whatever the record order.
#[test]
fn exemplar_is_the_std_record_on_a_collision() {
    let core_import = record(
        "core::fmt::Display",
        "core",
        9,
        PathContext::ImportDeclaration,
        Some("crate::fmt"),
    );

    for (order, records) in [
        ("core first", vec![core_import.clone(), std_usage(9)]),
        ("std first", vec![std_usage(9), core_import.clone()]),
    ] {
        let a = only(classify_spans(&vec![run(&["std"], records)]));

        assert_eq!(
            a.exemplar.span.usage_crate.as_deref(),
            Some("std"),
            "{order}: exemplar must be the std record"
        );
        assert_eq!(
            a.exemplar.context,
            PathContext::Other,
            "{order}: exemplar carries the std record's context"
        );
        assert!(
            a.exemplar.local_route.is_none(),
            "{order}: exemplar carries the std record's local_route"
        );
    }
}

/// Regression guard for the arm the fix did not touch: a span seen only as std,
/// but absent from one run, is still avoidable and still Conditional — with no
/// alternate crates, because it was absent there rather than resolved elsewhere.
#[test]
fn std_only_span_missing_from_a_run_is_still_conditional() {
    let runs = vec![
        run(&["std"], vec![std_usage(9)]),
        run(&["alloc"], vec![core_usage(20)]),
    ];

    let a = classify_spans(&runs)
        .into_iter()
        .find(|a| a.span.start_line == 9)
        .expect("span 9 classified");

    assert_eq!(
        a.verdict,
        SpanVerdict::Conditional {
            alternate_crates: vec![]
        }
    );
}

/// And the other end: no std record anywhere is NeverStd, collision or not.
#[test]
fn span_without_std_records_is_never_std() {
    let runs = vec![run(&["alloc"], vec![core_usage(9)])];

    let a = only(classify_spans(&runs));

    assert!(!a.std_in_every_run);
    assert_eq!(a.verdict, SpanVerdict::NeverStd);
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

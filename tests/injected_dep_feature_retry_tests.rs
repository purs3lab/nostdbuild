#![feature(rustc_private)]

//! The candidate set and the scout target behind the R34-11 build retry.
//!
//! `custom_no_std_feature_enabled` holds the `<dep>/<feat>` pairs a dependency's
//! own *isolated* solve asked for and no feature of the main crate could reach.
//! Every name in it is a real feature of the dependency — that is what keeps it
//! out of `solver::retain_selectable_features`, which drops only the names cargo
//! cannot accept at all — and it is the combination with the rest of the graph
//! that is wrong. `repr_offset 0.2.2` gets `tstr/cmp_traits` and
//! `tstr/const_generics`, both real, and `const_generics` selects a `&'static str`
//! const generic parameter the compiler has since forbidden. Nothing short of a
//! build sees that, which is why this is a retry and not a check: 847 crates that
//! build today carry an injected set.
//!
//! The end-to-end half of this guard is `main_tests::test_repr_offset`, which
//! records the retry's config as the emitted one. What is here is the two pieces
//! that decide *what* gets retried and *where* it is judged.

use nostd::compiler::{mark_build_records, scout_target};
use nostd::parser::without_injected_dep_features;
use nostd::{AllStats, Results, Status, Telemetry, consts};

fn failed(target: &str, error: &str) -> Results {
    Results {
        name: "fixture".to_string(),
        version: "0.0.0".to_string(),
        target: target.to_string(),
        args: Vec::new(),
        status: Status::Failed,
        error: Some(format!(
            "Cargo failed with status code: 101 and message:    Compiling fixture v0.0.0\n{error}\nerror: could not compile `fixture` (lib) due to 1 previous error\n"
        )),
    }
}

fn built(target: &str) -> Results {
    Results {
        name: "fixture".to_string(),
        version: "0.0.0".to_string(),
        target: target.to_string(),
        args: Vec::new(),
        status: Status::Success,
        error: None,
    }
}

/// The key is dropped and everything else on the dependency command line stays.
/// The pairs themselves live in the manifest, never on the command line, so this
/// is the whole edit — dropping the name is exactly emptying the key.
#[test]
fn the_injected_key_is_the_only_thing_dropped() {
    let deps_args = vec![
        "alloc".to_string(),
        consts::CUSTOM_FEATURES_ENABLED.to_string(),
        "libm".to_string(),
    ];
    assert_eq!(
        without_injected_dep_features(&deps_args),
        Some(vec!["alloc".to_string(), "libm".to_string()])
    );
}

/// No key, no candidate. An empty injected set is never put on the command line
/// (`final_feature_list_dep` pushes the key only for a non-empty `not_found`), so
/// its absence means there is nothing this retry could change — and a retry that
/// re-runs the identical build is a whole verification spent on nothing.
#[test]
fn a_command_line_without_the_key_has_nothing_to_retry() {
    let deps_args = vec!["alloc".to_string(), "libm".to_string()];
    assert_eq!(without_injected_dep_features(&deps_args), None);
    assert_eq!(without_injected_dep_features(&[]), None);
}

/// The candidate can be empty, and that is a real configuration rather than a
/// missing one: `repr_offset 0.2.2` emits `--features custom_no_std_feature_enabled`
/// and nothing else, so what builds is a command line with no `--features` at all.
/// `Some(vec![])` and `None` are the two answers that must not be confused here.
#[test]
fn dropping_the_only_argument_leaves_an_empty_candidate() {
    let deps_args = vec![consts::CUSTOM_FEATURES_ENABLED.to_string()];
    assert_eq!(without_injected_dep_features(&deps_args), Some(Vec::new()));
}

/// The scout is the target carrying the *modal* first error, not the first target
/// recorded. `etime-0.1.8` is the shape: 25 targets fail on `E0432 unresolved
/// import time_clock` and one on `E0463 can't find crate for std`, and only the
/// first says anything about the feature set. Judging the candidate on the odd one
/// out throws away the retry for a reason unrelated to what it changed.
#[test]
fn the_scout_is_the_target_with_the_modal_error() {
    let mut stats = AllStats::new("fixture".to_string());
    let telemetry = Telemetry::default();
    let mark = mark_build_records(&stats, &telemetry);

    stats.compilation_res.push(failed(
        "aarch64-unknown-none",
        "error[E0463]: can't find crate for `std`",
    ));
    for target in ["thumbv6m-none-eabi", "thumbv7em-none-eabi", "wasm32v1-none"] {
        stats.compilation_res.push(failed(
            target,
            "error[E0432]: unresolved import `time_clock`",
        ));
    }

    assert_eq!(
        scout_target(&stats, &mark).as_deref(),
        Some("thumbv6m-none-eabi")
    );
}

/// Rows recorded before the mark belong to an earlier attempt and must not vote.
/// Every retry is judged against the failure of the build it is replacing.
#[test]
fn only_failures_recorded_since_the_mark_are_counted() {
    let mut stats = AllStats::new("fixture".to_string());
    let telemetry = Telemetry::default();

    for _ in 0..5 {
        stats.compilation_res.push(failed(
            "aarch64-unknown-none",
            "error[E0463]: can't find crate for `std`",
        ));
    }
    let mark = mark_build_records(&stats, &telemetry);
    stats.compilation_res.push(failed(
        "thumbv7em-none-eabi",
        "error[E0432]: unresolved import `time_clock`",
    ));

    assert_eq!(
        scout_target(&stats, &mark).as_deref(),
        Some("thumbv7em-none-eabi")
    );
}

/// A target that built is not a failure to scout against. This cannot arise from
/// the retry's own guard — it fires only when nothing built — but `scout_target`
/// is the thing that decides where a build is spent, so it answers off failures
/// or not at all.
#[test]
fn successes_do_not_vote_and_no_failure_means_no_scout() {
    let mut stats = AllStats::new("fixture".to_string());
    let telemetry = Telemetry::default();
    let mark = mark_build_records(&stats, &telemetry);

    stats.compilation_res.push(built("x86_64-unknown-none"));
    stats.compilation_res.push(built("thumbv7em-none-eabi"));
    assert_eq!(scout_target(&stats, &mark), None);

    stats.compilation_res.push(failed(
        "riscv32i-unknown-none-elf",
        "error[E0432]: unresolved import `time_clock`",
    ));
    assert_eq!(
        scout_target(&stats, &mark).as_deref(),
        Some("riscv32i-unknown-none-elf")
    );
}

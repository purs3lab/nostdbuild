use anyhow::Context;
use log::debug;

use crate::{AllStats, Results, Status, Telemetry, consts, parser, timing};

/// Position of the build bookkeeping before a `try_compile` call, so a
/// speculative attempt can be taken back out again.
///
/// `try_compile` appends one `Results` row per target, plus a target entry in
/// the telemetry's success/fail lists. When a crate is built more than once —
/// the KI-11 retry drops optional-dep-only features and builds again — only the
/// attempt whose feature set we actually emit may leave records behind.
/// Otherwise `compilation_results.json` reports two different `args` for the
/// same target and nothing downstream can tell which one is the answer.
#[derive(Clone, Copy)]
pub struct BuildRecordMark {
    results: usize,
    success_targets: usize,
    success_count: u32,
    fail_targets: usize,
}

/// Snapshot the build bookkeeping before a speculative `try_compile`.
pub fn mark_build_records(stats: &AllStats, telemetry: &Telemetry) -> BuildRecordMark {
    BuildRecordMark {
        results: stats.compilation_res.len(),
        success_targets: telemetry.build_success_targets.len(),
        success_count: telemetry.build_success_count,
        fail_targets: telemetry.build_fail_targets.len(),
    }
}

/// Discard everything recorded since `mark` — used when a speculative attempt
/// loses and the caller keeps the earlier feature set.
pub fn rewind_build_records(stats: &mut AllStats, telemetry: &mut Telemetry, mark: &BuildRecordMark) {
    stats.compilation_res.truncate(mark.results);
    telemetry.build_success_targets.truncate(mark.success_targets);
    telemetry.build_fail_targets.truncate(mark.fail_targets);
    telemetry.build_success_count = mark.success_count;
}

/// Discard the records written between `from` and `to`, keeping anything after
/// `to` — used when a later attempt wins and supersedes an earlier one.
pub fn discard_build_records(
    stats: &mut AllStats,
    telemetry: &mut Telemetry,
    from: &BuildRecordMark,
    to: &BuildRecordMark,
) {
    stats.compilation_res.drain(from.results..to.results);
    telemetry
        .build_success_targets
        .drain(from.success_targets..to.success_targets);
    telemetry
        .build_fail_targets
        .drain(from.fail_targets..to.fail_targets);
    telemetry.build_success_count -= to.success_count - from.success_count;
}

/// The first `error…` line of a failed build's captured output, as a grouping
/// key. `Results::error` opens with this tool's own "Cargo failed with status
/// code" line, which never matches, so what comes back is rustc's own first
/// diagnostic. `""` when the output carries none.
fn first_error_key(message: &str) -> &str {
    message
        .lines()
        .map(str::trim)
        .find(|line| line.starts_with("error"))
        .unwrap_or("")
}

/// The one target to judge a speculative feature set on before the whole list is
/// paid for.
///
/// A retry that clears `TARGET_LIST` costs a full verification per candidate —
/// 24.6 h across the 336 corpus rows that carry an injected dependency set and
/// have a timing record. One target costs 1/26 of that, and the A/B evidence
/// behind R34-11 was itself produced on a single target (`thumbv7em-none-eabi`,
/// 89 of 93 rows). What is bought with the other 25 is coverage of a candidate
/// that repairs some targets and not this one; a retry is kept only when it
/// builds, so the cost of guessing wrong is a row that stays failed, which is
/// where it already was.
///
/// The target picked is the one carrying the **modal** first error among the
/// failures recorded since `mark`, so the candidate is judged against the
/// failure the crate actually has and not against whichever target sorts first:
/// `etime-0.1.8` fails 25 targets on `E0432 unresolved import time_clock` and
/// one on `E0463 can't find crate for std`, and only the first says anything
/// about its feature set. Ties go to the earlier target, which is `TARGET_LIST`
/// order because that is the order the rows were appended in.
///
/// `None` when nothing failed since `mark` — there is then no failure to scout
/// against and the caller should build the full list.
pub fn scout_target(stats: &AllStats, mark: &BuildRecordMark) -> Option<String> {
    let failures: Vec<(&str, &str)> = stats.compilation_res[mark.results..]
        .iter()
        .filter(|res| matches!(res.status, Status::Failed))
        .map(|res| {
            (
                res.target.as_str(),
                first_error_key(res.error.as_deref().unwrap_or("")),
            )
        })
        .collect();

    // `min_by_key` on the negated count, not `max_by_key`: the latter returns the
    // *last* maximum, which would hand a tie to the target furthest down
    // `TARGET_LIST` instead of the first one recorded.
    failures
        .iter()
        .min_by_key(|(_, key)| {
            std::cmp::Reverse(failures.iter().filter(|(_, other)| other == key).count())
        })
        .map(|(target, _)| target.to_string())
}

/// Run one speculative feature set after a build that failed on every target,
/// and leave exactly one attempt's records behind.
///
/// The retries in `bin/main.rs` all do the same bookkeeping around
/// `try_compile` — mark, build, then either drop the losing first attempt's rows
/// or take this attempt's back out again — and differ only in the feature set
/// they try and in what the caller does with a win. `compilation_results.json`
/// reports one `args` per target, so that bookkeeping has to be the same every
/// time, which is the reason it lives here rather than at each call.
///
/// `before_build` marks the first, failed attempt. `scout` judges the candidate
/// on one target before the full list is paid for (see `scout_target`); `None`
/// builds the full list straight away, which is what a retry whose evidence
/// comes from whole-list builds wants. Either way a scout leaves no records:
/// the attempt that ships is the one that built the full list.
pub fn try_alternative(
    name_with_version: &str,
    clitarget: &str,
    args: &[String],
    attempt: &str,
    before_build: &BuildRecordMark,
    scout: Option<&str>,
    stats: &mut AllStats,
    telemetry: &mut Telemetry,
) -> anyhow::Result<bool> {
    // Nothing to save when the caller already asked for a single target: the
    // scout build *is* the whole verification.
    if clitarget.is_empty()
        && let Some(scout) = scout
    {
        let before_scout = mark_build_records(stats, telemetry);
        let scout_built = {
            let t = timing::scope("verify_build", name_with_version);
            t.meta("attempt", format!("{attempt}_scout"));
            try_compile(name_with_version, scout, args, stats, telemetry)?
        };
        rewind_build_records(stats, telemetry, &before_scout);
        if !scout_built {
            debug!("Candidate {attempt} did not build for scout target {scout}");
            return Ok(false);
        }
    }

    let before_attempt = mark_build_records(stats, telemetry);
    let built = {
        let t = timing::scope("verify_build", name_with_version);
        t.meta("attempt", attempt);
        try_compile(name_with_version, clitarget, args, stats, telemetry)?
    };
    if built {
        // The retry is the emitted config now, so the failed attempt's rows are
        // dropped — one feature set per target in the results.
        discard_build_records(stats, telemetry, before_build, &before_attempt);
    } else {
        rewind_build_records(stats, telemetry, &before_attempt);
    }
    Ok(built)
}

pub fn try_compile(
    name_with_version: &str,
    clitarget: &str,
    enable: &[String],
    stats: &mut AllStats,
    telemetry: &mut Telemetry,
) -> anyhow::Result<bool> {
    let mut one_succeeded = false;
    if !clitarget.is_empty() {
        try_compile_for_target(
            name_with_version,
            clitarget,
            enable,
            stats,
            &mut one_succeeded,
            telemetry,
        )?;
        return Ok(one_succeeded);
    }

    for target in consts::TARGET_LIST.iter() {
        try_compile_for_target(
            name_with_version,
            target,
            enable,
            stats,
            &mut one_succeeded,
            telemetry,
        )?;
    }
    Ok(one_succeeded)
}

fn try_compile_for_target(
    name_with_version: &str,
    target: &str,
    enable: &[String],
    stats: &mut AllStats,
    one_succeeded: &mut bool,
    telemetry: &mut Telemetry,
) -> anyhow::Result<()> {
    let manifest = parser::determine_manifest_file(name_with_version, None);
    let bin_target = parser::toml_has_bin_target(&manifest);
    let mut args = vec![
        "+nightly",
        "build",
        "--release",
        "--target",
        target,
        "--manifest-path",
        manifest.as_str(),
    ];

    if !bin_target {
        args.push("--lib");
    }

    if !enable.is_empty() {
        args.extend(enable.iter().map(|s| s.as_str()).collect::<Vec<&str>>());
    }

    debug!("Running cargo with args: {}", args.join(" "));
    let build = timing::scope("verify_target", target);
    let output = std::process::Command::new("cargo")
        .args(&args)
        .output()
        .context("Failed to run cargo")?;
    build.meta("success", output.status.success().to_string());
    drop(build);

    let (name, version) = name_with_version.split_once(':').unwrap_or(("", ""));
    let result = Results {
        name: name.to_string(),
        version: version.to_string(),
        target: target.to_string(),
        args: args.iter().map(|s| s.to_string()).collect(),
        status: if output.status.success() {
            if !*one_succeeded {
                *one_succeeded = true;
            }
            telemetry.build_success_targets.push(target.to_string());
            telemetry.build_success_count += 1;
            Status::Success
        } else {
            telemetry.build_fail_targets.push(target.to_string());
            Status::Failed
        },
        error: if output.status.success() {
            None
        } else {
            Some(format!(
                "Cargo failed with status code: {} and message: {}",
                output.status.code().unwrap_or(-1),
                String::from_utf8_lossy(&output.stderr)
            ))
        },
    };
    debug!("Cargo build {:?} for target: {}", &result.status, target);
    stats.compilation_res.push(result);
    // Timed too: one `clean` per target is a real part of the verification cost,
    // and it is invisible in the build numbers it sits between.
    let _clean = timing::scope("cargo_clean", target);
    std::process::Command::new("cargo")
        .arg("+nightly")
        .arg("clean")
        .arg("--manifest-path")
        .arg(manifest)
        .status()
        .context("Failed to run cargo clean")?;
    Ok(())
}

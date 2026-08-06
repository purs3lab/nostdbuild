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

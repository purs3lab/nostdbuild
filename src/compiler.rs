use anyhow::Context;
use log::debug;

use crate::{consts, parser, Results, Status};

pub fn try_compile(
    name_with_version: &str,
    clitarget: &str,
    enable: &[String],
    possible_archs: &[String],
    results: &mut Vec<Results>,
) -> anyhow::Result<()> {
    if !clitarget.is_empty() {
        try_compile_for_target(name_with_version, clitarget, enable, results)?;
        return Ok(());
    }

    if possible_archs.is_empty() {
        for target in consts::TARGET_LIST.iter() {
            try_compile_for_target(name_with_version, target, enable, results)?;
        }
    } else {
        for arch in possible_archs.iter() {
            let targets = consts::get_target_names_from_arch(arch);
            if !targets.is_empty() {
                for target in targets.iter() {
                    try_compile_for_target(name_with_version, target, enable, results)?;
                }
            } else {
                return Err(anyhow::anyhow!(
                    "Invalid arch `{}`. Choose target from one of {:?}",
                    arch,
                    consts::TARGET_LIST
                ));
            }
        }
    }
    Ok(())
}

fn try_compile_for_target(
    name_with_version: &str,
    target: &str,
    enable: &[String],
    results: &mut Vec<Results>,
) -> anyhow::Result<()> {
    let cargo_path = parser::determine_cargo_toml(&name_with_version);
    let bin_target = parser::toml_has_bin_target(&cargo_path);
    let mut args = vec![
        "+nightly",
        "build",
        "--release",
        "--target",
        target,
        "--manifest-path",
        cargo_path.as_str(),
    ];

    if !bin_target {
        args.push("--lib");
    }

    if !enable.is_empty() {
        args.extend(enable.iter().map(|s| s.as_str()).collect::<Vec<&str>>());
    }

    debug!("Running cargo with args: {}", args.join(" "));
    let output = std::process::Command::new("cargo")
        .args(&args)
        .output()
        .context("Failed to run cargo")?;

    let (name, version) = name_with_version.split_once(':').unwrap_or(("", ""));
    let result = Results {
        name: name.to_string(),
        version: version.to_string(),
        target: target.to_string(),
        args: args.iter().map(|s| s.to_string()).collect(),
        status: if output.status.success() {
            Status::Success
        } else {
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
    results.push(result);
    std::process::Command::new("cargo")
        .arg("+nightly")
        .arg("clean")
        .arg("--manifest-path")
        .arg(cargo_path)
        .status()
        .context("Failed to run cargo clean")?;
    Ok(())
}

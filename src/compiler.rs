use crate::consts::DOWNLOAD_PATH;
use anyhow::Context;
use log::debug;
use std::path::Path;

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
    let dir = Path::new(DOWNLOAD_PATH).join(name_with_version.replace(':', "-"));
    let mut args = vec![
        "build".to_string(),
        "--release".to_string(),
        "--target".to_string(),
        target.to_string(),
        "--manifest-path".to_string(),
        parser::determine_cargo_toml(&dir),
    ];
    if !enable.is_empty() {
        args.extend_from_slice(enable);
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
        args: args.clone(),
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
        .arg("clean")
        .arg("--manifest-path")
        .arg(parser::determine_cargo_toml(&dir))
        .status()
        .context("Failed to run cargo clean")?;
    Ok(())
}

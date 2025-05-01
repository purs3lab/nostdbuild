use crate::consts::DOWNLOAD_PATH;
use anyhow::Context;
use log::debug;
use std::path::Path;

use crate::consts;

pub fn try_compile(
    name_with_version: &str,
    clitarget: &str,
    enable: &[String],
    possible_archs: &[String],
) -> anyhow::Result<()> {
    if !clitarget.is_empty() {
        try_compile_for_target(name_with_version, clitarget, enable)?;
        return Ok(());
    }

    if possible_archs.is_empty() {
        for target in consts::TARGET_LIST.iter() {
            try_compile_for_target(name_with_version, target, enable)?;
        }
    } else {
        for arch in possible_archs.iter() {
            let targets = consts::get_target_names_from_arch(arch);
            if !targets.is_empty() {
                for target in targets.iter() {
                    try_compile_for_target(name_with_version, target, enable)?;
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
) -> anyhow::Result<()> {
    let dir = Path::new(DOWNLOAD_PATH).join(name_with_version.replace(':', "-"));
    let mut args = vec![
        "build".to_string(),
        "--release".to_string(),
        "--target".to_string(),
        target.to_string(),
        "--manifest-path".to_string(),
        dir.join("Cargo.toml").to_str().unwrap().to_string(),
    ];
    if !enable.is_empty() {
        args.extend_from_slice(enable);
    }
    debug!("Running cargo with args: {:?}", args);
    let status = std::process::Command::new("cargo")
        .args(&args)
        .status()
        .context("Failed to run cargo")?;
    if !status.success() {
        return Err(anyhow::anyhow!(
            "Cargo failed with status code: {}",
            status.code().unwrap_or(-1)
        ));
    }
    debug!("Cargo build succeeded for target: {}", target);
    Ok(())
}

use crate::consts::DOWNLOAD_PATH;
use anyhow::Context;
use std::path::Path;
use log::debug;

use crate::consts;

pub fn try_compile(
    name_with_version: &str,
    enable: &[String],
    possible_archs: &[String],
) -> anyhow::Result<()> {
    if possible_archs.is_empty() {
        for target in consts::TARGET_LIST.iter() {
            try_compile_for_target(name_with_version, target, enable)?;
        }
    } else {
        todo!("Get the possible targets using the arch specified");
        for target in possible_archs.iter() {
            if consts::is_valid_target(target) {
                try_compile_for_target(name_with_version, target, enable)?;
            } else {
                return Err(anyhow::anyhow!(
                    "Invalid target `{}`. Choose one of {:?}",
                    target,
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
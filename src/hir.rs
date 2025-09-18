use log::debug;
use std::{fs, path, process::Command};
use which::which;

use crate::consts;
use crate::parser;

pub fn do_first_pass(crate_name: &str) {
    if !is_cargo_hir_installed() {
        panic!(
            "cargo-hir is not installed. Please install it by running `cargo install --path . --bin cargo-hir --bin hir-driver --force` from the root of the repository"
        );
    }

    let manifest = parser::determine_manifest_file(crate_name);

    if path::Path::new(&consts::HIR_VISITOR_SPAN_DUMP).exists() {
        fs::remove_file(consts::HIR_VISITOR_SPAN_DUMP)
            .expect("Failed to remove existing HIR visitor span dump file");
    }

    let output = Command::new("cargo")
        .arg("clean")
        .arg("--manifest-path")
        .arg(&manifest)
        .output()
        .expect("Failed to run cargo clean");

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        debug!(
            "cargo clean failed with status code: {} and message: {}",
            output.status.code().unwrap_or(-1),
            stderr
        );
    }

    let args = vec!["hir", "--", "--manifest-path", &manifest];
    let output = Command::new("cargo")
        .args(&args)
        .output()
        .expect("Failed to run cargo-hir");

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        debug!(
            "cargo-hir failed with status code: {} and message: {}",
            output.status.code().unwrap_or(-1),
            stderr
        );
    }
}

fn is_cargo_hir_installed() -> bool {
    which("cargo-hir").is_ok()
}

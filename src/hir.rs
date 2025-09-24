use log::debug;
use proc_macro2::Span;
use std::{fs, path, process::Command};
use which::which;

use crate::ReadableSpan;
use crate::consts;
use crate::parser;

pub fn hir_visit(crate_name: &str) {
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

pub fn check_for_unguarded_std_usages(spans: &[ReadableSpan]) -> bool {
    if !path::Path::new(consts::HIR_VISITOR_SPAN_DUMP).exists() {
        panic!(
            "HIR visitor span dump file does not exist. Please ensure that `cargo-hir` ran successfully."
        );
    }

    let data = fs::read_to_string(consts::HIR_VISITOR_SPAN_DUMP)
        .expect("Unable to read HIR visitor span dump file");
    let hir_spans: Vec<ReadableSpan> =
        serde_json::from_str(&data).expect("Unable to parse HIR visitor span dump file");

    debug!("Hir spans: {:?}", hir_spans);
    debug!("Proc macro spans: {:?}", spans);
    false
}

pub fn proc_macro_span_to_readable(spans: &[Span]) -> Vec<ReadableSpan> {
    spans
        .iter()
        .map(|s| ReadableSpan {
            file: s.file(),
            start_line: s.start().line,
            start_col: s.start().column,
            end_line: s.end().line,
            end_col: s.end().column,
        })
        .collect()
}

fn is_cargo_hir_installed() -> bool {
    which("cargo-hir").is_ok()
}

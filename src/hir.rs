use log::debug;
use proc_macro2::Span;
use std::{fs, path, process::Command, time::Instant};
use which::which;

use crate::{AllStats, ReadableSpan, Telemetry};
use crate::{
    consts,
    parser::{self, Attributes},
};

pub fn hir_visit(crate_name: &str, telemetry: &mut Telemetry) {
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
    let now = Instant::now();
    let output = Command::new("cargo")
        .args(&args)
        .output()
        .expect("Failed to run cargo-hir");
    telemetry.hir_driver_time_ms = now.elapsed().as_millis();

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        debug!(
            "cargo-hir failed with status code: {} and message: {}",
            output.status.code().unwrap_or(-1),
            stderr
        );
    }
}

pub fn check_for_unguarded_std_usages(
    spans: &[ReadableSpan],
    main_attributes: &Attributes,
    stats: &mut AllStats,
) -> bool {
    if !path::Path::new(consts::HIR_VISITOR_SPAN_DUMP).exists() {
        debug!(
            "ERROR:HIR visitor span dump file does not exist. Please ensure that `cargo-hir` ran successfully."
        );
        return false;
    }

    let mut patterns: Vec<String> = main_attributes
        .mods
        .iter()
        .flat_map(|m| [format!("/{m}.rs"), format!("/{m}/mod.rs")])
        .collect();

    main_attributes.files_in_cfg_attrs.iter().for_each(|f| {
        patterns.push(format!("/{f}"));
    });

    let data = fs::read_to_string(consts::HIR_VISITOR_SPAN_DUMP)
        .expect("Unable to read HIR visitor span dump file");
    let mut hir_spans: Vec<ReadableSpan> =
        serde_json::from_str(&data).expect("Unable to parse HIR visitor span dump file");

    hir_spans.retain(|span| !patterns.iter().any(|p| span.file.ends_with(p)));

    debug!("Hir spans: {:?}", hir_spans);
    debug!("Proc macro spans: {:?}", spans);

    let found: Vec<ReadableSpan> = hir_spans
        .iter()
        .filter(|hir| spans.iter().all(|s| !s.contains(hir)))
        .cloned()
        .collect();

    stats.std_usage_matches.extend(found.clone());

    !found.is_empty()
}

pub fn proc_macro_span_to_readable(spans: &[(Span, Option<String>)]) -> Vec<ReadableSpan> {
    spans
        .iter()
        .map(|(s, name)| ReadableSpan {
            file: name.clone().unwrap_or_else(|| "unknown".to_string()),
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

use log::debug;
use proc_macro2::Span;
use std::path::{Path, PathBuf};
use std::{fs, path, process::Command, time::Instant};
use which::which;

use crate::{AllStats, CrateInfo, ReadableSpan, Telemetry};
use crate::{Attributes, consts, parser};

/// Runs `cargo-hir` to generate AST driver span dump for the given crate.
/// If `all_feats` is true, it builds the crate with all features enabled.
/// The generated HIR visitor span dump file is stored in the results directory
/// with the name format: `<crate_name>_hir_visitor_span_dump.json
/// # Arguments
/// * `crate_name` - The name of the crate to run `cargo-hir`
/// * `telemetry` - Optional telemetry object to record the time taken by `hir-driver`
/// * `all_feats` - Whether to build the crate with all features enabled
/// * `crate_info` - Optional crate info object containing the features of the crate. Required if `all_feats` is true.
/// # Panics
/// Panics if `cargo-hir` is not installed or if there are issues running the command
pub fn hir_visit(
    crate_name: &str,
    telemetry: Option<&mut Telemetry>,
    all_feats: bool,
    crate_info: Option<&CrateInfo>,
    main_name: Option<&str>,
) {
    if !is_cargo_hir_installed() {
        panic!(
            "cargo-hir is not installed. Please install it by running `cargo install --path . --bin cargo-hir --bin hir-driver --force` from the root of the repository"
        );
    }

    let manifest = parser::determine_manifest_file(crate_name, main_name);
    let dir = parser::get_actual_dir(crate_name, main_name);

    // Store the final per crate file
    let output_file_name = get_hir_output_file(crate_name);
    // Stores the per visit file. This is used by the vistor itself.
    let visit_file_name = get_hir_visit_file(crate_name);

    // Each iteration appends to this in-memory vector.
    let mut results: Vec<ReadableSpan> = Vec::new();

    let backup_hir_manifest = dir.join("Cargo.toml.hir");

    if Path::new(&output_file_name).exists() {
        fs::remove_file(&output_file_name)
            .expect("Failed to remove existing HIR visitor span dump file");
    }

    if Path::new(&visit_file_name).exists() {
        fs::remove_file(&visit_file_name)
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
    let mut top_level_feats: Vec<String> = Vec::new();

    if all_feats {
        // Create a copy of the manifest file
        fs::copy(&manifest, &backup_hir_manifest).expect("Failed to create a copy of Cargo.toml");

        // We also need to remove features that are enabling things for dependencies.
        let toml = fs::read_to_string(&manifest).expect("Failed to read Cargo.toml");
        let mut cargo_toml: toml::Value = toml.parse().expect("Failed to parse Cargo.toml");
        let main_features_table = cargo_toml.get_mut("features");

        if let Some(main_features_table) = main_features_table {
            main_features_table
                .as_table_mut()
                .expect("Features should be a table")
                .into_iter()
                .for_each(|(_, vals)| {
                    vals.as_array_mut()
                        .expect("Feature values should be an array")
                        .retain(|v| !v.as_str().unwrap_or_default().contains("/"));
                });

            fs::write(
                &manifest,
                toml::to_string(&cargo_toml).expect("Failed to serialize modified Cargo.toml"),
            )
            .expect("Failed to write modified Cargo.toml");
        }

        top_level_feats = crate_info
            .unwrap()
            .features
            .iter()
            .map(|(top, _)| top.clone())
            .collect();

        debug!(
            "Running cargo-hir with all features enabled: features={:?}",
            top_level_feats
        );
    }

    let now = Instant::now();

    if !top_level_feats.is_empty() {
        for feat in top_level_feats {
            let mut feat_args = args.clone();
            feat_args.push("--features");
            feat_args.push(&feat);

            debug!("Running cargo-hir with feature: {}", feat);

            Command::new("cargo")
                .args(&feat_args)
                .output()
                .expect("Failed to run cargo-hir with features");

            collect_hir_results(&visit_file_name, &mut results);
        }
    } else {
        debug!("Running cargo-hir with no features");

        Command::new("cargo")
            .args(&args)
            .output()
            .expect("Failed to run cargo-hir");

        collect_hir_results(&visit_file_name, &mut results);
    }

    if all_feats {
        // Copy the file back
        fs::copy(&backup_hir_manifest, &manifest).expect("Failed to restore original Cargo.toml");
        fs::remove_file(&backup_hir_manifest).expect("Failed to remove temporary Cargo.toml.hir");
    }

    if let Some(telemetry) = telemetry {
        telemetry.hir_driver_time_ms = now.elapsed().as_millis();
    }

    dedup_results(&mut results);

    fs::write(
        &output_file_name,
        serde_json::to_string(&results).expect("Failed to serialize HIR visitor span dump"),
    )
    .expect("Failed to write HIR visitor span dump file");

    // Cleanup
    if path::Path::new(&visit_file_name).exists() {
        fs::remove_file(&visit_file_name).expect("Failed to remove HIR visitor visit file");
    }

    debug!("cargo-hir run finished in {} ms", now.elapsed().as_millis());
}

fn collect_hir_results(from: &Path, spans: &mut Vec<ReadableSpan>) {
    if !from.exists() {
        debug!(
            "HIR visitor span dump file does not exist. Please ensure that `cargo-hir` ran successfully"
        );
        return;
    }

    let data = fs::read_to_string(from).expect("Unable to read HIR visitor span dump file");
    let new_spans: Vec<ReadableSpan> =
        serde_json::from_str(&data).expect("Unable to parse HIR visitor span dump file");

    spans.extend(new_spans);
}

fn dedup_results(results: &mut Vec<ReadableSpan>) {
    results.sort_by(|a, b| {
        a.file
            .cmp(&b.file)
            .then(a.start_line.cmp(&b.start_line))
            .then(a.start_col.cmp(&b.start_col))
            .then(a.end_line.cmp(&b.end_line))
            .then(a.end_col.cmp(&b.end_col))
    });
    results.dedup();
}

pub fn read_hir_spans(crate_name: &str) -> Vec<ReadableSpan> {
    let output_file_name = get_hir_output_file(crate_name);

    if !path::Path::new(&output_file_name).exists() {
        // This does not necessarily mean a failure. We call `parse_crate` quite early in the process,
        // and it's possible that `hir_visit` has not been called yet. So we just return an empty vector in this case.
        debug!(
            "Warning:HIR visitor span dump file does not exist. Please ensure that `cargo-hir` ran successfully."
        );
        return vec![];
    }

    let data =
        fs::read_to_string(output_file_name).expect("Unable to read HIR visitor span dump file");
    let hir_spans: Vec<ReadableSpan> =
        serde_json::from_str(&data).expect("Unable to parse HIR visitor span dump file");

    hir_spans
}

pub fn check_for_unguarded_std_usages(
    crate_name: &str,
    spans: &[ReadableSpan],
    main_attributes: &Attributes,
    stats: &mut AllStats,
) -> bool {
    let mut hir_spans = read_hir_spans(crate_name);

    let mut patterns: Vec<String> = main_attributes
        .mods
        .iter()
        .flat_map(|(m, _attr)| [format!("/{m}.rs"), format!("/{m}/mod.rs")])
        .collect();

    main_attributes.files_in_cfg_attrs.iter().for_each(|f| {
        patterns.push(format!("/{f}"));
    });

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

pub fn proc_macro_spans_to_readables(spans: &[(Span, Option<String>)]) -> Vec<ReadableSpan> {
    spans
        .iter()
        .map(|(s, name)| proc_macro_span_to_readable(s, name.clone()))
        .collect()
}

pub fn proc_macro_span_to_readable(span: &Span, file: Option<String>) -> ReadableSpan {
    ReadableSpan {
        file: file.unwrap_or_else(|| "unknown".to_string()),
        start_line: span.start().line,
        start_col: span.start().column,
        end_line: span.end().line,
        end_col: span.end().column,
    }
}

fn is_cargo_hir_installed() -> bool {
    which("cargo-hir").is_ok()
}

fn get_hir_output_file(crate_name: &str) -> PathBuf {
    construct_path(crate_name, consts::HIR_VISITOR_SPAN_DUMP_SUFFIX)
}

fn get_hir_visit_file(crate_name: &str) -> PathBuf {
    construct_path(crate_name, consts::HIR_VISITOR_VISIT_FILE_SUFFIX)
}

fn construct_path(crate_name: &str, suffix: &str) -> PathBuf {
    Path::new(consts::RESULTS_PATH).join(format!(
        "{}-{}",
        crate_name.replace("-", "_").replace(":", "-"),
        suffix
    ))
}

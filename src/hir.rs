use log::debug;
use proc_macro2::Span;
use std::{fs, path, process::Command, time::Instant};
use which::which;

use crate::{AllStats, CrateInfo, ReadableSpan, Telemetry};
use crate::{
    consts,
    parser::{self, Attributes},
};

/// Runs `cargo-hir` to generate AST driver span dump for the given crate.
/// If `all_feats` is true, it builds the crate with all features enabled.
/// The generated HIR visitor span dump file is stored in the results directory
/// with the name format: `<crate_name>_hir_visitor_span_dump.json
/// # Arguments
/// * `crate_name` - The name of the crate to run `cargo-hir`
/// * `telemetry` - Optional telemetry object to record the time taken by `hir-driver`
/// * `all_feats` - Whether to build the crate with all features enabled
/// # Panics
/// Panics if `cargo-hir` is not installed or if there are issues running the command
pub fn hir_visit(
    crate_name: &str,
    telemetry: Option<&mut Telemetry>,
    all_feats: bool,
    crate_info: Option<&CrateInfo>,
) {
    if !is_cargo_hir_installed() {
        panic!(
            "cargo-hir is not installed. Please install it by running `cargo install --path . --bin cargo-hir --bin hir-driver --force` from the root of the repository"
        );
    }

    let manifest = parser::determine_manifest_file(crate_name);
    let dir = std::path::Path::new(consts::DOWNLOAD_PATH).join(crate_name.replace(':', "-"));
    let output_file_name = get_hir_output_file(crate_name);

    let backup_hir_manifest = dir.join("Cargo.toml.hir");

    if path::Path::new(&output_file_name).exists() {
        fs::remove_file(&output_file_name)
            .expect("Failed to remove existing HIR visitor span dump file");
    }

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

    let mut args = vec!["hir", "--", "--manifest-path", &manifest];
    let top_level_feats: String;

    if all_feats {
        top_level_feats = crate_info
            .unwrap()
            .features
            .iter()
            .map(|(top, _)| top.clone())
            .collect::<Vec<String>>()
            .join(",");
        if !top_level_feats.is_empty() {
            args.push("--features");
            args.push(&top_level_feats);
        }
        debug!(
            "Running cargo-hir with all features enabled: features={}",
            top_level_feats
        );

        // Create a copy of the manifest file
        fs::copy(&manifest, &backup_hir_manifest).expect("Failed to create a copy of Cargo.toml");

        // We also need to remove features that are enabling things for dependencies.
        let toml = fs::read_to_string(&manifest).expect("Failed to read Cargo.toml");
        let mut cargo_toml: toml::Value = toml.parse().expect("Failed to parse Cargo.toml");
        let main_features_table = cargo_toml
            .get_mut("features")
            .and_then(|f| f.as_table_mut())
            .expect("Cargo.toml does not have a [features] table");

        main_features_table.into_iter().for_each(|(feat, vals)| {
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

    debug!(
        "Running cargo-hir for crate: {} with args: {}",
        crate_name,
        args.join(" ")
    );

    let now = Instant::now();
    let output = Command::new("cargo")
        .args(&args)
        .output()
        .expect("Failed to run cargo-hir");

    if all_feats {
        // Copy the file back
        fs::copy(&backup_hir_manifest, &manifest).expect("Failed to restore original Cargo.toml");
        fs::remove_file(&backup_hir_manifest).expect("Failed to remove temporary Cargo.toml.hir");
    }

    if let Some(telemetry) = telemetry {
        telemetry.hir_driver_time_ms = now.elapsed().as_millis();
    }

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        debug!(
            "cargo-hir failed with status code: {} and message: {}",
            output.status.code().unwrap_or(-1),
            stderr
        );
    } else {
        debug!("cargo-hir ran successfully");
        std::fs::rename(consts::HIR_VISITOR_SPAN_DUMP, &output_file_name)
            .expect("Failed to rename HIR visitor span dump file");
    }
}

pub fn read_hir_spans(crate_name: &str) -> Vec<ReadableSpan> {
    let output_file_name = get_hir_output_file(crate_name);

    if !path::Path::new(&output_file_name).exists() {
        debug!(
            "ERROR:HIR visitor span dump file does not exist. Please ensure that `cargo-hir` ran successfully."
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

fn get_hir_output_file(crate_name: &str) -> String {
    format!(
        "{}/{}_{}",
        consts::RESULTS_PATH,
        crate_name.replace(":", "_"),
        consts::HIR_VISITOR_SPAN_DUMP_SUFFIX
    )
}

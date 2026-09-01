#![allow(dead_code)]

use std::path::PathBuf;
use std::process::Command;

use std::fs;

#[macro_export]
macro_rules! cargo_bin {
    ($name:literal) => {
        std::path::PathBuf::from(env!(concat!("CARGO_BIN_EXE_", $name)))
    };
}

pub(crate) fn get_sysroot_lib_path() -> String {
    let output = Command::new("rustc")
        .args(["--print", "sysroot"])
        .output()
        .expect("Failed to get sysroot");

    let sysroot = String::from_utf8(output.stdout).unwrap().trim().to_string();
    let mut path = PathBuf::from(sysroot);
    path.push("lib");

    path.to_str().unwrap().to_string()
}

pub(crate) fn compare_json_files(actual_path: &PathBuf, expected_path: &PathBuf) {
    assert!(
        actual_path.exists(),
        "Expected file: {} does not exist.",
        actual_path.display()
    );

    assert_eq!(
        normalize_json(actual_path),
        normalize_json(expected_path),
        "Actual HIR visitor span dump does not match expected output.\n\
         Actual build errors, if any:\n{}",
        build_errors(actual_path)
    );
}

/// Every `error` a `compilation_results.json` carries. `normalize_json` drops the
/// field, so a `Success` → `Failed` flip needs it reported separately.
fn build_errors(path: &PathBuf) -> String {
    let Ok(json_str) = fs::read_to_string(path) else {
        return "<unreadable>".to_string();
    };
    let Ok(entries) = serde_json::from_str::<Vec<serde_json::Value>>(&json_str) else {
        return "<unparseable>".to_string();
    };
    let errors: Vec<String> = entries
        .iter()
        .filter_map(|e| e["error"].as_str())
        .map(|e| e.to_string())
        .collect();
    if errors.is_empty() {
        "<none>".to_string()
    } else {
        errors.join("\n---\n")
    }
}

fn normalize_json(path: &PathBuf) -> String {
    let json_str = fs::read_to_string(path).expect("Unable to read JSON file");
    let mut json_value: Vec<serde_json::Value> =
        serde_json::from_str(&json_str).expect("Unable to parse JSON file");

    // `error` is a full rustc diagnostic, so a golden holding one is re-blessed
    // on every toolchain update over text that says nothing about this tool.
    // `status` still catches a broken emitted config; `build_errors` prints why.
    for entry in &mut json_value {
        if let Some(obj) = entry.as_object_mut()
            && obj.contains_key("error")
        {
            obj.insert("error".to_string(), serde_json::Value::Null);
        }
    }
    json_value.sort_by(|a, b| a.to_string().cmp(&b.to_string()));
    let sort_key = |v: &serde_json::Value| {
        (
            v["file"].as_str().unwrap_or("").to_string(),
            v["start_line"].as_u64().unwrap_or(0),
            v["start_col"].as_u64().unwrap_or(0),
        )
    };

    json_value.sort_by_key(sort_key);
    serde_json::to_string_pretty(&json_value).expect("Unable to serialize JSON")
}

/// R34-1's invariant, asserted on whatever the run just emitted.
///
/// `minimize` unlinks an optional dependency by deleting its `dep:D` entry out of
/// the feature that names it and parking that entry in
/// `custom_default_features`, leaving the feature itself enabled. That is sound
/// only while the feature is *off* in the emitted configuration. When the feature
/// is on the command line, every `#[cfg(feature = "F")]` it gates is live, and any
/// one of them that names `D` compiles against a crate cargo was never told to
/// link — `E0432`/`E0433` on every target.
///
/// F1 fixed one route to that state and left the guarantee as a prose list of
/// crate names ("`a7105`, `aht20-driver`, `bevy_input` must keep building"). The
/// list failed silently: bevy_input regressed and 31 green unit tests did not see
/// it, because they each build the pin set by hand and the defect was in how the
/// caller builds it. This is that list as an assertion, and it runs on every
/// golden rather than on three named crates.
///
/// Reads the *published* feature table, never the emitted one — the emitted table
/// is where the entry was deleted from, so it cannot be the source of truth about
/// which feature used to carry it.
/// `download_path` and `custom_key` are passed in rather than read from
/// `nostd::consts`: this file is also built as its own test target, and linking
/// the crate from a target without `#![feature(rustc_private)]` collides with the
/// sysroot ("cannot satisfy dependencies so `std` only shows up once").
pub(crate) fn assert_no_parked_dep_under_a_live_feature(
    download_path: &str,
    custom_key: &str,
    crate_name: &str,
    crate_version: &str,
    args: &[String],
) {
    let dir = std::path::Path::new(download_path).join(format!("{crate_name}-{crate_version}"));
    let emitted = dir.join("Cargo.toml");
    let published = ["Cargo.toml.pristine", "Cargo.toml.orig"]
        .iter()
        .map(|f| dir.join(f))
        .find(|p| p.exists());
    let (Ok(emitted), Some(published)) = (fs::read_to_string(&emitted), published) else {
        // Nothing emitted, or no published copy to compare against: this check has
        // no opinion. `run_main_test` already asserts the run produced results.
        return;
    };
    let Ok(published) = fs::read_to_string(&published) else {
        return;
    };
    let (Ok(emitted), Ok(published)) = (
        emitted.parse::<toml::Value>(),
        published.parse::<toml::Value>(),
    ) else {
        return;
    };

    let feature_values = |v: &toml::Value, key: &str| -> Vec<String> {
        v.get("features")
            .and_then(toml::Value::as_table)
            .and_then(|t| t.get(key))
            .and_then(toml::Value::as_array)
            .map(|a| {
                a.iter()
                    .filter_map(|x| x.as_str().map(str::to_string))
                    .collect()
            })
            .unwrap_or_default()
    };

    let parked: Vec<String> = feature_values(&emitted, custom_key)
        .into_iter()
        .filter(|e| e.starts_with("dep:"))
        .collect();
    if parked.is_empty() {
        return;
    }

    let on: std::collections::HashSet<String> = args
        .iter()
        .position(|a| a == "--features")
        .and_then(|i| args.get(i + 1))
        .map(|list| list.split(',').filter(|f| !f.is_empty()).map(str::to_string).collect())
        .unwrap_or_default();
    if on.is_empty() {
        return;
    }

    let published_features = published
        .get("features")
        .and_then(toml::Value::as_table)
        .cloned()
        .unwrap_or_default();

    for entry in &parked {
        let carriers: Vec<String> = published_features
            .iter()
            .filter(|(_, vals)| {
                vals.as_array().is_some_and(|a| {
                    a.iter().any(|v| v.as_str() == Some(entry.as_str()))
                })
            })
            .map(|(name, _)| name.clone())
            .collect();
        let live: Vec<&String> = carriers.iter().filter(|c| on.contains(*c)).collect();
        assert!(
            live.is_empty(),
            "{crate_name}-{crate_version}: `{entry}` was parked in `{}`, but the feature(s) \
             {live:?} that carried it are on the emitted command line ({on:?}). Every \
             `#[cfg(feature = ...)]` those gate is live, so any of them that names the \
             dependency compiles against a crate cargo was never told to link. This is the \
             R34-1 / F1 `FEATURE_ON_DEP_STRIPPED` signature.",
            custom_key,
        );
    }
}

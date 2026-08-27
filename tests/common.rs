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

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
        "Actual HIR visitor span dump does not match expected output."
    );
}

fn normalize_json(path: &PathBuf) -> String {
    let json_str = fs::read_to_string(path).expect("Unable to read JSON file");
    let mut json_value: Vec<serde_json::Value> =
        serde_json::from_str(&json_str).expect("Unable to parse JSON file");
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

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

    let actual_json =
        fs::read_to_string(actual_path).expect("Unable to read actual HIR visitor span dump file");
    let expected_json = fs::read_to_string(expected_path)
        .expect("Unable to read expected HIR visitor span dump file");

    assert_eq!(
        actual_json, expected_json,
        "Actual HIR visitor span dump does not match expected output."
    );
}

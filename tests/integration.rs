#![feature(rustc_private)]

use cargo_test_support::{Project, cargo_test, project};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use nostd::consts::HIR_VISITOR_SPAN_DUMP;

fn get_sysroot_lib_path() -> String {
    let output = Command::new("rustc")
        .args(["--print", "sysroot"])
        .output()
        .expect("Failed to get sysroot");

    let sysroot = String::from_utf8(output.stdout).unwrap().trim().to_string();
    let mut path = PathBuf::from(sysroot);
    path.push("lib");

    path.to_str().unwrap().to_string()
}

fn get_driver_path() -> std::path::PathBuf {
    std::path::PathBuf::from(env!("CARGO_BIN_EXE_cargo-hir"))
}

fn run_fixture_test(fixture_name: &str) {
    let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures")
        .join(fixture_name);

    let p = project()
        .at(fixture_name)
        .file(
            "Cargo.toml",
            &fs::read_to_string(fixture_path.join("Cargo.toml")).expect("Missing TOML"),
        )
        .file(
            "main.rs",
            &fs::read_to_string(fixture_path.join("main.rs")).expect("Missing Source"),
        )
        .build();

    p.process(get_driver_path())
        .env("LD_LIBRARY_PATH", get_sysroot_lib_path())
        .run();

    let actual_json_path = Path::new(HIR_VISITOR_SPAN_DUMP);
    let expected_json_path = fixture_path.join("expected.json");

    assert!(
        actual_json_path.exists(),
        "Expected HIR visitor span dump file does not exist. Please ensure that `cargo-hir` ran successfully."
    );

    let actual_json = fs::read_to_string(actual_json_path)
        .expect("Unable to read actual HIR visitor span dump file");
    let expected_json = fs::read_to_string(expected_json_path)
        .expect("Unable to read expected HIR visitor span dump file");

    assert_eq!(
        actual_json, expected_json,
        "Actual HIR visitor span dump does not match expected output."
    );
}

#[cargo_test]
fn test_extern_std() {
    run_fixture_test("test_extern_std");
}

#[cargo_test]
fn vec_direct_usage() {
    run_fixture_test("vec_direct_usage");
}

#[cargo_test]
fn test_vec_used_via_use() {
    run_fixture_test("vec_used_via_use");
}

#![feature(rustc_private)]

use std::path::Path;
use std::process::Command;

use cargo_test_support::cargo_test;

use nostd::consts;

mod common;

fn run_main_test(crate_name: &str, crate_version: &str, arch: &str) {
    let expected_json_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/main_tests_jsons")
        .join(format!(
            "{}-{}",
            crate_name.replace("-", "_"),
            crate_version
        ))
        .join("compilation_results.json");

    let args = vec![
        "--name",
        crate_name,
        "--version",
        crate_version,
        "--target",
        arch,
    ];

    let crate_download_dir =
        Path::new(consts::DOWNLOAD_PATH).join(format!("{}-{}", crate_name, crate_version));
    if crate_download_dir.exists() {
        std::fs::remove_dir_all(&crate_download_dir)
            .expect("Failed to remove existing crate download directory");
    }

    let output = Command::new(cargo_bin!("main"))
        .args(&args)
        .env("LD_LIBRARY_PATH", common::get_sysroot_lib_path())
        .output()
        .expect("Failed to run main binary");

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        panic!(
            "Main binary failed with status code: {} and message: {}",
            output.status.code().unwrap_or(-1),
            stderr
        );
    }

    let actual_json_path = Path::new(consts::RESULTS_PATH)
        .join(format!(
            "{}-{}",
            crate_name.replace("-", "_"),
            crate_version
        ))
        .join("compilation_results.json");

    common::compare_json_files(&actual_json_path, &expected_json_path);
}

#[cargo_test]
fn test_aberth() {
    run_main_test("aberth", "0.4.1", "x86_64-unknown-none");
}

#[cargo_test]
fn test_tarfs() {
    run_main_test("tarfs", "0.2.7", "x86_64-unknown-none");
}

#[cargo_test]
fn test_tinywasm() {
    run_main_test("tinywasm", "0.8.0", "x86_64-unknown-none");
}

#[cargo_test]
fn test_tinywasm_parser() {
    run_main_test("tinywasm-parser", "0.8.0", "x86_64-unknown-none");
}

#[cargo_test]
fn test_bytemuck() {
    run_main_test("bytemuck", "1.25.0", "x86_64-unknown-none");
}

#[cargo_test]
fn test_arc_ec() {
    run_main_test("ark-ec", "0.5.0", "x86_64-unknown-none");
}

#[cargo_test]
fn test_log() {
    run_main_test("log", "0.4.29", "x86_64-unknown-none");
}

#[cargo_test]
fn test_lazy_exclusive() {
    run_main_test("lazy-exclusive", "1.0.5", "x86_64-unknown-none");
}

#[cargo_test]
fn test_elfloader() {
    run_main_test("elfloader", "0.16.0", "x86_64-unknown-none");
}

#[cargo_test]
#[should_panic(expected = "Found unguarded std usage in the main crate")]
fn test_assertr() {
    run_main_test("assertr", "0.4.3", "x86_64-unknown-none");
}

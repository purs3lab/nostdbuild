#![feature(rustc_private)]

use cargo_test_support::{cargo_test, project};
use std::fs;
use std::path::Path;

use nostd::consts;

mod common;

fn run_fixture_test(fixture_name: &str, fixture_version: &str) {
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

    p.process(cargo_bin!("cargo-hir"))
        .env("LD_LIBRARY_PATH", common::get_sysroot_lib_path())
        .run();

    let actual_json_path = Path::new(consts::RESULTS_PATH).join(format!(
        "{}-{}-{}",
        fixture_name,
        fixture_version,
        consts::HIR_VISITOR_VISIT_FILE_SUFFIX
    ));

    let expected_json_path = fixture_path.join("expected.json");
    common::compare_json_files(&actual_json_path, &expected_json_path);
}

#[cargo_test]
fn test_extern_std() {
    run_fixture_test("test_extern_std", "0.1.0");
}

#[cargo_test]
fn vec_direct_usage() {
    run_fixture_test("test_vec_direct_usage", "0.1.0");
}

#[cargo_test]
fn test_vec_used_via_use() {
    run_fixture_test("test_vec_used_via_use", "0.1.0");
}

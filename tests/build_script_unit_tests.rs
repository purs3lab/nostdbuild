#![feature(rustc_private)]

use nostd::hir_driver::is_build_script_unit;

fn args(raw: &[&str]) -> Vec<String> {
    raw.iter().map(|s| s.to_string()).collect()
}

/// The two shapes measured off `cargo check -vv` on `mavlink 0.13.1`.
#[test]
fn custom_build_path_is_a_build_script() {
    assert!(is_build_script_unit(&args(&[
        "rustc",
        "--crate-name",
        "build_script_main",
        "build/main.rs",
        "--crate-type",
        "bin",
        "--emit=dep-info,link",
    ])));
}

#[test]
fn default_build_rs_still_matches() {
    assert!(is_build_script_unit(&args(&[
        "rustc",
        "--crate-name",
        "build_script_build",
        "build.rs",
        "--crate-type",
        "bin",
    ])));
}

#[test]
fn the_analysed_lib_is_not_a_build_script() {
    assert!(!is_build_script_unit(&args(&[
        "rustc",
        "--crate-name",
        "mavlink",
        "src/lib.rs",
        "--crate-type",
        "lib",
        "--emit=dep-info,metadata",
    ])));
}

/// The false positive a bare prefix test would introduce: three crates in the
/// index are libraries whose own name starts with `build_script_`, and waving
/// one through costs it every std record it has.
#[test]
fn a_lib_named_build_script_is_still_analysed() {
    for name in ["build_script_cfg", "build_script_file_gen", "build_script_utils"] {
        assert!(
            !is_build_script_unit(&args(&[
                "rustc",
                "--crate-name",
                name,
                "src/lib.rs",
                "--crate-type",
                "lib",
            ])),
            "{name} was skipped as a build script"
        );
    }
}

/// `build_script` proper has no trailing underscore and never matched.
#[test]
fn the_crate_named_build_script_does_not_match() {
    assert!(!is_build_script_unit(&args(&[
        "rustc",
        "--crate-name",
        "build_script",
        "src/lib.rs",
        "--crate-type",
        "bin",
    ])));
}

#[test]
fn inline_flag_values_are_read() {
    assert!(is_build_script_unit(&args(&[
        "rustc",
        "--crate-name=build_script_main",
        "--crate-type=bin",
    ])));
}

/// The old test was `any(|arg| arg == "build_script_build")`, which any
/// argv element could satisfy — a `--cfg` value, say.
#[test]
fn only_the_crate_name_operand_counts() {
    assert!(!is_build_script_unit(&args(&[
        "rustc",
        "--crate-name",
        "somecrate",
        "--cfg",
        "feature=\"build_script_build\"",
        "--crate-type",
        "lib",
    ])));
}

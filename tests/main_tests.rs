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
        "--no-recursive",
    ];

    let crate_download_dir =
        Path::new(consts::DOWNLOAD_PATH).join(format!("{}-{}", crate_name, crate_version));
    if crate_download_dir.exists() {
        std::fs::remove_dir_all(&crate_download_dir)
            .expect("Failed to remove existing crate download directory");
    }

    // Per-crate CARGO_TARGET_DIR, mirroring eval.py. The rustc_plugin framework
    // derives the plugin's `--target-dir` from `cargo metadata` run in the tool's
    // CWD, not the analysed crate (rustc_plugin `cli.rs`: `metadata.target_directory
    // .join("plugin-<channel>")`), so *every* plugin pass defaults to the single
    // shared `<cwd>/target/plugin-<channel>`. With the suite running 21 tests in
    // parallel, all their plugin passes then serialize on that one directory's cargo
    // build lock (silently — the plugin runs `cargo check -q`). Setting
    // CARGO_TARGET_DIR redirects `metadata.target_directory` (and thus the plugin
    // dir) to a per-crate path, so the tests build concurrently instead of queuing.
    // A stable per-crate path (not a fresh temp) keeps cargo's cache warm across
    // reruns. Does not affect emitted results — those go to consts::RESULTS_PATH.
    let cargo_target_dir = std::env::temp_dir()
        .join("nostd_main_tests")
        .join(format!("{}-{}", crate_name.replace('-', "_"), crate_version));
    std::fs::create_dir_all(&cargo_target_dir)
        .expect("Failed to create per-crate CARGO_TARGET_DIR");

    let output = Command::new(cargo_bin!("main"))
        .args(&args)
        .env("LD_LIBRARY_PATH", common::get_sysroot_lib_path())
        .env("CARGO_TARGET_DIR", &cargo_target_dir)
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

/// The KI-11 case: `use-locks` gets enabled, pulls in `libc`, and the build dies
/// on `libc::pthread_mutex_*` items that do not exist on a bare-metal target —
/// even though `libc` itself is no_std-capable and compiles for that target.
/// `use-locks` exists only to link an optional dep (and the crate's README calls
/// it "possible but untested"), so the failed build triggers a retry without it,
/// which succeeds. The golden records the retry's config, not the failed attempt.
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

#[cargo_test]
fn test_wg() {
    run_main_test("wg", "0.9.2", "x86_64-unknown-none");
}

#[cargo_test]
fn test_ya_smoltcp() {
    run_main_test("ya-smoltcp", "0.1.0", "x86_64-unknown-none");
}

#[cargo_test]
#[should_panic(expected = "Found unguarded std usage in the main crate")]
fn test_zyx_core() {
    run_main_test("zyx-core", "0.1.1", "x86_64-unknown-none");
}

#[cargo_test]
fn test_winter_fri() {
    run_main_test("winter-fri", "0.12.2", "x86_64-unknown-none");
}

#[cargo_test]
fn test_winter_crypto() {
    run_main_test("winter-crypto", "0.12.0", "x86_64-unknown-none");
}

#[cargo_test]
fn test_zeno() {
    run_main_test("zeno", "0.3.2", "x86_64-unknown-none");
}

#[cargo_test]
fn test_zlib_rs() {
    run_main_test("zlib-rs", "0.5.0", "armv7a-none-eabi");
}

#[cargo_test]
#[should_panic(expected = "Main crate is a proc-macro crate, which is not supported")]
fn test_educe() {
    run_main_test("educe", "0.6.0", "x86_64-unknown-none");
}

#[cargo_test]
fn test_uom() {
    run_main_test("uom", "0.38.0", "x86_64-unknown-none");
}

#[cargo_test]
fn test_watchface() {
    run_main_test("watchface", "0.4.0", "x86_64-unknown-none");
}

/// Regression: a crate whose only std usage lives in an auto-discovered bin
/// target. `chainable-if` is a no_std-clean library shipping the stock
/// `fn main() { println!("Hello, world!"); }` alongside it.
///
/// The HIR pass used to build the package's default targets, so the plugin
/// emitted records for `src/main.rs` — a file `find_entrypoints` deliberately
/// excludes (`is_lib || (is_bin && !has_lib)`). With no ModNode covering it,
/// the `println!` span found no gate, classified as AlwaysStd, and sank the
/// crate. Passing `--lib` keeps the two halves in agreement.
#[cargo_test]
fn test_chainable_if() {
    run_main_test("chainable-if", "0.1.1", "x86_64-unknown-none");
}

/// Regression: a `macro_rules!` whose body gates a std statement behind a
/// `#[cfg(...)]`, defined inside a submodule. `stak-vm`'s `mod vm` holds
///
/// ```ignore
/// macro_rules! trace {
///     ($p:literal, $d:expr) => {
///         #[cfg(feature = "trace_instruction")]
///         std::eprintln!("{}: {}", $p, $d);
///     };
/// }
/// ```
///
/// invoked in statement position deep inside fns. The plugin extracts a
/// macro body's `#[cfg]` and attaches it (via the expansion backtrace) as
/// `macro_body_cfgs` so the probe can disable the feature — but the pre-scan
/// only walked crate-root items, so a macro under `mod vm` got no entry and
/// all 13 `std::eprintln!` spans read as unguarded. `collect_macro_cfgs` now
/// recurses through modules; the probe disables `trace_instruction`/
/// `trace_memory` and the crate clears.
#[cargo_test]
fn test_stak_vm() {
    run_main_test("stak-vm", "0.10.21", "x86_64-unknown-none");
}

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
    let cargo_target_dir = std::env::temp_dir().join("nostd_main_tests").join(format!(
        "{}-{}",
        crate_name.replace('-', "_"),
        crate_version
    ));
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

/// The KI-27 case: multiexp writes `groupings.zeroize()` on a `Vec<Vec<u8>>` and
/// the impl that serves it is `#[cfg(feature = "alloc")] impl<Z> Zeroize for
/// Vec<Z>` in zeroize — an item with no identifier, that multiexp's source never
/// names. The method it *does* name is ungated, so the item-usage check finds
/// nothing to justify `alloc`, and multiexp's only route to it (`std =
/// ["zeroize/std"]`) is the one route a no_std build cannot take. All 26 targets
/// used to fail on `E0599 … the method zeroize exists for struct Vec<Vec<u8>>,
/// but its trait bounds were not satisfied`.
///
/// The compiler is what knows the call needs that impl, so the plugin records
/// which impl each obligation selected and the dependency's own solve is given
/// the gate as a constraint. `alloc` is then in zeroize's `enable`, no multiexp
/// feature reaches it, and it is parked in `custom_no_std_feature_enabled` —
/// which is the emitted config the golden records.
///
/// The crate also has **zero** covering runs: without the impl it does not type
/// check, so every covering pass fails and the default-features pass is the only
/// compilation in the run that ever resolved the obligation. That is not
/// incidental to this test — it is the shape the fix has to work in.
#[cargo_test]
fn test_multiexp() {
    run_main_test("multiexp", "0.4.0", "x86_64-unknown-none");
}

/// The R34-11 case: `tstr`'s own isolated solve asks for `cmp_traits` and
/// `const_generics` to be no_std, no feature of `repr_offset` reaches either, and
/// both are parked in `custom_no_std_feature_enabled` and enabled. They are real
/// features of `tstr` — nothing about the *manifest* is wrong, so no pre-write
/// check finds this — but `const_generics` selects a `&'static str` const generic
/// parameter that the compiler has since forbidden, and the build dies on it. The
/// per-dep solve cannot see that; only the build can. The failed build triggers a
/// retry with the injected set dropped, which succeeds, and the golden records the
/// retry's config: no `--features` at all.
#[cargo_test]
fn test_repr_offset() {
    run_main_test("repr_offset", "0.2.2", "x86_64-unknown-none");
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

/// The one golden that records a `Failed` build. The args are the assertion;
/// the failure is `wg`'s own `#![deny(warnings)]` meeting the nightly
/// `fetch_update` → `try_update` rename in `src/no_std.rs`, which no emitted
/// feature set can dodge — `DENY_LINT_FALLOUT`, not a tool regression.
#[cargo_test]
fn test_wg() {
    run_main_test("wg", "0.9.2", "x86_64-unknown-none");
}

/// Baseline re-blessed with T1 (`ALL_TARGET_FAILURES.md`): the emitted set now also
/// carries `log`. `default` lists the optional dep `log` by bare name, and
/// `process_dep_crate_wrapper` used to read that linkage entry as a *feature of*
/// `log` and hand it to `final_feature_list_main` as one to disable — which took the
/// implicit feature out of the default closure as a side effect. Only genuine
/// `<dep>/<subfeat>` references count now, so `log` stays on. Builds either way.
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

/// O-9: a proc-macro dependency's `std` feature selects the tokens it injects into
/// *this* crate. displaydoc 0.2.6 (`default = ["std"]`) emits `extern crate std;` and
/// `impl PathToDisplayDoc for std::path::Path` into every `#[derive(Display)]` site,
/// so dfu-core's four derives read as unguarded std — at spans the crate never wrote
/// — and no bare-metal target could link (`E0463 can't find crate for std`). Proc
/// macros are skipped by the dependency walk, correctly for their own host-side std
/// and wrongly for that default, so nothing ever turned it off. The walk now parks a
/// proc-macro's default `std` on the edge (`parser::park_proc_macro_std_default`).
#[cargo_test]
fn test_dfu_core() {
    run_main_test("dfu-core", "0.7.0", "x86_64-unknown-none");
}

/// A `compile_error!` whose features are disjoint from the no_std condition is
/// withheld from the feature solve on purpose (`excluded_compile_error_eqs` —
/// uom's 21-way disjunction shows what asserting it costs), and until now a
/// violation of one was reported only as a warning. It is not a warning: the
/// compiler stops on the macro, so the emitted config cannot build. lexical-util
/// 1.0.6 shipped `--no-default-features --features floats` against
/// `#[cfg(all(feature = "floats", not(any(feature = "write-floats", feature =
/// "parse-floats"))))] compile_error!(…)` and lost every target to it.
///
/// The repair is applied as a retry in the KI-11 shape — only after a build that
/// failed everywhere, kept only because the rebuild succeeded — so this test is
/// what proves the wiring: `parser::compile_error_repair_features` returning the
/// right feature is not the same as `bin/main.rs` retrying with it. The golden
/// holds ONE row, the repaired build, because the failed attempt's records are
/// discarded.
#[cargo_test]
fn test_lexical_util() {
    run_main_test("lexical-util", "1.0.6", "x86_64-unknown-none");
}

/// R34-13: the probe's condition was thrown away before it could be solved from.
///
/// bitreader is `#![no_std]` with `default = ["std"]` and writes its `extern crate
/// std` inside a `cfg_if::cfg_if!` body. The probe handles that shape and settles it
/// with a compile — `Negating gate: (not (or std))`, then a run with no features that
/// yields zero std spans — so `hard_constraints` arrives at `process_crate` holding
/// `¬std`. It never got read: `parse_item_extern_crates` finds no `#[cfg]`-carrying
/// `extern crate` item, and the early return that fact triggers sits a hundred lines
/// above `hard_constraint_vec`. With an empty `disable` list
/// `final_feature_list_main` sees nothing in the default list to turn off, emits no
/// `--no-default-features`, and cargo turns `std` back on — `E0463 can't find crate
/// for std` on all 26 targets. The golden is the one flag, which is the whole repair:
/// no manifest surgery, the existing solver emits it once the condition survives.
#[cargo_test]
fn test_bitreader() {
    run_main_test("bitreader", "0.3.11", "x86_64-unknown-none");
}

/// The same defect through a different gate shape, which is why both are here.
/// bitreader hides the declaration in a macro body; parc writes a plain `#[cfg(feature
/// = "std")] mod imports { extern crate std; … }`. Neither is visible to the syn side:
/// `ItemExternCrates` keeps an item only when the attribute is on the item itself, and
/// `ModCollector` reads the gate only at a file's top level. A fix tested on one shape
/// alone would not have shown that it is the *return* that is wrong rather than either
/// reader.
#[cargo_test]
fn test_parc() {
    run_main_test("parc", "1.0.1", "x86_64-unknown-none");
}

/// The other direction of the same change: a crate that must keep emitting nothing.
///
/// dilate reaches the identical early return — `#![no_std]`, no `#[cfg]`-carrying
/// `extern crate` item — and builds on all 26 targets today with an empty argv. Its
/// probe leaves no condition, so `hard_constraints` is `None` and the return still
/// stands. That is the common case by a wide margin: of the 5575 crates in the corpus
/// that reach this return, 5519 have no probe condition, and a fix that fell through
/// unconditionally would have put every one of them through a solve they never needed.
/// The golden is deliberately unchanged from before the fix.
#[cargo_test]
fn test_dilate() {
    run_main_test("dilate", "0.6.3", "x86_64-unknown-none");
}

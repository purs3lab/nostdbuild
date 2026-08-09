#![feature(rustc_private)]

//! O-7: an external no_std shim whose own `std` feature is on resolves to std,
//! and the std-off run that was supposed to prove otherwise is a *host* build.
//!
//! `--no-default-features` on the main crate does not take std out of the
//! dependency graph. A shim like `core2` keeps its own `default = ["std"]`, so
//! every bare-metal target fails inside the shim — before the main crate is
//! compiled at all — and `run_rustc_plugin_pass_with` falls back to the host.
//! There `core2::io` *is* `std::io`, so the run reports the very std it was run
//! to rule out: bitstream-io 4.0.0 collects 528 such records and 436 spans are
//! reported as unguarded std usage.
//!
//! The rule under test: a *probe* that lands on such a build cannot come back
//! "still std". Negating the gate there proves nothing — the shim resolves to
//! std whatever the gate says — so the span is reported unproven instead of
//! hard. The discriminator is which package cargo gave up on: a failure inside a
//! dependency means the main crate was never compiled for the target, while a
//! failure in the main crate is the case the host fallback exists for (tarfs).
//!
//! ⚠ Deliberately confined to the probe. Discounting such a run's std records in
//! `classify_spans` as well was tried and reverted: it silences genuine std that
//! merely happens to sit in a crate whose dependencies fail on bare metal —
//! `main_tests` caught assertr 0.4.3 (ungated `use std::marker::PhantomData`)
//! and tinywasm-parser 0.8.0 (`impl crate::std::error::Error`, which then
//! emitted a config that does not build).

use std::fs;
use std::path::Path;

use cargo_test_support::{Project, cargo_test, project};

use nostd::Telemetry;
use nostd::driver::{analyze_crate, compile_failure_names_crate};

/// Copy a whole fixture directory into a cargo test project — one of the
/// fixtures ships a path dependency, so Cargo.toml + lib.rs is not enough.
fn load_fixture(name: &str) -> (Project, String) {
    let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures")
        .join(name);

    let mut files: Vec<(String, String)> = Vec::new();
    collect_files(&fixture_path, &fixture_path, &mut files);
    assert!(!files.is_empty(), "fixture {name} has no files");

    let mut builder = project().at(name);
    for (rel, contents) in &files {
        builder = builder.file(rel, contents);
    }
    let p = builder.build();
    let manifest = p.root().join("Cargo.toml").to_str().unwrap().to_string();
    (p, manifest)
}

fn collect_files(root: &Path, dir: &Path, out: &mut Vec<(String, String)>) {
    for entry in fs::read_dir(dir).unwrap_or_else(|e| panic!("reading {dir:?}: {e}")) {
        let path = entry.expect("dir entry").path();
        if path.is_dir() {
            collect_files(root, &path, out);
        } else {
            let rel = path.strip_prefix(root).expect("under root");
            out.push((
                rel.to_string_lossy().to_string(),
                fs::read_to_string(&path).unwrap_or_else(|e| panic!("reading {path:?}: {e}")),
            ));
        }
    }
}

/// bitstream-io's shape. The only std here belongs to the shim's configuration,
/// and the run that says otherwise never left the host.
#[cargo_test]
fn std_reached_through_a_shim_on_a_host_only_run_is_not_hard_evidence() {
    let (_p, manifest) = load_fixture("host_only_shim");

    let ctx = z3::Context::new(&z3::Config::new());
    let mut telemetry = Telemetry::default();
    let (hard_spans, _cond, _cov, _ce, _root, _records, unproven) =
        analyze_crate(&ctx, &manifest, "host_only_shim", &mut telemetry);

    assert!(
        telemetry.std_inconclusive_runs > 0,
        "expected at least one covering run to be host-only with every bare-metal \
         attempt dying inside `shim`; none was flagged"
    );
    assert!(
        hard_spans.is_empty(),
        "`io::Error` is std only because the shim was built with its own default \
         `std` feature, on a host build no bare-metal target ever reached — negating \
         the gate there is not proof this crate cannot be no_std, got {hard_spans:?}"
    );
    assert!(
        !unproven.is_empty(),
        "and it must not be silently cleared either: with no run that ever put this \
         crate in a no_std environment, the span is unproven, not clean"
    );
}

/// The control, and the case the host fallback exists for. Nothing compiles
/// bare metal here either, but the compile dies in *this* crate, so the host
/// build is the only place the std usage surfaces and it must still be reported.
#[cargo_test]
fn a_crates_own_std_is_still_reported_when_only_the_host_compiles() {
    let (_p, manifest) = load_fixture("host_only_own_std");

    let ctx = z3::Context::new(&z3::Config::new());
    let mut telemetry = Telemetry::default();
    let (hard_spans, ..) = analyze_crate(&ctx, &manifest, "host_only_own_std", &mut telemetry);

    assert_eq!(
        telemetry.std_inconclusive_runs, 0,
        "the bare-metal builds failed while compiling this crate, so its host run \
         is exactly the evidence the fallback exists to collect"
    );
    assert!(
        !hard_spans.is_empty(),
        "`std::string::String` is ungated and unavoidable; it must still be \
         reported when only the host compiles"
    );
}

/// The discriminator itself. Cargo names the package it gave up on, and stops
/// there — so a line naming the crate proves it was reached and anything else
/// proves it was not.
#[cargo_test]
fn cargos_give_up_line_says_whether_the_crate_was_reached() {
    let dep_failed = "error[E0463]: can't find crate for `std`\n\
                      error: could not compile `core2` (lib) due to 4 previous errors\n";
    assert!(!compile_failure_names_crate(dep_failed, "bitstream-io:4.0.0"));

    let own_failed = "error[E0433]: failed to resolve: use of unresolved module `std`\n\
                      error: could not compile `tarfs` (lib) due to 2 previous errors\n";
    assert!(compile_failure_names_crate(own_failed, "tarfs:0.2.7"));

    // A build script that panics names no package at all: the dependency was
    // never compiled, so neither was this crate (fatfs → core_io).
    let build_script = "error: failed to run custom build command for `core_io v0.1.20210325`\n\
                        thread 'main' panicked at build.rs:73:69:\n";
    assert!(!compile_failure_names_crate(build_script, "fatfs:0.3.6"));

    // Cargo prints the package name with dashes; the pipeline uses either
    // spelling.
    let underscored = "error: could not compile `bitstream_io` (lib) due to 32 previous errors\n";
    assert!(compile_failure_names_crate(underscored, "bitstream-io:4.0.0"));

    // A dependency whose name merely contains the crate's is not the crate.
    let lookalike = "error: could not compile `fatfs-utils` (lib) due to 1 previous error\n";
    assert!(!compile_failure_names_crate(lookalike, "fatfs:0.3.6"));
}

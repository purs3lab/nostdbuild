#![feature(rustc_private)]

//! R34-12 / KI-26: the crate's own lint level must not decide its no_std verdict.
//!
//! A probe negates a feature gate to test whether a std span survives without it.
//! Negating a gate deletes code, and deleted code makes other code dead — so in a
//! crate that writes `#![deny(warnings)]` the probe fails with a diagnostic that
//! says nothing about std, and every span it was carrying comes back unproven.
//! `agnostic-lite-0.5.5`: 74 probe compiles, all of them `error: struct
//! `JoinError` is never constructed`, 114 spans unproven, run emits nothing.
//! `tiny-ec-core-0.3.1` is the same sentence with `deny(unused_imports)`, in a
//! crate that writes `#![cfg_attr(not(feature = "std"), no_std)]`.
//!
//! A plugin pass is an analysis compile, so it caps lints — in the driver, where
//! the rustc arguments are built and cargo's target-information probe is not
//! involved. `RUSTFLAGS=--cap-lints` was measured and does not degrade that
//! probe, it deletes it (KI-26). The verification build in `compiler.rs` is
//! deliberately left uncapped: a configuration that trips the crate's own lints
//! really does fail to build, and that failure is the crate's.

use std::fs;
use std::path::Path;

use cargo_test_support::{Project, cargo_test, project};

use nostd::Telemetry;
use nostd::driver::{analyze_crate, reset_target_cache};

/// Serialises against the process globals `analyze_crate` caches into — a stale
/// `LAST_GOOD_TARGET` makes a later test measure scheduling rather than
/// behaviour.
static SERIAL: std::sync::Mutex<()> = std::sync::Mutex::new(());

fn isolated() -> std::sync::MutexGuard<'static, ()> {
    let guard = SERIAL.lock().unwrap_or_else(|e| e.into_inner());
    reset_target_cache();
    guard
}

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

/// The regression. Without the cap the probe dies on `struct `Inner` is never
/// constructed` and the `impl std::error::Error` span is unproven; with it the
/// probe compiles, finds no std record under ¬std, and the crate clears.
#[cargo_test]
fn a_denied_lint_in_the_probed_configuration_does_not_decide_the_verdict() {
    let _serial = isolated();
    let (_p, manifest) = load_fixture("deny_lint_probe");

    let ctx = z3::Context::new(&z3::Config::new());
    let mut telemetry = Telemetry::default();

    let (hard_spans, condition, _coverage, _ce, _root, _records, unproven, _) =
        analyze_crate(&ctx, &manifest, "deny_lint_probe", &mut telemetry);

    // The failure this fixture reproduces: the probe never compiles, so the span
    // is neither cleared nor condemned. `Inner` is what breaks it, and `Inner`
    // has nothing to do with std.
    assert!(
        unproven.is_empty(),
        "the probed configuration only fails a *lint*, so no span should be left \
         unproven: {unproven:?}"
    );
    assert_eq!(telemetry.unproven_std_spans, 0);

    // And the verdict the crate was built to get.
    assert!(
        hard_spans.is_empty(),
        "`impl std::error::Error` is behind `feature = \"std\"` and is avoidable: \
         {hard_spans:?}"
    );

    // The condition has to actually turn `std` off — a run that cleared the span
    // by leaving `std` on would satisfy the two assertions above and prove
    // nothing.
    let condition = condition.expect("a no_std condition should have been proven");
    let solver = z3::Solver::new(&ctx);
    solver.assert(&condition);
    solver.assert(&z3::ast::Bool::new_const(&ctx, "std"));
    assert_eq!(
        solver.check(),
        z3::SatResult::Unsat,
        "the proven condition must force `std` off, got {condition}"
    );
}

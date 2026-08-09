#![feature(rustc_private)]

//! A crate that names its own library after a sysroot crate.
//!
//! `Cargo.toml` may say `[lib] name = "std"` — stdworld 0.1.1 does — and then
//! `tcx.crate_name(LOCAL_CRATE)` answers `"std"` for every item the crate
//! defines itself, down to its generic parameters. The plugin reported those
//! records with `usage_crate: "std"`, the pipeline reads that string as the
//! identity of the sysroot crate, and all 47 of stdworld's own names became
//! unguarded std usage that no feature set could remove.
//!
//! The rule under test: std-ness is decided by whether a `DefId` is local, not
//! by what the local crate is called.

use cargo_test_support::{cargo_test, project};
use std::fs;
use std::path::Path;

use nostd::Telemetry;
use nostd::driver::analyze_crate;

fn load_fixture(name: &str) -> (cargo_test_support::Project, String) {
    let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures")
        .join(name);

    let p = project()
        .at(name)
        .file(
            "Cargo.toml",
            &fs::read_to_string(fixture_path.join("Cargo.toml"))
                .unwrap_or_else(|_| panic!("Missing Cargo.toml for fixture {name}")),
        )
        .file(
            "main.rs",
            &fs::read_to_string(fixture_path.join("main.rs"))
                .unwrap_or_else(|_| panic!("Missing main.rs for fixture {name}")),
        )
        .build();

    let manifest = p.root().join("Cargo.toml").to_str().unwrap().to_string();
    (p, manifest)
}

fn run_analyze(manifest: &str, crate_name: &str) -> Vec<nostd::types::ReadableSpan> {
    let ctx = z3::Context::new(&z3::Config::new());
    let mut telemetry = Telemetry::default();
    let (hard_spans, ..) = analyze_crate(&ctx, manifest, crate_name, &mut telemetry);
    hard_spans
}

/// stdworld's shape. Every name in the fixture is defined by the fixture; the
/// only reason any of it looked like std was the library's name.
#[cargo_test]
fn a_crate_named_std_does_not_use_std_by_being_named_std() {
    let (_p, manifest) = load_fixture("lib_named_std");
    let hard_spans = run_analyze(&manifest, "lib_named_std");
    assert!(
        hard_spans.is_empty(),
        "Expected no hard std spans: `[lib] name = \"std\"` makes the crate's own \
         items answer to the name `std`, but they are local definitions, got {:?}",
        hard_spans
    );
}

/// The control that keeps the fix honest. Same `[lib] name = "std"`, but this
/// crate does reach the sysroot std through the extern prelude — a non-local
/// `DefId` — so the usage must still be reported. A fix that keyed on the crate
/// *name* instead of on locality would silence this too.
#[cargo_test]
fn real_std_is_still_reported_from_a_crate_named_std() {
    let (_p, manifest) = load_fixture("lib_named_std_real_std");
    let hard_spans = run_analyze(&manifest, "lib_named_std_real_std");
    assert!(
        !hard_spans.is_empty(),
        "Expected hard std spans: `std::collections::HashMap` resolves to the \
         sysroot std even though the local library is also called `std`"
    );
}

#![feature(rustc_private)]

//! Bucket 11: a cfg-selected backend that lives behind an **optional**
//! dependency.
//!
//! ```ignore
//! #[cfg(not(feature = "std"))] use hashbrown::{hash_map, HashMap, HashSet};
//! #[cfg(feature = "std")]      use std::collections::{hash_map, HashMap, HashSet};
//! ```
//! with `[dependencies.hashbrown] optional = true` and no `[features]` entry
//! linking the two. The solver knew `core => hashbrown` (a plain feature link)
//! but nothing told it that turning `std` **off** requires `hashbrown` **on**, so
//! the ¬std covering run went out as `--no-default-features`, failed to compile
//! with `E0432: unresolved import`, and was discarded. With the std run left as
//! the only survivor, every span that resolves elsewhere in the ¬std build reads
//! as std-in-every-run.
//!
//! The fix pairs each gated `use`/`extern crate` with the features that link the
//! dependency it roots at, and asserts `cond => (f1 ∨ … ∨ fn)`.

use std::fs;
use std::path::{Path, PathBuf};

use cargo_test_support::{Project, cargo_test, project};
use z3::ast::Bool;

use nostd::Telemetry;
use nostd::downloader::optional_dep_enablers;
use nostd::driver::analyze_crate;
use nostd::solver::optional_dep_use_constraints;
use nostd::visitor::{ModCollector, collect_gated_extern_roots};

// ---------------------------------------------------------------------------
// Extraction: which (root, condition) pairs the tree yields
// ---------------------------------------------------------------------------

fn roots_fixture() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/optional_dep_roots/lib.rs")
}

/// The three spellings a crate uses to name a cfg-selected dependency must all
/// produce a root: a plain `use`, an `extern crate`, and a `use` buried in a
/// `cfg_if!` arm. The last is the one that needs the token scan — the arm is a
/// single opaque `LocalItem`, so it carries no `use_path` for anything inside it.
#[test]
fn gated_use_extern_crate_and_cfg_if_arm_all_yield_roots() {
    let ctx = z3::Context::new(&z3::Config::new());
    let mut collector = ModCollector::new(&ctx);
    let node = collector.collect(&roots_fixture(), "optional_dep_roots");

    let roots = collect_gated_extern_roots(&node, &ctx);
    let names: Vec<&str> = roots.iter().map(|(r, _)| r.as_str()).collect();

    for expected in ["hashbrown", "core_io", "libm"] {
        assert!(
            names.contains(&expected),
            "`{expected}` must be reported as a gated crate root, got {names:?}"
        );
    }

    // Control: an ungated `use` says nothing about *when* a dependency is needed,
    // so it must not appear — otherwise every import would force its dep on.
    assert!(
        !names.contains(&"always_there"),
        "an ungated `use` must yield no root, got {names:?}"
    );

    // Each root carries the cfg that gates it, not a blanket `true`.
    let (_, hashbrown_cond) = roots
        .iter()
        .find(|(r, _)| r == "hashbrown")
        .expect("hashbrown root");
    assert!(
        hashbrown_cond.to_string().contains("std"),
        "the hashbrown root must be gated by the `std` cfg, got {hashbrown_cond}"
    );
}

// ---------------------------------------------------------------------------
// Constraints: what the solver is told
// ---------------------------------------------------------------------------

/// `hashbrown` is optional and never named `dep:`, so Cargo synthesises an
/// implicit feature of the same name — the enabler the edge points at.
fn implicit_enablers(deps: &[&str]) -> Vec<(String, Vec<String>)> {
    deps.iter()
        .map(|d| (d.to_string(), vec![d.to_string()]))
        .collect()
}

#[test]
fn edge_forbids_the_cfg_with_its_dependency_off() {
    let ctx = z3::Context::new(&z3::Config::new());
    let mut collector = ModCollector::new(&ctx);
    let node = collector.collect(&roots_fixture(), "optional_dep_roots");
    let roots = collect_gated_extern_roots(&node, &ctx);

    let constraints = optional_dep_use_constraints(
        &ctx,
        &roots,
        &implicit_enablers(&["hashbrown", "core_io", "libm"]),
    );
    assert!(
        !constraints.is_empty(),
        "expected at least one optional-dep edge"
    );

    let std_var = Bool::new_const(&ctx, "std");
    let hashbrown = Bool::new_const(&ctx, "hashbrown");

    let solver = z3::Solver::new(&ctx);
    for c in &constraints {
        solver.assert(c);
    }
    solver.assert(&std_var.not());
    solver.assert(&hashbrown.not());
    assert_eq!(
        solver.check(),
        z3::SatResult::Unsat,
        "std off with hashbrown off is exactly the set Cargo accepts and rustc rejects"
    );

    // Control: the edges constrain only the ¬std half. A std-on model must stay
    // free to leave the backend unlinked, or the fix would drag optional
    // dependencies into every build.
    let solver = z3::Solver::new(&ctx);
    for c in &constraints {
        solver.assert(c);
    }
    solver.assert(&std_var);
    solver.assert(&hashbrown.not());
    assert_eq!(
        solver.check(),
        z3::SatResult::Sat,
        "with std ON the backend must remain optional"
    );
}

/// The erasure guard. Non-feature cfg atoms are erased to constants rather than
/// weakened (policy G), so `#[cfg(target_arch = "wasm32")] use wasm_only::…`
/// arrives as the condition `true`. Emitting `true => wasm_only` would link the
/// dependency on every target, for every feature set.
#[test]
fn constant_condition_yields_no_edge() {
    let ctx = z3::Context::new(&z3::Config::new());

    let always = vec![("wasm_only".to_string(), Bool::from_bool(&ctx, true))];
    let constraints =
        optional_dep_use_constraints(&ctx, &always, &implicit_enablers(&["wasm_only"]));
    assert!(
        constraints.is_empty(),
        "a condition that erased to a constant must produce no edge, got {constraints:?}"
    );

    // Same through the real tree: the fixture's wasm-only import must not reach
    // the solver either.
    let mut collector = ModCollector::new(&ctx);
    let node = collector.collect(&roots_fixture(), "optional_dep_roots");
    let roots = collect_gated_extern_roots(&node, &ctx);
    let constraints =
        optional_dep_use_constraints(&ctx, &roots, &implicit_enablers(&["wasm_only"]));
    assert!(
        constraints.is_empty(),
        "the target-cfg import must produce no edge, got {constraints:?}"
    );

    // Mutual control: the same call with a feature-gated dep does produce one, so
    // the emptiness above is the guard firing and not a broken lookup.
    let constraints =
        optional_dep_use_constraints(&ctx, &roots, &implicit_enablers(&["hashbrown"]));
    assert_eq!(
        constraints.len(),
        1,
        "a feature-gated import must still produce its edge, got {constraints:?}"
    );
}

#[test]
fn root_that_names_no_optional_dep_yields_no_edge() {
    let ctx = z3::Context::new(&z3::Config::new());
    let mut collector = ModCollector::new(&ctx);
    let node = collector.collect(&roots_fixture(), "optional_dep_roots");
    let roots = collect_gated_extern_roots(&node, &ctx);

    // A required dependency is always linked, so there is nothing to assert — and
    // no implicit feature to name if we tried.
    let constraints = optional_dep_use_constraints(&ctx, &roots, &[]);
    assert!(
        constraints.is_empty(),
        "roots naming no optional dep must produce no edges, got {constraints:?}"
    );
}

// ---------------------------------------------------------------------------
// Enablers: which features Cargo accepts to link the dependency
// ---------------------------------------------------------------------------

fn parse(manifest: &str) -> toml::Value {
    manifest.parse().expect("failed to parse fixture manifest")
}

fn declared(names: &[&str]) -> std::collections::HashSet<String> {
    names.iter().map(|s| s.to_string()).collect()
}

/// alexcrichton-cranelift-codegen's shape: `hashbrown` optional, referenced only
/// as a bare name in `core = ["hashbrown"]`, so Cargo's implicit feature is alive
/// and `--features hashbrown` links the dep directly.
#[test]
fn implicit_feature_is_the_enabler() {
    let toml = parse(
        "[package]\nname=\"x\"\nversion=\"0.0.0\"\n\n\
         [features]\ncore = [\"hashbrown\"]\nstd = []\n\n\
         [dependencies.hashbrown]\nversion=\"0.6\"\noptional=true\n\n\
         [dependencies.smallvec]\nversion=\"1\"\n",
    );
    // What `cargo metadata` reports: the implicit `hashbrown` feature included.
    let enablers = optional_dep_enablers(&toml, &declared(&["core", "std", "hashbrown"]));
    assert_eq!(
        enablers,
        vec![("hashbrown".to_string(), vec!["hashbrown".to_string()])],
        "the implicit feature must be the enabler; required deps must not appear"
    );
}

/// `dep:` suppresses the implicit feature, so the enabler is the feature that
/// names it. `declared` is the authority: the suppressed name is simply absent
/// from `cargo metadata`.
#[test]
fn dep_prefixed_reference_makes_its_feature_the_enabler() {
    let toml = parse(
        "[package]\nname=\"x\"\nversion=\"0.0.0\"\n\n\
         [features]\nbackend = [\"dep:hashbrown\"]\nstd = []\n\n\
         [dependencies.hashbrown]\nversion=\"0.6\"\noptional=true\n",
    );
    let enablers = optional_dep_enablers(&toml, &declared(&["backend", "std"]));
    assert_eq!(
        enablers,
        vec![("hashbrown".to_string(), vec!["backend".to_string()])],
        "with the implicit feature suppressed, the `dep:` feature is the only enabler"
    );
}

#[test]
fn dependency_with_no_enabler_is_dropped() {
    // Pathological but reachable: an optional dep no feature can turn on. There is
    // nothing to assert, and an empty disjunction would be `false`, forbidding the
    // cfg outright.
    let toml = parse(
        "[package]\nname=\"x\"\nversion=\"0.0.0\"\n\n\
         [features]\nstd = []\n\n\
         [dependencies.hashbrown]\nversion=\"0.6\"\noptional=true\n",
    );
    let enablers = optional_dep_enablers(&toml, &declared(&["std"]));
    assert!(
        enablers.is_empty(),
        "a dep with no way to enable it must be dropped, got {enablers:?}"
    );
}

// ---------------------------------------------------------------------------
// Wiring: the real entry point
// ---------------------------------------------------------------------------

/// Copy a whole fixture directory into a cargo test project — the fixture here
/// ships a path dependency, so Cargo.toml + main.rs is not enough.
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

/// Wiring guard (handoff rule 4): drives `analyze_crate`, not the constraint
/// builder in isolation.
///
/// The observable is the number of covering runs. `hard_spans` cannot serve here:
/// with the ¬std run failing, the probe for the std import exhausts its models and
/// the span is dropped as `CompileFailed` — so the broken build reports an empty
/// `hard_spans` too. What actually changes is that a second, no_std configuration
/// now compiles and contributes records.
#[cargo_test]
fn optional_backend_gives_the_no_std_run_a_compiling_feature_set() {
    let (_p, manifest) = load_fixture("optional_dep_backend");
    let ctx = z3::Context::new(&z3::Config::new());
    let mut telemetry = Telemetry::default();
    let (hard_spans, final_condition, coverage, ..) =
        analyze_crate(&ctx, &manifest, "optional_dep_backend", &mut telemetry);

    let coverage = coverage.expect("coverage comparison");
    assert!(
        coverage.num_covering_runs >= 2,
        "the ¬std covering run must compile once `shim` is linked — got \
         {} run(s); without the edge only the std run survives",
        coverage.num_covering_runs
    );

    assert!(
        hard_spans.is_empty(),
        "the `String` import is gated on `std` and has a no_std alternative, so \
         nothing may be hard std: {hard_spans:?}"
    );

    // The std-off half is reachable, so the crate has a no_std condition to report
    // rather than no answer at all.
    assert!(
        final_condition.is_some(),
        "expected a no_std condition once the ¬std configuration compiles"
    );
}

// ---------------------------------------------------------------------------
// `dep_adding_features` — which declared features put a NEW CRATE into the
// graph. The enabler search subtracts them when its all-on trial fails, because
// only they can bring in a crate that itself fails to build for a bare-metal
// target (proptest 1.6.0: five such candidates against the two, `alloc` and
// `no_std`, that its no_std build actually needs).
// ---------------------------------------------------------------------------

fn dep_adding(manifest: &str, declared: &[&str]) -> Vec<String> {
    let toml: toml::Value = toml::from_str(manifest).expect("fixture manifest parses");
    let declared: std::collections::HashSet<String> =
        declared.iter().map(|s| s.to_string()).collect();
    let mut out: Vec<String> =
        nostd::downloader::dep_adding_features(&toml, &declared).into_iter().collect();
    out.sort();
    out
}

#[test]
fn a_feature_naming_dep_marker_adds_a_crate() {
    let out = dep_adding(
        r#"
        [package]
        name = "x"
        version = "0.1.0"
        [dependencies.bit-set]
        version = "0.5"
        optional = true
        [features]
        alloc = []
        bit-set = ["dep:bit-set"]
        "#,
        &["alloc", "bit-set"],
    );
    assert_eq!(out, vec!["bit-set".to_string()], "`alloc = []` links nothing");
}

#[test]
fn the_implicit_same_name_feature_adds_a_crate() {
    // proptest's `lazy_static` / `tempfile` shape: no `[features]` entry at all,
    // just the optional dep, so cargo synthesises the feature.
    let out = dep_adding(
        r#"
        [package]
        name = "x"
        version = "0.1.0"
        [dependencies.lazy_static]
        version = "1"
        optional = true
        [features]
        alloc = []
        "#,
        &["alloc", "lazy_static"],
    );
    assert_eq!(out, vec!["lazy_static".to_string()]);
}

#[test]
fn a_feature_enabling_a_dep_adding_feature_adds_a_crate_too() {
    // proptest's `attr-macro = ["proptest-macro"]`: the link is one hop away,
    // through the implicit feature rather than through a `dep:` marker.
    let out = dep_adding(
        r#"
        [package]
        name = "x"
        version = "0.1.0"
        [dependencies.pm]
        version = "1"
        optional = true
        [features]
        alloc = []
        attr-macro = ["pm"]
        outer = ["attr-macro"]
        "#,
        &["alloc", "attr-macro", "outer", "pm"],
    );
    assert_eq!(
        out,
        vec!["attr-macro".to_string(), "outer".to_string(), "pm".to_string()],
        "the closure must run to a fixpoint, not one level"
    );
}

#[test]
fn a_strong_dep_slash_feature_reference_adds_a_crate_but_a_weak_one_does_not() {
    // `optdep/feat` enables `optdep`; `optdep?/feat` only turns on a feature of
    // it if something else already did — the reading `optional_dep_feature_edges`
    // takes.
    let out = dep_adding(
        r#"
        [package]
        name = "x"
        version = "0.1.0"
        [dependencies.serde]
        version = "1"
        optional = true
        [features]
        strong = ["serde/derive"]
        weak = ["serde?/derive"]
        "#,
        &["strong", "weak"],
    );
    assert_eq!(out, vec!["strong".to_string()]);
}

#[test]
fn a_feature_of_a_required_dependency_adds_nothing() {
    // proptest's `no_std = ["num-traits/libm"]` — the crate is already in the
    // graph, so turning one of its features on cannot introduce a new build.
    // This is the case that MUST survive the subtraction: it is half the answer.
    let out = dep_adding(
        r#"
        [package]
        name = "x"
        version = "0.1.0"
        [dependencies.num-traits]
        version = "0.2"
        default-features = false
        [features]
        no_std = ["num-traits/libm"]
        std = ["num-traits/std"]
        "#,
        &["no_std", "std"],
    );
    assert!(out.is_empty(), "no optional dep is named anywhere, got {out:?}");
}

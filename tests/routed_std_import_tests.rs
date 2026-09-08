#![feature(rustc_private)]

//! KI-7 — brace and routed std imports: the bound names never reached the driver.
//!
//! `use std::collections::{hash_map, HashMap}` emits exactly **one** plugin
//! record, `path_text: "std::collections"`, and `use super::HashMap;` emits none
//! at all. So the names the import actually binds were invisible, every use site
//! reached the prober with `ancestors: None`, and `initial_ungated_results`
//! short-circuited it to `StillStd` **without compiling anything** — a false
//! positive on a crate whose std usage is fully `#[cfg(feature = "std")]`-gated.
//!
//! These tests drive the real `ModCollector` over a fixture in the
//! alexcrichton-cranelift-frontend 0.53.0 shape, so the binding table is built
//! from genuinely parsed use trees (braces, renames, `super::`, globs) rather
//! than hand-shaped `LocalItem`s.

use std::fs;
use std::path::{Path, PathBuf};

use cargo_test_support::{Project, cargo_test, project};

use nostd::Telemetry;
use nostd::driver::{ancestors_for_record, analyze_crate, resolve_import_to_use_gateways};
use nostd::types::{FeatureRunOutput, PathContext, PathRecord, ReadableSpan};
use nostd::visitor::{ModCollector, ancestors_for_span};

fn fixture(name: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures")
        .join(name)
        .join("lib.rs")
}

/// The line a substring appears on, 1-based. Multi-line needles are how the
/// tests name a *gated* import: syn's item span includes the item's attributes,
/// so the anchor a use site inherits starts on the `#[cfg(...)]` line, not on
/// the `use` line.
fn line_of(name: &str, needle: &str) -> usize {
    let content = std::fs::read_to_string(fixture(name)).unwrap();
    let idx = content
        .find(needle)
        .unwrap_or_else(|| panic!("substring {needle:?} not found in {name}/lib.rs"));
    content[..idx].matches('\n').count() + 1
}

/// A std record sitting on the line where `needle` appears, with no `#[cfg]` of
/// its own — the shape every one of KI-7's five false positives has.
fn std_record_at(name: &str, needle: &str, path_text: &str) -> PathRecord {
    let line = line_of(name, needle);
    PathRecord {
        definition_span: None,
        path_text: path_text.to_string(),
        definition_crate: "std".to_string(),
        context: PathContext::Other,
        span: ReadableSpan {
            file: "lib.rs".to_string(),
            start_line: line,
            start_col: 0,
            end_line: line,
            end_col: 200,
            usage_crate: Some("std".to_string()),
        },
        local_route: None,
        defining_module: None,
        macro_body_cfgs: vec![],
        expansion_crate: None,
        is_extern_crate: false,
        gateway_anchor: None,
        is_float_primitive_method: false,
    }
}

fn output(records: Vec<PathRecord>) -> FeatureRunOutput {
    FeatureRunOutput {
        records,
        macro_module_imports: vec![],
        out_dir: None,
        impls: Vec::new(),
    }
}

/// Build the tree for a fixture and run the join over the given records.
fn resolve(fixture_name: &str, records: Vec<PathRecord>) -> (FeatureRunOutput, usize) {
    let ctx = z3::Context::new(&z3::Config::new());
    let mut collector = ModCollector::new(&ctx);
    let root = collector.collect(&fixture(fixture_name), fixture_name);
    let mut out = output(records);
    let anchored = resolve_import_to_use_gateways(&mut out, &root);
    (out, anchored)
}

fn anchor_line(r: &PathRecord) -> Option<usize> {
    r.gateway_anchor.as_ref().map(|a| a.start_line)
}

// ---------------------------------------------------------------------------
// Part A + B — the binding table
// ---------------------------------------------------------------------------

/// The core case. `HashMap` is bound by a *brace leaf* of a feature-gated std
/// import, which the plugin collapses to one `"std::collections"` record — so
/// this can only work off the syn tree. The bare `HashMap::new()` in
/// `mod switch` inherits that import's span as its anchor.
#[test]
fn brace_leaf_lends_its_gate_to_a_bare_use() {
    let (out, anchored) = resolve(
        "routed_std_import",
        vec![std_record_at(
            "routed_std_import",
            "cases: HashMap::new()",
            "HashMap::new",
        )],
    );

    assert_eq!(
        anchor_line(&out.records[0]),
        Some(line_of(
            "routed_std_import",
            "#[cfg(feature = \"std\")]\npub use std::collections::{HashMap, hash_map};"
        )),
        "the bare `HashMap::new()` must inherit the gated brace import's span"
    );
    assert_eq!(anchored, 1, "the pass must report what it anchored");
}

/// Two hops. `use crate::hash_map::Entry` makes `Entry` std-bound because
/// `hash_map` is — and the anchor stays the **std-rooted seed**, not the
/// intermediate re-export, because negating the seed's gate removes the binding
/// at its source.
#[test]
fn two_hop_binding_is_anchored_to_the_std_rooted_seed() {
    let (out, _) = resolve(
        "routed_std_import",
        vec![
            std_record_at(
                "routed_std_import",
                "let _ = Entry::Occupied;",
                "Entry::Occupied",
            ),
            std_record_at(
                "routed_std_import",
                "let _ = Entry::Vacant;",
                "Entry::Vacant",
            ),
        ],
    );

    let seed = line_of(
        "routed_std_import",
        "#[cfg(feature = \"std\")]\npub use std::collections::{HashMap, hash_map};",
    );
    let hop = line_of("routed_std_import", "use crate::hash_map::Entry;");
    assert_ne!(seed, hop, "fixture must keep the two lines distinguishable");

    for r in &out.records {
        assert_eq!(
            anchor_line(r),
            Some(seed),
            "{} must anchor to the std-rooted seed, not the intermediate re-export",
            r.path_text
        );
    }
}

/// The routed import record itself. `use crate::hash_map::Entry` carries a
/// `local_route`, a `defining_module` **and** `context == ImportDeclaration` —
/// all three of the exclusions the pass used to apply — yet it has no gate
/// anywhere above it and is one of cranelift-frontend's five false positives.
/// The exclusions are now a single "does this span have a gate of its own?"
/// test, which it fails, so it is anchored.
#[test]
fn routed_import_record_is_anchored_despite_route_and_context() {
    let mut routed = std_record_at(
        "routed_std_import",
        "use crate::hash_map::Entry;",
        "crate::hash_map::Entry",
    );
    routed.context = PathContext::ImportDeclaration;
    routed.local_route = Some("crate".to_string());
    routed.defining_module = Some("crate::frontend".to_string());

    let (out, _) = resolve("routed_std_import", vec![routed]);

    assert_eq!(
        anchor_line(&out.records[0]),
        Some(line_of(
            "routed_std_import",
            "#[cfg(feature = \"std\")]\npub use std::collections::{HashMap, hash_map};"
        )),
        "a routed import with no gate of its own is the leak, not the facade pass's business"
    );
}

/// The routed import and the uses it enables arrive in the **same** run, which
/// is where the two seeds collide: the plugin reports `use crate::hash_map::Entry`
/// as a std `ImportDeclaration` binding the name `Entry`, on a line with no
/// `#[cfg]`. Seeding that as an independent ungated std binding would clear
/// `Entry`'s all-gated flag and block the very uses the hop exists to excuse —
/// the fix would defeat itself, and only a test holding both record kinds at
/// once can see it. Seed 1 therefore skips routed paths and lets the tree, which
/// knows `hash_map` is std-rooted, establish `Entry` instead.
#[test]
fn a_routed_import_record_does_not_poison_the_name_it_binds() {
    let mut routed = std_record_at(
        "routed_std_import",
        "use crate::hash_map::Entry;",
        "crate::hash_map::Entry",
    );
    routed.context = PathContext::ImportDeclaration;
    routed.local_route = Some("crate".to_string());
    routed.defining_module = Some("crate::frontend".to_string());

    let (out, anchored) = resolve(
        "routed_std_import",
        vec![
            routed,
            std_record_at(
                "routed_std_import",
                "let _ = Entry::Occupied;",
                "Entry::Occupied",
            ),
        ],
    );

    assert_eq!(
        anchored,
        2,
        "both the routed import and the use it binds must be excused, got {:?}",
        out.records
            .iter()
            .map(|r| (r.path_text.clone(), anchor_line(r)))
            .collect::<Vec<_>>()
    );
}

/// A rename binds the *alias*: `use std::collections::BTreeMap as Map` makes
/// `Map` the name use sites reference, and `BTreeMap` no name at all.
#[test]
fn rename_binds_the_alias_not_the_original() {
    let (out, _) = resolve(
        "routed_std_import",
        vec![
            std_record_at("routed_std_import", "let _ = Map::new();", "Map::new"),
            std_record_at("routed_std_import", "pub fn insert()", "BTreeMap::new"),
        ],
    );

    assert_eq!(
        anchor_line(&out.records[0]),
        Some(line_of(
            "routed_std_import",
            "#[cfg(feature = \"std\")]\npub use std::collections::BTreeMap as Map;"
        )),
        "the alias `Map` is the bound name"
    );
    assert_eq!(
        anchor_line(&out.records[1]),
        None,
        "`BTreeMap` is not bound by that import — only `Map` is"
    );
}

/// CONTROL: a glob binds names syn cannot enumerate, so the fixpoint refuses to
/// follow it. `hash_map` is a perfectly good gated std binding, but nothing
/// `use super::hash_map::*` brings in may be excused off the back of it.
#[test]
fn glob_reexport_is_not_followed() {
    let (out, anchored) = resolve(
        "routed_std_import",
        vec![std_record_at(
            "routed_std_import",
            "use super::hash_map::*;",
            "Iter::next",
        )],
    );

    assert_eq!(anchor_line(&out.records[0]), None);
    assert_eq!(anchored, 0);
}

/// CONTROL: the all-bindings-must-agree rule. The mixed fixture binds `HashMap`
/// and `hash_map` a second time, unconditionally, in a submodule — so the names
/// resolve to std with the feature off and nothing may be excused. `Entry` goes
/// down with them: it derives from `hash_map`, which is poisoned.
#[test]
fn an_ungated_sibling_binding_blocks_every_excuse() {
    let (out, anchored) = resolve(
        "routed_std_import_mixed",
        vec![
            std_record_at(
                "routed_std_import_mixed",
                "let _ = HashMap::new();",
                "HashMap::new",
            ),
            std_record_at(
                "routed_std_import_mixed",
                "let _ = Entry::Occupied;",
                "Entry::Occupied",
            ),
        ],
    );

    assert_eq!(
        anchored,
        0,
        "an ungated std binding of the name must block the excuse, got {:?}",
        out.records
            .iter()
            .map(|r| (r.path_text.clone(), anchor_line(r)))
            .collect::<Vec<_>>()
    );
}

/// CONTROL: only std-*rooted* bindings seed the table. `mod nostd_only` binds
/// `HashMap` ungated from hashbrown; if the root check were dropped it would
/// clear the all-gated flag and `brace_leaf_lends_its_gate_to_a_bare_use` would
/// go with it. Asserted here as its own fact so the reason is visible.
#[test]
fn a_non_std_root_neither_seeds_nor_blocks() {
    let (out, _) = resolve(
        "routed_std_import",
        vec![std_record_at(
            "routed_std_import",
            "cases: HashMap::new()",
            "HashMap::new",
        )],
    );

    assert!(
        out.records[0].gateway_anchor.is_some(),
        "the ungated hashbrown binding of `HashMap` must not enter the table"
    );
}

// ---------------------------------------------------------------------------
// Part C — the anchor has to actually reach the prober
// ---------------------------------------------------------------------------

/// The wiring guard. Parts A and B alone change nothing: an anchor's only
/// consumer used to be `span_externally_gated`, which is false for
/// `#[cfg(feature = "std")]`. The two assertions are mutual controls — the first
/// pins the pre-fix behaviour (the span has no gate of its own, so
/// `ancestors_for_span` is None and the prober short-circuits), the second that
/// `ancestors_for_record` recovers the gate through the anchor. Revert part C to
/// a bare `ancestors_for_span` and the second fails.
#[test]
fn a_feature_gated_anchor_yields_gate_ancestors() {
    let ctx = z3::Context::new(&z3::Config::new());
    let mut collector = ModCollector::new(&ctx);
    let root = collector.collect(&fixture("routed_std_import"), "routed_std_import");

    let mut out = output(vec![std_record_at(
        "routed_std_import",
        "cases: HashMap::new()",
        "HashMap::new",
    )]);
    resolve_import_to_use_gateways(&mut out, &root);
    let rec = &out.records[0];

    assert!(
        ancestors_for_span(&root, &rec.span).is_none(),
        "the use site carries no #[cfg] of its own — that is the whole problem"
    );

    let ancestors = ancestors_for_record(&root, rec)
        .expect("the anchor's #[cfg(feature = \"std\")] must reach the prober");
    let text = ancestors
        .iter()
        .map(|b| b.to_string())
        .collect::<Vec<_>>()
        .join(" ");
    assert!(
        text.contains("std"),
        "the recovered gate must be the import's `std` feature; got {text:?}"
    );
}

/// CONTROL for part C: a record with no anchor at all is unchanged — the gate
/// recovery is additive, it does not invent ancestors.
#[test]
fn a_record_with_no_anchor_still_has_no_ancestors() {
    let ctx = z3::Context::new(&z3::Config::new());
    let mut collector = ModCollector::new(&ctx);
    let root = collector.collect(&fixture("routed_std_import"), "routed_std_import");

    let rec = std_record_at("routed_std_import", "use super::hash_map::*;", "Iter::next");

    assert!(rec.gateway_anchor.is_none());
    assert!(ancestors_for_record(&root, &rec).is_none());
}

// ---------------------------------------------------------------------------
// Existence proof: brace/routed std imports clear via a probe that actually
// compiles, against a real no_std replacement — the KI-7 fixture gap
// ---------------------------------------------------------------------------
//
// Parts A-C above drive `resolve_import_to_use_gateways`/`ancestors_for_record`
// directly against `routed_std_import`, which is deliberately unparseable as a
// real crate (no `hashbrown` dependency — "Not meant to compile"). Every
// corpus clearance of the underlying false positive (cranelift-frontend
// 0.53.0) went through `ProbeDecision::CompileFailed`: the ¬std build dies
// inside cranelift-codegen, unrelated to the hashbrown swap itself. No crate
// has ever shown this shape clearing via a probe that actually compiled.
// `routed_import_wiring` is that crate: its `mapshim` path dependency is a
// genuine (if minimal) no_std replacement, so the ¬std run compiles clean.
//
// This is deliberately NOT framed as a regression guard for
// `ancestors_for_record`/`resolve_import_to_use_gateways` specifically — it
// was built as one, and verification (stubbing each function out in turn and
// re-running) found it stays green with *both* disabled. Root cause: this
// fixture's std/no_std swap is a crate-ROOT `#[cfg]`, which
// `find_feature_combs_for_all_code`'s own general-purpose covering-set search
// discovers and compiles on its own — a completely different, KI-7-unrelated
// mechanism (it exists to cover every `#[cfg]` branch in the source at least
// once) — before the per-span gate-recovery path is ever reached. Once both
// configs are gathered, `switch`/`frontend` are ungated and compile in both,
// so plain run-divergence already tells the spans apart. KI-7's fix is only
// load-bearing when the *general* gathering's own ¬std attempt fails to
// compile (cranelift-frontend's actual shape) — reproducing that specific
// asymmetry (general gathering fails, the later per-span probe's retry
// succeeds anyway) needs a fixture engineered around the CEGAR retry
// internals, not a small swap-in dependency; left for whoever picks this back
// up, if a real corpus crate ever surfaces the shape (see `KNOWN_ISSUES.md`).
// Parts A-C remain the actual regression guard for the mechanism itself. What
// this test adds is the narrower, still-real thing the residual asked for:
// proof the recovered gate is *correct* (a genuinely working no_std
// alternative), not just quieter — closing the literal "no span has yet
// cleared via a probe that actually compiled" gap.

/// Copy a whole fixture directory into a cargo test project — the fixture here
/// ships a path dependency (`mapshim`), so `Cargo.toml` + `lib.rs` is not
/// enough.
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

/// Drives `analyze_crate` end to end (not any single pass in isolation)
/// against a fixture whose ¬std configuration genuinely compiles.
///
/// `hard_spans` alone cannot distinguish "excused by the recovered gate" from
/// "silently dropped as `CompileFailed`" — both read as empty. What actually
/// changes here is that a second, no_std configuration compiles and
/// contributes records, so `coverage.num_covering_runs` is the second half of
/// the assertion: without a working `mapshim` link the ¬std run does not
/// compile and only the std run survives. This does NOT specifically pin down
/// the KI-7 gate-recovery call sites — see the section comment above for why
/// (the general covering-set search reaches the same answer on its own for a
/// crate-root cfg swap like this one); Parts A-C are what regression-guard
/// `ancestors_for_record`/`resolve_import_to_use_gateways` themselves.
#[cargo_test]
fn routed_import_clears_via_a_probe_that_actually_compiles() {
    let (_p, manifest) = load_fixture("routed_import_wiring");
    let ctx = z3::Context::new(&z3::Config::new());
    let mut telemetry = Telemetry::default();
    let (hard_spans, _final_condition, coverage, ..) =
        analyze_crate(&ctx, &manifest, "routed_import_wiring", &mut telemetry);

    let coverage = coverage.expect("coverage comparison");
    assert!(
        coverage.num_covering_runs >= 2,
        "the ¬std covering run must compile once `mapshim` provides HashMap/ \
         RandomState — got {} run(s); without it only the std run survives",
        coverage.num_covering_runs
    );

    assert!(
        hard_spans.is_empty(),
        "the routed `super::HashMap` use and the two-hop `crate::hash_map::\
         RandomState` use both inherit the root brace import's `feature = \
         \"std\"` gate and have a working no_std alternative, so nothing may be \
         hard std: {hard_spans:?}"
    );
}

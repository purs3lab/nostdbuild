#![feature(rustc_private)]

//! D2: `#![no_std]` declared on a *target* predicate.
//!
//! ```text
//! #![cfg_attr(target_arch = "spirv", no_std)]   // macaw, renderling, saft-sdf, rukako-shader
//! #![cfg_attr(target_os   = "cuda",  no_std)]   // cuda_std
//! #![cfg_attr(target_os   = "none",  no_std)]   // xous-ipc, xous-api-names
//! ```
//!
//! Policy G erases the atom, so `parse_meta_for_cfg_attr` returns no equation
//! and the crate gets no no_std condition — the mechanism O-14(a) fixes for
//! `not(test)`, which is known-false. A target atom is not known-false; it is
//! not known at all *until a target is named*, and every build this tool
//! performs names one. So the predicate is decided per run instead of being
//! turned into a feature formula.
//!
//! What that buys, and why O-7 does not already cover it: on every bare-metal
//! target the predicate is false, the attribute does not apply, the crate is a
//! plain std crate, and the build dies **in the crate itself** with `can't find
//! crate for std`. O-7's discriminator asks which package cargo gave up on and
//! gets the crate's own name back — "bare metal reached the crate" — so the host
//! fallback keeps its authority. But the host does not satisfy the predicate
//! either: the crate compiled there is not no_std, and its std records describe a
//! configuration that was never under test.
//!
//! The rule under test: such a host-only run is `std_inconclusive`, exactly like
//! O-7's, and everything downstream of that flag is O-7's already.

use std::fs;
use std::path::Path;

use cargo_test_support::{Project, cargo_test, project};

use nostd::Telemetry;
use nostd::consts;
use nostd::driver::analyze_crate;
use nostd::target_cfg::{CfgPred, holds_for, is_decidable, supported_no_std_targets};

/// Copy a whole fixture directory into a cargo test project.
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

fn atom(key: &str, value: Option<&str>) -> CfgPred {
    CfgPred::Atom {
        key: key.to_string(),
        value: value.map(str::to_string),
    }
}

/// The predicate of an inner `#![cfg_attr(<pred>, no_std)]`, parsed the way the
/// visitor parses it — from real source, not from a hand-built `Meta`.
fn parse(src: &str) -> CfgPred {
    let file = syn::parse_file(src).expect("source parses");
    let attr = file.attrs.first().expect("an inner attribute");
    let syn::Meta::List(list) = &attr.meta else {
        panic!("not a list");
    };
    let metas = list
        .parse_args_with(
            syn::punctuated::Punctuated::<syn::Meta, syn::Token![,]>::parse_terminated,
        )
        .expect("args parse");
    CfgPred::parse(metas.first().expect("a predicate")).expect("a predicate CfgPred models")
}

/// The positive. Nothing here can be answered by a compile, and the crate must
/// say so rather than report the host's answer as a proof.
#[cargo_test]
fn no_std_on_a_target_predicate_makes_a_host_only_run_inconclusive() {
    let (_p, manifest) = load_fixture("target_cfg_no_std");

    let ctx = z3::Context::new(&z3::Config::new());
    let mut telemetry = Telemetry::default();
    let (hard_spans, _cond, _cov, _ce, _root, _records, unproven) =
        analyze_crate(&ctx, &manifest, "target_cfg_no_std", &mut telemetry);

    assert_eq!(
        telemetry.no_std_cfg_predicate.as_deref(),
        Some("target_arch = \"spirv\""),
        "the crate's own no_std predicate should have been recognised and kept as written"
    );
    assert!(
        telemetry.no_std_predicate_targets.is_empty(),
        "no target in TARGET_LIST is spirv — rustc has no spirv target at all — so \
         there is no build in which this crate is no_std, got {:?}",
        telemetry.no_std_predicate_targets
    );
    assert!(
        telemetry.std_inconclusive_runs > 0,
        "every run compiled on the host, where `#![no_std]` never applied; such a \
         run is not a no_std environment and must be flagged"
    );
    assert!(
        hard_spans.is_empty(),
        "`f32::trunc` binding std's inherent method in a *std* build of the crate is \
         not proof the crate cannot be no_std, got {hard_spans:?}"
    );
    assert!(
        !unproven.is_empty(),
        "and it must not read clean either — the span is unproven, not absent"
    );
}

/// The control that keeps the two genuinely-std crates in the bucket failing.
/// Same shape, same host-only run, same inconclusive flag — but the crate spells
/// `std` itself, so `crate_named_std_in_path` holds the verdict.
#[cargo_test]
fn a_crates_own_spelled_out_std_survives_the_target_predicate_rule() {
    let (_p, manifest) = load_fixture("target_cfg_no_std_own_std");

    let ctx = z3::Context::new(&z3::Config::new());
    let mut telemetry = Telemetry::default();
    let (hard_spans, ..) = analyze_crate(&ctx, &manifest, "target_cfg_no_std_own_std", &mut telemetry);

    assert_eq!(
        telemetry.no_std_cfg_predicate.as_deref(),
        Some("target_os = \"none\""),
        "the predicate should be recognised here too — this crate differs only in \
         what its span says"
    );
    assert!(
        !telemetry.no_std_predicate_targets.is_empty(),
        "`target_os = \"none\"` holds on most of TARGET_LIST; an empty list would mean \
         the per-target evaluation is broken"
    );
    assert!(
        !hard_spans.is_empty(),
        "`std::string::String` is spelled out in this crate's own source and cannot \
         resolve under the no_std it declares for `target_os = \"none\"`; the \
         weakening must not reach it (xous-ipc, xous-api-names)"
    );
}

/// The predicate is decided, not erased: `rustc --print cfg --target <t>` is the
/// complete cfg set for a target, so an atom absent from it is false exactly as
/// rustc has it.
#[test]
fn a_target_predicate_is_decidable_once_a_target_is_named() {
    let spirv = parse(r#"#![cfg_attr(target_arch = "spirv", no_std)]"#);
    let cuda = parse(r#"#![cfg_attr(target_os = "cuda", no_std)]"#);
    let none = parse(r#"#![cfg_attr(target_os = "none", no_std)]"#);

    assert_eq!(spirv, atom("target_arch", Some("spirv")));

    // The host is none of these, which is the whole point: a host build of any
    // of these crates is a std build.
    for pred in [&spirv, &cuda, &none] {
        assert_eq!(
            holds_for(pred, None),
            Some(false),
            "{pred} must not hold on the host"
        );
    }

    // And the targets separate cleanly. spirv has no target at all; cuda has
    // exactly one; `none` is nearly the whole list.
    assert!(supported_no_std_targets(&spirv).is_empty());
    assert_eq!(
        supported_no_std_targets(&cuda),
        vec!["nvptx64-nvidia-cuda"],
        "cuda_std's no_std target is in the sweep — it fails to compile for a \
         toolchain reason (O-12a), which is a different fact"
    );
    let none_targets = supported_no_std_targets(&none);
    assert!(
        none_targets.len() > consts::TARGET_LIST.len() / 2,
        "`target_os = \"none\"` holds on most bare-metal targets, got {none_targets:?}"
    );
    assert!(!none_targets.contains(&"nvptx64-nvidia-cuda"));
}

/// The combinators, and the rule for an atom nobody set. `test` and `doc` are
/// false for every build this tool performs, which is what lets O-14(a) treat
/// `not(test)` as true — the same evaluation, one layer down.
#[test]
fn absent_atoms_are_false_and_the_combinators_follow_rustc() {
    let host = nostd::target_cfg::cfg_set(None).expect("host cfg set");

    assert!(!atom("test", None).holds(&host));
    assert!(!atom("doc", None).holds(&host));
    assert!(CfgPred::Not(Box::new(atom("test", None))).holds(&host));

    let any_none_or_test = parse(r#"#![cfg_attr(any(target_os = "none", test), no_std)]"#);
    assert!(!any_none_or_test.holds(&host));
    let any_with_a_true = parse(r#"#![cfg_attr(any(target_os = "none", unix), no_std)]"#);
    assert_eq!(any_with_a_true.holds(&host), atom("unix", None).holds(&host));

    let all_of_nothing = CfgPred::All(vec![]);
    assert!(all_of_nothing.holds(&host), "all() is true, as in rustc");
    let any_of_nothing = CfgPred::Any(vec![]);
    assert!(!any_of_nothing.holds(&host), "any() is false, as in rustc");
}

/// An atom rustc does not derive from the target is *undecided*, not false.
///
/// `rustc --print cfg` is silent about build-script and `--cfg` injection, so
/// reading "absent from the set" as "false" would have this rule fire on crates
/// whose predicate is set by their own build script — and fire the wrong way,
/// since such a crate may well be no_std in the build that actually happens.
#[test]
fn an_atom_rustc_does_not_derive_from_the_target_is_undecided() {
    // Injected from outside: a build script (`cargo:rustc-cfg=rustc_1_6`,
    // str_overlap 0.4.3), a `--cfg` flag (docsrs, mesalock_sgx), or the test
    // harness. rustc reports none of them for any target.
    for src in [
        r#"#![cfg_attr(rustc_1_6, no_std)]"#,
        r#"#![cfg_attr(docsrs, no_std)]"#,
        r#"#![cfg_attr(not(test), no_std)]"#,
        r#"#![cfg_attr(not(any(test, doc)), no_std)]"#,
        r#"#![cfg_attr(all(target_os = "none", not(fuzzing)), no_std)]"#,
    ] {
        let pred = parse(src);
        assert!(!is_decidable(&pred), "{pred} must not be decided here");
        assert_eq!(holds_for(&pred, None), None, "{pred}");
        assert!(supported_no_std_targets(&pred).is_empty(), "{pred}");
    }

    // And the target atoms still are decided — including `unix`, which no
    // bare-metal target reports but the host does, so the key is target-derived.
    for src in [
        r#"#![cfg_attr(target_os = "none", no_std)]"#,
        r#"#![cfg_attr(any(unix, target_arch = "spirv"), no_std)]"#,
    ] {
        let pred = parse(src);
        assert!(is_decidable(&pred), "{pred} must be decided here");
        assert!(holds_for(&pred, None).is_some(), "{pred}");
    }

    // `not(test)` needs no decision from here: O-14(a) already treats it as
    // unconditionally no_std, on the separate grounds that nothing this tool
    // builds sets `test`.
    assert!(!is_decidable(&parse(r#"#![cfg_attr(not(test), no_std)]"#)));
}

/// A predicate naming a feature is not this rule's business. `#![cfg_attr(not(
/// feature = "std"), no_std)]` already yields a solver equation and a no_std
/// condition; second-guessing it here would put the target axis in front of the
/// crate's own feature switch.
#[test]
fn a_predicate_naming_a_feature_is_left_to_the_solver() {
    assert!(parse(r#"#![cfg_attr(not(feature = "std"), no_std)]"#).mentions("feature"));
    assert!(
        parse(r#"#![cfg_attr(all(target_os = "none", feature = "nostd"), no_std)]"#)
            .mentions("feature"),
        "a mixed predicate counts as naming a feature — utralib 0.1.25, xous-api-log"
    );
    assert!(!parse(r#"#![cfg_attr(target_os = "none", no_std)]"#).mentions("feature"));
}

#![feature(rustc_private)]

//! Regression tests for std usage guarded by a `#[cfg(...)]` that names no
//! feature — `target_arch`/`target_os` (canopen_rust, hpm_rt), a build-script
//! `--cfg` (discord_indexmap), `test`, and so on.
//!
//! `parse_token_stream` only turns `feature = "..."` into a Z3 Bool; every other
//! predicate atom is recorded as a bare string and its literal dropped. Such a
//! cfg therefore yields `None`, which is indistinguishable from having no
//! `#[cfg]` at all — that is how target-gated std came to be reported as
//! unguarded. `externally_gated_for_span` answers the question the
//! `Option<Vec<Bool>>` return cannot express.

use std::path::{Path, PathBuf};

use nostd::types::ReadableSpan;
use nostd::visitor::{ModCollector, ancestors_for_span, externally_gated_for_span};

fn fixture() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/external_cfg_gate/lib.rs")
}

fn span_of(content: &str, needle: &str) -> ReadableSpan {
    let idx = content
        .find(needle)
        .unwrap_or_else(|| panic!("substring {needle:?} not found in fixture"));
    let before = &content[..idx];
    let line = before.matches('\n').count() + 1;
    let col = idx - before.rfind('\n').map(|p| p + 1).unwrap_or(0);
    ReadableSpan {
        file: "lib.rs".to_string(),
        start_line: line,
        start_col: col,
        end_line: line,
        end_col: col + needle.len(),
        usage_crate: Some("std".to_string()),
    }
}

fn with_tree<F: FnOnce(&nostd::visitor::ModNode<'_>, &str)>(f: F) {
    let root = fixture();
    let content = std::fs::read_to_string(&root).unwrap();
    let cfg = z3::Config::new();
    let ctx = z3::Context::new(&cfg);
    let mut collector = ModCollector::new(&ctx);
    let tree = collector.collect(&root, "lib");
    f(&tree, &content);
}

#[test]
fn target_cfg_module_is_externally_gated() {
    with_tree(|tree, content| {
        let span = span_of(content, "std::collections::HashMap");
        assert!(
            externally_gated_for_span(tree, &span),
            "`mod std_items` under #[cfg(all(target_arch, target_os))] must be externally gated"
        );
    });
}

#[test]
fn cfg_if_else_arm_is_externally_gated() {
    with_tree(|tree, content| {
        // The else-arm gate is `not(all(target_arch, target_os))`. Polarity must
        // not matter: substituting `false` for the target atoms would make the
        // if-condition false, *select* the else arm, and leave its std usage
        // reported. Neither arm names a feature, so both are excused.
        let span = span_of(content, "std::process::abort");
        assert!(
            externally_gated_for_span(tree, &span),
            "a cfg_if else-arm gated only by target predicates must be externally gated"
        );
    });
}

#[test]
fn build_script_cfg_is_externally_gated() {
    with_tree(|tree, content| {
        let span = span_of(content, "std::collections::hash_map::RandomState");
        assert!(
            externally_gated_for_span(tree, &span),
            "#[cfg(has_std)] is a build-script cfg, not a feature — must be externally gated"
        );
    });
}

#[test]
fn statement_level_target_cfg_is_externally_gated() {
    with_tree(|tree, content| {
        let span = span_of(content, "std::fs::File::open");
        assert!(
            externally_gated_for_span(tree, &span),
            "a statement-level target cfg must push a LocalItem and be externally gated"
        );
    });
}

#[test]
fn negated_single_atom_cfg_is_externally_gated() {
    with_tree(|tree, content| {
        // backtrace: `#[cfg(not(backtrace_in_libstd))] extern crate std as mystd;`.
        // The atom lives in a group that yields no feature Bool, so the
        // group-empty branch of `parse_token_stream` truncated the one constant
        // proving a gate exists. `not(all(a, b))` survived only because it
        // contributed two constants and missed the `+ 1` test.
        let span = span_of(content, "std::sync::atomic::AtomicUsize");
        assert!(
            externally_gated_for_span(tree, &span),
            "#[cfg(not(has_std))] names no feature and must be externally gated"
        );
    });
}

#[test]
fn negated_single_atom_extern_crate_is_externally_gated() {
    with_tree(|tree, content| {
        // The gateway anchor itself. `resolve_local_facade_gateways` propagates
        // this declaration's crate to every use site of the alias, so if the
        // anchor is not excusable neither is anything downstream of it.
        let span = span_of(content, "extern crate std as mystd");
        assert!(
            externally_gated_for_span(tree, &span),
            "a gated `extern crate std as X` anchor must be externally gated"
        );
    });
}

// ---------------------------------------------------------------------------
// Parse-level tests. `is_externally_gated` reduces to `equation.is_none() &&
// !constants.is_empty()`, so the truncation is observable directly on
// `ParsedAttr` — including the `feature(no_std)` case the truncation exists to
// protect, which has no route through the tree API.
// ---------------------------------------------------------------------------

fn constants_of(attr: syn::Attribute) -> (bool, Vec<String>) {
    let cfg = z3::Config::new();
    let ctx = z3::Context::new(&cfg);
    let (equation, parsed) = nostd::parser::parse_main_attributes_direct_with(&attr, &ctx, None);
    (equation.is_some(), parsed.constants)
}

#[test]
fn negated_single_atom_keeps_its_constant() {
    let (has_eq, constants) = constants_of(syn::parse_quote!(#[cfg(not(backtrace_in_libstd))]));
    assert!(!has_eq, "a non-feature cfg yields no equation");
    assert_eq!(
        constants,
        vec!["backtrace_in_libstd".to_string()],
        "the atom must survive as evidence that a gate was written"
    );
}

#[test]
fn feature_group_no_std_is_still_truncated() {
    // CONTROL for the fix. This is the shape the truncation exists for: a group
    // directly following a `feature` ident. Were `no_std` left in `constants`,
    // `is_no_std` would read this crate as unconditionally no_std.
    let (_, constants) = constants_of(syn::parse_quote!(#[cfg_attr(nightly, feature(no_std))]));
    assert!(
        !constants.contains(&"no_std".to_string()),
        "`feature(no_std)` must not leak a no_std constant"
    );
}

// ---------------------------------------------------------------------------
// Controls — these must NOT be excused, or the rule is a blanket amnesty.
// ---------------------------------------------------------------------------

#[test]
fn negated_single_feature_is_not_externally_gated() {
    // The polarity mirror of the fix, on the feature axis. `not(feature = "std")`
    // produces a real Bool, so the group is non-empty and never reaches the
    // truncation at all — it must stay probe-able rather than being swept up.
    let (has_eq, constants) = constants_of(syn::parse_quote!(#[cfg(not(feature = "std"))]));
    assert!(has_eq, "a negated feature gate must still yield an equation");
    assert!(
        constants.is_empty(),
        "a feature gate contributes no bare constants"
    );
}

#[test]
fn plain_feature_gate_is_not_externally_gated() {
    with_tree(|tree, content| {
        let span = span_of(content, "std::vec::Vec");
        assert!(
            !externally_gated_for_span(tree, &span),
            "#[cfg(feature = \"std\")] is on the feature axis and must stay probe-able"
        );
        assert!(
            ancestors_for_span(tree, &span).is_some(),
            "a feature gate must still yield probe ancestors"
        );
    });
}

#[test]
fn mixed_predicate_keeps_its_feature_gate() {
    with_tree(|tree, content| {
        // `all(target_os = "linux", feature = "std")` parses to just `std` —
        // the non-feature atom is already projected out existentially. It must
        // remain a normal feature gate, not get excused wholesale.
        let span = span_of(content, "std::fs::File;");
        assert!(
            !externally_gated_for_span(tree, &span),
            "a mixed predicate naming a feature must not be excused"
        );
        assert!(
            ancestors_for_span(tree, &span).is_some(),
            "the feature half of a mixed predicate must still gate the span"
        );
    });
}

#[test]
fn ungated_std_is_not_externally_gated() {
    with_tree(|tree, content| {
        let span = span_of(content, "std::io::Write");
        assert!(
            !externally_gated_for_span(tree, &span),
            "genuinely ungated std must remain hard std"
        );
        assert!(
            ancestors_for_span(tree, &span).is_none(),
            "ungated std has no ancestors"
        );
    });
}

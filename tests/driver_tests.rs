#![feature(rustc_private)]

use nostd::driver::{
    extract_hard_std_candidates, is_local_reexport, load_plugin_output, neutralize_panic_expansions,
    resolve_local_facade_gateways,
};
use nostd::phases::classify_spans;
use nostd::types::{
    CoveringRun, FeatureRunOutput, PathContext, PathRecord, ReadableSpan, SpanVerdict,
};

fn span(usage_crate: Option<&str>) -> ReadableSpan {
    ReadableSpan {
        file: "src/lib.rs".to_string(),
        start_line: 1,
        start_col: 0,
        end_line: 1,
        end_col: 10,
        usage_crate: usage_crate.map(String::from),
    }
}

fn extern_crate_record(alias: &str, target: &str, defining_module: &str) -> PathRecord {
    PathRecord {
        path_text: alias.to_string(),
        definition_crate: target.to_string(),
        context: PathContext::ImportDeclaration,
        span: span(Some(target)),
        local_route: None,
        defining_module: Some(defining_module.to_string()),
        macro_body_cfgs: vec![],
        is_extern_crate: true,
        gateway_anchor: None,
    }
}

fn usage_record(
    path_text: &str,
    definition_crate: &str,
    usage_crate: Option<&str>,
    local_route: Option<&str>,
) -> PathRecord {
    PathRecord {
        path_text: path_text.to_string(),
        definition_crate: definition_crate.to_string(),
        context: PathContext::Other,
        span: span(usage_crate),
        local_route: local_route.map(String::from),
        defining_module: None,
        macro_body_cfgs: vec![],
        is_extern_crate: false,
        gateway_anchor: None,
    }
}

fn output(records: Vec<PathRecord>) -> FeatureRunOutput {
    FeatureRunOutput {
        records,
        macro_module_imports: vec![],
        out_dir: None,
    }
}

// ---------------------------------------------------------------------------
// resolve_local_facade_gateways
// ---------------------------------------------------------------------------

/// Core case: `extern crate std` inside the exact module that local_route names
/// upgrades usage_crate from core to std.
#[test]
fn facade_upgrades_core_to_std_when_extern_crate_in_same_module() {
    let mut out = output(vec![
        extern_crate_record("std", "std", "crate::std_shim::error"),
        usage_record(
            "crate::std_shim::error::Error",
            "core",
            Some("core"),
            Some("crate::std_shim::error"),
        ),
    ]);

    resolve_local_facade_gateways(&mut out);

    assert_eq!(out.records[1].span.usage_crate.as_deref(), Some("std"));
}

/// `extern crate std` in the *parent* module propagates down to deeper local routes.
#[test]
fn facade_upgrades_when_extern_crate_in_parent_module() {
    let mut out = output(vec![
        extern_crate_record("std", "std", "crate::std_shim"),
        usage_record(
            "crate::std_shim::error::Error",
            "core",
            Some("core"),
            Some("crate::std_shim::error"),
        ),
    ]);

    resolve_local_facade_gateways(&mut out);

    assert_eq!(out.records[1].span.usage_crate.as_deref(), Some("std"));
}

/// Records already at usage_crate=std are left unchanged.
#[test]
fn facade_does_not_overwrite_already_std() {
    let mut out = output(vec![
        extern_crate_record("std", "std", "crate::lib"),
        usage_record("crate::lib::Vec", "alloc", Some("std"), Some("crate::lib")),
    ]);

    resolve_local_facade_gateways(&mut out);

    assert_eq!(out.records[1].span.usage_crate.as_deref(), Some("std"));
}

/// A `use std::vec::Vec` import (is_extern_crate=false) must NOT trigger
/// gateway resolution — prevents the false positive from crates that have an
/// unconditional `use std::SomeType` in a module that conditionally uses core.
#[test]
fn facade_ignores_use_imports_not_extern_crate_decls() {
    let use_import = PathRecord {
        path_text: "std::vec::Vec".to_string(),
        definition_crate: "alloc".to_string(),
        context: PathContext::ImportDeclaration,
        span: span(Some("std")),
        local_route: None,
        defining_module: Some("crate::lib".to_string()),
        macro_body_cfgs: vec![],
        is_extern_crate: false, // not an extern crate declaration
        gateway_anchor: None,
    };

    let mut out = output(vec![
        use_import,
        // feature=use_std OFF: Error correctly resolves to core, should stay core
        usage_record(
            "crate::lib::error::Error",
            "core",
            Some("core"),
            Some("crate::lib"),
        ),
    ]);

    resolve_local_facade_gateways(&mut out);

    assert_eq!(
        out.records[1].span.usage_crate.as_deref(),
        Some("core"),
        "unconditional `use std::X` imports must not cause false-positive gateway upgrade"
    );
}

/// No extern crate records at all → output is unchanged.
#[test]
fn facade_noop_when_no_extern_crate_records() {
    let mut out = output(vec![usage_record(
        "crate::local::Type",
        "my_crate",
        Some("core"),
        Some("crate::local"),
    )]);

    resolve_local_facade_gateways(&mut out);

    assert_eq!(out.records[0].span.usage_crate.as_deref(), Some("core"));
}

// ---------------------------------------------------------------------------
// gateway_anchor propagation
//
// The gateway is only std because an `extern crate` declaration says so, so a
// `#[cfg]` on that declaration gates every use site downstream — even though
// those use sites carry no attribute. Recording which declaration a record
// inherited from is what lets the gate travel the resolution route (backtrace:
// one gated `extern crate std as mystd`, 18 unannotated uses of the alias).
// ---------------------------------------------------------------------------

/// An anchor at a distinguishable location, so an inherited anchor can be told
/// apart from the record's own span.
fn anchor_at(line: usize, defining_module: &str) -> PathRecord {
    let mut r = extern_crate_record("mystd", "std", defining_module);
    r.span.start_line = line;
    r.span.end_line = line;
    r
}

#[test]
fn upgraded_record_records_the_anchor_it_inherited_from() {
    let mut out = output(vec![
        anchor_at(27, "crate::symbolize::gimli"),
        usage_record(
            "crate::symbolize::gimli::mystd::fs::File",
            "core",
            Some("core"),
            Some("crate::symbolize::gimli"),
        ),
    ]);

    resolve_local_facade_gateways(&mut out);

    assert_eq!(out.records[1].span.usage_crate.as_deref(), Some("std"));
    let anchor = out.records[1]
        .gateway_anchor
        .as_ref()
        .expect("an upgraded record must record its anchor");
    assert_eq!(
        anchor.start_line, 27,
        "the anchor must be the extern crate declaration's span, not the use site's"
    );
}

#[test]
fn record_not_upgraded_has_no_anchor() {
    // CONTROL: the anchor is in an unrelated module, so no upgrade happens and
    // nothing may be excused downstream.
    let mut out = output(vec![
        anchor_at(27, "crate::other_module"),
        usage_record(
            "crate::local::Type",
            "core",
            Some("core"),
            Some("crate::local"),
        ),
    ]);

    resolve_local_facade_gateways(&mut out);

    assert_eq!(out.records[1].span.usage_crate.as_deref(), Some("core"));
    assert!(
        out.records[1].gateway_anchor.is_none(),
        "a record that was never upgraded must not carry an anchor to be excused by"
    );
}

#[test]
fn already_std_record_gets_no_anchor() {
    // CONTROL: records already at std are skipped before the route walk, so they
    // gain no anchor — a genuine ungated `use std::X` must not become excusable
    // just because some other module happens to declare a gated extern crate.
    let mut out = output(vec![
        anchor_at(27, "crate::lib"),
        usage_record("crate::lib::Vec", "alloc", Some("std"), Some("crate::lib")),
    ]);

    resolve_local_facade_gateways(&mut out);

    assert!(
        out.records[1].gateway_anchor.is_none(),
        "an already-std record must not inherit an anchor"
    );
}

// ---------------------------------------------------------------------------
// The crate root is not a facade module
//
// The prefix walk descends to the bare `crate` prefix, which every
// crate-internal route shares. A root-level `#[cfg(feature = "std")]
// extern crate std;` — the ordinary way to name std in a `#![no_std]` crate —
// would therefore stamp `usage_crate = "std"` onto every `use crate::…` in the
// crate, and those spans carry no `#[cfg]`, so the probe short-circuits them to
// StillStd without compiling. Guaranteed false positive.
// ---------------------------------------------------------------------------

/// regalloc2 shape: `#[cfg(feature = "std")] extern crate std;` at the crate
/// root, and `use crate::alloc::vec::Vec` in a submodule, which the resolver
/// correctly reports as `alloc` with a bare `crate` route.
#[test]
fn root_extern_crate_does_not_upgrade_bare_crate_route() {
    let mut out = output(vec![
        extern_crate_record("std", "std", "crate"),
        usage_record(
            "crate::alloc::vec::Vec",
            "alloc",
            Some("alloc"),
            Some("crate"),
        ),
    ]);

    resolve_local_facade_gateways(&mut out);

    assert_eq!(
        out.records[1].span.usage_crate.as_deref(),
        Some("alloc"),
        "a crate-internal path must not become std just because the crate root \
         declares `extern crate std`"
    );
    assert!(
        out.records[1].gateway_anchor.is_none(),
        "no upgrade means no anchor to be excused by"
    );
}

/// celestia-tendermint shape: the route is a proper submodule, but the prefix
/// walk still descends past it to the bare `crate` prefix. Guards the descent
/// specifically — a fix that only skipped routes *equal* to `crate` would leave
/// this one stamped.
#[test]
fn root_extern_crate_does_not_upgrade_deeper_route() {
    let mut out = output(vec![
        extern_crate_record("std", "std", "crate"),
        usage_record(
            "crate::serializers::cow_str::CowStr",
            "tendermint_proto",
            Some("tendermint_proto"),
            Some("crate::serializers"),
        ),
    ]);

    resolve_local_facade_gateways(&mut out);

    assert_eq!(
        out.records[1].span.usage_crate.as_deref(),
        Some("tendermint_proto"),
        "the prefix walk must not fall back to the crate root"
    );
}

/// CONTROL: a genuine `use std::X` in a crate whose root declares
/// `extern crate std` must still be std. It arrives already at `usage_crate ==
/// "std"` from the resolver, so it never depended on the root anchor — this
/// asserts the fix did not cost us the detection.
#[test]
fn genuine_std_use_survives_alongside_root_extern_crate() {
    let mut out = output(vec![
        extern_crate_record("std", "std", "crate"),
        usage_record("std::fs::File", "std", Some("std"), None),
    ]);

    resolve_local_facade_gateways(&mut out);

    assert_eq!(out.records[1].span.usage_crate.as_deref(), Some("std"));
}

/// CONTROL: a submodule facade still upgrades even when the crate root *also*
/// declares `extern crate std`. Dropping root anchors must not drop the
/// submodule anchor that shares the record's route (backtrace: a root
/// `extern crate std` at lib.rs plus `extern crate std as mystd` in
/// symbolize/gimli.rs — only the latter is load-bearing).
#[test]
fn submodule_facade_still_upgrades_when_root_also_declares_extern_crate() {
    let mut out = output(vec![
        extern_crate_record("std", "std", "crate"),
        anchor_at(27, "crate::symbolize::gimli"),
        usage_record(
            "crate::symbolize::gimli::mystd::fs::File",
            "core",
            Some("core"),
            Some("crate::symbolize::gimli"),
        ),
    ]);

    resolve_local_facade_gateways(&mut out);

    assert_eq!(out.records[2].span.usage_crate.as_deref(), Some("std"));
    assert_eq!(
        out.records[2]
            .gateway_anchor
            .as_ref()
            .expect("submodule anchor must still be recorded")
            .start_line,
        27,
    );
}

/// A root-level `extern crate` for a non-std crate is dropped as an anchor too —
/// the rule is about the root carrying no information, not about std specifically.
#[test]
fn root_anchor_is_dropped_regardless_of_target_crate() {
    let mut out = output(vec![
        extern_crate_record("alloc", "alloc", "crate"),
        extern_crate_record("std", "std", "crate"),
        usage_record("crate::local::Ty", "my_crate", Some("core"), Some("crate")),
    ]);

    resolve_local_facade_gateways(&mut out);

    assert_eq!(out.records[2].span.usage_crate.as_deref(), Some("core"));
}

// ---------------------------------------------------------------------------
// is_local_reexport
// ---------------------------------------------------------------------------

/// A path that the HIR resolved through an external gateway (usage_crate set to
/// an external crate) is never a local re-export, even when it starts with crate::.
#[test]
fn is_local_reexport_false_when_usage_crate_is_external() {
    let r = usage_record(
        "crate::std_shim::error::Error",
        "core",
        Some("std"),
        Some("crate::std_shim::error"),
    );
    assert!(!is_local_reexport(&r));
}

/// A path whose gateway is LOCAL (no external crate in scope) is a local re-export.
#[test]
fn is_local_reexport_true_when_usage_crate_is_local() {
    let r = usage_record(
        "crate::my_mod::MyType",
        "my_crate",
        Some("LOCAL"),
        Some("crate::my_mod"),
    );
    assert!(is_local_reexport(&r));
}

/// A path whose usage_crate is None falls back to the syntactic check.
#[test]
fn is_local_reexport_falls_back_to_syntactic_check_on_none() {
    let r = usage_record("crate::foo::Bar", "some_crate", None, Some("crate::foo"));
    assert!(is_local_reexport(&r));
}

// ---------------------------------------------------------------------------
// resolve_macro_module_file / is_mod_rs_style
//
// A macro that declares `mod X;` (e.g. agnostic_lite's `cfg_time!`) reports its
// callsite file to the plugin; the driver must resolve X's source file honouring
// rustc's mod-rs rules. The bug fixed here: children of a non-mod-rs file
// `src/wasm.rs` live in `src/wasm/`, not the callsite's own directory `src/`.
// ---------------------------------------------------------------------------

use nostd::driver::{is_mod_rs_style, resolve_macro_module_file};
use std::path::{Path, PathBuf};

fn fixture(rel: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/macro_mod_resolve")
        .join(rel)
}

#[test]
fn resolves_child_of_non_mod_rs_file_into_its_subdir() {
    // THE BUG: `mod after;` declared in non-mod-rs `src/wasm.rs` must resolve to
    // `src/wasm/after.rs` — the old code looked in `src/after.rs` and gave up.
    let callsite = fixture("src/wasm.rs");
    let got = resolve_macro_module_file(&callsite, false, "after");
    assert_eq!(got, Some(fixture("src/wasm/after.rs")));
}

#[test]
fn resolves_child_of_entrypoint_in_same_dir() {
    // `mod wasm;` declared in the crate root resolves beside it: `src/wasm.rs`.
    let callsite = fixture("src/lib.rs");
    let got = resolve_macro_module_file(&callsite, true, "wasm");
    assert_eq!(got, Some(fixture("src/wasm.rs")));
}

#[test]
fn resolves_child_of_mod_rs_in_same_dir() {
    // `mod timeout;` declared in `src/time/mod.rs` resolves to `src/time/timeout.rs`.
    let callsite = fixture("src/time/mod.rs");
    let got = resolve_macro_module_file(&callsite, true, "timeout");
    assert_eq!(got, Some(fixture("src/time/timeout.rs")));
}

#[test]
fn returns_none_when_no_source_file_exists() {
    let callsite = fixture("src/wasm.rs");
    assert_eq!(resolve_macro_module_file(&callsite, false, "does_not_exist"), None);
}

#[test]
fn is_mod_rs_style_classifies_entrypoint_modrs_and_plain_files() {
    let entry = fixture("src/lib.rs");
    assert!(is_mod_rs_style(&fixture("src/lib.rs"), &entry), "entrypoint is mod-rs style");
    assert!(is_mod_rs_style(&fixture("src/time/mod.rs"), &entry), "mod.rs is mod-rs style");
    assert!(!is_mod_rs_style(&fixture("src/wasm.rs"), &entry), "plain foo.rs is NOT mod-rs style");
}

// ---------------------------------------------------------------------------
// panic! expansions attributed to std
// ---------------------------------------------------------------------------

/// A record at one of std's `panic!` expansion paths is re-attributed to core:
/// the same source resolves to `core::panicking::*` once `extern crate std` is gone.
#[test]
fn std_panic_expansion_is_reattributed_to_core() {
    // `panic!("lit")` under `#[macro_use] extern crate std;`.
    let begin = usage_record("$crate::rt::begin_panic", "std", Some("std"), None);
    // `panic!("{}", x)` — note the definition crate is `core`, not `std`.
    let fmt = usage_record("$crate::rt::panic_fmt", "core", Some("std"), None);

    let mut out = output(vec![begin, fmt]);
    neutralize_panic_expansions(&mut out);

    for rec in &out.records {
        assert_eq!(
            rec.span.usage_crate.as_deref(),
            Some("core"),
            "{} should be attributed to core",
            rec.path_text
        );
    }
    assert!(extract_hard_std_candidates(&out, None).is_empty());
}

/// The regression this fix originally missed: `classify_spans` reads
/// `run.output.records` directly rather than going through
/// `extract_hard_std_candidates`, so filtering only the latter left the panic
/// span still failing the crate. Neutralising at load time must reach it.
#[test]
fn classify_spans_does_not_see_panic_expansion_as_std() {
    let mut out = output(vec![usage_record(
        "$crate::rt::begin_panic",
        "std",
        Some("std"),
        None,
    )]);
    neutralize_panic_expansions(&mut out);

    let runs = vec![CoveringRun {
        features: vec!["std".to_string()],
        output: out,
    }];
    let analyses = classify_spans(&runs);

    assert_eq!(analyses.len(), 1);
    assert!(
        matches!(analyses[0].verdict, SpanVerdict::NeverStd),
        "panic expansion should classify as NeverStd, got {:?}",
        analyses[0].verdict
    );
}

/// Control: the rewrite keys on the expansion path, not on "something to do with
/// panicking". A real `use std::panic::catch_unwind` is std-only and must survive.
#[test]
fn genuine_std_panic_api_is_still_a_hard_candidate() {
    let mut out = output(vec![
        usage_record("std::panic::catch_unwind", "std", Some("std"), None),
        usage_record("std::panic::panic_any", "std", Some("std"), None),
    ]);
    neutralize_panic_expansions(&mut out);

    assert_eq!(
        extract_hard_std_candidates(&out, None).len(),
        2,
        "std::panic::* APIs must not be excused"
    );
}

/// Control: ordinary std usage in the same output is untouched, so the rewrite
/// cannot be passing by simply blanking every record.
#[test]
fn panic_rewrite_leaves_other_std_usage_alone() {
    let mut out = output(vec![
        usage_record("$crate::rt::begin_panic", "std", Some("std"), None),
        usage_record("std::fs::File", "std", Some("std"), None),
    ]);
    neutralize_panic_expansions(&mut out);

    let got = extract_hard_std_candidates(&out, None);
    assert_eq!(got.len(), 1, "only the panic record should be dropped");
}

/// Wiring guard. The unit tests above call `neutralize_panic_expansions`
/// directly, so they stay green even if nothing ever invokes it — which is the
/// bug that shipped the first time: the rewrite lived in
/// `extract_hard_std_candidates`, a function the failing code path never calls.
/// This drives the real entry point, so removing the call from
/// `load_plugin_output` turns it red.
#[test]
fn load_plugin_output_neutralizes_panic_expansions() {
    let dir = std::env::temp_dir().join(format!("nostd_panic_wiring_{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join("plugin.json");

    let out = output(vec![
        usage_record("$crate::rt::begin_panic", "std", Some("std"), None),
        usage_record("std::fs::File", "std", Some("std"), None),
    ]);
    std::fs::write(&path, serde_json::to_string(&out).unwrap()).unwrap();

    let loaded = load_plugin_output(&path).expect("load");
    let by_path = |p: &str| {
        loaded
            .records
            .iter()
            .find(|r| r.path_text == p)
            .unwrap()
            .span
            .usage_crate
            .clone()
    };

    assert_eq!(by_path("$crate::rt::begin_panic").as_deref(), Some("core"));
    assert_eq!(by_path("std::fs::File").as_deref(), Some("std"));

    let _ = std::fs::remove_dir_all(&dir);
}

// ---------------------------------------------------------------------------
// resolve_import_to_use_gateways — a routeless bare std use inherits the gate
// of the externally-gated import that introduced its name (canopen_rust shape).
// ---------------------------------------------------------------------------

mod import_to_use {
    use super::*;
    use nostd::driver::resolve_import_to_use_gateways;
    use nostd::visitor::{LocalItem, ModNode};
    use std::path::PathBuf;

    /// A record at a specific line so import and use spans are distinguishable.
    fn at(mut r: PathRecord, line: usize) -> PathRecord {
        r.span.start_line = line;
        r.span.end_line = line;
        r
    }

    fn std_import(path_text: &str, line: usize) -> PathRecord {
        let mut r = usage_record(path_text, "std", Some("std"), None);
        r.context = PathContext::ImportDeclaration;
        r.defining_module = Some("crate::prelude::std_items".to_string());
        at(r, line)
    }

    /// A routeless bare std use (no local_route, no defining_module).
    fn bare_std_use(path_text: &str, line: usize) -> PathRecord {
        let mut r = usage_record(path_text, "std", Some("std"), None);
        r.context = PathContext::Type;
        at(r, line)
    }

    /// A tree over `src/lib.rs` that reports exactly the given lines as
    /// externally gated (one gated `LocalItem` per line, spanning cols 0..200).
    fn tree_gating(gated_lines: &[usize]) -> ModNode<'static> {
        let local_items = gated_lines
            .iter()
            .map(|&line| LocalItem {
                own_condition: None,
                span: ReadableSpan {
                    file: "src/lib.rs".to_string(),
                    start_line: line,
                    start_col: 0,
                    end_line: line,
                    end_col: 200,
                    usage_crate: None,
                },
                name: None,
                externally_gated: true,
            })
            .collect();
        ModNode {
            name: "crate".to_string(),
            source_file: PathBuf::from("src/lib.rs"),
            source_dir: PathBuf::from("src"),
            entry_condition: None,
            local_items,
            children: vec![],
            is_inline: false,
            externally_gated: false,
        }
    }

    /// Core case: the only std import of `HashMap` is externally gated, so the
    /// bare use inherits its gate via `gateway_anchor`.
    #[test]
    fn gated_import_lends_its_anchor_to_the_bare_use() {
        let mut out = output(vec![
            std_import("std::collections::HashMap", 10),
            bare_std_use("HashMap", 20),
        ]);

        resolve_import_to_use_gateways(&mut out, &tree_gating(&[10]));

        let anchor = out.records[1]
            .gateway_anchor
            .as_ref()
            .expect("the bare use must inherit the gated import's anchor");
        assert_eq!(
            anchor.start_line, 10,
            "the anchor must be the import's span, not the use site's"
        );
    }

    /// `HashMap::new` — the use's first path segment is the bound name.
    #[test]
    fn method_call_use_is_matched_by_first_segment() {
        let mut out = output(vec![
            std_import("std::collections::HashMap", 10),
            bare_std_use("HashMap::new", 20),
        ]);

        resolve_import_to_use_gateways(&mut out, &tree_gating(&[10]));

        assert!(out.records[1].gateway_anchor.is_some());
    }

    /// CONTROL: an *ungated* std import of the name must not excuse the use —
    /// the name genuinely resolves to std even in the no_std configuration.
    #[test]
    fn ungated_import_does_not_excuse_the_use() {
        let mut out = output(vec![
            std_import("std::collections::HashMap", 10),
            bare_std_use("HashMap", 20),
        ]);

        // line 10 is NOT gated
        resolve_import_to_use_gateways(&mut out, &tree_gating(&[]));

        assert!(
            out.records[1].gateway_anchor.is_none(),
            "an ungated import must not lend a gate"
        );
    }

    /// CONTROL: the all-imports-must-agree rule. `HashMap` is imported both by a
    /// gated import and by an unrelated ungated one — refuse to excuse, since the
    /// ungated path keeps the name std-resolving without the gate.
    #[test]
    fn mixed_gated_and_ungated_imports_do_not_excuse() {
        let mut out = output(vec![
            std_import("std::collections::HashMap", 10), // gated
            std_import("std::collections::HashMap", 15), // ungated
            bare_std_use("HashMap", 20),
        ]);

        resolve_import_to_use_gateways(&mut out, &tree_gating(&[10]));

        assert!(
            out.records[2].gateway_anchor.is_none(),
            "any ungated import of the name must block the excuse"
        );
    }

    /// CONTROL: a differently-named bare use is not touched by a gated import.
    #[test]
    fn name_must_match() {
        let mut out = output(vec![
            std_import("std::collections::HashMap", 10),
            bare_std_use("Vec", 20),
        ]);

        resolve_import_to_use_gateways(&mut out, &tree_gating(&[10]));

        assert!(out.records[1].gateway_anchor.is_none());
    }

    /// CONTROL: a *routed* use (has a local_route) is the facade pass's domain,
    /// not this one — left untouched here.
    #[test]
    fn routed_use_is_left_to_the_facade_pass() {
        let mut routed = usage_record("HashMap", "std", Some("std"), Some("crate::foo"));
        routed.context = PathContext::Type;
        let mut out = output(vec![std_import("std::collections::HashMap", 10), at(routed, 20)]);

        resolve_import_to_use_gateways(&mut out, &tree_gating(&[10]));

        assert!(out.records[1].gateway_anchor.is_none());
    }

    /// CONTROL: an already-anchored use is not re-anchored.
    #[test]
    fn existing_anchor_is_preserved() {
        let mut used = bare_std_use("HashMap", 20);
        used.gateway_anchor = Some(span(Some("std")));
        let existing = used.gateway_anchor.clone();
        let mut out = output(vec![std_import("std::collections::HashMap", 10), used]);

        resolve_import_to_use_gateways(&mut out, &tree_gating(&[10]));

        assert_eq!(out.records[1].gateway_anchor, existing);
    }

    /// CONTROL: a non-std import (e.g. the no_std arm binding `HashMap` to
    /// hashbrown) is ignored — only std imports establish the gate set, and only
    /// std uses are excused.
    #[test]
    fn non_std_import_is_ignored() {
        let mut hashbrown = usage_record("hashbrown::HashMap", "hashbrown", Some("hashbrown"), None);
        hashbrown.context = PathContext::ImportDeclaration;
        let mut out = output(vec![at(hashbrown, 10), bare_std_use("HashMap", 20)]);

        resolve_import_to_use_gateways(&mut out, &tree_gating(&[10]));

        assert!(
            out.records[1].gateway_anchor.is_none(),
            "no std import of `HashMap` exists, so there is nothing to inherit"
        );
    }
}

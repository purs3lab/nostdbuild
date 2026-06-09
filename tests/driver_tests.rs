#![feature(rustc_private)]

use nostd::driver::{is_local_reexport, resolve_local_facade_gateways};
use nostd::types::{FeatureRunOutput, PathContext, PathRecord, ReadableSpan};

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
    }
}

fn output(records: Vec<PathRecord>) -> FeatureRunOutput {
    FeatureRunOutput {
        records,
        macro_module_imports: vec![],
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

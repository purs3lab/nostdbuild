use log::{debug, warn};
use proc_macro2::Span;
use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::{fs, process::Command};
use uuid::Uuid;
use which::which;
use z3::ast::Ast;

use z3::{Context, ast::Bool};

use serde_json;

use crate::phases::*;
use crate::types::*;
use crate::visitor::{self, ModCollector, ModNode};
use crate::{
    ReadableSpan, Telemetry,
    consts::{self, PLUGIN_OUTPUT_ENV},
    downloader, parser, solver,
};

fn unique_output_path(crate_name: &str) -> PathBuf {
    let sanitized = crate_name.replace('-', "_").replace(':', "-");
    let id = Uuid::new_v4();
    Path::new(consts::RESULTS_PATH).join(format!("{}__{}.json", sanitized, id))
}

pub fn load_plugin_output(path: &Path) -> Result<FeatureRunOutput, String> {
    let data = fs::read_to_string(path).map_err(|e| format!("read {:?}: {}", path, e))?;
    let mut out: FeatureRunOutput =
        serde_json::from_str(&data).map_err(|e| format!("parse {:?}: {}", path, e))?;
    // Rewrite build-script-generated (`OUT_DIR`) paths to a stable, hash-free
    // form so the same generated span aggregates across runs (the
    // `build/<pkg>-<hash>/out` hash changes with the feature set) and so it can
    // be matched against the `include!` site's condition in the module tree.
    for rec in &mut out.records {
        rec.span.file = normalize_generated_path(&rec.span.file);
    }
    neutralize_panic_expansions(&mut out);
    Ok(out)
}

/// Canonicalise a cargo build-script output path
/// (`…/build/<pkg>-<16hex>/out/<tail>`) to `$OUT_DIR/<pkg>/<tail>`. Any other
/// path is returned unchanged. The per-feature-set hash in `<pkg>-<hash>` is
/// dropped so a generated span has one stable identity across runs.
pub fn normalize_generated_path(file: &str) -> String {
    let Some(build_idx) = file.find("/build/") else {
        return file.to_string();
    };
    let after = &file[build_idx + "/build/".len()..];
    let Some(out_idx) = after.find("/out/") else {
        return file.to_string();
    };
    let dir = &after[..out_idx]; // <pkg>-<hash>
    let tail = &after[out_idx + "/out/".len()..];
    let Some(dash) = dir.rfind('-') else {
        return file.to_string();
    };
    let (pkg, hash) = (&dir[..dash], &dir[dash + 1..]);
    if hash.len() == 16 && hash.chars().all(|c| c.is_ascii_hexdigit()) {
        format!("$OUT_DIR/{}/{}", pkg, tail)
    } else {
        file.to_string()
    }
}

/// Whether `callsite` follows mod-rs conventions for locating its child modules:
/// either it is a `mod.rs`, or it is the crate entrypoint (`lib.rs`/`main.rs` or
/// a custom `[lib] path`). Such files' children live in the *same* directory;
/// every other file `foo.rs` keeps its children in a sibling directory `foo/`.
pub fn is_mod_rs_style(callsite: &Path, entry_path: &Path) -> bool {
    if callsite.file_name().is_some_and(|n| n == "mod.rs") {
        return true;
    }
    // The crate entrypoint is mod-rs style regardless of its file name.
    match (callsite.canonicalize(), entry_path.canonicalize()) {
        (Ok(a), Ok(b)) => a == b,
        _ => callsite == entry_path,
    }
}

/// Resolve the source file of a module `modname` that a macro declared inside
/// `callsite` (the source file where the macro was invoked).
///
/// Mirrors rustc's module-file rules — the piece the previous inline resolution
/// got wrong: a macro like `cfg_time! { mod after; }` invoked in the non-mod-rs
/// file `src/wasm.rs` declares a module whose file is `src/wasm/after.rs`, *not*
/// `src/after.rs`. Children of a `mod.rs`/entrypoint live beside it; children of
/// a plain `foo.rs` live in the sibling `foo/` directory. Returns the existing
/// `<dir>/<modname>.rs` or `<dir>/<modname>/mod.rs`, or `None` if neither exists.
pub fn resolve_macro_module_file(
    callsite: &Path,
    is_mod_rs_style: bool,
    modname: &str,
) -> Option<PathBuf> {
    let parent = callsite.parent().unwrap_or(Path::new("."));
    let search_dir = if is_mod_rs_style {
        parent.to_path_buf()
    } else {
        let stem = callsite.file_stem().and_then(|s| s.to_str()).unwrap_or("");
        parent.join(stem)
    };
    let rs = search_dir.join(format!("{modname}.rs"));
    if rs.exists() {
        return Some(rs);
    }
    let mod_rs = search_dir.join(modname).join("mod.rs");
    if mod_rs.exists() {
        return Some(mod_rs);
    }
    None
}

pub fn extract_hard_std_candidates(
    out: &FeatureRunOutput,
    context_filter: Option<PathContext>,
) -> Vec<ReadableSpan> {
    out.records
        .iter()
        .filter(|r| context_filter.is_none_or(|ctx| r.context == ctx))
        .filter(|r| r.span.usage_crate.as_deref() == Some("std"))
        .filter(|r| !is_local_reexport(r))
        // .filter(|r| !r.span.is_dummy())
        .map(|r| r.span.clone())
        .collect()
}

/// Paths that `std`'s `panic!` expands to. `core` has a `panic!` of its own with
/// the same syntax, so a call site that lands on one of these never *requires*
/// std — it resolves here only because std happened to win the macro namespace.
///
/// **These two paths are lossy: three source forms collapse onto them, and only
/// the first is safe to excuse.** Verified against the plugin (edition 2018,
/// `#[macro_use] extern crate std;`) — all three emit `$crate::rt::begin_panic`
/// with `definition_crate: "std"`, byte-identical records:
///
/// ```ignore
/// panic!("lit")                         // core-compatible — the case we mean to excuse
/// std::panic!("explicit")               // std-only: `std::` does not resolve under no_std
/// panic!(some_value)                    // std-only: core's panic! takes no arbitrary payload
/// ```
///
/// The latter two are genuine std dependencies that this filter wrongly excuses,
/// i.e. false *negatives*. Neither occurs in the current false-positive set
/// (checked across all six affected crates), and the payload form is a hard
/// error from edition 2021 on, but both are reachable in principle.
///
/// Closing the gap means inverting this into a whitelist: have the syn visitor
/// collect the spans of `panic!` invocations it can prove core-compatible (macro
/// path is a bare `panic`, first argument is a string literal) and excuse only
/// records whose span matches one. syn tokenizes properly, so multi-line and
/// `concat!` arguments come for free. That follows the same rule as
/// `macro_rules_uniform_cfg`: yield nothing rather than guess, because a
/// spurious gate excuses real std.
const STD_PANIC_EXPANSIONS: [&str; 2] = ["$crate::rt::begin_panic", "$crate::rt::panic_fmt"];

/// Re-attribute `panic!` expansions from `std` to `core`.
///
/// A crate that writes `#[macro_use] extern crate std;` — or that is only
/// `no_std` in some configurations, via `#![cfg_attr(not(feature = "std"), no_std)]`
/// — pulls std's `panic!` into the macro namespace, shadowing core's. Every
/// `panic!` in the crate then expands to `$crate::rt::begin_panic` (string
/// literal) or `$crate::rt::panic_fmt` (format args) with `$crate` = std, and
/// each call site is reported as std usage. Drop the `extern crate std` and the
/// identical source resolves to `core::panicking::*` and compiles unchanged, so
/// `core` is the honest attribution.
///
/// Runs in `load_plugin_output`, the single point where plugin JSON enters the
/// system, because the consumers disagree about where they read records from:
/// `classify_spans` — which produces the verdict that fails a crate — walks
/// `run.output.records` directly and never sees `extract_hard_std_candidates`.
/// Filtering in one consumer leaves the other reporting the span.
///
/// Matching on `path_text` is deliberate: `panic_fmt` records carry
/// `definition_crate: "core"` while `begin_panic` carries `"std"`, so the
/// definition crate does not separate them. The literal `$crate` token cannot
/// appear in hand-written source, so this only ever matches macro output.
///
/// See `STD_PANIC_EXPANSIONS` for the two source forms this wrongly excuses and
/// what closing that gap would take.
pub fn neutralize_panic_expansions(out: &mut FeatureRunOutput) {
    for rec in &mut out.records {
        if STD_PANIC_EXPANSIONS.contains(&rec.path_text.as_str())
            && rec.span.usage_crate.as_deref() == Some("std")
        {
            rec.span.usage_crate = Some("core".to_string());
        }
    }
}

/// A record represents a local re-export if its syntactic path begins with
/// crate::, self::, or super::. We check local_route first (your JSON has it
/// populated for usage records); fall back to path_text if the record carries
/// one; otherwise treat as non-local.
///
/// If the HIR driver (or post-processing) has already resolved a non-LOCAL
/// gateway crate into `usage_crate`, the path provably goes through an external
/// crate and is therefore NOT a pure local re-export.
pub fn is_local_reexport(r: &PathRecord) -> bool {
    // If usage_crate is set and resolved to something other than LOCAL, the
    // gateway is already known to be external — not a local-only path.
    if matches!(r.span.usage_crate.as_deref(), Some(c) if c != "LOCAL") {
        return false;
    }
    let candidates = [r.local_route.as_deref(), Some(&r.path_text)];
    for c in candidates.iter().flatten() {
        let t = c.trim_start_matches("::");
        if t.starts_with("crate::")
            // || t.starts_with("self::")
            || t.starts_with("super::")
            || t == "crate"
            // || t == "self"
            || t == "super"
        {
            return true;
        }
    }
    false
}

/// Is this record's span excused by a non-feature cfg — either one written on
/// the span itself, or one on the `extern crate` declaration it inherited its
/// std gateway from?
///
/// The second half matters because `resolve_local_facade_gateways` is what makes
/// these spans std in the first place. backtrace writes an aliased
/// `extern crate std` once under a negated build-script cfg, then uses the alias
/// in three files with no attribute anywhere; the gate reaches those use sites
/// only by travelling the same resolution route the gateway did.
fn span_externally_gated(root: &ModNode<'_>, exemplar: &PathRecord) -> bool {
    visitor::externally_gated_for_span(root, &exemplar.span)
        || exemplar
            .gateway_anchor
            .as_ref()
            .is_some_and(|anchor| visitor::externally_gated_for_span(root, anchor))
}

/// How the crate root's module path is spelled in `PathRecord::defining_module`
/// and `local_route`. The plugin seeds `current_module_path` with this, so the
/// root is exactly `crate` — never the empty string, and never rustc's internal
/// `{{root}}` (which shows up as a `path_text` segment but never as a module).
pub const CRATE_ROOT_MODULE: &str = "crate";

/// For crates that wrap an external crate behind a local module facade (e.g.
/// `mod std { extern crate std; pub use std::*; mod error { extern crate std;
/// pub use std::error::Error; } }`), the HIR resolver sees the inner segments
/// as local and reports the CANONICAL definition crate (`core`) rather than
/// the intended gateway (`std`).
///
/// This function fixes that by post-processing the output: any `extern crate X`
/// declaration (identified by `is_extern_crate == true`) records the module
/// where it was declared.  Usage records whose `local_route` passes through one
/// of those modules then inherit the corresponding gateway crate in
/// `usage_crate`, and the declaration's span in `gateway_anchor` so a `#[cfg]`
/// on it can excuse them.
///
/// Only `extern crate` declarations (not glob `use` imports) are used as
/// anchors to avoid false positives from unconditional `use std::SomeType`
/// imports that happen to live in a module that also handles non-std paths.
///
/// A declaration at the **crate root** is never an anchor. The prefix walk below
/// descends to the bare `crate` prefix, which every crate-internal route shares,
/// so a root-level `#[cfg(feature = "std")] extern crate std;` — the ordinary way
/// to name std in a `#![no_std]` crate — would stamp `usage_crate = "std"` onto
/// every `use crate::…` in the crate. The root prefix carries no information: a
/// match is evidence of passing through a facade only when the prefix is a proper
/// submodule. Nothing real is lost, because this pass only ever fires on records
/// the resolver classified as non-std, and both root-level shapes are already
/// resolved correctly without it — a genuine `use std::X` and an aliased
/// `extern crate std as alloc` both arrive with `usage_crate == "std"` and are
/// skipped by the early-continue below.
pub fn resolve_local_facade_gateways(out: &mut FeatureRunOutput) {
    // Build: module_path → gateway crate names, from extern crate declarations.
    // A module may declare multiple extern crates; collect all so any prefix
    // match on a local_route can find the right one.
    let mut module_extern_crates: std::collections::HashMap<String, Vec<(String, ReadableSpan)>> =
        std::collections::HashMap::new();

    for r in &out.records {
        if r.is_extern_crate
            && let Some(dm) = r.defining_module.as_deref()
            // The crate root is not a facade module — see the doc comment.
            // `current_module_path` is seeded with `["crate"]`, so the root
            // module path is spelled exactly `crate`.
            && dm != CRATE_ROOT_MODULE
        {
            module_extern_crates
                .entry(dm.to_string())
                .or_default()
                .push((r.definition_crate.clone(), r.span.clone()));
        }
    }

    if module_extern_crates.is_empty() {
        return;
    }

    debug!(
        "Modules with extern crate declarations: {:#?}",
        module_extern_crates
    );

    // For each usage record whose gateway wasn't already resolved to std,
    // walk the prefixes of its local_route and check if any ancestor module
    // declared `extern crate std`.
    for r in &mut out.records {
        if r.span.usage_crate.as_deref() == Some("std") {
            continue;
        }
        let local_route = match r.local_route.as_deref() {
            Some(lr) if !lr.is_empty() => lr,
            _ => continue,
        };

        // Generate all prefixes of local_route (longest first for earliest
        // specific match), e.g. "crate::std::error" → ["crate::std::error",
        // "crate::std", "crate"].
        let segments: Vec<&str> = local_route.split("::").collect();
        let found = (1..=segments.len()).rev().find_map(|len| {
            let prefix = segments[..len].join("::");
            module_extern_crates.get(&prefix).and_then(|crates| {
                crates
                    .iter()
                    .find(|(c, _)| c == "std")
                    .map(|(_, anchor)| anchor.clone())
            })
        });

        if let Some(anchor) = found {
            debug!(
                "For record with local_route '{}' and span {:?}, found std gateway in ancestors declared at {:?}",
                local_route, r.span, anchor
            );
            r.span.usage_crate = Some("std".to_string());
            r.gateway_anchor = Some(anchor);
        }
    }
}

/// The name a path binds or references: its first `::`-segment for a use site
/// (`HashMap::new` → `HashMap`), its last for an import (`std::collections::HashMap`
/// → `HashMap`, the name it brings into scope).
fn use_name(path_text: &str) -> Option<&str> {
    path_text.split("::").next().filter(|s| !s.is_empty())
}
fn import_bound_name(path_text: &str) -> Option<&str> {
    path_text.rsplit("::").next().filter(|s| !s.is_empty())
}

/// Propagate an externally-gated `use` import's gate onto the routeless bare
/// uses of the name it introduced.
///
/// A crate that splits std vs. no_std by a **non-feature cfg** (e.g. per-target)
/// typically imports the std item in one arm and its no_std replacement in the
/// other:
///
/// ```ignore
/// #[cfg(all(target_arch = "x86_64", target_os = "linux"))]
/// mod std_items { pub use std::collections::HashMap; }   // externally gated
/// #[cfg(...linux)] pub use std_items::*;
/// #[cfg(all(target_arch = "arm", target_os = "none"))]
/// mod no_std_items { pub use hashbrown::HashMap; }        // the no_std arm
///
/// use crate::prelude::*;
/// fn f(m: &HashMap<K, V>) {}   // bare `HashMap`, reported std on the host build
/// ```
///
/// The bare use resolves to `std::collections::HashMap` on the host, but carries
/// no `local_route` and no `defining_module`, so `resolve_local_facade_gateways`
/// (which needs a route) never links it back to the gated import.
///
/// We join import → use on the **bound name** (the import's last path segment,
/// the use's first), restricted to std records on both sides. This is more robust
/// than a resolved-`def_path` join, which does not survive std's re-exports:
/// `use std::string::String` resolves to `alloc::string::String`,
/// `use std::fmt::Debug` to `core::fmt::Debug` — the import's enclosing module is
/// not a prefix of the canonical item, so the two sides' def paths differ.
///
/// A routeless bare std use inherits a gate (via `gateway_anchor`, the same
/// mechanism `resolve_local_facade_gateways` uses) iff **every** std `use` import
/// that binds its name is externally gated. The all-imports-must-agree rule is
/// load-bearing: if any import of that name is *un*gated, the name genuinely
/// resolves to std even in the no_std configuration, so we attach nothing and let
/// the use fail. (This follows J's "yield nothing rather than guess" precedent.)
/// Because we only touch std-resolved records on both sides, a same-named *local*
/// item never enters — its uses resolve to the crate, not std.
pub fn resolve_import_to_use_gateways(out: &mut FeatureRunOutput, root: &ModNode<'_>) {
    // bound name → (every std import of it is externally gated?, an anchor span).
    // The anchor is the span of one such gated import; a `#[cfg]` on it gates the
    // inheriting uses.
    let mut import_gate: std::collections::HashMap<String, (bool, Option<ReadableSpan>)> =
        std::collections::HashMap::new();

    for r in &out.records {
        if r.context != PathContext::ImportDeclaration
            || r.span.usage_crate.as_deref() != Some("std")
        {
            continue;
        }
        let Some(name) = import_bound_name(&r.path_text) else {
            continue;
        };
        let gated = visitor::externally_gated_for_span(root, &r.span);
        let entry = import_gate.entry(name.to_string()).or_insert((true, None));
        entry.0 &= gated;
        if gated && entry.1.is_none() {
            entry.1 = Some(r.span.clone());
        }
    }

    // Keep only names whose every std import is externally gated and that have a
    // concrete anchor to point at.
    let gated_imports: std::collections::HashMap<String, ReadableSpan> = import_gate
        .into_iter()
        .filter_map(|(name, (all_gated, anchor))| match (all_gated, anchor) {
            (true, Some(a)) => Some((name, a)),
            _ => None,
        })
        .collect();

    if gated_imports.is_empty() {
        return;
    }

    for r in &mut out.records {
        // Only routeless bare std *uses* — the leak `resolve_local_facade_gateways`
        // cannot see. Imports (which carry the gate themselves) and routed facade
        // uses are excluded.
        if r.span.usage_crate.as_deref() != Some("std")
            || r.context == PathContext::ImportDeclaration
            || r.local_route.is_some()
            || r.defining_module.is_some()
            || r.gateway_anchor.is_some()
        {
            continue;
        }
        let Some(name) = use_name(&r.path_text) else {
            continue;
        };
        if let Some(anchor) = gated_imports.get(name) {
            debug!(
                "Routeless std use '{}' at {:?} inherits the gate of its externally-gated import(s) of `{}` at {:?}",
                r.path_text, r.span, name, anchor
            );
            r.gateway_anchor = Some(anchor.clone());
        }
    }
}

/// Runs the plugin with the crate's default features (no --no-default-features, no extra flags).
/// Used to produce a baseline for coverage comparison — simulating what a default-only tool sees.
pub fn run_default_features_pass(manifest: &str, crate_name: &str) -> PassOutcome {
    if !is_cargo_hir_installed() {
        return PassOutcome::CompileFailed {
            stderr: "cargo-hir is not installed or not found in PATH".to_string(),
            exit_code: None,
        };
    }

    let output_path = unique_output_path(crate_name);

    if output_path.exists()
        && let Err(e) = fs::remove_file(&output_path)
    {
        warn!(
            "Failed to remove stale plugin output {:?}: {}",
            output_path, e
        );
    }

    let args = ["hir", "--", "--manifest-path", manifest];

    debug!(
        "Running default-features pass for {}, output -> {:?}",
        crate_name, output_path
    );

    let output = match Command::new("cargo")
        .args(args)
        .env(PLUGIN_OUTPUT_ENV, &output_path)
        .output()
    {
        Ok(o) => o,
        Err(e) => {
            return PassOutcome::CompileFailed {
                stderr: format!("failed to spawn cargo: {}", e),
                exit_code: None,
            };
        }
    };

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
        debug!(
            "default-features pass failed for {} (exit {}): {}",
            crate_name,
            output.status.code().unwrap_or(-1),
            stderr
        );
        let _ = fs::remove_file(&output_path);
        return PassOutcome::CompileFailed {
            stderr,
            exit_code: output.status.code(),
        };
    }

    if !output_path.exists() {
        warn!(
            "Default-features pass succeeded but output missing at {:?}",
            output_path
        );
        return PassOutcome::PluginMissingOutput {
            expected_path: output_path,
        };
    }

    let mut full_output = match load_plugin_output(&output_path) {
        Ok(o) => o,
        Err(e) => {
            warn!(
                "Failed to parse default-features output {:?}: {}",
                output_path, e
            );
            let _ = fs::remove_file(&output_path);
            return PassOutcome::PluginMissingOutput {
                expected_path: output_path,
            };
        }
    };
    let _ = fs::remove_file(&output_path);

    resolve_local_facade_gateways(&mut full_output);
    let std_spans = extract_hard_std_candidates(&full_output, None);
    let macro_modules = full_output.macro_module_imports.clone();

    PassOutcome::Success {
        macro_modules,
        std_spans,
        full_output,
    }
}

pub fn compute_coverage_comparison(
    default_output: &FeatureRunOutput,
    covering_runs: &[CoveringRun],
) -> CoverageComparison {
    use std::collections::HashSet;

    let default_spans: HashSet<&ReadableSpan> =
        default_output.records.iter().map(|r| &r.span).collect();
    let covering_spans: HashSet<&ReadableSpan> = covering_runs
        .iter()
        .flat_map(|r| r.output.records.iter())
        .map(|r| &r.span)
        .collect();

    let default_std_spans: HashSet<&ReadableSpan> = default_output
        .records
        .iter()
        .filter(|r| r.span.usage_crate.as_deref() == Some("std"))
        .map(|r| &r.span)
        .collect();
    let covering_std_spans: HashSet<&ReadableSpan> = covering_runs
        .iter()
        .flat_map(|r| r.output.records.iter())
        .filter(|r| r.span.usage_crate.as_deref() == Some("std"))
        .map(|r| &r.span)
        .collect();

    let spans_only_in_covering = covering_spans.difference(&default_spans).count();
    let std_spans_only_in_covering = covering_std_spans.difference(&default_std_spans).count();

    CoverageComparison {
        default_unique_spans: default_spans.len(),
        covering_unique_spans: covering_spans.len(),
        spans_only_in_covering,
        default_std_spans: default_std_spans.len(),
        covering_std_spans: covering_std_spans.len(),
        std_spans_only_in_covering,
        num_covering_runs: covering_runs.len(),
    }
}

pub fn run_rustc_plugin_pass(
    manifest: &str,
    crate_name: &str,
    enable: &[String],
    context_filter: Option<PathContext>,
) -> PassOutcome {
    if !is_cargo_hir_installed() {
        return PassOutcome::CompileFailed {
            stderr: "cargo-hir is not installed or not found in PATH".to_string(),
            exit_code: None,
        };
    }

    let output_path = unique_output_path(crate_name);

    // Best-effort cleanup; if removal fails we'll detect a stale file via the
    // UUID being unique per call, so this is just hygiene.
    if output_path.exists()
        && let Err(e) = fs::remove_file(&output_path)
    {
        warn!(
            "Failed to remove stale plugin output {:?}: {}",
            output_path, e
        );
    }

    // TODO: Update to use main_name when running with dependencies.
    let feats = enable.join(",");

    // Restrict the pass to the library target when one exists. `find_entrypoints`
    // analyses the lib and ignores bins (`is_lib || (is_bin && !has_lib)`), so
    // without `--lib` cargo also builds the bin targets and the plugin emits HIR
    // records for files the ModNode tree does not cover. Gates only exist inside
    // that tree, so such a span finds no ancestor, is classified AlwaysStd, and is
    // reported as unguarded std usage — e.g. the `println!` in a stock
    // `fn main() { println!("Hello, world!"); }` sinks an otherwise no_std crate.
    // Bin-only crates keep building their bin, matching the entrypoint rule.
    let mut args = vec![
        "hir",
        "--",
        "--manifest-path",
        manifest,
        "--no-default-features",
        "--features",
        &feats,
    ];
    if visitor::package_has_lib(manifest) {
        args.push("--lib");
    }

    debug!(
        "Running rustc plugin pass for {} with features [{}], output -> {:?}",
        crate_name, feats, output_path
    );

    let output = match Command::new("cargo")
        .args(args)
        .env(PLUGIN_OUTPUT_ENV, &output_path)
        .output()
    {
        Ok(o) => o,
        Err(e) => {
            return PassOutcome::CompileFailed {
                stderr: format!("failed to spawn cargo: {}", e),
                exit_code: None,
            };
        }
    };

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
        debug!(
            "cargo hir failed for {} (exit {}): {}",
            crate_name,
            output.status.code().unwrap_or(-1),
            stderr
        );
        let _ = fs::remove_file(&output_path);
        return PassOutcome::CompileFailed {
            stderr,
            exit_code: output.status.code(),
        };
    }

    debug!("cargo hir succeeded for {}", crate_name,);

    if !output_path.exists() {
        warn!(
            "Plugin succeeded but output file missing at {:?} (crate {})",
            output_path, crate_name
        );
        return PassOutcome::PluginMissingOutput {
            expected_path: output_path,
        };
    }

    let mut full_output = match load_plugin_output(&output_path) {
        Ok(o) => o,
        Err(e) => {
            warn!("Failed to parse plugin output {:?}: {}", output_path, e);
            let _ = fs::remove_file(&output_path);
            return PassOutcome::PluginMissingOutput {
                expected_path: output_path,
            };
        }
    };
    let _ = fs::remove_file(&output_path);

    resolve_local_facade_gateways(&mut full_output);
    let std_spans = extract_hard_std_candidates(&full_output, context_filter);
    let macro_modules = full_output.macro_module_imports.clone();

    debug!(
        "Pass for {} yielded {} std-candidate spans and {} macro modules",
        crate_name,
        std_spans.len(),
        macro_modules.len()
    );

    PassOutcome::Success {
        macro_modules,
        std_spans,
        full_output,
    }
}

/// Extend a covering-set equation with an extra constraint, check SAT, and if
/// satisfiable return the feature list derived from the extended model.
fn features_for_mode<'a>(
    ctx: &'a Context,
    eq: &[Bool<'a>],
    extra: &Bool<'a>,
) -> Option<Vec<String>> {
    let mut extended: Vec<Bool<'a>> = eq.to_vec();
    extended.push(extra.clone());
    let check = z3::Solver::new(ctx);
    for c in &extended {
        check.assert(c);
    }
    if check.check() != z3::SatResult::Sat {
        return None;
    }
    Some(solver::eqs_to_features(ctx, &extended).0)
}

/// Builds a Z3 constraint that forbids the exact feature assignment that failed.
/// The constraint is `NOT(feat_a=true AND feat_b=true AND ... AND feat_x=false AND ...)`,
/// which forces Z3 to find a different assignment in subsequent solve calls.
/// Computes the feature modes (std / no_std) for one covering set.
fn covering_set_modes<'a>(
    ctx: &'a Context,
    eq_set: &[Bool<'a>],
    no_std_cond: &Option<Bool<'a>>,
) -> Vec<Vec<String>> {
    if let Some(cond) = no_std_cond {
        let not_cond = cond.not();
        let mut m = Vec::new();
        if let Some(f) = features_for_mode(ctx, eq_set, cond) {
            m.push(f);
        }
        if let Some(f) = features_for_mode(ctx, eq_set, &not_cond) {
            m.push(f);
        }
        m
    } else {
        vec![solver::eqs_to_features(ctx, eq_set).0]
    }
}

/// Finds the combinations of features that when used will cover all the code
/// in the crate.
pub fn find_feature_combs_for_all_code<'a>(
    ctx: &'a Context,
    manifest: &str,
    crate_name: &str,
    telemetry: &mut Telemetry,
) -> (ModNode<'a>, Vec<CoveringRun>, Vec<Bool<'a>>, Vec<Bool<'a>>) {
    let mut entrypoints: Vec<std::path::PathBuf> = Vec::new();
    let mut covering_runs: Vec<CoveringRun> = Vec::new();
    let mut previously_ran_feats: HashSet<Vec<String>> = HashSet::new();

    let crate_root = visitor::find_entrypoints(manifest, &mut entrypoints);
    debug!("Crate root: {}", crate_root.display());

    let manifest_toml: toml::Value = fs::read_to_string(manifest)
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(toml::Value::Table(toml::map::Map::new()));
    let feat_map = downloader::read_local_features(&manifest_toml);
    let impl_constraints = solver::feature_implication_constraints(ctx, &feat_map);

    // Every feature cargo will accept for this package. A `cfg(feature = "X")`
    // naming anything else is set from outside the feature system — typically a
    // build script keying off the target — so it must not become a solver
    // variable. See `parser::parse_main_attributes_direct_with`.
    let known_features = visitor::declared_features(manifest);
    debug!("Declared features for {}: {:?}", manifest, known_features);

    for entry_path in &entrypoints {
        if !entry_path.exists() {
            debug!(
                "Entrypoint does not exist: {}. Skipping.",
                entry_path.display()
            );
            continue;
        }

        let name = entry_path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("unknown");

        let mut collector = ModCollector::with_known_features(ctx, known_features.clone());
        let mut root = collector.collect(entry_path, name);
        let no_std_cond = collector.no_std_condition.clone();
        let mut solved_files: HashSet<PathBuf> = HashSet::new();

        let mut items = visitor::collect_all_items(&root, ctx);
        items.retain(|f| f.simplify() != Bool::from_bool(ctx, false)); // Filter out trivially unsatisfiable items, which can arise from cfgs that don't match the entrypoint file.

        let mut compile_error_constraints: Vec<Bool> = collector.hard_constraints.clone();
        debug!(
            "Compile-error constraints: {:#?}",
            compile_error_constraints
        );
        let mut all_hard: Vec<Bool> = compile_error_constraints.clone();
        all_hard.extend(impl_constraints.iter().cloned());

        let mut pending_modules: Vec<(Option<Bool>, String, String)> = vec![];

        // When the crate has cfg_attr(condition, no_std), always do a baseline
        // no_std run (typically --no-default-features with no extra features).
        // This covers code paths that are active in no_std mode but not gated
        // by any specific feature — paths that would otherwise be missed when
        // every covering set requires std (e.g., all features → std transitively).
        if let Some(ref cond) = no_std_cond
            && let Some(baseline_feats) = features_for_mode(ctx, &[], cond)
            && previously_ran_feats.insert(baseline_feats.clone())
        {
            compile_error_constraints.push(cond.clone());
            debug!("Baseline no_std run: features = {:?}", baseline_feats);
            match run_rustc_plugin_pass(manifest, crate_name, &baseline_feats, None) {
                PassOutcome::Success {
                    macro_modules,
                    std_spans: _,
                    full_output,
                } => {
                    debug!(
                        "Baseline no_std run succeeded with {} records",
                        full_output.records.len()
                    );
                    covering_runs.push(CoveringRun {
                        features: baseline_feats,
                        output: full_output,
                    });
                    pending_modules.extend(macro_modules.into_iter().map(|(filename, modname)| {
                        (
                            visitor::get_entry_condition_for_file(&crate_root, &root, &filename),
                            modname,
                            filename,
                        )
                    }));
                }
                PassOutcome::CompileFailed { stderr, exit_code } => {
                    warn!(
                        "Baseline no_std run failed (exit {:?}): {}",
                        exit_code, stderr
                    );
                }
                PassOutcome::PluginMissingOutput { expected_path } => {
                    warn!(
                        "Baseline no_std run: plugin produced no output at {:?}",
                        expected_path
                    );
                }
            }
        }

        // CEGAR loop: items are partitioned into covering sets by the Z3 solver.
        // When a covering set fails to compile, the failed feature assignment is
        // added as a forbidden constraint and the uncovered items are re-partitioned
        // in the next iteration. Covered items remain in the pool so they can serve
        // as pairing companions — this fixes the greedy ordering bug where an early
        // incompatible eq blocks the "correct" partner from joining the seed.
        let mut uncovered_items: Vec<Bool> = items;
        let mut covered_items: Vec<Bool> = Vec::new();
        let mut forbidden: Vec<Bool> = Vec::new();

        // Fallback: if there are no items at all, run once with just hard constraints
        // to catch the "all code is std-guarded" case.
        if uncovered_items.is_empty() {
            let modes = covering_set_modes(ctx, &all_hard, &no_std_cond);
            for enable in modes {
                if previously_ran_feats.insert(enable.clone()) {
                    match run_rustc_plugin_pass(manifest, crate_name, &enable, None) {
                        PassOutcome::Success {
                            macro_modules,
                            std_spans: _,
                            full_output,
                        } => {
                            covering_runs.push(CoveringRun {
                                features: enable.clone(),
                                output: full_output,
                            });
                            pending_modules.extend(macro_modules.into_iter().map(
                                |(filename, modname)| {
                                    (
                                        visitor::get_entry_condition_for_file(
                                            &crate_root,
                                            &root,
                                            &filename,
                                        ),
                                        modname,
                                        filename,
                                    )
                                },
                            ));
                        }
                        PassOutcome::CompileFailed { stderr, exit_code } => {
                            debug!(
                                "Empty-items fallback run {:?} failed (exit {:?}): {}",
                                enable, exit_code, stderr
                            );
                        }
                        PassOutcome::PluginMissingOutput { expected_path } => {
                            debug!(
                                "Empty-items fallback: plugin produced no output at {:?}",
                                expected_path
                            );
                        }
                    }
                }
            }
        }

        let mut cegar_iter = 0usize;
        loop {
            cegar_iter += 1;
            // Pool = uncovered ∪ covered so covered items can serve as companions
            // when Z3 re-partitions after a failure.
            let pool: Vec<Bool> = uncovered_items
                .iter()
                .chain(covered_items.iter())
                .cloned()
                .collect();

            if pool.is_empty() {
                break;
            }

            let eqs_with_soft =
                solver::get_solved_sets(ctx, crate_name, pool, &all_hard, &forbidden, telemetry);

            if eqs_with_soft.is_empty() {
                break;
            }

            // Only schedule runs for sets that contain at least one uncovered item.
            let sets_to_run: Vec<_> = eqs_with_soft
                .iter()
                .filter(|(_, soft)| soft.iter().any(|item| uncovered_items.contains(item)))
                .collect();

            if sets_to_run.is_empty() {
                break;
            }

            debug!(
                "[cegar iter {cegar_iter}] {}/{} sets need runs ({} uncovered items remaining)",
                sets_to_run.len(),
                eqs_with_soft.len(),
                uncovered_items.len(),
            );

            let mut made_progress = false;

            for (i, (eq_set, soft_items)) in sets_to_run.iter().enumerate() {
                let set_num = i + 1;
                let set_total = sets_to_run.len();
                let modes = covering_set_modes(ctx, eq_set, &no_std_cond);

                for enable in modes {
                    if previously_ran_feats.contains(&enable) {
                        debug!(
                            "[cegar iter {cegar_iter}] set {set_num}/{set_total}: features {enable:?} — skipped (already ran)"
                        );
                        continue;
                    }
                    previously_ran_feats.insert(enable.clone());

                    debug!(
                        "[cegar iter {cegar_iter}] set {set_num}/{set_total}: running features {enable:?}"
                    );

                    match run_rustc_plugin_pass(manifest, crate_name, &enable, None) {
                        PassOutcome::Success {
                            macro_modules,
                            std_spans: _,
                            full_output,
                        } => {
                            debug!(
                                "[cegar iter {cegar_iter}] set {set_num}/{set_total}: ok ({} records, {} macro modules)",
                                full_output.records.len(),
                                macro_modules.len(),
                            );
                            // Move items covered by this set from uncovered → covered.
                            for item in soft_items.iter() {
                                if let Some(pos) = uncovered_items.iter().position(|u| u == item) {
                                    covered_items.push(uncovered_items.remove(pos));
                                    made_progress = true;
                                }
                            }
                            covering_runs.push(CoveringRun {
                                features: enable.clone(),
                                output: full_output,
                            });
                            pending_modules.extend(macro_modules.into_iter().map(
                                |(filename, modname)| {
                                    (
                                        visitor::get_entry_condition_for_file(
                                            &crate_root,
                                            &root,
                                            &filename,
                                        ),
                                        modname,
                                        filename,
                                    )
                                },
                            ));
                        }
                        PassOutcome::CompileFailed { stderr, exit_code } => {
                            let first_line = stderr.lines().next().unwrap_or("").trim();
                            debug!(
                                "[cegar iter {cegar_iter}] set {set_num}/{set_total}: FAILED (exit {exit_code:?}): {first_line}"
                            );
                            // Record the failed feature assignment as forbidden so Z3
                            // is forced to find a different pairing in the next iteration.
                            let (en, dis) = solver::eqs_to_features(ctx, eq_set);
                            debug!(
                                "[cegar iter {cegar_iter}] set {set_num}/{set_total}: adding forbidden — enable {en:?}, disable {dis:?}"
                            );
                            forbidden.push(solver::build_forbidden_constraint(ctx, &en, &dis));
                        }
                        PassOutcome::PluginMissingOutput { expected_path } => {
                            debug!(
                                "[cegar iter {cegar_iter}] set {set_num}/{set_total}: missing plugin output at {expected_path:?}"
                            );
                        }
                    }
                }
            }

            if !made_progress {
                break;
            }
        }

        debug!(
            "[cegar] done after {cegar_iter} iter(s): {} successful run(s): {}",
            covering_runs.len(),
            covering_runs
                .iter()
                .map(|r| format!("[{}]", r.features.join(", ")))
                .collect::<Vec<_>>()
                .join(" | ")
        );

        solved_files.insert(entry_path.canonicalize().unwrap());

        // fixpoint loop for newly discovered macro-generated modules
        loop {
            if pending_modules.is_empty() {
                break;
            }
            let mut next_pending: Vec<(Option<Bool>, String, String)> = vec![];

            for (eq, modname, filename) in &pending_modules {
                let callsite = crate_root.join(filename);
                // Resolve the module's file honouring rustc's mod-rs vs non-mod-rs
                // rules: a macro-declared `mod after;` in the non-mod-rs file
                // `src/wasm.rs` lives at `src/wasm/after.rs`, not `src/after.rs`.
                let mod_path = match resolve_macro_module_file(
                    &callsite,
                    is_mod_rs_style(&callsite, entry_path),
                    modname,
                ) {
                    Some(p) => p,
                    None => {
                        debug!(
                            "No source file for module {} (callsite {})",
                            modname,
                            callsite.display()
                        );
                        continue;
                    }
                };

                let canonical = mod_path.canonicalize().unwrap();
                let new_node = collector.visit_file(&mod_path, modname, eq.clone());
                visitor::insert_child_into_tree(&crate_root, &mut root, filename, new_node);

                let new_items = visitor::collect_items_for_new_file(
                    &crate_root,
                    &root,
                    &mod_path,
                    ctx,
                    &solved_files,
                );

                // Apply the same CEGAR structure as the main loop: track uncovered/covered
                // per module and retry with forbidden constraints on compilation failure.
                let mut uncovered_new: Vec<Bool> = new_items;
                let mut covered_new: Vec<Bool> = Vec::new();
                let mut fp_iter = 0usize;

                loop {
                    fp_iter += 1;
                    let pool: Vec<Bool> = uncovered_new
                        .iter()
                        .chain(covered_new.iter())
                        .cloned()
                        .collect();

                    if pool.is_empty() {
                        break;
                    }

                    let new_eqs = solver::get_solved_sets(
                        ctx, crate_name, pool, &all_hard, &forbidden, telemetry,
                    );

                    let sets_to_run: Vec<_> = new_eqs
                        .iter()
                        .filter(|(_, soft)| soft.iter().any(|item| uncovered_new.contains(item)))
                        .collect();

                    if sets_to_run.is_empty() {
                        break;
                    }

                    debug!(
                        "[fixpoint mod '{modname}', iter {fp_iter}] {}/{} sets need runs ({} uncovered items remaining)",
                        sets_to_run.len(),
                        new_eqs.len(),
                        uncovered_new.len(),
                    );

                    let mut made_progress = false;

                    for (i, (eq_set, soft_items)) in sets_to_run.iter().enumerate() {
                        let set_num = i + 1;
                        let set_total = sets_to_run.len();
                        let modes = covering_set_modes(ctx, eq_set, &no_std_cond);

                        for enable in modes {
                            if previously_ran_feats.contains(&enable) {
                                debug!(
                                    "[fixpoint mod '{modname}', iter {fp_iter}] set {set_num}/{set_total}: features {enable:?} — skipped (already ran)"
                                );
                                continue;
                            }
                            previously_ran_feats.insert(enable.clone());

                            debug!(
                                "[fixpoint mod '{modname}', iter {fp_iter}] set {set_num}/{set_total}: running features {enable:?}"
                            );

                            match run_rustc_plugin_pass(manifest, crate_name, &enable, None) {
                                PassOutcome::Success {
                                    macro_modules,
                                    std_spans: _,
                                    full_output,
                                } => {
                                    debug!(
                                        "[fixpoint mod '{modname}', iter {fp_iter}] set {set_num}/{set_total}: ok ({} records, {} macro modules)",
                                        full_output.records.len(),
                                        macro_modules.len(),
                                    );
                                    for item in soft_items.iter() {
                                        if let Some(pos) =
                                            uncovered_new.iter().position(|u| u == item)
                                        {
                                            covered_new.push(uncovered_new.remove(pos));
                                            made_progress = true;
                                        }
                                    }
                                    covering_runs.push(CoveringRun {
                                        features: enable.clone(),
                                        output: full_output,
                                    });
                                    next_pending.extend(macro_modules.into_iter().map(
                                        |(filename, modname)| {
                                            (
                                                visitor::get_entry_condition_for_file(
                                                    &crate_root,
                                                    &root,
                                                    &filename,
                                                ),
                                                modname,
                                                filename,
                                            )
                                        },
                                    ));
                                }
                                PassOutcome::CompileFailed { stderr, exit_code } => {
                                    let first_line = stderr.lines().next().unwrap_or("").trim();
                                    debug!(
                                        "[fixpoint mod '{modname}', iter {fp_iter}] set {set_num}/{set_total}: FAILED (exit {exit_code:?}): {first_line}"
                                    );
                                    let (en, dis) = solver::eqs_to_features(ctx, eq_set);
                                    debug!(
                                        "[fixpoint mod '{modname}', iter {fp_iter}] set {set_num}/{set_total}: adding forbidden — enable {en:?}, disable {dis:?}"
                                    );
                                    forbidden
                                        .push(solver::build_forbidden_constraint(ctx, &en, &dis));
                                }
                                PassOutcome::PluginMissingOutput { expected_path } => {
                                    debug!(
                                        "[fixpoint mod '{modname}', iter {fp_iter}] set {set_num}/{set_total}: missing plugin output at {expected_path:?}"
                                    );
                                }
                            }
                        }
                    }

                    if !made_progress {
                        break;
                    }
                }

                solved_files.insert(canonical);
            }

            pending_modules = next_pending;
        }

        debug!(
            "[fixpoint] done: {} total successful run(s): {}",
            covering_runs.len(),
            covering_runs
                .iter()
                .map(|r| format!("[{}]", r.features.join(", ")))
                .collect::<Vec<_>>()
                .join(" | ")
        );

        // Include the no_std condition in the hard constraints returned to the
        // probing stage so that solve_with_negation always finds features in
        // no_std mode. Without this, Z3 may pick std=true for free variables,
        // causing probes to classify spans as NonStd based on std-mode runs.
        if let Some(ref cond) = no_std_cond {
            all_hard.push(cond.clone());
        }

        // Now that runs have revealed OUT_DIR, splice any build-script-generated
        // (`include!(concat!(env!("OUT_DIR"), …))`) files into the tree, gated by
        // their include-site condition. Without this their std usage — reported
        // by the HIR pass at real OUT_DIR paths — looks ungated/hard std.
        if !collector.pending_includes.is_empty()
            && let Some(out_dir) = covering_runs
                .iter()
                .rev()
                .find_map(|r| r.output.out_dir.clone())
        {
            visitor::resolve_pending_includes(
                ctx,
                &mut root,
                &collector.pending_includes,
                &out_dir,
            );
        }

        return (
            root,
            covering_runs,
            all_hard.clone(),
            compile_error_constraints,
        );
    }
    unreachable!("No entrypoints found for crate {}", crate_name);
}

pub fn analyze_crate_wrapper<'a>(
    ctx: &'a Context,
    crate_name: &str,
    main_name: Option<&str>,
    telemetry: &mut Telemetry,
) -> (
    Vec<ReadableSpan>,
    Option<Bool<'a>>,
    Option<CoverageComparison>,
    Vec<Bool<'a>>,
    visitor::ModNode<'a>,
    HashSet<CrossCrateRef>,
) {
    let manifest = parser::determine_manifest_file(crate_name, main_name);
    analyze_crate(ctx, &manifest, crate_name, telemetry)
}

/// Traverse the ModNode tree to find the full condition (root→leaf) for the
/// innermost item whose span contains `target`. Returns None when the item
/// is unconditional (reachable regardless of features).
fn find_condition_for_span<'a>(
    node: &ModNode<'a>,
    target: &ReadableSpan,
    ctx: &'a Context,
    inherited: Option<Bool<'a>>,
) -> Option<Bool<'a>> {
    let module_gate = match (&inherited, &node.entry_condition) {
        (Some(i), Some(e)) => Some(Bool::and(ctx, &[i, e])),
        (Some(i), None) => Some(i.clone()),
        (None, Some(e)) => Some(e.clone()),
        (None, None) => None,
    };

    // Only inspect items/children that belong to the same source file as target.
    let node_file = node.source_file.to_string_lossy();
    if node_file == target.file {
        for item in &node.local_items {
            if item.span_matches(target) {
                return Some(match (&module_gate, &item.own_condition) {
                    (Some(g), Some(c)) => Bool::and(ctx, &[g, c]),
                    (Some(g), None) => g.clone(),
                    (None, Some(c)) => c.clone(),
                    (None, None) => Bool::from_bool(ctx, true),
                });
            }
        }
    }

    for child in &node.children {
        if let Some(cond) = find_condition_for_span(child, target, ctx, module_gate.clone()) {
            return Some(cond);
        }
    }

    // Target file matched this node but no item-level span matched —
    // the use site is in this module's scope, so return the module gate.
    if node_file == target.file {
        return module_gate;
    }

    None
}

/// For every covering-run reference to an external crate, find the full
/// condition (root→leaf) for the containing item in the given crate's tree.
/// If that condition is compatible with no_std (condition AND NOT(hard) is
/// SAT, or there are no hard constraints), include the item in the result set.
///
/// `records` arrives already filtered (no `LOCAL`, no `extern crate`) and
/// deduplicated — see `CrossCrateRef` and the projection in `analyze_crate`.
///
/// Generic over which crate's source is being analyzed — `root` and
/// `records` come from that crate's own `analyze_crate_wrapper` call, so
/// this can be used for the main crate as well as for any dependency
/// acting as a "parent" in the recursive requirement check.
pub fn compute_valid_cross_crate_items<'a>(
    root: &ModNode<'a>,
    records: &HashSet<CrossCrateRef>,
    hard: Option<&Bool<'a>>,
    ctx: &'a Context,
) -> HashSet<(String, String)> {
    // Keyed by borrows into `records` so the hot loop's membership probe below
    // costs no allocation; materialized into owned pairs on the way out.
    let mut accepted: HashSet<(&str, &str)> = HashSet::new();

    // Collect all external items grouped by dep for the initial summary print.
    // A set, not a Vec: the same item is referenced from many spans, and
    // accumulating one entry per reference is pure waste for a sorted print.
    let mut all_by_dep: std::collections::BTreeMap<&str, std::collections::BTreeSet<&str>> =
        std::collections::BTreeMap::new();
    for record in records {
        all_by_dep
            .entry(&record.dep)
            .or_default()
            .insert(&record.item);
    }
    println!("[cross_crate] All external items referenced by crate in covering runs:");
    for (dep, items) in &all_by_dep {
        println!("  dep={}: {:?}", dep, items);
    }

    for record in records {
        // An item qualifies as soon as one of its references is accessible, so
        // once accepted the remaining spans cannot change the answer. Skipping
        // them avoids a Z3 solver per reference rather than per item.
        if accepted.contains(&(record.dep.as_str(), record.item.as_str())) {
            continue;
        }

        let is_accessible = match hard {
            None => true,
            Some(h) => {
                match find_condition_for_span(root, &record.span, ctx, None) {
                    None => true, // unconditional
                    Some(c) => {
                        let sat = {
                            let s = z3::Solver::new(ctx);
                            s.assert(&c);
                            s.assert(&h.not());
                            s.check() == z3::SatResult::Sat
                        };
                        println!(
                            "[cross_crate] dep={} item={} condition_AND_NOT_hard={}",
                            record.dep,
                            record.item,
                            if sat {
                                "SAT (accessible)"
                            } else {
                                "UNSAT (blocked by hard)"
                            }
                        );
                        sat
                    }
                }
            }
        };

        if is_accessible {
            accepted.insert((&record.dep, &record.item));
        }
    }

    let result: HashSet<(String, String)> = accepted
        .into_iter()
        .map(|(dep, item)| (dep.to_string(), item.to_string()))
        .collect();

    println!("[cross_crate] Final valid cross-crate items (no_std-accessible):");
    let mut final_sorted: Vec<_> = result.iter().collect();
    final_sorted.sort();
    for (dep, item) in &final_sorted {
        println!("  dep={} item={}", dep, item);
    }

    result
}

/// Converts the raw `#[cfg(…)]` strings stored in `PathRecord::macro_body_cfgs`
/// into Z3 Bool ancestors, reusing the existing `parse_main_attributes_direct`
/// path.  Returns `None` when the list is empty (so callers can chain with
/// `or_else`).
fn macro_body_cfgs_to_ancestors<'a>(
    ctx: &'a Context,
    cfgs: &[String],
    known_features: &HashSet<String>,
) -> Option<Vec<Bool<'a>>> {
    if cfgs.is_empty() {
        return None;
    }

    let bools: Vec<Bool<'a>> = cfgs
        .iter()
        .filter_map(|s| {
            use syn::parse::Parser;
            let attrs = syn::Attribute::parse_outer.parse_str(s).ok()?;
            let attr = attrs.into_iter().next()?;
            // These cfgs come from the main crate's own macro bodies, so the
            // same undeclared-feature erasure applies: blst's `sig_variant_impl!`
            // body carries `#[cfg(feature = "std")]`, which would otherwise
            // reintroduce the `std` variable the visitor just erased.
            let (bool_opt, _) =
                parser::parse_main_attributes_direct_with(&attr, ctx, Some(known_features));
            bool_opt
        })
        .collect();
    if bools.is_empty() { None } else { Some(bools) }
}

pub fn analyze_crate<'a>(
    ctx: &'a Context,
    manifest: &str,
    crate_name: &str,
    telemetry: &mut Telemetry,
) -> (
    Vec<ReadableSpan>,
    Option<Bool<'a>>,
    Option<CoverageComparison>,
    Vec<Bool<'a>>,
    visitor::ModNode<'a>,
    HashSet<CrossCrateRef>,
) {
    let (root, mut covering_runs, hard_constraints, compile_error_constraints) =
        find_feature_combs_for_all_code(ctx, manifest, crate_name, telemetry);

    // A routeless bare std use (e.g. a `HashMap` brought in by a glob re-export of
    // an externally-gated `use std::collections::HashMap`) carries no route back
    // to the import `resolve_local_facade_gateways` needs. Join it to that import
    // by `def_path` and inherit the gate. Needs the module tree (`root`) to know
    // which imports are externally gated, so it runs here rather than at load time.
    for run in &mut covering_runs {
        resolve_import_to_use_gateways(&mut run.output, &root);
    }

    // Same set the module tree was built against — macro-body cfgs must undergo
    // the identical undeclared-feature erasure or they reintroduce variables the
    // visitor already dropped.
    let known_features = visitor::declared_features(manifest);

    let coverage_comparison = match run_default_features_pass(manifest, crate_name) {
        PassOutcome::Success { full_output, .. } => {
            Some(compute_coverage_comparison(&full_output, &covering_runs))
        }
        _ => {
            warn!(
                "Default-features pass failed; skipping coverage comparison for {}",
                crate_name
            );
            None
        }
    };

    let all_constraints = visitor::collect_all_items(&root, ctx);

    let analyses = classify_spans(&covering_runs);
    let always_std_imports = get_always_std_imports(&analyses);
    let mut always_std_others: Vec<SpanAnalysis> = get_always_std_others(&analyses)
        .into_iter()
        .cloned()
        .collect();

    let probe_candidates_imports = always_std_imports
        .into_iter()
        .filter(|a| !is_local_reexport(&a.exemplar))
        .map(|a| ProbeTarget {
            analysis: a.clone(),
            ancestors: visitor::ancestors_for_span(&root, &a.exemplar.span)
                .or_else(|| macro_body_cfgs_to_ancestors(ctx, &a.exemplar.macro_body_cfgs, &known_features)),
            externally_gated: span_externally_gated(&root, &a.exemplar),
        })
        .collect::<Vec<_>>();

    let hard_imports = probe_candidates(
        ctx,
        crate_name,
        manifest,
        probe_candidates_imports,
        &mut always_std_others,
        &hard_constraints,
        &all_constraints,
    );

    let probe_candidates_usages = always_std_others
        .into_iter()
        .map(|a| ProbeTarget {
            analysis: a.clone(),
            ancestors: visitor::ancestors_for_span(&root, &a.exemplar.span)
                .or_else(|| macro_body_cfgs_to_ancestors(ctx, &a.exemplar.macro_body_cfgs, &known_features)),
            externally_gated: span_externally_gated(&root, &a.exemplar),
        })
        .collect::<Vec<_>>();

    debug!(
        "Probe candidates (other usages): {:#?}",
        probe_candidates_usages
    );

    let hard_usages = probe_usages(
        ctx,
        crate_name,
        manifest,
        probe_candidates_usages,
        &hard_constraints,
        &all_constraints,
    );

    let conditional_targets = get_conditional_spans(&analyses)
        .into_iter()
        .filter(|a| !is_local_reexport(&a.exemplar))
        .map(|a| ProbeTarget {
            analysis: a.clone(),
            ancestors: visitor::ancestors_for_span(&root, &a.exemplar.span)
                .or_else(|| macro_body_cfgs_to_ancestors(ctx, &a.exemplar.macro_body_cfgs, &known_features)),
            externally_gated: span_externally_gated(&root, &a.exemplar),
        })
        .collect::<Vec<_>>();

    let conditional_results = probe_conditional_spans(
        ctx,
        crate_name,
        manifest,
        conditional_targets,
        &hard_constraints,
        &all_constraints,
    );

    let final_condition = hard_imports
        .iter()
        .chain(hard_usages.iter())
        .chain(conditional_results.iter())
        .filter(|a| matches!(a.decision, ProbeDecision::NonStd { .. }))
        .filter_map(|a| a.condition.clone())
        .fold(None, |acc: Option<Bool>, c| {
            Some(match acc {
                Some(a) => Bool::and(ctx, &[&a, &c]),
                None => c,
            })
        })
        .map(|c| c.simplify());

    let externally_gated_spans = hard_imports
        .iter()
        .chain(hard_usages.iter())
        .chain(conditional_results.iter())
        .filter(|a| matches!(a.decision, ProbeDecision::ExternallyGated { .. }))
        .count();
    if externally_gated_spans > 0 {
        debug!(
            "{} std span(s) excused as externally gated (cfg naming no feature)",
            externally_gated_spans
        );
    }
    telemetry.externally_gated_spans = externally_gated_spans;

    let all_hard: Vec<ReadableSpan> = hard_imports
        .into_iter()
        .chain(hard_usages)
        .filter(|a| matches!(a.decision, ProbeDecision::StillStd { .. }))
        .map(|f| f.target.analysis.span)
        .collect();

    // Consume the runs rather than cloning out of them: the records are only
    // needed as `CrossCrateRef`, and holding the originals plus a full copy is
    // what made feature-heavy crates (web-sys) exhaust memory here.
    let covering_records: HashSet<CrossCrateRef> = covering_runs
        .into_iter()
        .flat_map(|run| run.output.records)
        .filter(|r| r.definition_crate != "LOCAL" && !r.is_extern_crate)
        .filter_map(|r| {
            let item = r.path_text.rsplit("::").next().unwrap_or(&r.path_text);
            if item.is_empty() {
                return None;
            }
            Some(CrossCrateRef {
                dep: r.definition_crate.replace('-', "_"),
                item: item.to_string(),
                span: r.span,
            })
        })
        .collect();

    (
        all_hard,
        final_condition,
        coverage_comparison,
        compile_error_constraints,
        root,
        covering_records,
    )
}

pub enum ImportInfo<'ctx> {
    Hard { avoidance_gate: Option<Bool<'ctx>> },
    Conditional,
}

pub fn proc_macro_spans_to_readables(spans: &[(Span, Option<String>)]) -> Vec<ReadableSpan> {
    spans
        .iter()
        .map(|(s, name)| proc_macro_span_to_readable(s, name.clone()))
        .collect()
}

pub fn proc_macro_span_to_readable(span: &Span, file: Option<String>) -> ReadableSpan {
    ReadableSpan {
        file: file.unwrap_or_else(|| "unknown".to_string()),
        start_line: span.start().line,
        start_col: span.start().column,
        end_line: span.end().line,
        end_col: span.end().column,
        usage_crate: None,
    }
}

fn is_cargo_hir_installed() -> bool {
    which("cargo-hir").is_ok()
}

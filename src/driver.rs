use log::{debug, warn};
use proc_macro2::Span;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::Mutex;
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
    downloader, parser, solver, timing,
};

/// The first bare-metal `--target` that successfully compiled a no_std plugin
/// pass for this crate. Once set, every later pass reuses *only* it (plus the
/// host fallback) instead of re-scanning `TARGET_LIST` — the many covering-set
/// and CEGAR runs for one crate would otherwise each grind through all 26
/// targets, and for a feature combo that does not compile that is 26 wasted
/// builds per iteration. A combo that fails on the crate's established good
/// target is almost never rescued by a different triple, and the host fallback
/// still catches genuine std; the cost is a minor precision loss (host cfgs) for
/// exactly those combos. Process-global is safe: the tool analyses one main
/// crate per process.
static LAST_GOOD_TARGET: Mutex<Option<&'static str>> = Mutex::new(None);

/// The `--target` the user pinned on the command line, if any. When set, the
/// plugin record pass compiles *only* for this target (host as the genuine-std
/// fallback) and never sweeps `TARGET_LIST` — a pinned target is the environment
/// to analyse, not a hint to search for one that happens to compile. Takes
/// precedence over `LAST_GOOD_TARGET`.
static EXPLICIT_TARGET: Mutex<Option<&'static str>> = Mutex::new(None);

/// Pin the plugin record pass to a single target (the CLI `--target`). `target`
/// must be a member of `TARGET_LIST`; a non-member is ignored (the CLI already
/// validates it, so this only guards against a stray caller).
pub fn set_explicit_target(target: &str) {
    if let Some(t) = consts::TARGET_LIST.iter().find(|t| **t == target) {
        *EXPLICIT_TARGET.lock().unwrap() = Some(*t);
    }
}

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

/// The feature gates above a record's span — its own if it has any, otherwise
/// the ones above the `gateway_anchor` it inherited.
///
/// Without the second half an anchor can only ever say "externally gated"
/// (`span_externally_gated`), so a record whose std-ness comes entirely from a
/// `#[cfg(feature = "std")]` import reaches `initial_ungated_results` with
/// `ancestors: None` and is short-circuited to `StillStd` **without compiling
/// anything**. That is the KI-7 false positive: the gate exists, the tool just
/// never looked at it.
///
/// `.or_else` rather than AND: a use site's own `#[cfg]` wins outright when it
/// has one. ANDing the two is arguably more correct (both gates must hold for
/// the span to be std) and is left as a follow-up — no crate has needed it.
pub fn ancestors_for_record<'a>(root: &ModNode<'a>, rec: &PathRecord) -> Option<Vec<Bool<'a>>> {
    visitor::ancestors_for_span(root, &rec.span).or_else(|| {
        rec.gateway_anchor
            .as_ref()
            .and_then(|anchor| visitor::ancestors_for_span(root, anchor))
    })
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
///
/// A leading `crate` / `self` / `super` is a routing prefix, not a name: the
/// segment identifying the binding in `crate::hash_map::Entry` is `hash_map`.
/// `strip_route_prefix` drops those first, so routed and bare references key the
/// same table.
fn use_name(path_text: &str) -> Option<&str> {
    strip_route_prefix(path_text)
        .split("::")
        .next()
        .filter(|s| !s.is_empty())
}
fn import_bound_name(path_text: &str) -> Option<&str> {
    path_text.rsplit("::").next().filter(|s| !s.is_empty())
}

const ROUTE_PREFIXES: [&str; 3] = ["crate", "self", "super"];

fn strip_route_prefix(path_text: &str) -> &str {
    let mut t = path_text.trim_start_matches("::");
    loop {
        let Some((head, rest)) = t.split_once("::") else {
            return t;
        };
        if ROUTE_PREFIXES.contains(&head) {
            t = rest;
        } else {
            return t;
        }
    }
}

fn strip_route_segments(segments: &[String]) -> &[String] {
    let mut s = segments;
    while s.len() > 1 && ROUTE_PREFIXES.contains(&s[0].as_str()) {
        s = &s[1..];
    }
    s
}

/// Where a name's std binding is gated, and on which axis.
///
/// `resolve_import_to_use_gateways` originally asked only "externally gated?",
/// because that was the only kind of gate an anchor could express: an anchor fed
/// `span_externally_gated` and nothing else. Now that `ancestors_for_record`
/// also reads anchors, a `#[cfg(feature = "std")]` import is a usable gate too,
/// so the answer has to distinguish the two axes rather than collapse to a bool.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum GateKind {
    /// No `#[cfg]` above the span at all.
    None,
    /// A cfg naming no feature (`target_os`, a build-script `--cfg`, …).
    External,
    /// A cfg the solver has a variable for.
    Feature,
}

/// Bound name → (is every std binding of it gated?, the anchor to inherit).
/// Built by `resolve_import_to_use_gateways`; see its docs for the rules.
type BindingTable = std::collections::HashMap<String, (bool, Option<(ReadableSpan, GateKind)>)>;

fn gate_kind(root: &ModNode<'_>, span: &ReadableSpan) -> GateKind {
    if visitor::externally_gated_for_span(root, span) {
        GateKind::External
    } else if visitor::ancestors_for_span(root, span).is_some() {
        GateKind::Feature
    } else {
        GateKind::None
    }
}

/// Propagate a gated `use` import's gate onto the uses of the name it
/// introduced, following the binding through re-exports.
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
/// A std use inherits a gate (via `gateway_anchor`, the same mechanism
/// `resolve_local_facade_gateways` uses) iff **every** std binding of its name is
/// gated. The all-bindings-must-agree rule is load-bearing: if any binding of
/// that name is *un*gated, the name genuinely resolves to std even in the no_std
/// configuration, so we attach nothing and let the use fail. (This follows J's
/// "yield nothing rather than guess" precedent.) Because we only touch
/// std-resolved records on the attach side, a same-named *local* item never
/// enters — its uses resolve to the crate, not std.
///
/// # Two sources, because neither alone is enough
///
/// **Plugin records** see macro-generated imports the syn tree cannot, but they
/// cannot see brace leaves or local re-exports: `use std::collections::{hash_map,
/// HashMap}` emits exactly *one* record, `path_text: "std::collections"`, so the
/// bound names never reach the driver; and `use super::HashMap;` emits **no
/// record at all**. **The syn tree** (`LocalItem::use_path`) has both. Seeding
/// from the union is also strictly safer for the all-must-agree rule, which only
/// ever gains blockers from a second source.
///
/// # Following the binding
///
/// The seed round takes std-rooted bindings (`use std::…`). A fixpoint then
/// follows re-exports: a use whose first segment (after `crate`/`self`/`super`)
/// is already a known std-bound name contributes *its* bound name, so
/// `use crate::hash_map::Entry` makes `Entry` std-bound once `hash_map` is.
///
/// The anchor stays the **std-rooted seed's** span through every hop. That is
/// sound because negating the seed's gate removes the binding at its source —
/// nothing downstream can still be std — so intermediate gates are not
/// load-bearing and one anchor span suffices. A hop off an ungated seed
/// propagates ungated, which blocks the derived name too.
///
/// Globs are not followed: `use super::hash_map::Entry::*` binds names syn cannot
/// enumerate, so the fixpoint would have to guess. They are skipped, and whatever
/// they bind stays unexcused.
///
/// Returns the number of records that received an anchor.
pub fn resolve_import_to_use_gateways(out: &mut FeatureRunOutput, root: &ModNode<'_>) -> usize {
    // bound name → (every std binding of it is gated?, the anchor to inherit).
    // The anchor is the span of one such gated binding; a `#[cfg]` on it gates
    // everything that inherits it.
    let mut binding: BindingTable = std::collections::HashMap::new();

    // Record one std binding of `name`. `kind == None` means ungated, which
    // poisons the name for good — the flag only ever goes false.
    fn note(binding: &mut BindingTable, name: &str, span: &ReadableSpan, kind: GateKind) {
        if name == "*" {
            return;
        }
        let entry = binding.entry(name.to_string()).or_insert((true, None));
        entry.0 &= kind != GateKind::None;
        // Prefer a Feature anchor over an External one: an External anchor
        // excuses the span outright (`ProbeDecision::ExternallyGated`), a Feature
        // anchor sends it to be probed. When a name is bound on both axes, the
        // probe is the more conservative answer.
        let better = match (&entry.1, kind) {
            (_, GateKind::None) => false,
            (None, _) => true,
            (Some((_, GateKind::External)), GateKind::Feature) => true,
            _ => false,
        };
        if better {
            entry.1 = Some((span.clone(), kind));
        }
    }

    // Collected before seed 1 so seed 1 can tell which imports the tree already
    // names correctly — see the brace-prefix guard below.
    let use_bindings = visitor::collect_use_bindings(root);

    // Imports the syn tree already enumerates, keyed by source position. A
    // plugin record on one of these lines carries no information the tree does
    // not have, and the tree's version is strictly better: it lists the brace
    // leaves the plugin collapses away.
    let tree_named: std::collections::HashSet<(&str, usize)> = use_bindings
        .iter()
        .map(|(_, _, s)| (s.file.as_str(), s.start_line))
        .collect();

    // --- Seed 1: plugin import records (macro-generated imports live here) ---
    for r in &out.records {
        if r.context != PathContext::ImportDeclaration
            || r.span.usage_crate.as_deref() != Some("std")
        {
            continue;
        }
        // A braced import emits exactly one record holding the *prefix*, not a
        // bound name: `use std::{borrow, …}` → `path_text: "std"`,
        // `use std::sync::{Arc, Mutex}` → `"std::sync"`. `import_bound_name`
        // cannot see the braces, so it reads the prefix's last segment as the
        // binding — and for `use std::{…}` that registers the *extern crate name*
        // `std` as a locally bound name. Every `std::…`-rooted path in the crate
        // then keys the table on `std` (`use_name` takes the first segment) and
        // inherits that import's gate.
        //
        // wasmer-compiler 6.0.0 is the case in point: `use std::{borrow, …}` sits
        // in `#[cfg(feature = "std")] pub mod std` inside its `mod lib` facade,
        // and `use std::{collections::…, ptr::…}` sits under
        // `#[cfg(not(target_arch = "wasm32"))]`. Both spell `path_text: "std"`,
        // nothing binds `std` ungated, so the all-gated rule holds and 15 genuine
        // std spans — `use std::sync::atomic`, `std::sync::MutexGuard`,
        // `std::any::Any`, … — inherited a `feature = "std"` gate they do not
        // have. Negating it probes the crate in `core` mode, which does not
        // compile, so all 15 left as `CompileFailed`: dropped from `all_hard`
        // with no counter. The crate reported 2 std spans (the two bare `Mutex`
        // references, saved only because the tree seeds brace leaves ungated).
        //
        // Where the tree names the import, defer to it. Seed 1 keeps naming only
        // what the tree cannot see — macro-generated imports — and even there a
        // bare single-segment path is a brace prefix, never a leaf.
        if tree_named.contains(&(r.span.file.as_str(), r.span.start_line))
            || !r.path_text.contains("::")
        {
            continue;
        }
        // A *routed* import (`use crate::hash_map::Entry`) re-exports a binding
        // that already exists; whether it is gated is the source binding's
        // question, and only the tree can answer it — the record's `path_text`
        // does not say what `hash_map` was rooted at. Seeding it here as an
        // independent, ungated std binding would poison exactly the name the
        // fixpoint exists to derive: `Entry` would be blocked by the very import
        // that establishes it. Skip; the tree sees every routed `use`.
        if strip_route_prefix(&r.path_text) != r.path_text.trim_start_matches("::") {
            continue;
        }
        let Some(name) = import_bound_name(&r.path_text) else {
            continue;
        };
        note(&mut binding, name, &r.span, gate_kind(root, &r.span));
    }

    // --- Seed 2: std-rooted `use` items from the syn tree (brace leaves) ---
    for (segments, name, span) in &use_bindings {
        if segments.first().map(String::as_str) != Some("std") {
            continue;
        }
        note(&mut binding, name, span, gate_kind(root, span));
    }

    // Comparable view of the table, for fixpoint termination.
    fn snapshot(binding: &BindingTable) -> Vec<(String, bool, Option<usize>)> {
        let mut v: Vec<_> = binding
            .iter()
            .map(|(k, (gated, anchor))| {
                (k.clone(), *gated, anchor.as_ref().map(|(s, _)| s.start_line))
            })
            .collect();
        v.sort();
        v
    }

    // --- Fixpoint: follow re-exports of an already-std-bound name ---
    // Bounded because each round can only add names or clear `all_gated` flags,
    // both monotone; the cap is belt-and-braces against a pathological cycle.
    for _ in 0..8 {
        let before = snapshot(&binding);

        for (segments, name, span) in &use_bindings {
            let routed = strip_route_segments(segments);
            // `use super::HashMap` strips to `["HashMap"]`, so head == name: the
            // name is re-derived from itself with the same anchor, which is a
            // no-op rather than a cycle. Left in deliberately — the hop is real
            // (a different module's binding) and blocking it would need module
            // scoping this table does not have.
            let Some(head) = routed.first() else { continue };
            let Some((src_gated, src_anchor)) = binding.get(head).cloned() else {
                continue;
            };
            match (src_gated, src_anchor) {
                (true, Some((anchor, kind))) => note(&mut binding, name, &anchor, kind),
                // Derived from an ungated std binding: the derived name is
                // reachable ungated too.
                _ => note(&mut binding, name, span, GateKind::None),
            }
        }

        if before == snapshot(&binding) {
            break;
        }
    }

    // Keep only names whose every std binding is gated and that have a concrete
    // anchor to point at.
    let gated_imports: std::collections::HashMap<String, ReadableSpan> = binding
        .into_iter()
        .filter_map(|(name, (all_gated, anchor))| match (all_gated, anchor) {
            (true, Some((a, _))) => Some((name, a)),
            _ => None,
        })
        .collect();

    if gated_imports.is_empty() {
        return 0;
    }

    let mut anchored = 0usize;
    for r in &mut out.records {
        if r.span.usage_crate.as_deref() != Some("std") || r.gateway_anchor.is_some() {
            continue;
        }
        let Some(name) = use_name(&r.path_text) else {
            continue;
        };
        // Name lookup before `gate_kind`: the latter is up to two tree walks per
        // record, and only records whose name is actually in the table can be
        // anchored. Keeps the added cost proportional to candidates, not to every
        // std record in the crate.
        if let Some(anchor) = gated_imports.get(name) {
            // Only spans with no gate of their own. This replaces the older
            // `local_route.is_some() || defining_module.is_some() || context ==
            // ImportDeclaration` exclusions, which were proxies for the same
            // question and excluded the routed case this pass now exists to catch
            // (`use crate::hash_map::Entry` carries a route, a defining module
            // *and* is an import, yet has no gate anywhere above it). A span that
            // does carry a gate reaches the prober through `ancestors_for_span`
            // already, and `resolve_local_facade_gateways` — which runs first —
            // has set `gateway_anchor` wherever a route was load-bearing.
            if gate_kind(root, &r.span) != GateKind::None {
                continue;
            }
            debug!(
                "Std use '{}' at {:?} inherits the gate of its gated binding(s) of `{}` at {:?}",
                r.path_text, r.span, name, anchor
            );
            r.gateway_anchor = Some(anchor.clone());
            anchored += 1;
        }
    }
    anchored
}

/// Runs the plugin with the crate's default features (no --no-default-features, no extra flags).
/// Used to produce a baseline for coverage comparison — simulating what a default-only tool sees.
pub fn run_default_features_pass(manifest: &str, crate_name: &str) -> PassOutcome {
    let _t = timing::scope("default_features_pass", crate_name);
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

    let attempt = timing::scope("cargo_hir", "host");
    attempt.meta("features", "<default>");
    let output = match Command::new("cargo")
        .args(args)
        .env(PLUGIN_OUTPUT_ENV, &output_path)
        .output()
    {
        Ok(o) => o,
        Err(e) => {
            attempt.meta("success", "false");
            return PassOutcome::CompileFailed {
                stderr: format!("failed to spawn cargo: {}", e),
                exit_code: None,
            };
        }
    };
    attempt.meta("success", output.status.success().to_string());
    drop(attempt);

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
    run_rustc_plugin_pass_with(manifest, crate_name, enable, context_filter, true, None)
}

/// As [`run_rustc_plugin_pass`], but with the target sweep under caller control.
///
/// `allow_host_fallback = false` asks the strictly stronger question *does this
/// feature set compile for a bare-metal target*, which is what
/// [`discover_build_enablers`] needs: a set that only builds on the host proves
/// nothing about no_std and would make every candidate look like a fix.
///
/// `pin_target = Some(t)` tries only `t`. This is the cost control for a search
/// that expects to fail: with `LAST_GOOD_TARGET` unset, *every* failing call
/// otherwise grinds through all 26 triples, so a handful of rejected candidates
/// costs more than the probes the search exists to save.
pub fn run_rustc_plugin_pass_with(
    manifest: &str,
    crate_name: &str,
    enable: &[String],
    context_filter: Option<PathContext>,
    allow_host_fallback: bool,
    pin_target: Option<&'static str>,
) -> PassOutcome {
    // Wraps the target sweep *and* the post-processing of the plugin's JSON
    // (`load_plugin_output`, facade resolution, candidate extraction), so the gap
    // between this scope and its `cargo_hir` children is the record-handling cost
    // — the part that grows with output size rather than with build time.
    let _pass = timing::scope("plugin_pass", crate_name);
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
    let has_lib = visitor::package_has_lib(manifest);

    // The plugin must gather records on a target the no_std config actually
    // compiles for. The host (no `--target`) is the wrong environment for a
    // no_std verdict: it is 64-bit and its target cfgs resolve to std-enabled
    // values (`target_os = "linux"`, `unix`, wide pointers), so `#[cfg(target_os
    // = "none")]` no_std guards take their std branch and 64-bit static asserts in
    // deps like sp-runtime-interface (`assert_eq_size!(*const u8, u32)`) hard-fail,
    // sinking the whole Substrate/Polkadot family. So compile for a bare-metal
    // target (like the verification compile in `compiler.rs` already does) and
    // gather records from it. Only if the bare-metal build fails do we fall back
    // to the host — that is the case of a crate that genuinely needs std (e.g.
    // unconditional `std::vec::Vec`, which fails to resolve on every no_std
    // target), where the host build is what still locates the std usage instead
    // of dropping it as an uncompilable probe.
    //
    // Which bare-metal target(s) to try, in order:
    //   * an explicit CLI `--target` pins the analysis to exactly that target;
    //   * else the crate's already-established good target (`LAST_GOOD_TARGET`),
    //     so the covering-set/CEGAR runs don't re-scan all 26 every call;
    //   * else, on the very first pass, scan `TARGET_LIST` for the first that
    //     compiles and cache it.
    let explicit = *EXPLICIT_TARGET.lock().unwrap();
    let cached = *LAST_GOOD_TARGET.lock().unwrap();
    let mut targets: Vec<Option<&'static str>> = Vec::with_capacity(consts::TARGET_LIST.len() + 1);
    if let Some(t) = pin_target {
        targets.push(Some(t));
    } else if let Some(t) = explicit {
        targets.push(Some(t));
    } else if let Some(t) = cached {
        targets.push(Some(t));
    } else {
        for t in consts::TARGET_LIST.iter() {
            targets.push(Some(*t));
        }
    }
    // Host fallback, tried last. Load-bearing, not merely a genuine-std backstop:
    // a feature combo that fails on the bare-metal target (e.g. it enables a
    // feature whose code does `use std::fs::File`) emits no records there, so the
    // host build is the *only* place that std usage surfaces. Gating this on
    // "no target has compiled yet" makes such spans vanish as CompileFailed, and
    // the covering-set/minimize logic then treats the std-requiring feature as
    // std-free and enables it — emitting a config that does not build (observed:
    // tarfs enabling `builtin_devices`, E0433 on `std`). The extra build per
    // failing combo is the cost of detecting std in feature-gated code.
    if allow_host_fallback {
        targets.push(None);
    }

    let mut last_stderr = String::new();
    let mut last_exit: Option<i32> = None;
    let mut succeeded_on: Option<Option<&'static str>> = None;

    for target in targets {
        // Fresh output slot per attempt so a stale success can't be mistaken for
        // this attempt's.
        if output_path.exists() {
            let _ = fs::remove_file(&output_path);
        }

        let mut args = vec![
            "hir",
            "--",
            "--manifest-path",
            manifest,
            "--no-default-features",
            "--features",
            &feats,
        ];
        if let Some(t) = target {
            args.push("--target");
            args.push(t);
        }
        if has_lib {
            args.push("--lib");
        }

        debug!(
            "Running rustc plugin pass for {} with features [{}] target [{}], output -> {:?}",
            crate_name,
            feats,
            target.unwrap_or("host"),
            output_path
        );

        // Timed per *attempt*, not per pass: this loop is where a pass with no
        // cached target burns through the whole triple list, and only a per-attempt
        // event shows that the pass cost N failed builds plus one that linked.
        let attempt = timing::scope("cargo_hir", target.unwrap_or("host"));
        attempt.meta("features", &feats);
        let output = match Command::new("cargo")
            .args(&args)
            .env(PLUGIN_OUTPUT_ENV, &output_path)
            .output()
        {
            Ok(o) => o,
            Err(e) => {
                attempt.meta("success", "false");
                attempt.meta("outcome", "spawn_failed");
                return PassOutcome::CompileFailed {
                    stderr: format!("failed to spawn cargo: {}", e),
                    exit_code: None,
                };
            }
        };
        attempt.meta("success", output.status.success().to_string());
        drop(attempt);

        if output.status.success() {
            if output_path.exists() {
                succeeded_on = Some(target);
                break;
            }
            // Compiled but wrote no JSON — a plugin/env fault, not target-specific;
            // retrying other targets will not help.
            warn!(
                "Plugin succeeded but output file missing at {:?} (crate {}, target [{}])",
                output_path,
                crate_name,
                target.unwrap_or("host")
            );
            return PassOutcome::PluginMissingOutput {
                expected_path: output_path,
            };
        }

        last_stderr = String::from_utf8_lossy(&output.stderr).into_owned();
        last_exit = output.status.code();
        debug!(
            "cargo hir failed for {} on target [{}] (exit {})",
            crate_name,
            target.unwrap_or("host"),
            last_exit.unwrap_or(-1)
        );
    }

    let Some(succeeded_target) = succeeded_on else {
        let _ = fs::remove_file(&output_path);
        return PassOutcome::CompileFailed {
            stderr: last_stderr,
            exit_code: last_exit,
        };
    };

    match succeeded_target {
        Some(t) => {
            *LAST_GOOD_TARGET.lock().unwrap() = Some(t);
            debug!("cargo hir succeeded for {} on target {}", crate_name, t);
        }
        None => debug!(
            "cargo hir succeeded for {} on host (no bare-metal target compiled)",
            crate_name
        ),
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

/// Parse a crate's manifest, or an empty table when it cannot be read.
pub fn read_manifest_toml(manifest: &str) -> toml::Value {
    fs::read_to_string(manifest)
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(toml::Value::Table(toml::map::Map::new()))
}

/// The `cfg => optional-dependency` edges for one crate: every gated
/// `use`/`extern crate` in `root` paired with the features that link the
/// dependency it names (bucket 11).
///
/// Both consumers need the identical set. The covering-run solver needs it so a
/// no_std run is handed a feature set that actually links its backend; the final
/// feature selection in `bin/main.rs` needs it for the same reason — otherwise the
/// analysis clears the crate and the emitted config fails to build for exactly the
/// reason the discarded run did (caches-0.3.0: cleared, then
/// `can't find crate hashbrown`).
///
/// `known_features` must be Cargo's declared set (`visitor::declared_features`),
/// not the `[features]` table: the implicit feature of an optional dependency
/// exists only in `cargo metadata`.
/// Returns the edges plus the feature names they can require, which the caller
/// needs to keep `minimize` from stripping an enabler it just switched on: such a
/// feature gates no code of its own, so the "exists only to pull in an optional
/// dep" rule drops it (see implicit_dep_feature_tests) — correct in general,
/// wrong for exactly these.
pub fn optional_dep_link_constraints<'a>(
    ctx: &'a Context,
    manifest_toml: &toml::Value,
    known_features: &HashSet<String>,
    root: &ModNode<'a>,
) -> (Vec<Bool<'a>>, HashSet<String>) {
    let dep_enablers = downloader::optional_dep_enablers(manifest_toml, known_features);
    if dep_enablers.is_empty() {
        return (Vec::new(), HashSet::new());
    }
    debug!("Optional-dep enablers: {:?}", dep_enablers);
    let constraints = solver::optional_dep_use_constraints(
        ctx,
        &visitor::collect_gated_extern_roots(root, ctx),
        &dep_enablers,
    );
    let names = dep_enablers
        .into_iter()
        .flat_map(|(_, enablers)| enablers)
        .collect();
    (constraints, names)
}

/// Whether the manifest's `[features]` table declares `name` itself. Distinguishes
/// an explicit feature that merely shares a name with an optional dependency from
/// cargo's synthesised `D = ["dep:D"]`, which exists only in `cargo metadata`.
fn manifest_declares_feature(manifest_toml: &toml::Value, name: &str) -> bool {
    manifest_toml
        .get("features")
        .and_then(toml::Value::as_table)
        .is_some_and(|feats| feats.contains_key(name))
}

/// The optional dependencies `minimize` must not unlink: ones the crate imports
/// from under a cfg that stays **true** once the dependency is gone.
///
/// `minimize` unlinks an optional dependency by deleting its entry out of the
/// feature that names it, leaving that feature itself enabled. The only cfg that
/// flips as a result is cargo's implicit `feature = "<dep>"`, so the edit is sound
/// exactly when every import of the dependency sits behind that feature —
/// watchface's `std = ["chrono"]` with `#[cfg(feature = "chrono")] use chrono::…`
/// is the shape it was written for.
///
/// a7105 is the shape it breaks on: `default = ["async"]`, `async =
/// ["embedded-hal-async"]`, and the import gated by `#[cfg(feature = "async")]`.
/// Stripping the entry leaves `async` on, so the `use embedded_hal_async::…`
/// still compiles — against a crate cargo was never told to link (`E0433`).
///
/// The evidence used is the crate's `use`/`extern crate` items, because those are
/// the only references whose gate is known exactly: the condition is read off the
/// item's own attributes as it is parsed. Each is evaluated in the world the edit
/// would create — every declared feature pinned to its value in `active_features`,
/// the dependency's own implicit feature pinned false. An import that is still
/// satisfiable there survives the unlink and pins the dependency; an ungated import
/// pins it outright. Gates carrying non-`feature` cfgs keep those atoms free, which
/// can only make an import look reachable — erring towards keeping the dep linked.
///
/// When a dependency has **no** import at all but the covering runs recorded
/// references to it, every reference is an inline path — aht20-driver-2.0.0 names
/// `defmt` only as `defmt::debug!(…)`. There is no import to read a condition off,
/// so the unlink cannot be shown safe and the dependency is pinned. (Resolving those
/// spans against the module tree was tried and is not sound: a span the tree has no
/// item for yields the same "no condition" answer as a genuinely ungated one, which
/// pinned watchface's `chrono` and cost it its build.)
///
/// Only cargo's *synthesised* `D = ["dep:D"]` is pinned false: it is the one feature
/// the edit can switch off. A feature the manifest declares itself survives the edit
/// even when it shares the dependency's name (bevy_input's `smol_str = ["dep:smol_str",
/// "bevy_reflect/smol_str"]`), so it is pinned to its value in `active_features` like
/// any other — otherwise a live gate reads as dead and the import is left dangling.
/// The `dep:D` spelling suppresses the implicit feature entirely, so absent an explicit
/// entry nothing in the manifest can turn such an import's gate off and the dependency
/// comes back pinned.
pub fn deps_pinned_by_active_use<'a>(
    ctx: &'a Context,
    manifest_toml: &toml::Value,
    known_features: &HashSet<String>,
    active_features: &HashSet<String>,
    root: &ModNode<'a>,
    records: &HashSet<CrossCrateRef>,
) -> HashSet<String> {
    let optional_deps = downloader::optional_deps_in_manifest(manifest_toml);
    if optional_deps.is_empty() {
        return HashSet::new();
    }
    let roots = visitor::collect_extern_roots_with_gates(root, ctx);

    let mut pinned = HashSet::new();
    for dep in optional_deps {
        let dep_norm = dep.replace('-', "_");
        let imports: Vec<&Option<Bool>> = roots
            .iter()
            .filter(|(name, _)| *name == dep_norm)
            .map(|(_, gate)| gate)
            .collect();

        if imports.is_empty() {
            if records.iter().any(|record| record.dep == dep_norm) {
                debug!(
                    "Optional dep '{dep}' is referenced only by inline paths — no import to read \
                     a condition off, so it must not be unlinked"
                );
                pinned.insert(dep);
            }
            continue;
        }

        // Deleting the entry only turns `feature = "<dep>"` off when that feature is
        // the one cargo synthesises. An explicitly declared feature of the same name
        // survives the edit — bevy_input's `smol_str = ["dep:smol_str",
        // "bevy_reflect/smol_str"]` keeps its other value, stays on the command line,
        // and `#[cfg(feature = "smol_str")] use smol_str::SmolStr` keeps compiling
        // against a crate cargo no longer links. Pinning it false regardless was
        // reading the gate as dead when it is live.
        let dep_feature_is_implicit = !manifest_declares_feature(manifest_toml, &dep);
        let solver = z3::Solver::new(ctx);
        for feat in known_features {
            let var = Bool::new_const(ctx, feat.as_str());
            let forced_off = *feat == dep && dep_feature_is_implicit;
            if !forced_off && active_features.contains(feat) {
                solver.assert(&var);
            } else {
                solver.assert(&var.not());
            }
        }
        for gate in imports {
            let survives = match gate {
                None => true,
                Some(gate) => {
                    solver.push();
                    solver.assert(gate);
                    let sat = solver.check() == z3::SatResult::Sat;
                    solver.pop(1);
                    sat
                }
            };
            if survives {
                debug!(
                    "Optional dep '{dep}' is imported under a gate that stays true without it \
                     ({gate:?}); it must not be unlinked"
                );
                pinned.insert(dep);
                break;
            }
        }
    }
    pinned
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
///
/// The last element is the crate's own `#![cfg_attr(<cond>, no_std)]` condition
/// per entrypoint — the author's statement of which features decide whether this
/// crate is no_std. It also goes into the returned hard constraints, but only
/// mixed in with everything else there; `probe_conditional_spans`'s caller needs
/// it on its own to tell a declared std switch from any other feature.
pub fn find_feature_combs_for_all_code<'a>(
    ctx: &'a Context,
    manifest: &str,
    crate_name: &str,
    telemetry: &mut Telemetry,
) -> (
    ModNode<'a>,
    Vec<CoveringRun>,
    Vec<Bool<'a>>,
    Vec<Bool<'a>>,
    Vec<Bool<'a>>,
) {
    let _cov = timing::scope("coverage", crate_name);

    let mut entrypoints: Vec<std::path::PathBuf> = Vec::new();
    let mut covering_runs: Vec<CoveringRun> = Vec::new();
    let mut previously_ran_feats: HashSet<Vec<String>> = HashSet::new();
    let mut no_std_conditions: Vec<Bool<'a>> = Vec::new();

    let crate_root = visitor::find_entrypoints(manifest, &mut entrypoints);
    debug!("Crate root: {}", crate_root.display());

    let manifest_toml = read_manifest_toml(manifest);
    let feat_map = downloader::read_local_features(&manifest_toml);
    let mut impl_constraints = solver::feature_implication_constraints(ctx, &feat_map);
    // `dep/feat` references to optional dependencies also enable that dep's
    // implicit feature; teach the solver `feat => dep` so it can't pick a set
    // Cargo would silently re-unify (bucket 3c).
    let opt_dep_edges = downloader::optional_dep_feature_edges(&manifest_toml);
    impl_constraints.extend(solver::optional_dep_implication_constraints(
        ctx,
        &opt_dep_edges,
    ));

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
        // `#[cfg(C)] use <optional dep>::…` means C requires that dep linked. Without
        // the edge the solver can satisfy C with the dep off, cargo accepts the set,
        // and rustc fails the whole run on `unresolved import` — losing every record
        // that run would have contributed (bucket 11).
        let (optdep_constraints, _) =
            optional_dep_link_constraints(ctx, &manifest_toml, &known_features, &root);

        let mut all_hard: Vec<Bool> = compile_error_constraints.clone();
        all_hard.extend(impl_constraints.iter().cloned());
        all_hard.extend(optdep_constraints.iter().cloned());

        let mut pending_modules: Vec<(Option<Bool>, String, String)> = vec![];

        // When the crate has cfg_attr(condition, no_std), always do a baseline
        // no_std run (typically --no-default-features with no extra features).
        // This covers code paths that are active in no_std mode but not gated
        // by any specific feature — paths that would otherwise be missed when
        // every covering set requires std (e.g., all features → std transitively).
        // Solved against `all_hard`, the same constraint set `covering_set_modes`
        // uses. It used to see `optdep_constraints` alone, which made the baseline
        // the one run in the system solved without the crate's own `compile_error!`
        // and without the feature-implication edges — so it routinely picked a set
        // the crate itself forbids, died, and left no std-off run at all. parley:
        // `(std ∨ libm) ∧ ¬std` forces `libm`, but the baseline ran `[]` and hit
        // "kurbo requires either the `std` or `libm` feature".
        // `all_hard` is snapshotted above, before `cond` is pushed onto
        // `compile_error_constraints`, so passing it as `extra` does not double it.
        if let Some(ref cond) = no_std_cond
            && let Some(baseline_feats) = features_for_mode(ctx, &all_hard, cond)
            && previously_ran_feats.insert(baseline_feats.clone())
        {
            compile_error_constraints.push(cond.clone());
            debug!("Baseline no_std run: features = {:?}", baseline_feats);
            let run = timing::scope("coverage_run", baseline_feats.join(","));
            run.meta("kind", "baseline");
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
                    run.meta("outcome", "success");
                    run.meta("records", full_output.records.len().to_string());
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
                    run.meta("outcome", "compile_failed");
                    warn!(
                        "Baseline no_std run failed (exit {:?}): {}",
                        exit_code, stderr
                    );
                }
                PassOutcome::PluginMissingOutput { expected_path } => {
                    run.meta("outcome", "plugin_missing_output");
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
                    let run = timing::scope("coverage_run", enable.join(","));
                    run.meta("kind", "empty_items_fallback");
                    match run_rustc_plugin_pass(manifest, crate_name, &enable, None) {
                        PassOutcome::Success {
                            macro_modules,
                            std_spans: _,
                            full_output,
                        } => {
                            run.meta("outcome", "success");
                            run.meta("records", full_output.records.len().to_string());
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
                            run.meta("outcome", "compile_failed");
                            debug!(
                                "Empty-items fallback run {:?} failed (exit {:?}): {}",
                                enable, exit_code, stderr
                            );
                        }
                        PassOutcome::PluginMissingOutput { expected_path } => {
                            run.meta("outcome", "plugin_missing_output");
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

            let eqs_with_soft = {
                let _s = timing::scope("coverage_solve", format!("cegar iter {cegar_iter}"));
                solver::get_solved_sets(ctx, crate_name, pool, &all_hard, &forbidden, telemetry)
            };

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

                    let run = timing::scope("coverage_run", enable.join(","));
                    run.meta("kind", format!("cegar iter {cegar_iter} set {set_num}/{set_total}"));
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
                            run.meta("outcome", "success");
                            run.meta("records", full_output.records.len().to_string());
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
                            run.meta("outcome", "compile_failed");
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
                            run.meta("outcome", "plugin_missing_output");
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

                    let new_eqs = {
                        let _s = timing::scope(
                            "coverage_solve",
                            format!("fixpoint mod '{modname}' iter {fp_iter}"),
                        );
                        solver::get_solved_sets(
                            ctx, crate_name, pool, &all_hard, &forbidden, telemetry,
                        )
                    };

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

                            let run = timing::scope("coverage_run", enable.join(","));
                            run.meta(
                                "kind",
                                format!(
                                    "fixpoint mod '{modname}' iter {fp_iter} set {set_num}/{set_total}"
                                ),
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
                                    run.meta("outcome", "success");
                                    run.meta("records", full_output.records.len().to_string());
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
                                    run.meta("outcome", "compile_failed");
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
                                    run.meta("outcome", "plugin_missing_output");
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
            no_std_conditions.push(cond.clone());
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
            no_std_conditions,
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
    Vec<ReadableSpan>,
) {
    // The one place that names *whose* analysis follows. Dependencies run the
    // same coverage/probe code as the main crate, so without an ambient crate on
    // the timing stack their cost is indistinguishable from the main crate's.
    let _t = timing::crate_scope("analyze", crate_name);
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

/// The declared features a Z3 condition mentions.
///
/// Reads them off the s-expression `Bool` prints, the same way
/// `solver::length_and_depth` measures one: every feature is a Bool constant
/// whose name is the feature, so a whitespace/paren split and a membership test
/// against the declared set is exact. Intersecting with `known_features` is what
/// keeps the operators (`and`, `not`, `or`) and any non-feature atom out — the
/// same rule `parse_main_attributes_direct_with` applies when it builds the
/// condition in the first place.
fn feature_atoms(cond: &Bool<'_>, known_features: &HashSet<String>) -> HashSet<String> {
    cond.to_string()
        .split(|c: char| c.is_whitespace() || c == '(' || c == ')')
        .filter(|t| !t.is_empty())
        .filter(|t| known_features.contains(*t))
        .map(str::to_string)
        .collect()
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

/// Upper bound on the compiles [`discover_build_enablers`] will spend. The first
/// is the all-candidates trial; the rest go to shrinking it, or — when that trial
/// fails — to trying candidates alone. Only the first sweeps `TARGET_LIST`; the
/// rest are pinned to one triple, so the worst case is ~26 + 15 builds against
/// the ~160 the probes it replaces were spending before aborting.
const MAX_ENABLER_PROBES: usize = 16;

/// Features the crate cannot compile *at all* without on a bare-metal target.
///
/// A feature like bevy_input's `libm` (`libm = ["bevy_math/libm"]`) gates no code
/// of the crate's own, so no `#[cfg]` ever mentions it and it is never a variable
/// the covering-set or probe solves reason about — which means every model leaves
/// it off. With `std` off and `libm` off, glam has no `sqrt`, so *every* feature
/// set the prober tries fails to compile, every span comes back
/// `ProbeDecision::CompileFailed`, and the crate is reported as "std usage could
/// not be proven avoidable" (triage bucket T2) even though
/// `--no-default-features --features libm` builds clean on all 26 targets.
///
/// This finds such features by search rather than by name: start from the base
/// no_std set plus *every* candidate the constraints allow, and shrink back to a
/// set nothing can be removed from. What survives is, by construction, a feature
/// the crate does not build without — so pinning it true costs nothing that was
/// reachable anyway.
///
/// The search runs in the world the prober is trying to reach, not in an
/// arbitrary model: every `AlwaysStd` gate is asserted *false* first, so a
/// candidate that would switch a std gate back on is dropped before it costs a
/// compile. totsu_core is why — `std = ["num-traits/std"]` and
/// `libm = ["num-traits/libm"]` are both "features without which `Float` has no
/// `sqrt`", and offering `std` as the fix is offering to give up. The bare-metal
/// requirement is the second guard: a candidate that "fixes" the build only on the
/// host is not a fix.
///
/// Returns the empty vector when the base set already compiles, when no candidate
/// set does, or when there are no candidates — i.e. this never manufactures a
/// verdict, it only reports one it compiled.
///
/// The second half of the answer is the run that *did* compile. These trials are
/// full plugin passes, and this search only runs when nothing else has compiled
/// for a bare-metal target — so a trial that succeeds is very often the only
/// std-off evidence the crate will ever produce, and reporting the feature name
/// while discarding the records throws it away. xmrs 0.9.9 is the case: its one
/// covering run has `std` on, so its ungated `f32::{powf,round,…}` calls bind
/// std's inherent methods and every span is `AlwaysStd`; the trial that compiles
/// with `["default"]` resolves all eight to `micromath::F32Ext` and holds no std
/// record at all.
///
/// Only the *last* success is returned, and that is not an arbitrary choice: the
/// halving and removal passes below only ever move `keep` to a set that has just
/// compiled, so the last successful trial is always the one for the final `keep`
/// — the smallest compiling configuration found. Keeping just it, rather than
/// every trial, bounds this to one record set for a crate that emits hundreds of
/// thousands of them.
pub fn discover_build_enablers<'a>(
    ctx: &'a Context,
    manifest: &str,
    crate_name: &str,
    hard_constraints: &[Bool<'a>],
    avoid_gates: &[Bool<'a>],
) -> (Vec<String>, Option<CoveringRun>) {
    let declared = visitor::declared_features(manifest);
    if declared.is_empty() {
        return (Vec::new(), None);
    }

    // The constraints the enabler has to live under: the crate's own, plus each
    // std gate held off. Gates are added one at a time and skipped when they
    // conflict — two spans can be gated by mutually exclusive features, and the
    // prober negates one gate at a time rather than all at once.
    let mut constraints: Vec<Bool<'a>> = hard_constraints.to_vec();
    {
        let probe = z3::Solver::new(ctx);
        for c in &constraints {
            probe.assert(c);
        }
        if probe.check() != z3::SatResult::Sat {
            debug!("[enablers] hard constraints are unsatisfiable; skipping discovery");
            return (Vec::new(), None);
        }
        for gate in avoid_gates {
            let negated = gate.not();
            probe.push();
            probe.assert(&negated);
            let ok = probe.check() == z3::SatResult::Sat;
            probe.pop(1);
            if ok {
                probe.assert(&negated);
                constraints.push(negated);
            }
        }
    }

    // The set the prober would start from. Solved here rather than taken from a
    // covering run because the covering runs are exactly the ones that failed.
    let solver = z3::Solver::new(ctx);
    for c in &constraints {
        solver.assert(c);
    }
    if solver.check() != z3::SatResult::Sat {
        debug!("[enablers] constraints are unsatisfiable; skipping discovery");
        return (Vec::new(), None);
    }
    let base = solver::model_to_features(&solver.get_model()).0;
    let base_set: HashSet<&String> = base.iter().collect();

    let mut candidates: Vec<String> = declared
        .iter()
        .filter(|f| !base_set.contains(f))
        .filter(|f| {
            // A candidate the constraints cannot hold together with is not a
            // configuration the rest of the analysis could ever emit — and with
            // the gates negated above, that is exactly where `std` (and anything
            // implying it) drops out.
            let s = z3::Solver::new(ctx);
            for c in &constraints {
                s.assert(c);
            }
            s.assert(&Bool::new_const(ctx, f.as_str()));
            s.check() == z3::SatResult::Sat
        })
        .cloned()
        .collect();
    candidates.sort();

    if candidates.is_empty() {
        return (Vec::new(), None);
    }

    debug!(
        "[enablers] no bare-metal target has compiled; base {:?}, trying {} candidate feature(s): {:?}",
        base, candidates.len(), candidates
    );

    let budget = std::cell::Cell::new(MAX_ENABLER_PROBES);
    // Only the first trial is allowed to sweep `TARGET_LIST` looking for a triple
    // that works; after that every trial is pinned to one. A trial that succeeds
    // sets `LAST_GOOD_TARGET` and pins itself; a trial that fails leaves the cache
    // empty, and without this each subsequent failure would cost another 26
    // builds. `TARGET_LIST[0]` is the arbitrary-but-fixed stand-in for that case —
    // a crate that builds bare-metal at all almost always builds for most triples,
    // and if this one is wrong the search just reports nothing, which is where it
    // would have been anyway.
    let pinned = std::cell::Cell::new(false);
    // The records of the most recent trial that compiled, kept so the caller can
    // adopt it as a covering run instead of paying for the build and dropping it.
    let mut last_success: Option<CoveringRun> = None;
    let mut compiles = |extra: &[String]| -> bool {
        if budget.get() == 0 {
            return false;
        }
        budget.set(budget.get() - 1);
        let pin = match (pinned.get(), *LAST_GOOD_TARGET.lock().unwrap()) {
            (false, _) => None,
            (true, Some(t)) => Some(t),
            (true, None) => Some(consts::TARGET_LIST[0]),
        };
        pinned.set(true);
        let mut feats = base.clone();
        feats.extend(extra.iter().cloned());
        let trial = timing::scope("enabler_trial", extra.join(","));
        let outcome = run_rustc_plugin_pass_with(manifest, crate_name, &feats, None, false, pin);
        let ok = matches!(outcome, PassOutcome::Success { .. });
        if let PassOutcome::Success { full_output, .. } = outcome {
            trial.meta("records", full_output.records.len().to_string());
            last_success = Some(CoveringRun {
                features: feats,
                output: full_output,
            });
        }
        trial.meta("compiles", ok.to_string());
        drop(trial);
        debug!(
            "[enablers] {} with extra {:?}",
            if ok { "compiles" } else { "fails" },
            extra
        );
        ok
    };

    // All-on first: one compile that answers "is any of this the problem?", and
    // when it succeeds the shrink below is a descent rather than a search.
    let mut keep = if compiles(&candidates) {
        candidates
    } else {
        // All-on can fail for a reason unrelated to the enabler — two candidates
        // that cannot be on together, or one that breaks the build by itself. Fall
        // back to trying each alone; the shrink then has nothing left to do.
        debug!("[enablers] every candidate on does not compile; trying them one at a time");
        let single = candidates
            .iter()
            .find(|c| compiles(std::slice::from_ref(*c)))
            .cloned();
        match single {
            Some(c) => vec![c],
            None => {
                debug!("[enablers] no candidate makes the crate build for a bare-metal target");
                return (Vec::new(), None);
            }
        }
    };

    // Halving pass: cheap way down from a large candidate list when a single
    // feature is responsible, which is the usual shape (`libm`, `alloc`).
    while keep.len() > 1 && budget.get() > 0 {
        let mid = keep.len() / 2;
        let left: Vec<String> = keep[..mid].to_vec();
        let right: Vec<String> = keep[mid..].to_vec();
        if compiles(&left) {
            keep = left;
        } else if compiles(&right) {
            keep = right;
        } else {
            // The enabler set straddles the split; the removal pass finishes it.
            break;
        }
    }

    // Removal pass: drop anything the build does not actually need, including —
    // when `keep` is down to one element — that last one, which is how a base set
    // that compiles on its own returns the empty answer.
    for cand in keep.clone() {
        if budget.get() == 0 {
            break;
        }
        let trial: Vec<String> = keep.iter().filter(|f| **f != cand).cloned().collect();
        if compiles(&trial) {
            keep = trial;
        }
    }

    if keep.is_empty() {
        debug!("[enablers] base set compiles on its own; nothing to pin");
    } else {
        debug!("[enablers] crate does not build for any bare-metal target without {keep:?}");
    }
    (keep, last_success)
}

/// Classify every span the covering runs recorded and split off the two
/// `AlwaysStd` populations the prober works on: imports and everything else.
///
/// Its own function because it runs twice — once over the runs the covering-set
/// search produced, and again when `discover_build_enablers` adopts a run that
/// compiled. Re-classifying is the whole point of that adoption: `AlwaysStd`
/// means "std in *every* run", so it is a verdict about the run set, not about a
/// span, and it has to be recomputed when the run set grows.
fn classify_and_split(
    covering_runs: &[CoveringRun],
    crate_name: &str,
    telemetry: &mut Telemetry,
) -> (Vec<SpanAnalysis>, Vec<SpanAnalysis>, Vec<SpanAnalysis>) {
    let analyses = {
        let t = timing::scope("classify", crate_name);
        t.meta("runs", covering_runs.len().to_string());
        classify_spans(covering_runs)
    };

    // Spans where a derive-style collision and unavoidable std-ness coincide —
    // see `Telemetry::collided_std_spans`. Recorded before any probing so the
    // count reflects classification alone.
    telemetry.collided_std_spans = analyses
        .iter()
        .filter(|a| a.std_in_every_run && !a.non_std_configs.is_empty())
        .count();
    if telemetry.collided_std_spans > 0 {
        debug!(
            "{} std span(s) collide with non-std records at the same position and are std in every run",
            telemetry.collided_std_spans
        );
    }

    let imports = get_always_std_imports(&analyses)
        .into_iter()
        .cloned()
        .collect();
    let others = get_always_std_others(&analyses)
        .into_iter()
        .cloned()
        .collect();
    (analyses, imports, others)
}

/// The last element is the *unproven* spans — std spans that are std in every
/// covering run and whose probe never compiled. They are not in `all_hard`,
/// which means proven-unavoidable std, but a crate holding any of them has not
/// been shown clean either. See `Telemetry::unproven_std_spans`.
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
    Vec<ReadableSpan>,
) {
    let (root, mut covering_runs, mut hard_constraints, compile_error_constraints, no_std_conds) =
        find_feature_combs_for_all_code(ctx, manifest, crate_name, telemetry);

    // A routeless bare std use (e.g. a `HashMap` brought in by a glob re-export of
    // an externally-gated `use std::collections::HashMap`) carries no route back
    // to the import `resolve_local_facade_gateways` needs. Join it to that import
    // by `def_path` and inherit the gate. Needs the module tree (`root`) to know
    // which imports are externally gated, so it runs here rather than at load time.
    for run in &mut covering_runs {
        telemetry.routed_import_anchors += resolve_import_to_use_gateways(&mut run.output, &root);
    }
    if telemetry.routed_import_anchors > 0 {
        debug!(
            "{} std record(s) inherited a gate from the import that bound their name",
            telemetry.routed_import_anchors
        );
    }

    // Same set the module tree was built against — macro-body cfgs must undergo
    // the identical undeclared-feature erasure or they reintroduce variables the
    // visitor already dropped.
    let known_features = visitor::declared_features(manifest);

    // The comparison is against the *final* run set, so it is computed after the
    // enabler search below may have added one. Only the pass stays here — it is a
    // compile, and moving it would reorder the builds.
    let default_features_output = match run_default_features_pass(manifest, crate_name) {
        PassOutcome::Success { full_output, .. } => Some(full_output),
        _ => {
            warn!(
                "Default-features pass failed; skipping coverage comparison for {}",
                crate_name
            );
            None
        }
    };

    let all_constraints = visitor::collect_all_items(&root, ctx);

    let (mut analyses, mut always_std_imports, mut always_std_others) =
        classify_and_split(&covering_runs, crate_name, telemetry);

    // `LAST_GOOD_TARGET` is still unset only when not one covering run compiled
    // for a bare-metal target — every record above came from the host fallback.
    // Probing from here is doomed: each probe compiles the same way and comes back
    // `CompileFailed`, so look for the feature the crate needs to build at all
    // before spending them. Gated on there being an `AlwaysStd` span to probe,
    // because those are the only ones a failed probe turns into `unproven` — a
    // crate with nothing to prove has nothing to gain from the search.
    //
    // Skipped outright for a crate that already has a good target, which is the
    // overwhelming majority. When it does run and fails, the cost is one probe's
    // worth of builds; when it succeeds it also fixes `LAST_GOOD_TARGET`, so every
    // probe after it stops sweeping all 26 targets.
    // Local, not read back off `telemetry`: one `Telemetry` is shared by the main
    // crate and every dependency analysed after it, so recovering the list from
    // there hands the main crate's `libm` to each dep's solve — and a dep that has
    // no such feature emits it as `<dep>/libm` in `custom_no_std_feature_enabled`,
    // which cargo rejects outright (observed on totsu_core → `log/libm`).
    let mut build_enablers: Vec<String> = Vec::new();
    if LAST_GOOD_TARGET.lock().unwrap().is_none()
        && !(always_std_imports.is_empty() && always_std_others.is_empty())
    {
        // The gates the prober is about to negate. Passed in so the search never
        // proposes a feature that satisfies one of them — `std` is otherwise a
        // perfectly good answer to "what makes this crate compile".
        let avoid_gates: Vec<Bool> = always_std_imports
            .iter()
            .map(|a| &a.exemplar)
            .chain(always_std_others.iter().map(|a| &a.exemplar))
            .filter_map(|ex| {
                ancestors_for_record(&root, ex)
                    .or_else(|| macro_body_cfgs_to_ancestors(ctx, &ex.macro_body_cfgs, &known_features))
            })
            .flatten()
            .collect();
        let enabler_run;
        (build_enablers, enabler_run) = {
            let _t = timing::scope("build_enablers", crate_name);
            discover_build_enablers(ctx, manifest, crate_name, &hard_constraints, &avoid_gates)
        };
        for f in &build_enablers {
            println!(
                "Enabling feature '{f}' — {crate_name} does not build for any bare-metal target without it"
            );
            hard_constraints.push(Bool::new_const(ctx, f.as_str()));
        }
        telemetry
            .build_enabler_features
            .extend(build_enablers.iter().cloned());

        // A trial that compiled is a successful bare-metal build of a real feature
        // set — the same thing every other `CoveringRun` is — and it is the only
        // one this crate has. Adopting it is what makes the search's records count:
        // pinning `default` for xmrs 0.9.9 still leaves its eight ungated
        // `f32::{powf,round,…}` calls `AlwaysStd`, because `std_in_every_run` is
        // trivially true over the one std-on run, and an ungated span is
        // short-circuited to `StillStd` by `initial_ungated_results` without ever
        // compiling. The adopted run resolves all eight to `micromath::F32Ext`.
        //
        // Adding runs only ever weakens an `AlwaysStd` verdict — the verdict needs
        // the span to be std in *every* run — so this cannot fail a crate that
        // passes today. A span only the new run witnesses arrives `Conditional`,
        // which keeps it out of `all_hard` as well.
        if let Some(mut run) = enabler_run {
            // The same normalisation every other covering run gets on the way in.
            telemetry.routed_import_anchors +=
                resolve_import_to_use_gateways(&mut run.output, &root);
            debug!(
                "[enablers] adopting the trial that compiled ({:?}, {} records) as a covering run",
                run.features,
                run.output.records.len()
            );
            covering_runs.push(run);
            (analyses, always_std_imports, always_std_others) =
                classify_and_split(&covering_runs, crate_name, telemetry);
        }
    }

    let coverage_comparison = default_features_output
        .as_ref()
        .map(|out| compute_coverage_comparison(out, &covering_runs));

    let probe_candidates_imports = always_std_imports
        .into_iter()
        .filter(|a| !is_local_reexport(&a.exemplar))
        .map(|a| ProbeTarget {
            analysis: a.clone(),
            ancestors: ancestors_for_record(&root, &a.exemplar)
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
            ancestors: ancestors_for_record(&root, &a.exemplar)
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

    let conditional_candidates = get_conditional_spans(&analyses)
        .into_iter()
        .filter(|a| !is_local_reexport(&a.exemplar))
        .map(|a| ProbeTarget {
            analysis: a.clone(),
            ancestors: ancestors_for_record(&root, &a.exemplar)
                .or_else(|| macro_body_cfgs_to_ancestors(ctx, &a.exemplar.macro_body_cfgs, &known_features)),
            externally_gated: span_externally_gated(&root, &a.exemplar),
        })
        .collect::<Vec<_>>();

    // The features the crate's own `#![cfg_attr(<cond>, no_std)]` names: the
    // author's statement of what decides this crate's no_std-ness. Only these
    // are eligible for the run-derived attribution below.
    //
    // Run evidence alone is not enough to name a cause. wg 0.9.2 has four
    // covering runs in which `triomphe` is on in exactly the runs where
    // `parking_lot` is off, so `triomphe` satisfies
    // `phases::feature_explaining_std` perfectly while the std-ness is really
    // `parking_lot`'s — and blaming it cost wg its whole feature list. Requiring
    // the candidate to be a *declared* no_std switch is what separates that from
    // uom, whose `#![cfg_attr(not(feature = "std"), no_std)]` says outright that
    // `std` is the feature in question.
    let no_std_switch: HashSet<String> = no_std_conds
        .iter()
        .flat_map(|c| feature_atoms(c, &known_features))
        .collect();
    debug!("Declared no_std switch features: {:?}", no_std_switch);

    // A conditional span whose covering runs already name the feature its
    // std-ness rides on needs no probe: the answer is stronger than one the
    // probe can give (see `phases::feature_explaining_std`) and it costs no
    // compile. Everything else keeps going through the ancestor probe.
    //
    // Only the population the probe would otherwise *mis-blame* is diverted —
    // the gated, feature-axis spans. A span with no gate ancestors, or one
    // guarded by a cfg naming no feature, is answered by
    // `initial_ungated_results` with no condition at all, and run evidence is no
    // reason to start constraining it: that would take features away from crates
    // that build today.
    let explains = |t: &ProbeTarget<'a>| -> Option<String> {
        feature_explaining_std(&t.analysis).filter(|f| no_std_switch.contains(f))
    };

    let (explained, conditional_targets): (Vec<_>, Vec<_>) =
        conditional_candidates.into_iter().partition(|t| {
            !t.externally_gated && t.ancestors.is_some() && explains(t).is_some()
        });

    let explained_results: Vec<ProbeResult> = explained
        .into_iter()
        .map(|target| {
            let feature = explains(&target).expect("partitioned on this being Some");
            debug!(
                "Conditional span {:?} is std only when '{}' is on (every std run has it, at least one non-std run does not) — condition ¬{}, no probe",
                target.analysis.span, feature, feature
            );
            let condition = Bool::new_const(ctx, feature.as_str()).not();
            ProbeResult {
                target,
                decision: ProbeDecision::NonStd {
                    reason: format!(
                        "the covering runs resolve this span to std only with '{}' enabled",
                        feature
                    ),
                    alternate_crate: "unknown".to_string(),
                },
                history: Vec::new(),
                condition: Some(condition),
            }
        })
        .collect();

    let mut conditional_results = probe_conditional_spans(
        ctx,
        crate_name,
        manifest,
        conditional_targets,
        &hard_constraints,
        &all_constraints,
    );
    // A gate negation that merely deleted the code reads as "not std" to the
    // prober. Where the runs hold a witness that says otherwise — the span
    // present and non-std with that gate satisfied — the condition is dropped
    // and the span contributes none. It keeps its `NonStd` verdict: the witness
    // is exactly the evidence that this span does not stop the crate being
    // no_std, so there is nothing left to constrain.
    for result in &mut conditional_results {
        if matches!(result.decision, ProbeDecision::NonStd { .. })
            && let Some(cond) = result.condition.clone()
            && condition_contradicted_by_runs(ctx, &result.target.analysis, &cond, &known_features)
        {
            debug!(
                "Dropping condition {} for span {:?}: a covering run has it false with the span present and non-std",
                cond, result.target.analysis.span
            );
            telemetry.conditions_contradicted_by_runs += 1;
            result.condition = None;
        }
    }

    // The same veto for spans the covering runs never witnessed at all — the
    // ones the check above cannot speak for, because no run has the span present
    // and non-std. Rather than let the probe's "the code disappeared" stand,
    // compile the witness: a configuration that *satisfies* the gate under the
    // hard constraints. If the span is not std there, the gate is not what makes
    // it non-std. See `phases::gate_satisfied_std_spans` for why zeno clears this
    // and tarfs and wg do not.
    //
    // One compile per distinct gate, cached — the population is small (only
    // witness-less spans whose probe returned NonStd with a condition), and
    // targets sharing a gate share the answer.
    let mut gate_runs: HashMap<Vec<String>, Option<Vec<ReadableSpan>>> = HashMap::new();
    for result in &mut conditional_results {
        if !matches!(result.decision, ProbeDecision::NonStd { .. })
            || result.condition.is_none()
            || !result.target.analysis.non_std_configs.is_empty()
        {
            continue;
        }
        let Some(ancestors) = result.target.ancestors.clone() else {
            continue;
        };

        let key: Vec<String> = ancestors.iter().map(|b| b.to_string()).collect();
        let std_spans = gate_runs.entry(key).or_insert_with(|| {
            gate_satisfied_std_spans(
                ctx,
                crate_name,
                manifest,
                &ancestors,
                &hard_constraints,
                &all_constraints,
            )
        });

        if let Some(spans) = std_spans
            && !spans.iter().any(|s| {
                *s == result.target.analysis.span && s.usage_crate.as_deref() == Some("std")
            })
        {
            debug!(
                "Dropping condition {:?} for span {:?}: a configuration satisfying the gate compiles with the span not std",
                result.condition, result.target.analysis.span
            );
            telemetry.conditions_refuted_by_gate_run += 1;
            result.condition = None;
        }
    }

    // Joined here so every consumer below — `final_condition`, the
    // externally-gated and compile-failed counters — sees one conditional
    // population, as it did before the split.
    conditional_results.extend(explained_results);

    // The discovered build enablers ride out with the probe conditions rather
    // than staying local to the probing: `final_condition` is what `main.rs`
    // solves the emitted feature list from *and* what `hard_constraint_features`
    // protects from `minimize`. A probe that only compiled because `libm` was on
    // proves nothing if the config shipped afterwards leaves it off.
    let final_condition = hard_imports
        .iter()
        .chain(hard_usages.iter())
        .chain(conditional_results.iter())
        .filter(|a| matches!(a.decision, ProbeDecision::NonStd { .. }))
        .filter_map(|a| a.condition.clone())
        .chain(
            build_enablers
                .iter()
                .map(|f| Bool::new_const(ctx, f.as_str())),
        )
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

    let compile_failed_spans = hard_imports
        .iter()
        .chain(hard_usages.iter())
        .chain(conditional_results.iter())
        .filter(|a| matches!(a.decision, ProbeDecision::CompileFailed))
        .count();
    if compile_failed_spans > 0 {
        debug!(
            "{} std span(s) dropped from all_hard because their probe never compiled",
            compile_failed_spans
        );
    }
    telemetry.compile_failed_spans = compile_failed_spans;

    // The subset of those that leaves the crate's std-ness *unknown*, reported
    // separately so a quiet clearance cannot pass for a proven one.
    //
    // Drawn only from the two AlwaysStd populations. A `Conditional` span has
    // direct evidence it can be non-std — a covering run in which it produced no
    // std record at all — so its probe failing leaves the *condition* unpinned,
    // not the avoidability, and folding it in here would fail crates that are
    // demonstrably fine. `compile_failed_spans` keeps counting all three, so the
    // existing metric is unchanged and the difference between the two is exactly
    // the conditional-origin failures.
    let unproven: Vec<ReadableSpan> = hard_imports
        .iter()
        .chain(hard_usages.iter())
        .filter(|a| matches!(a.decision, ProbeDecision::CompileFailed))
        .map(|a| a.target.analysis.span.clone())
        .collect();
    if !unproven.is_empty() {
        debug!(
            "{} std span(s) could not be proven avoidable: every feature set negating their gate failed to compile",
            unproven.len()
        );
    }
    telemetry.unproven_std_spans = unproven.len();

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
        unproven,
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

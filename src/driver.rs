use log::{debug, warn};
use proc_macro2::Span;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::Mutex;
use std::{fs, process::Command};
use uuid::Uuid;
use walkdir::WalkDir;
use which::which;
use z3::ast::Ast;

use z3::{Context, ast::Bool};

use serde_json;

use crate::phases::*;
use crate::types::*;
use crate::visitor::{self, ModCollector, ModNode};
use crate::{
    ProcMacroDep, ReadableSpan, Telemetry, ablation,
    consts::{self, PLUGIN_OUTPUT_ENV},
    downloader, parser, solver, target_cfg, timing,
};

/// The first bare-metal `--target` that successfully compiled a no_std plugin
/// pass for this crate. Once set, every later pass reuses *only* it (plus the
/// host fallback) instead of re-scanning `TARGET_LIST` — the many covering-set
/// and CEGAR runs for one crate would otherwise each grind through all 26
/// targets, and for a feature combo that does not compile that is 26 wasted
/// builds per iteration. A combo that fails on the crate's established good
/// target is almost never rescued by a different triple, and the host fallback
/// still catches genuine std; the cost is a minor precision loss (host cfgs) for
/// exactly those combos.
///
/// **Process-global on purpose, and deliberately not reset between crates.** A
/// triple that linked for one crate is a good first guess for the next, and
/// dependencies are analysed in this same process — so a dep inherits the main
/// crate's answer instead of re-scanning `TARGET_LIST`. What this cache is *not*
/// is a statement about the crate currently under analysis: "the cache is set"
/// answers "has anything compiled bare-metal in this process", never "has *this*
/// crate". Anything that needs the second question wants
/// [`CRATE_REACHED_BARE_METAL`]; reading this one instead is how KI-28 happened.
static LAST_GOOD_TARGET: Mutex<Option<&'static str>> = Mutex::new(None);

/// Did any plugin pass for the crate *currently* under analysis compile for a
/// bare-metal target?
///
/// The per-crate half of [`LAST_GOOD_TARGET`], and the one
/// [`discover_build_enablers`] is gated on. The two used to be the same flag,
/// which silently made the enabler search main-crate-only: `analyze_crate` runs
/// for every dependency in the same process, so the first crate that compiled
/// bare-metal left the cache set and every crate after it skipped the search.
/// The unit-sphere 0.4.0 run measured it — 19 `analyze` calls, one
/// `build_enablers` call lasting 16 ms, which was the featureless main crate
/// returning on `declared.is_empty()`. Not one of its 18 dependencies was asked.
///
/// KI-28, and latent as far as anything observed: the shapes the search exists
/// for (euclid, bevy_input, totsu_core) are all documented as *main* crates, and
/// no corpus crate is yet known to convert because a *dependency* got its
/// enabler. It does not explain unit-sphere — nalgebra builds bare-metal without
/// `libm` perfectly well, so the search is right to skip it there; that one is
/// KI-27 plus an arbitrary Z3 don't-care.
///
/// Reset per crate next to [`HOST_NOT_NO_STD`], at the top of
/// `find_feature_combs_for_all_code`, so the previous crate's answer cannot leak
/// into this one. Set wherever `LAST_GOOD_TARGET` is.
static CRATE_REACHED_BARE_METAL: Mutex<bool> = Mutex::new(false);

/// The feature sets that compiled for a bare-metal target, for the crate
/// *currently* under analysis.
///
/// [`CRATE_REACHED_BARE_METAL`] is the same evidence collapsed to a bool, and
/// collapsing it is what KI-30 is: "this crate compiled bare-metal once" is a
/// fact about *a* configuration, and the configuration the solve goes on to
/// emit is usually a different one. mavlink-core 0.13.1 is the shape — 13
/// plugin passes, exactly one of them (`--features embedded-hal-02`) compiles,
/// and the emitted set contains none of `std`, `embedded` or `embedded-hal-02`,
/// which leaves the crate with no IO prelude at all (9 × `E0405 cannot find
/// trait Read`). Keeping the sets lets a later stage ask the question the bool
/// cannot answer: *is the set about to be emitted one this crate has actually
/// compiled in?*
///
/// Written and reset wherever `CRATE_REACHED_BARE_METAL` is, and read only
/// through [`bare_metal_compiling_sets`].
static CRATE_BARE_METAL_SETS: Mutex<Vec<Vec<String>>> = Mutex::new(Vec::new());

/// Does the crate under analysis declare `#![no_std]` on a *target* predicate
/// that the host does not satisfy?
///
/// If so, the crate compiled without `--target` is not merely a std-linking
/// build of a no_std crate — it is a build in which the crate's own `#![no_std]`
/// was never applied, i.e. a different crate configuration from the one under
/// test. Its std records are inconclusive for the same reason O-7's are, and
/// more strongly: there, `--no-default-features` at least reached the crate with
/// no_std in force and a dependency answered `std`; here the attribute itself is
/// off. See `run_rustc_plugin_pass`.
///
/// Set per crate from `visitor::ModCollector::non_feature_no_std_predicate`, at the
/// top of `find_feature_combs_for_all_code` — before that crate's first pass and
/// after the previous crate's last, so a dependency is never judged by the main
/// crate's attribute. False for the overwhelming majority: 46 crates of the
/// 20789-crate corpus carry a target-conditional `#![no_std]` at all.
static HOST_NOT_NO_STD: Mutex<bool> = Mutex::new(false);

/// Clear the process-global per-crate caches. **For tests only.**
///
/// `CRATE_REACHED_BARE_METAL` is reset per crate in the normal course of a run,
/// so a test that goes through `find_feature_combs_for_all_code` does not need
/// this. A test that calls the lower layers directly does, and clearing it is
/// cheap, so it is cleared here too.
///
/// `LAST_GOOD_TARGET` is the one that genuinely leaks: nothing resets it between
/// crates by design, and in a test binary several `analyze_crate` calls share one
/// process, so a fixture that compiles for a bare-metal target hands its triple
/// to every test after it. That used to also decide whether
/// `discover_build_enablers` ran at all — the suite then passed or failed on
/// thread scheduling rather than on behaviour, which is how
/// `the_trial_that_compiled_becomes_a_covering_run` came to fail under
/// `--test-threads=1` while passing by default. Splitting the enabler gate onto
/// `CRATE_REACHED_BARE_METAL` removes that particular coupling; the target hint
/// still crosses tests, which is why this exists.
///
/// Callers must also serialise against each other; resetting the cache while
/// another test is mid-analysis would take that test's target away.
pub fn reset_target_cache() {
    *LAST_GOOD_TARGET.lock().unwrap() = None;
    *HOST_NOT_NO_STD.lock().unwrap() = false;
    *CRATE_REACHED_BARE_METAL.lock().unwrap() = false;
    CRATE_BARE_METAL_SETS.lock().unwrap().clear();
}

/// The feature sets that compiled for a bare-metal target for the crate last
/// analysed. Empty when none did — the case the enabler search already covers.
pub fn bare_metal_compiling_sets() -> Vec<Vec<String>> {
    CRATE_BARE_METAL_SETS.lock().unwrap().clone()
}

/// Is `selection` a configuration this crate has actually compiled in?
///
/// True when some set that compiled bare-metal is a *subset* of the selection:
/// everything that build had, this one has too. Not a proof that the selection
/// compiles — features are not monotone, and `embedded` + `embedded-hal-02`
/// together are a `compile_error!` in the crate this was written for — but it
/// is the honest cheap answer to "has the crate been seen standing on these
/// features", and it is used only to decide whether spending builds on a search
/// is worth it. A crate with no compiling set is never witnessed.
///
/// ⚠ Reads the live per-crate record, so it answers for **whichever crate was
/// analysed last** — `analyze_crate` runs for every dependency too and clears
/// the record each time. Only a caller that runs immediately after the crate's
/// own analysis may use it; `bin/main` asks about the main crate after every
/// dependency has been analysed, so it snapshots
/// [`bare_metal_compiling_sets`] first and calls [`set_is_witnessed`]. That
/// distinction is KI-28 one flag down.
pub fn selection_is_witnessed(selection: &HashSet<String>) -> bool {
    set_is_witnessed(&CRATE_BARE_METAL_SETS.lock().unwrap(), selection)
}

/// The subset rule of [`selection_is_witnessed`], over an explicit list of
/// compiling sets so it can be tested without a compile.
pub fn set_is_witnessed(compiled_sets: &[Vec<String>], selection: &HashSet<String>) -> bool {
    compiled_sets
        .iter()
        .any(|compiled| compiled.iter().all(|f| selection.contains(f)))
}

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
                (
                    k.clone(),
                    *gated,
                    anchor.as_ref().map(|(s, _)| s.start_line),
                )
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

    // `--lib` for the same reason every other pass takes it (bucket F): the pass
    // stops before codegen, so no rmeta is emitted for the lib and a `[[bin]]`
    // that uses its own crate fails on `extern location for X does not exist`.
    // The whole pass then fails and its records are lost — which is how bebytes
    // 0.7.1's `bin/macro_test.rs` silently cost this crate its evidence.
    let mut args: Vec<&str> = vec!["hir", "--", "--manifest-path", manifest];
    if visitor::package_has_lib(manifest) {
        args.push("--lib");
    }

    debug!(
        "Running default-features pass for {}, output -> {:?}",
        crate_name, output_path
    );

    let attempt = timing::scope("cargo_hir", "host");
    attempt.meta("features", "<default>");
    let has_lib = args.contains(&"--lib");
    let cargo_hir_result = run_cargo_hir_cached(
        manifest,
        &args,
        &output_path,
        crate_name,
        "<default>",
        None,
        has_lib,
    );
    let (success, stderr, exit_code) = match cargo_hir_result {
        CargoHirAttempt::SpawnFailed(e) => {
            attempt.meta("success", "false");
            return PassOutcome::CompileFailed {
                stderr: e,
                exit_code: None,
            };
        }
        CargoHirAttempt::Compiled { .. } => (true, String::new(), None),
        CargoHirAttempt::CompileFailed { stderr, exit_code } => (false, stderr, exit_code),
    };
    attempt.meta("success", success.to_string());
    drop(attempt);

    if !success {
        debug!(
            "default-features pass failed for {} (exit {}): {}",
            crate_name,
            exit_code.unwrap_or(-1),
            stderr
        );
        let _ = fs::remove_file(&output_path);
        return PassOutcome::CompileFailed { stderr, exit_code };
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

    if !ablation::flags().no_gateway_resolution {
        resolve_local_facade_gateways(&mut full_output);
    }
    let std_spans = extract_hard_std_candidates(&full_output, None);
    let macro_modules = full_output.macro_module_imports.clone();

    PassOutcome::Success {
        macro_modules,
        std_spans,
        full_output,
        // The default-features pass is a host build of the crate's *default*
        // configuration — a std run by construction, and never classified.
        std_inconclusive: false,
    }
}

/// Upper bound on the trial builds [`park_injecting_proc_macros`] spends on one
/// macro. The answer is a single feature in every case measured, and the ordering
/// puts the likely one first, so this only bites a macro with a long default list
/// that injects std under a late one — where the alternative is a build per
/// default (`educe` declares twelve).
const MAX_PROC_MACRO_PARK_TRIALS: usize = 4;

/// Turn off the default feature of a proc-macro dependency that the compiler shows
/// is injecting `std` into this crate — and only if turning it off both removes
/// that std and leaves the crate building.
///
/// A proc macro is skipped by the no_std walk because it is compiled for the host
/// and run there: its own `use std::collections::HashMap` says nothing about the
/// crate being analysed. Its `[features]` are a different matter — they choose the
/// tokens it *injects*, which land in this crate at the macro's call span, ungated
/// and unremovable by anything the crate itself declares:
///
/// ```ignore
/// // displaydoc 0.2.6, src/expand.rs — `default = ["std"]`
/// #[cfg(feature = "std")]
/// fn specialization() -> TokenStream {
///     quote! { extern crate std;                                  // ← the consumer's
///              impl PathToDisplayDoc for std::path::Path { … } } }
/// #[cfg(not(feature = "std"))]                                    // emits nothing
/// ```
///
/// **The question is which feature, and it is answered by building, not by
/// reading names.** `PathRecord::expansion_crate` carries the crate that *defines*
/// the macro a record came out of, so a std record attributed to `displaydoc` is
/// the compiler saying displaydoc put it there. Each of that macro's defaults is
/// then parked in turn and the crate recompiled: the trial is accepted when the
/// records attributed to it are gone **and** the pass still compiled, and rolled
/// back otherwise.
///
/// Both halves are load-bearing, and the name test they replace was wrong in both
/// directions:
///
/// * *attribution* — parking a macro that never injected anything is a manifest
///   change with nothing to gain. 115 of the corpus's 1719 proc-macro crates have
///   a `std`-named default; almost none of them inject std.
/// * *the build* — of those 115, **8** use that default to guard their own host
///   code, so parking it stops the macro compiling and every target build of the
///   consumer dies inside it. bebytes 0.7.1 is the measured case: parking
///   `bebytes_derive/std` cost it all 26 targets on `E0433 use of unresolved
///   module std` at `bebytes_derive/src/bit_validation.rs:5`, and bebytes_derive
///   injects no std at all — evidence rejects the parking twice over.
///
/// Cost is zero for a crate with no proc-macro dependency carrying defaults (4% of
/// the corpus have one), one host build for those, and one build per trial only
/// where the evidence pass actually found injected std.
///
/// Only *defaults* are candidates. A feature the consumer names on the edge itself
/// (`features = ["std"]`) is the author asking for it, not something on by omission,
/// and removing it is a different decision that belongs to the ordinary feature
/// solve. The old rule drew the same line.
pub fn park_injecting_proc_macros(
    main_name: &str,
    manifest: &str,
    proc_macro_deps: &[ProcMacroDep],
    telemetry: &mut Telemetry,
) {
    let candidates: Vec<(&ProcMacroDep, Vec<String>)> = proc_macro_deps
        .iter()
        .map(|dep| {
            let mut defaults = parser::proc_macro_default_features(&dep.manifest);
            // Ordering only — see `parser::std_feature_name_first`. The trial that
            // is kept is the one that compiled with the std records gone.
            defaults.sort_by_key(|f| !parser::std_feature_name_first(f));
            (dep, defaults)
        })
        .filter(|(_, defaults)| !defaults.is_empty())
        .collect();
    if candidates.is_empty() {
        return;
    }

    let _t = timing::scope("proc_macro_evidence", main_name);
    debug!(
        "Proc-macro parking: {} candidate dependenc(ies) with defaults: {:?}",
        candidates.len(),
        candidates
    );

    // The crate as its author wrote it: every macro's defaults on, and the host, so
    // the pass compiles for a crate whose no_std configuration is not known yet.
    // A crate that does not build this way yields no evidence, and no evidence is
    // no parking — the same place the run would have been without this.
    let PassOutcome::Success { full_output, .. } = run_default_features_pass(manifest, main_name)
    else {
        debug!(
            "Proc-macro parking: the crate does not compile with its default features; no evidence to park on"
        );
        return;
    };

    for (dep, defaults) in candidates {
        // The edge is keyed by the package name; the records name the *crate*.
        let krate = parser::dep_crate_name(&dep.manifest, &dep.package);
        if injected_std_records(&full_output, &krate) == 0 {
            continue;
        }
        telemetry.proc_macro_std_injectors.push(dep.package.clone());
        park_one_proc_macro(main_name, manifest, dep, &krate, &defaults, telemetry);
    }
}

/// Try each default in turn, keep the first trial that compiles with the macro's
/// std records gone, and leave the manifest untouched if none does.
fn park_one_proc_macro(
    main_name: &str,
    manifest: &str,
    dep: &ProcMacroDep,
    krate: &str,
    defaults: &[String],
    telemetry: &mut Telemetry,
) {
    let package = dep.package.as_str();
    let Ok(original) = fs::read_to_string(manifest) else {
        warn!("Proc-macro parking: cannot read {}", manifest);
        return;
    };
    let restore = |what: &str| {
        if let Err(e) = fs::write(manifest, &original) {
            warn!("Proc-macro parking: cannot restore {}: {}", manifest, e);
        } else {
            debug!("Proc-macro parking: rolled back {} — {}", package, what);
        }
    };

    for feature in defaults.iter().take(MAX_PROC_MACRO_PARK_TRIALS) {
        if !parser::park_proc_macro_default(manifest, &dep.manifest, package, feature) {
            continue;
        }
        match run_default_features_pass(manifest, main_name) {
            PassOutcome::Success { full_output, .. } => {
                let left = injected_std_records(&full_output, krate);
                if left == 0 {
                    debug!(
                        "Proc-macro {}: parking `{}` removed every std record it injected",
                        package, feature
                    );
                    telemetry
                        .proc_macro_std_parked
                        .push(format!("{}/{}", package, feature));
                    return;
                }
                // Turning the *default* off did not turn the feature off. The
                // usual reason is the consumer's own `std` forwarding it —
                // ibc-types-core-client's `std = [… "displaydoc/std" …]`, whose
                // displaydoc edge already carries `default-features = false` and
                // still gets the std expansion. Nothing here can fix that, and
                // nothing needs to: the feature solve turns the crate's own `std`
                // off, and that takes the macro's with it.
                restore(&format!(
                    "`{}` off still leaves {} injected std record(s)",
                    feature, left
                ));
            }
            // The macro itself is what stopped compiling: this feature is its own
            // host code's, whatever else it may also select. bebytes_derive 0.8.1
            // is the case — `use std::vec::Vec` under `#![cfg_attr(not(feature =
            // "std"), no_std)]` — and parking it there cost bebytes 0.7.1 all 26
            // of its target builds inside a crate it never wrote.
            PassOutcome::CompileFailed { ref stderr, .. }
                if compile_failure_names_crate(stderr, package) =>
            {
                restore(&format!("the macro's own build needs `{}`", feature))
            }
            // The macro built; what failed is *this crate*, in its **default**
            // configuration — which is not the configuration under test. The
            // expansion the macro emits with the feature off is the no_std one,
            // and it does not have to fit a std build:
            // multiwii_serial_protocol_v2 0.1.12 is the case, where
            // packed_struct_codegen switches `::std::result::Result` for
            // `::core::result::Result` and the crate's own std configuration then
            // fails on `you might be missing crate core`. Rejecting here would
            // throw away a parking the no_std build wants, on evidence from a
            // build that was never the question — the same mistake O-7 and D2 are
            // both about. So it is kept, and recorded as what it is: a parking
            // whose *effect on the std records* could not be confirmed. The
            // covering runs are where that gets settled.
            PassOutcome::CompileFailed { .. } => {
                debug!(
                    "Proc-macro {}: `{}` off does not compile in this crate's DEFAULT \
                     configuration, but the macro itself built — keeping the parking \
                     unverified, the no_std runs decide",
                    package, feature
                );
                telemetry
                    .proc_macro_std_parked
                    .push(format!("{}/{}", package, feature));
                telemetry
                    .proc_macro_std_parked_unverified
                    .push(package.to_string());
                return;
            }
            // No records and no compile error to read: nothing was learned, so
            // nothing is changed.
            outcome => restore(&format!(
                "`{}` off yielded no evidence ({})",
                feature,
                outcome_summary(&outcome)
            )),
        }
    }

    debug!(
        "Proc-macro {}: injects std and no default of it can be turned off; leaving the edge alone",
        package
    );
    telemetry
        .proc_macro_std_unparkable
        .push(package.to_string());
}

/// How many std records this run attributes to a macro defined in crate `krate`.
///
/// `usage_crate` is the std identity the pipeline reads (a `panic_fmt` record
/// carries `definition_crate: "core"` and `usage_crate: "std"`), and both names are
/// compared with `-`/`_` folded — `derive-new` the package is `derive_new` the
/// crate. Where the two differ by more than that, `parser::dep_crate_name` is what
/// resolves it.
pub fn injected_std_records(output: &FeatureRunOutput, krate: &str) -> usize {
    let wanted = krate.replace('-', "_");
    output
        .records
        .iter()
        .filter(|r| r.span.usage_crate.as_deref() == Some("std"))
        .filter(|r| {
            r.expansion_crate
                .as_ref()
                .is_some_and(|c| c.replace('-', "_") == wanted)
        })
        .count()
}

/// Std this crate got from a proc macro the parking cannot reach: one the
/// manifest has no edge to. **KI-22.**
///
/// `driver::park_injecting_proc_macros` turns a macro's injected std off by
/// writing `default-features = false` on an edge of *this* crate's manifest.
/// A proc macro two levels down — `sp-debug-derive` below `sp-core`,
/// `displaydoc` below any of the 87 corpus crates that reach it only
/// transitively — is not an edge this manifest owns, so it keeps its `std`
/// feature and keeps injecting `extern crate std` at every invocation site. The
/// same wall `recursive_dep_requirement_check` reports for ordinary
/// dependencies: the tool cannot rewrite a manifest it does not emit.
///
/// So it is reported rather than silently failing a bare-metal build with
/// `E0463 can't find crate for std` at a span the crate never wrote. The
/// evidence is the compiler's: a std record whose `expansion_crate` names the
/// macro is that macro putting std here, the same signal the parking itself
/// decides on.
///
/// Runs on the default-features pass the coverage comparison already performs,
/// so it costs no build. It runs *after* the parking, which means a direct
/// injector that was successfully parked has no records left to find and what
/// survives here is genuinely out of reach.
///
/// Three exclusions, all of which would otherwise be reported as unreachable:
/// crates this manifest does have an edge to (the parking's own territory,
/// whether or not it succeeded), the crate under analysis and the sysroot
/// crates (`LOCAL`, `std`, `core`, `alloc` — a `std::println!` expansion is the
/// crate's own std, not an injection), and any expansion crate that is not a
/// proc macro, since a `macro_rules!` carries no feature of its own to turn off.
pub fn report_unreachable_proc_macro_injectors(
    output: &FeatureRunOutput,
    manifest: &str,
    crate_name: &str,
    telemetry: &mut Telemetry,
) {
    let Ok(text) = fs::read_to_string(manifest) else {
        return;
    };
    let Ok(manifest_toml) = toml::from_str::<toml::Value>(&text) else {
        return;
    };
    let declared = parser::declared_dependency_crate_names(&manifest_toml);
    let own = crate_name
        .split(':')
        .next()
        .unwrap_or(crate_name)
        .replace('_', "-");

    let mut injected: BTreeMap<String, usize> = BTreeMap::new();
    for record in &output.records {
        if record.span.usage_crate.as_deref() != Some("std") {
            continue;
        }
        let Some(expansion) = record.expansion_crate.as_deref() else {
            continue;
        };
        let folded = expansion.replace('_', "-");
        if folded == own
            || declared.contains(&folded)
            || consts::SYSROOT_CRATE_NAMES.contains(&expansion)
            || expansion == "LOCAL"
        {
            continue;
        }
        *injected.entry(expansion.to_string()).or_default() += 1;
    }

    for (macro_crate, records) in injected {
        let Some(dir) = parser::find_sibling_crate_dir(manifest, &macro_crate) else {
            continue;
        };
        if !parser::crate_dir_is_proc_macro(&dir) {
            continue;
        }
        let parents = parser::crate_edge_owners(manifest, &macro_crate);
        println!(
            "WARNING: proc macro `{}` injected {} std record(s) into {} and cannot be reached: \
             {} has no dependency edge to it{}",
            macro_crate,
            records,
            crate_name,
            crate_name,
            if parents.is_empty() {
                String::new()
            } else {
                format!(" — it comes in through {:?}", parents)
            }
        );
        telemetry
            .proc_macro_std_unreachable_injectors
            .push(crate::UnreachableProcMacro {
                macro_crate,
                consumer: crate_name.to_string(),
                parents,
                records,
            });
    }
}

fn outcome_summary(outcome: &PassOutcome) -> String {
    match outcome {
        PassOutcome::Success { .. } => "compiled".to_string(),
        PassOutcome::CompileFailed { exit_code, .. } => {
            format!("compile failed, exit {:?}", exit_code)
        }
        PassOutcome::PluginMissingOutput { .. } => "plugin produced no output".to_string(),
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

/// The smallest set of inputs that determines one `cargo hir` compile's
/// result: which manifest, which features, which target, and whether `--lib`
/// was passed. Two calls with an equal key run the identical `cargo` command
/// against byte-identical inputs.
///
/// Sound because of what this tool does *not* touch: every `fs::write` under
/// `src/` that reaches a crate's own directory writes a `Cargo.toml` — never a
/// `.rs` file, never `Cargo.lock`. A dependency's source tree is immutable for
/// the life of a process once downloaded, so hashing the two manifests it
/// *can* rewrite is exactly the surface a repeat call could differ on.
#[derive(Hash, Eq, PartialEq, Clone)]
struct CargoHirCacheKey {
    manifest_hash: u64,
    feats_sorted: String,
    target: Option<&'static str>,
    has_lib: bool,
}

/// What survived the compile — everything a cache hit needs to answer without
/// re-running it. `output_json` is the plugin's own file content, so a hit
/// restores it exactly as a fresh run would have written it.
///
/// `Encode`/`Decode` so this can also be the persisted form on disk for the
/// cross-process cache below — the in-process and cross-process caches share
/// one value type on purpose, since a hit from either answers the identical
/// question.
#[derive(Clone, bincode::Encode, bincode::Decode)]
enum CargoHirCacheValue {
    Succeeded { output_json: Vec<u8> },
    SucceededNoOutput,
    Failed { stderr: String, exit_code: Option<i32> },
}

/// Process-lifetime memo of every `cargo hir` compile this run has already
/// paid for. Two independent search mechanisms — the CEGAR covering-set loop
/// and `phases.rs`'s per-span satisfiability check — routinely re-derive the
/// same `(dependency, feature set, target)` question; measured on
/// `bridge-runtime-common-0.21.0`, 393 of 1150 plugin-pass calls (57%) were
/// exact repeats of an earlier call in the same run, 2.06 of the 3.64 hours
/// spent compiling. Neither mechanism knows about the other, so the fix sits
/// under both: this caches the one thing they both eventually call rather
/// than teaching either search to remember what the other already asked.
static CARGO_HIR_CACHE: std::sync::LazyLock<Mutex<HashMap<CargoHirCacheKey, CargoHirCacheValue>>> =
    std::sync::LazyLock::new(|| Mutex::new(HashMap::new()));

/// Hash of a manifest's bytes. `None` when it cannot even be read — the
/// caller treats that as "do not cache this attempt" rather than guessing at
/// a key for a file that is not there.
///
/// Deliberately **not** `Cargo.lock` too, despite `cargo hir` reading one:
/// a from-scratch compile *creates* the lock file for a dependency that did
/// not ship one, which most published libraries do not — measured directly,
/// the guard test below caught this the first time it was tried. Hashing
/// `Cargo.lock` alongside the manifest then makes every second call compute
/// a different key from the first (no lock existed yet vs. one now does),
/// permanently missing the cache it was meant to make safer. Cargo.toml
/// alone is sufficient: it is the tool's entire write surface (every
/// `fs::write` under `src/` that reaches a crate's directory targets a
/// `Cargo.toml`, never a `.rs` file or `Cargo.lock`), and once a lock is
/// resolved from an unchanged manifest, cargo does not silently re-resolve
/// it to something else mid-run.
fn hash_manifest(manifest: &str) -> Option<u64> {
    use std::hash::{Hash, Hasher};
    let mut h = std::collections::hash_map::DefaultHasher::new();
    let bytes = fs::read(manifest).ok()?;
    bytes.hash(&mut h);
    Some(h.finish())
}

/// Count of cache hits this process has served. Test-only signal: cache size
/// alone cannot distinguish "hit" from "never tried to cache" (both leave the
/// map unchanged), so a guard needs this to assert a hit actually happened
/// rather than that nothing regressed.
///
/// Not `#[cfg(test)]`: integration tests under `tests/` link against this
/// crate built the ordinary way, so a `cfg(test)` item here would not exist
/// for them to call — the same reason [`clear_cargo_hir_cache_for_test`] and
/// its neighbours below are plain `pub fn`, matching how `analyze_crate` and
/// the rest of this module's test-facing API are already exposed.
static CARGO_HIR_CACHE_HITS: Mutex<u64> = Mutex::new(0);

/// Count of calls into [`run_cargo_hir_cached`] this process has made,
/// regardless of whether they resolved via L1, L2, or a fresh compile.
/// Denominator for the L1/L2 hit rates reported in `telemetry.json`
/// (`cargo_hir_cache_attempts`) — see `evaluation_plan.md` §6.1.
static CARGO_HIR_CACHE_ATTEMPTS: Mutex<u64> = Mutex::new(0);

/// Cross-process companion to [`CARGO_HIR_CACHE`]. The in-process cache only
/// helps one `main` invocation; this lets every process analysing a crate
/// that pulls in a dependency at the same pinned version reuse a compile
/// another process already paid for — the common case for the Substrate/
/// Polkadot-SDK family, where `frame-support`/`sp-*` are pulled in by
/// hundreds of different roots at an identical version (measured directly:
/// 205 different roots share a byte-identical `frame-support-40.1.0`
/// manifest, since the tool never rewrites a shared dependency's own
/// `Cargo.toml` — only the root's).
///
/// One file per key under [`persistent_cache_dir`], named by a hash of the
/// key plus [`plugin_version_stamp`] — a rebuild of `cargo-hir`/`hir-driver`
/// makes every old entry an unreachable filename rather than silently
/// serving a stale answer ([[db-cache-invalidates-verification]] is exactly
/// this failure mode for `db.bin`). Written by [`write_persistent_cache_entry`]
/// via `fs::rename` from a per-process temp file, atomic on the same
/// filesystem: a reader only ever opens the final name, so it sees either
/// nothing or a complete value, never a torn write — no reader-side lock.
///
/// The *writer* side does use a lock ([`persistent_cache_claim_and_compute`],
/// `<key>.lock`, advisory via `File::try_lock`/`lock`), but only to avoid two
/// processes compiling the same cold key at the same time — never for
/// correctness, since same key means equivalent output either way. A process
/// that loses the race waits on the lock, then re-checks the final file (the
/// winner will have written it by the time it releases); a process that
/// waits past [`persistent_cache_wait_timeout`] gives up on reuse and just
/// compiles the key itself, so one stuck compile (the "spo-rhai" shape —
/// 6h+ once a crate's std-off configs stop being trivially UNSAT) can never
/// wedge every other process wanting the same dependency.
///
/// No eviction. The population this targets is a bounded, slow-growing set
/// of shared dependency versions, not one entry per analysed root, so
/// unbounded growth is a smaller risk here than it would be elsewhere —
/// revisit if disk usage becomes a real problem.
///
/// A `cargo test` run that does not set
/// `NO_STD_TOOL_TEST_CARGO_HIR_CACHE_DIR` writes its fixtures' tiny entries
/// into this same real directory — there is no cheap, reliable "am I running
/// under `cargo test`" signal available here, since this code is compiled
/// once as part of the library and shared by every consumer (`main`,
/// `cargo-hir`, `hir-driver`, and every test binary alike), so
/// `env!("CARGO_TARGET_TMPDIR")` (only ever set for the *test binary's own*
/// compilation, not its library dependency's) is not visible from this file.
/// Harmless — content-addressed keys mean a stray fixture entry cannot
/// answer a real corpus crate's query — and safe to clear at any time
/// (`rm -rf`, same as clearing any other cache): the whole point of this
/// cache is that every entry is reconstructible from a real compile.
static CARGO_HIR_PERSISTENT_CACHE_HITS: Mutex<u64> = Mutex::new(0);

/// Test-only: how many `(key, this process wrote nothing, another process's
/// entry answered)` outcomes have been served since the process started.
/// Distinct from [`CARGO_HIR_CACHE_HITS`], which counts *in-process* hits —
/// a cross-process hit also populates the in-process cache so a third
/// identical call in the same process is an L1 hit, not another L2 one.
static CARGO_HIR_PERSISTENT_CACHE_WRITES: Mutex<u64> = Mutex::new(0);

/// How long a process waits on another's lock for the same cold key before
/// giving up and computing independently. Overridable for tests
/// (`NO_STD_TOOL_TEST_PERSISTENT_CACHE_TIMEOUT_MS`) so a guard for the
/// give-up path does not need to actually wait 45 minutes.
fn persistent_cache_wait_timeout() -> std::time::Duration {
    if let Ok(ms) = std::env::var("NO_STD_TOOL_TEST_PERSISTENT_CACHE_TIMEOUT_MS")
        && let Ok(ms) = ms.parse()
    {
        return std::time::Duration::from_millis(ms);
    }
    std::time::Duration::from_secs(45 * 60)
}

/// How often a waiting process re-tries the lock. Negligible next to the
/// multi-minute compiles this cache targets; kept short mainly so tests
/// using a shortened [`persistent_cache_wait_timeout`] still get a couple of
/// polls in before giving up.
const PERSISTENT_CACHE_POLL_INTERVAL: std::time::Duration = std::time::Duration::from_millis(500);

/// Root directory for the persistent cache. Overridable
/// (`NO_STD_TOOL_TEST_CARGO_HIR_CACHE_DIR`) so tests never touch the real
/// shared directory or collide with a production run on this box.
fn persistent_cache_dir() -> PathBuf {
    PathBuf::from(
        std::env::var("NO_STD_TOOL_TEST_CARGO_HIR_CACHE_DIR")
            .unwrap_or_else(|_| consts::CARGO_HIR_CACHE_DIR.to_string()),
    )
}

/// The two binaries whose staleness would make a cached answer wrong. Real
/// path via `which` (matches [`is_cargo_hir_installed`]); overridable
/// (`NO_STD_TOOL_TEST_PLUGIN_BIN_DIR`) so a test can simulate "the plugin was
/// rebuilt" by touching two files it controls, instead of the real installed
/// `cargo-hir`/`hir-driver` this process is actually running.
fn plugin_binary_paths() -> Option<(PathBuf, PathBuf)> {
    if let Ok(dir) = std::env::var("NO_STD_TOOL_TEST_PLUGIN_BIN_DIR") {
        return Some((Path::new(&dir).join("cargo-hir"), Path::new(&dir).join("hir-driver")));
    }
    Some((which("cargo-hir").ok()?, which("hir-driver").ok()?))
}

/// A stamp that changes whenever `cargo-hir`/`hir-driver` do, so a disk cache
/// entry from a since-rebuilt plugin becomes an unreachable filename instead
/// of being served silently. Cheap on purpose (size + mtime, not file
/// contents): called on every persistent-cache lookup, not cached across
/// them, so a rebuild mid-run (e.g. `cargo install ... --force` from another
/// terminal) is picked up by the very next call rather than needing this
/// process restarted. `None` if either binary cannot be found or stat'd —
/// the caller treats that as "persistent cache unavailable this call",
/// exactly like [`hash_manifest`] returning `None` for an unreadable
/// manifest.
fn plugin_version_stamp() -> Option<u64> {
    use std::hash::{Hash, Hasher};
    let (hir_bin, driver_bin) = plugin_binary_paths()?;
    let mut h = std::collections::hash_map::DefaultHasher::new();
    for bin in [&hir_bin, &driver_bin] {
        let meta = fs::metadata(bin).ok()?;
        meta.len().hash(&mut h);
        meta.modified().ok()?.hash(&mut h);
    }
    Some(h.finish())
}

/// The final cache-entry path and its companion lock path for a key, or
/// `None` if [`plugin_version_stamp`] is unavailable — in which case the
/// caller skips the persistent cache entirely for this call rather than
/// caching under a key that cannot distinguish plugin versions.
fn persistent_cache_paths(key: &CargoHirCacheKey) -> Option<(PathBuf, PathBuf)> {
    use std::hash::{Hash, Hasher};
    let stamp = plugin_version_stamp()?;
    let mut h = std::collections::hash_map::DefaultHasher::new();
    key.hash(&mut h);
    stamp.hash(&mut h);
    let name = format!("{:016x}", h.finish());
    let dir = persistent_cache_dir();
    Some((dir.join(&name), dir.join(format!("{name}.lock"))))
}

/// Read a persisted entry. Any failure — missing file, truncated/corrupt
/// content, a decode error from a format this binary no longer writes —
/// is treated as a miss, never propagated: a persistent cache must never be
/// able to fail a run, only fail to help it. Safe against a concurrent
/// writer by construction: [`write_persistent_cache_entry`] only ever makes
/// this path exist via an atomic rename, so an open here sees a complete
/// file or none at all.
fn read_persistent_cache_entry(path: &Path) -> Option<CargoHirCacheValue> {
    let bytes = fs::read(path).ok()?;
    bincode::decode_from_slice(&bytes, bincode::config::standard())
        .ok()
        .map(|(v, _)| v)
}

/// Write a persisted entry via temp-file-then-rename. Best-effort: a write
/// failure (read-only filesystem, directory creation race, disk full) just
/// means the next process to want this key pays the compile again — the
/// same degradation as never having reached the persistent cache at all.
fn write_persistent_cache_entry(path: &Path, value: &CargoHirCacheValue) {
    let Ok(bytes) = bincode::encode_to_vec(value, bincode::config::standard()) else {
        return;
    };
    let dir = path.parent().unwrap_or_else(|| Path::new("."));
    if fs::create_dir_all(dir).is_err() {
        return;
    }
    let Some(file_name) = path.file_name() else {
        return;
    };
    let tmp = path.with_file_name(format!(
        "{}.tmp.{}",
        file_name.to_string_lossy(),
        std::process::id()
    ));
    if fs::write(&tmp, &bytes).is_err() {
        return;
    }
    let _ = fs::rename(&tmp, path);
}

/// Run the actual `cargo` subprocess with no caching involved — the shared
/// tail every cache path (in-process miss, persistent-cache miss, persistent
/// cache unavailable) eventually calls. `Err` is a spawn failure, which must
/// never be cached under any key: it is this process's own infrastructure
/// problem (e.g. `cargo` not on `PATH` for this invocation), not an answer
/// about the key.
fn compile_cargo_hir_uncached(args: &[&str], output_path: &Path) -> Result<CargoHirCacheValue, String> {
    let output = Command::new("cargo")
        .args(args)
        .env(PLUGIN_OUTPUT_ENV, output_path)
        .output()
        .map_err(|e| format!("failed to spawn cargo: {}", e))?;
    let success = output.status.success();
    let wrote_json = success && output_path.exists();
    let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
    let exit_code = output.status.code();
    Ok(if wrote_json {
        match fs::read(output_path) {
            Ok(bytes) => CargoHirCacheValue::Succeeded { output_json: bytes },
            Err(_) => CargoHirCacheValue::SucceededNoOutput,
        }
    } else if success {
        CargoHirCacheValue::SucceededNoOutput
    } else {
        CargoHirCacheValue::Failed { stderr, exit_code }
    })
}

/// Turn a cached value (from either L1 or L2) into the `CargoHirAttempt` the
/// caller expects, restoring `output_path` exactly as a fresh run would have
/// written it. Shared by every hit path so a hit is indistinguishable from a
/// fresh compile to everything above this function.
fn cache_value_to_attempt(value: CargoHirCacheValue, output_path: &Path) -> CargoHirAttempt {
    match value {
        CargoHirCacheValue::Succeeded { output_json } => {
            let _ = fs::write(output_path, &output_json);
            CargoHirAttempt::Compiled { wrote_json: true }
        }
        CargoHirCacheValue::SucceededNoOutput => CargoHirAttempt::Compiled { wrote_json: false },
        CargoHirCacheValue::Failed { stderr, exit_code } => {
            CargoHirAttempt::CompileFailed { stderr, exit_code }
        }
    }
}

/// Cold path for a key with nothing on disk yet: become the writer if
/// nothing else is already computing this key, or wait for whoever is.
///
/// The lock is purely a "don't do redundant work" optimisation, never a
/// safety requirement — [`write_persistent_cache_entry`]'s atomic rename is
/// what makes concurrent writers safe. So on any infrastructure failure
/// around the lock itself (can't open/create the lock file), this degrades
/// straight to an uncached compile rather than treating it as an error.
fn persistent_cache_claim_and_compute(
    final_path: &Path,
    lock_path: &Path,
    args: &[&str],
    output_path: &Path,
) -> Result<CargoHirCacheValue, String> {
    if let Some(dir) = lock_path.parent() {
        let _ = fs::create_dir_all(dir);
    }
    let Ok(lock_file) = fs::OpenOptions::new()
        .create(true)
        .truncate(false)
        .write(true)
        .open(lock_path)
    else {
        return compile_cargo_hir_uncached(args, output_path);
    };

    let deadline = std::time::Instant::now() + persistent_cache_wait_timeout();
    loop {
        match lock_file.try_lock() {
            Ok(()) => break,
            Err(std::fs::TryLockError::WouldBlock) => {
                if std::time::Instant::now() >= deadline {
                    // Gave up waiting on whoever holds it. Compute
                    // independently rather than block forever on one stuck
                    // compile — the "spo-rhai" shape this exists to survive.
                    return compile_cargo_hir_uncached(args, output_path);
                }
                std::thread::sleep(PERSISTENT_CACHE_POLL_INTERVAL);
            }
            Err(std::fs::TryLockError::Error(_)) => {
                // Locking unsupported/broken on this filesystem — degrade,
                // same as failing to open the lock file at all.
                return compile_cargo_hir_uncached(args, output_path);
            }
        }
    }

    // Holding the lock now, but the previous holder may have finished and
    // written the answer between the caller's miss check and this line.
    if let Some(value) = read_persistent_cache_entry(final_path) {
        return Ok(value);
    }

    let value = compile_cargo_hir_uncached(args, output_path)?;
    write_persistent_cache_entry(final_path, &value);
    *CARGO_HIR_PERSISTENT_CACHE_WRITES.lock().unwrap() += 1;
    // `lock_file` drops here, releasing the flock. The lock file itself is
    // left on disk deliberately — removing it while another process might
    // still have it open would let a third process's fresh open/lock race
    // against the old inode's lingering lock, exactly the unlink-vs-flock
    // hazard this design avoids by not touching it after creation.
    Ok(value)
}

/// One `cargo hir` attempt, cached. Scoped to exactly the subprocess call —
/// every caller keeps its own target-selection, retry loop and side-effect
/// bookkeeping (`LAST_GOOD_TARGET` and friends) untouched, so a cache hit
/// cannot skip any of the reasoning those callers do, only the compile
/// itself, which `CargoHirCacheKey` is built to be a pure function of.
enum CargoHirAttempt {
    /// Cargo exited 0. `wrote_json` is false for the plugin-succeeded-but-no-
    /// output-file case, which callers treat as an infrastructure fault.
    Compiled { wrote_json: bool },
    /// Cargo ran and exited non-zero — an ordinary "this target/feature set
    /// does not build" result, not a tool fault.
    CompileFailed {
        stderr: String,
        exit_code: Option<i32>,
    },
    /// The `cargo` process could not even be spawned. Distinguished from
    /// `CompileFailed` because every caller treats this as immediately fatal
    /// rather than "try the next target."
    SpawnFailed(String),
}

/// Run `cargo hir` once, or reuse a prior identical result within this
/// process. `args` must already carry every flag that affects the compile
/// (`--features`, `--target`, `--lib`, …); `feats`/`target`/`has_lib` are
/// passed separately only to build the cache key and the debug log line.
fn run_cargo_hir_cached(
    manifest: &str,
    args: &[&str],
    output_path: &Path,
    crate_name: &str,
    feats: &str,
    target: Option<&'static str>,
    has_lib: bool,
) -> CargoHirAttempt {
    *CARGO_HIR_CACHE_ATTEMPTS.lock().unwrap() += 1;
    let key = hash_manifest(manifest).map(|manifest_hash| {
        let mut sorted: Vec<&str> = feats.split(',').filter(|s| !s.is_empty()).collect();
        sorted.sort_unstable();
        CargoHirCacheKey {
            manifest_hash,
            feats_sorted: sorted.join(","),
            target,
            has_lib,
        }
    });

    // §3.8 (`--no-local-cache`): skip the L1 lookup so this call always falls
    // through to L2/compile, as if L1 never has anything in it.
    if !ablation::flags().no_local_cache
        && let Some(k) = &key
        && let Some(cached) = CARGO_HIR_CACHE.lock().unwrap().get(k).cloned()
    {
        debug!(
            "cargo hir cache hit (in-process) for {} on target [{}] (features [{}])",
            crate_name,
            target.unwrap_or("host"),
            feats
        );
        *CARGO_HIR_CACHE_HITS.lock().unwrap() += 1;
        return cache_value_to_attempt(cached, output_path);
    }

    // L2: the cross-process cache. A key that could not be built at all
    // (unreadable manifest) never reaches here — same as L1 above.
    //
    // §3.9 (`--no-global-cache`): skip this whole block on a declared miss so
    // the call falls straight to the uncached-compile branch below, never
    // reading or writing the on-disk cache at all.
    if !ablation::flags().no_global_cache
        && let Some(k) = &key
        && let Some((final_path, lock_path)) = persistent_cache_paths(k)
    {
        if let Some(value) = read_persistent_cache_entry(&final_path) {
            debug!(
                "cargo hir cache hit (cross-process) for {} on target [{}] (features [{}])",
                crate_name,
                target.unwrap_or("host"),
                feats
            );
            *CARGO_HIR_PERSISTENT_CACHE_HITS.lock().unwrap() += 1;
            if !ablation::flags().no_local_cache {
                CARGO_HIR_CACHE.lock().unwrap().insert(k.clone(), value.clone());
            }
            return cache_value_to_attempt(value, output_path);
        }

        return match persistent_cache_claim_and_compute(&final_path, &lock_path, args, output_path)
        {
            Ok(value) => {
                if !ablation::flags().no_local_cache {
                    CARGO_HIR_CACHE.lock().unwrap().insert(k.clone(), value.clone());
                }
                cache_value_to_attempt(value, output_path)
            }
            Err(e) => CargoHirAttempt::SpawnFailed(e),
        };
    }

    // No key, or the persistent cache is unavailable this call (plugin
    // binaries not found/stat-able), or `--no-global-cache` forced this path
    // — same behaviour as before this cache existed: compile once, cache
    // in-process only if there is a key at all and `--no-local-cache` isn't
    // also set.
    match compile_cargo_hir_uncached(args, output_path) {
        Ok(value) => {
            if !ablation::flags().no_local_cache
                && let Some(k) = key
            {
                CARGO_HIR_CACHE.lock().unwrap().insert(k, value.clone());
            }
            cache_value_to_attempt(value, output_path)
        }
        Err(e) => CargoHirAttempt::SpawnFailed(e),
    }
}

/// Test-only: forget every cached `cargo hir` result. `cargo test` runs many
/// tests in one process against this one `static`, so a test asserting on
/// hit/miss behaviour needs a clean slate regardless of what ran before it.
pub fn clear_cargo_hir_cache_for_test() {
    CARGO_HIR_CACHE.lock().unwrap().clear();
    *CARGO_HIR_CACHE_HITS.lock().unwrap() = 0;
    *CARGO_HIR_CACHE_ATTEMPTS.lock().unwrap() = 0;
    *CARGO_HIR_PERSISTENT_CACHE_HITS.lock().unwrap() = 0;
    *CARGO_HIR_PERSISTENT_CACHE_WRITES.lock().unwrap() = 0;
}

/// Test-only: clear only the in-process (L1) cache, leaving L2 counters and
/// whatever is on disk untouched. A cross-process test primes L2 with one
/// call, then needs *this* — not the full [`clear_cargo_hir_cache_for_test`]
/// — before its second call, so the second call is forced past L1 and
/// exercises the disk path instead of just hitting the L1 entry the first
/// call already left behind.
pub fn clear_in_process_cargo_hir_cache_for_test() {
    CARGO_HIR_CACHE.lock().unwrap().clear();
}

/// Test-only: how many `(key, another process's disk entry answered)` hits
/// this process has served since the last [`clear_cargo_hir_cache_for_test`].
pub fn cargo_hir_persistent_cache_hits_for_test() -> u64 {
    *CARGO_HIR_PERSISTENT_CACHE_HITS.lock().unwrap()
}

/// Test-only: how many times this process actually won the write race and
/// ran a real compile for a cold persistent-cache key (as opposed to losing
/// the lock race and reading back the winner's entry). The concurrency
/// guard's whole point is asserting this stays at 1 across N racing callers
/// for the same key, not N.
pub fn cargo_hir_persistent_cache_writes_for_test() -> u64 {
    *CARGO_HIR_PERSISTENT_CACHE_WRITES.lock().unwrap()
}

/// Test-only: how many cached `cargo hir` results this process is holding.
/// Lets a test assert a second identical call did not grow the cache (it hit
/// the first entry) without depending on timing.
pub fn cargo_hir_cache_len_for_test() -> usize {
    CARGO_HIR_CACHE.lock().unwrap().len()
}

/// Test-only: how many cache hits this process has served since the last
/// [`clear_cargo_hir_cache_for_test`]. Cache size alone cannot tell "hit"
/// apart from "never tried to cache" — both leave the map the same size — so
/// a guard needs this to assert a hit actually happened.
pub fn cargo_hir_cache_hits_for_test() -> u64 {
    *CARGO_HIR_CACHE_HITS.lock().unwrap()
}

/// Production accessor for `telemetry.json`: total calls into
/// [`run_cargo_hir_cached`] this process has made (the denominator for the
/// L1/L2 hit rates below). Unlike the `_for_test` accessors above, this one
/// is read once at dump time by `Stats::dump`, not by a test guard.
pub fn cargo_hir_cache_attempts() -> u64 {
    *CARGO_HIR_CACHE_ATTEMPTS.lock().unwrap()
}

/// Production accessor for `telemetry.json`: in-process (L1) `cargo hir`
/// cache hits this process has served. Same counter as
/// [`cargo_hir_cache_hits_for_test`]; this name is the one `Stats::dump`
/// calls so the "test-only" doc comment above stays accurate for its caller.
pub fn cargo_hir_l1_cache_hits() -> u64 {
    *CARGO_HIR_CACHE_HITS.lock().unwrap()
}

/// Production accessor for `telemetry.json`: cross-process (L2) `cargo hir`
/// cache hits this process has served. Same counter as
/// [`cargo_hir_persistent_cache_hits_for_test`]; this name is the one
/// `Stats::dump` calls.
pub fn cargo_hir_l2_cache_hits() -> u64 {
    *CARGO_HIR_PERSISTENT_CACHE_HITS.lock().unwrap()
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
    //   * else the last triple that linked in this process (`LAST_GOOD_TARGET`),
    //     so the covering-set/CEGAR runs don't re-scan all 26 every call — a hint
    //     that outlives the crate that produced it, and may be a dependency's;
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
    // Did any bare-metal attempt get as far as compiling *this* crate? A failure
    // inside a dependency stops cargo before the main crate is ever built, so a
    // host fallback after one of those says nothing about this crate's own std
    // usage — see `PassOutcome::Success::std_inconclusive`.
    let mut bare_metal_reached_crate = false;

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
        let cargo_hir_result = run_cargo_hir_cached(
            manifest,
            &args,
            &output_path,
            crate_name,
            &feats,
            target,
            has_lib,
        );
        let (success, wrote_json, last_stderr_this, last_exit_this) = match cargo_hir_result {
            CargoHirAttempt::SpawnFailed(e) => {
                attempt.meta("success", "false");
                attempt.meta("outcome", "spawn_failed");
                return PassOutcome::CompileFailed {
                    stderr: e,
                    exit_code: None,
                };
            }
            CargoHirAttempt::Compiled { wrote_json } => (true, wrote_json, String::new(), None),
            CargoHirAttempt::CompileFailed { stderr, exit_code } => {
                (false, false, stderr, exit_code)
            }
        };
        attempt.meta("success", success.to_string());
        drop(attempt);

        if success {
            if wrote_json {
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

        last_stderr = last_stderr_this;
        last_exit = last_exit_this;
        if target.is_some() && compile_failure_names_crate(&last_stderr, crate_name) {
            bare_metal_reached_crate = true;
        }
        debug!(
            "cargo hir failed for {} on target [{}] (exit {}): {}",
            crate_name,
            target.unwrap_or("host"),
            last_exit.unwrap_or(-1),
            first_error_line(&last_stderr)
        );
    }

    let Some(succeeded_target) = succeeded_on else {
        let _ = fs::remove_file(&output_path);
        return PassOutcome::CompileFailed {
            stderr: last_stderr,
            exit_code: last_exit,
        };
    };

    // Two ways a host-only run fails to be a no_std environment, and the second
    // is invisible to the first. `bare_metal_reached_crate` asks which package
    // cargo gave up on, which is the right question when the crate is `#![no_std]`
    // everywhere: a failure inside a dependency means the crate was never
    // compiled, a failure in the crate itself means it was. For a crate whose
    // `#![no_std]` hangs off a target predicate the host does not satisfy, that
    // question is the wrong one — every bare-metal build fails *in the crate*,
    // with `can't find crate for std`, precisely because the attribute does not
    // apply there either, so the discriminator says "reached" and the host run
    // keeps its authority. But on the host the crate is not no_std at all, and
    // "this compiles with std" was never in doubt. cuda_std 0.2.2 is the case:
    // 58 spans, all of them `f64::…` inherent methods that the crate's own
    // `f32_intrinsic!` replaces with `intrinsics::…` under `target_os = "cuda"`.
    let host_not_no_std = *HOST_NOT_NO_STD.lock().unwrap();
    let std_inconclusive =
        succeeded_target.is_none() && (!bare_metal_reached_crate || host_not_no_std);

    match succeeded_target {
        Some(t) => {
            *LAST_GOOD_TARGET.lock().unwrap() = Some(t);
            // The per-crate half. `succeeded_target` is `Some` only for a real
            // `--target` build, so this says exactly what `discover_build_enablers`
            // needs to know: *this* crate has been compiled for bare metal at least
            // once, and a search for the feature it cannot build without would be
            // searching for something that does not exist.
            *CRATE_REACHED_BARE_METAL.lock().unwrap() = true;
            // …and the set it compiled with, which is the half the bool throws
            // away (KI-30). `enable` is exactly what went on `--features`.
            CRATE_BARE_METAL_SETS
                .lock()
                .unwrap()
                .push(enable.to_vec());
            debug!("cargo hir succeeded for {} on target {}", crate_name, t);
        }
        None if std_inconclusive && host_not_no_std => debug!(
            "cargo hir succeeded for {} on host (no bare-metal target compiled, and this \
             crate's `#![no_std]` does not apply to the host — std records inconclusive)",
            crate_name
        ),
        None if std_inconclusive => debug!(
            "cargo hir succeeded for {} on host (no bare-metal target compiled, and every \
             bare-metal attempt died inside a dependency — std records inconclusive)",
            crate_name
        ),
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

    if !ablation::flags().no_gateway_resolution {
        resolve_local_facade_gateways(&mut full_output);
    }
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
        std_inconclusive,
    }
}

/// Set [`HOST_NOT_NO_STD`] for the crate whose coverage phase is starting, and
/// record what was seen.
///
/// Always assigns, so the previous crate's answer cannot leak into this one —
/// dependencies are analysed in the same process and go through the same phase.
/// A predicate that cannot be decided leaves the flag false, which is the
/// pre-existing behaviour: the rule is not applied rather than applied on a
/// guess. That covers a rustc this environment cannot run, and — the case that
/// matters — a predicate naming an atom rustc does not derive from the target
/// (`target_cfg::is_decidable`): a build script's `cargo:rustc-cfg=rustc_1_6`
/// is absent from `--print cfg` yet true in the build that actually happens.
fn set_host_no_std_applicability(pred: Option<&target_cfg::CfgPred>, telemetry: &mut Telemetry) {
    let host_not_no_std = match pred {
        Some(p) => target_cfg::holds_for(p, None) == Some(false),
        None => false,
    };
    *HOST_NOT_NO_STD.lock().unwrap() = host_not_no_std;

    let Some(p) = pred else { return };
    if !target_cfg::is_decidable(p) {
        // `not(test)` and friends land here, and nothing is recorded: an empty
        // target list would read as "no target makes this crate no_std", which
        // is the opposite of the truth for a predicate that is simply not the
        // target's to answer.
        debug!("`#![no_std]` is conditional on `{p}`, which no target decides; rule not applied");
        return;
    }
    let targets = target_cfg::supported_no_std_targets(p);
    debug!(
        "`#![no_std]` is conditional on the target ({p}): holds on the host = {}, and on \
         {} of the {} targets in TARGET_LIST{}",
        !host_not_no_std,
        targets.len(),
        consts::TARGET_LIST.len(),
        if targets.is_empty() {
            " — no target this tool builds makes this crate no_std".to_string()
        } else {
            format!(" ({})", targets.join(", "))
        }
    );
    // Telemetry is shared with every dependency analysed after the main crate,
    // so record only the first (main-crate) sighting — the same reason
    // `std_inconclusive_runs` is kept as a high-water mark.
    if telemetry.no_std_cfg_predicate.is_none() {
        telemetry.no_std_cfg_predicate = Some(p.to_string());
        telemetry.no_std_predicate_targets = targets.iter().map(|t| t.to_string()).collect();
    }
}

/// The first line of a failing build's stderr that says what went wrong, for the
/// log line that reports the failure.
///
/// A pass that fails on every target and the host logs 27 identical "cargo hir
/// failed (exit 101)" lines and no reason, which is the state mtxgroup 0.1.1 was
/// triaged in: the run looked like a target problem and was a `compile_error!`
/// firing in the crate. Cargo's own summary line (`error: could not compile …`)
/// is last and names no cause, so the first `error` line is the one to keep.
pub fn first_error_line(stderr: &str) -> &str {
    stderr
        .lines()
        .map(str::trim)
        .find(|line| line.starts_with("error"))
        .unwrap_or("<no error line>")
}

/// Did this failing build get as far as compiling `crate_name` itself?
///
/// Cargo reports the package it gave up on — ``error: could not compile `core2`
/// (lib) due to 4 previous errors`` — and stops there, so a line naming this
/// crate is proof the crate was reached and a line naming something else is
/// proof it was not. A build-script panic or a resolver error names nothing at
/// all, which is also "not reached".
///
/// `crate_name` arrives as `name:version`; cargo prints the package name with
/// dashes, while the rest of the pipeline uses either spelling, so both are
/// normalised before comparing.
pub fn compile_failure_names_crate(stderr: &str, crate_name: &str) -> bool {
    let name = crate_name.split(':').next().unwrap_or(crate_name);
    let norm = name.replace('-', "_");
    stderr.lines().filter_map(compiled_package).any(|pkg| {
        let pkg = pkg.replace('-', "_");
        pkg == norm
    })
}

/// Did the crate spell `std` itself anywhere in this path?
///
/// `std::marker::PhantomData` and `crate::std::error::Error` (a root facade) did;
/// `io::Result` and `Write::write_all` did not — those name a *local binding*,
/// and which crate it resolves to is decided by the dependency graph. On a run
/// that never left the host that distinction is the whole question: the shim
/// under `io` was built with its own default `std` feature, so it answers `std`
/// no matter what this crate asks for.
///
/// A path a *dependency's macro* wrote is not the crate's either, however
/// std-looking the text reads — **O-16**. bp-wococo 0.3.0's whole source at the
/// reported span is `decl_bridge_finality_runtime_apis!(wococo, grandpa);`, and
/// the record there is `std::result::Result` with
/// `expansion_crate: Some("bp_runtime")`: the compiler saying another crate put
/// this here, which is the same thing the host-only rule already says about a
/// shim's re-export. `expansion_crate` is `None` for the crate's own source *and*
/// for its own macros (`expansion_def_crate` reports nothing for `LOCAL_CRATE`),
/// so it is exactly the "this crate wrote it" test.
///
/// No carve-out for the sysroot's own macros, deliberately: a `println!` records
/// `$crate::io::_print`, which has no `std` segment and so never reaches the
/// expansion check. Measured over every probe candidate in the corpus logs — 83
/// records with `expansion_crate: Some("std")` and 40 with `Some("core")`, and
/// **not one** of them names `std` in its path text.
pub fn crate_named_std_in_path(record: &PathRecord) -> bool {
    record.expansion_crate.is_none() && record.path_text.split("::").any(|seg| seg.trim() == "std")
}

/// Is a host-only-run's "still std" for this item safe to treat as a
/// don't-care, rather than as unproven-and-therefore-blocking (R34-3)?
///
/// Two classes say yes, and both are read off what the compiler resolved —
/// never off `path_text`, which is only ever a rendering:
///
/// * **`definition_crate` is `core` or `alloc`.** The record is std only
///   because the *path* went through std's re-export facade; the item itself
///   is defined where `*-none` has it too.
/// * **`definition_crate` is `std` and the record is a primitive float's own
///   inherent method** (`is_float_primitive_method`, set in the plugin from
///   `self_ty.kind() == ty::Float(_)`, not from matching `f32`/`f64` text).
///   This is the transcendental-math set (`sqrt`, `sin`, `powi`, …) that needs
///   a libm binding — real on this toolchain, confirmed by hand against
///   `tuit 0.2.1`: with std off, `((..) as f32).sqrt()` fails
///   `E0599: no method named 'sqrt' found for type 'f32'`, not a resolution
///   into `core`.
///
/// Everything else — `HashMap`, `io::Write`, `thread::LocalKey`, `Instant`,
/// `File`, `process::Command` — is genuinely std-only on every target there
/// is, and treating *that* as a don't-care would report a std crate as
/// no_std on the absence of evidence ([[probe-code-deletion-fallacy]]). Those
/// stay unproven and keep blocking, with the reason already printed.
pub fn host_only_downgrade_is_safe(record: &PathRecord) -> bool {
    matches!(record.definition_crate.as_str(), "core" | "alloc")
        || (record.definition_crate == "std" && record.is_float_primitive_method)
}

/// The package name in a ``could not compile `X` `` line, if the line is one.
fn compiled_package(line: &str) -> Option<&str> {
    let rest = line.trim().strip_prefix("error: could not compile ")?;
    let rest = rest.strip_prefix('`')?;
    rest.split('`').next()
}

/// Parse a crate's manifest, or an empty table when it cannot be read.
pub fn read_manifest_toml(manifest: &str) -> toml::Value {
    fs::read_to_string(manifest)
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(toml::Value::Table(toml::map::Map::new()))
}

/// What a *dependency's* `compile_error!` requires of **this** crate's features
/// — O-12(e).
///
/// O-2 gave a crate's own `compile_error!` to the solve that picks its covering
/// runs, because a run that ignores it dies on the macro and contributes no
/// records. The same thing happens one level down and nothing carried it: a
/// dependency's `compile_error!` fails this crate's build just as hard, and this
/// crate's solve has never seen it.
///
/// peniko 0.3.1 declares none of its own (`Compile-error constraints: []`,
/// correctly), so its baseline solved to `[]` and died on
///
/// ```text
/// error: color requires either the `std` or `libm` feature
/// error: kurbo requires either the `std` or `libm` feature
/// ```
///
/// leaving it with no std-off run at all. Its *emitted* config is
/// `--features libm` and builds on 14 targets — the dependency walk gets there —
/// so what was lost was only evidence, which is the bottleneck this whole
/// section is about. 87 crates in the corpus have a baseline killed by a
/// dependency's `compile_error!`.
///
/// The translation is per atom, and it is the only interesting part. A
/// constraint over the *dependency's* feature names says nothing until each is
/// re-expressed in this crate's:
///
/// * on unconditionally — the edge does not set `default-features = false` and
///   the feature is in the dependency's `default` closure, or the edge names it
///   in `features = [...]`. Cargo enables it no matter what this crate's own
///   features do, so the atom is `true`.
/// * otherwise — the disjunction of this crate's features that reach
///   `<dep>/<feat>`, transitively through its own `[features]` table. `libm` is
///   how peniko reaches `color/libm` and `kurbo/libm`.
/// * nothing reaches it — the constraint is **dropped**. See the comment at that
///   branch: `false` is the tempting reading and the dangerous one.
///
/// The substitution equates `<dep>/<feat>` with the disjunction, which is
/// stronger than the truth (`(f1 ∨ …) ⇒ <dep>/<feat>`, not the converse — the
/// dependency walk can also supply it). That is deliberate: what it forces on is
/// a feature of this crate that exists for exactly this purpose, and it is what
/// the emitted config ends up doing anyway. peniko's is `--features libm`.
///
/// Z3 name collisions are why substitution is required rather than asserting the
/// dependency's formula directly: color's `libm` and peniko's `libm` are the
/// same `Bool::new_const` name and *not* the same feature. Here they happen to
/// coincide; `std` routinely does not.
///
/// Optional dependencies are skipped. Their constraint holds only in the
/// configurations that link them, and asserting it unconditionally would impose
/// a dependency's requirement on builds that never compile it — the same reason
/// `optional_dep_link_constraints` guards its own edges.
///
/// `compile_error_constraint` is reused for each attribute, so O-1's rule
/// carries over unchanged: a cfg naming an atom policy G erases yields no
/// constraint, because its negation admits no truth value.
pub fn dependency_compile_error_constraints<'a>(
    ctx: &'a Context,
    manifest: &str,
    manifest_toml: &toml::Value,
) -> Vec<Bool<'a>> {
    let mut constraints = Vec::new();
    let optional = downloader::optional_deps_in_manifest(manifest_toml);

    for (dep_key, dep_value) in parser::dependency_edges(manifest_toml) {
        let package = dep_value
            .get("package")
            .and_then(|p| p.as_str())
            .unwrap_or(&dep_key)
            .to_string();
        if optional.contains(&dep_key) || optional.contains(&package) {
            continue;
        }
        let Some(dep_dir) = parser::find_sibling_crate_dir(manifest, &package) else {
            continue;
        };
        let files = compile_error_source_files(&dep_dir);
        if files.is_empty() {
            continue;
        }

        let dep_attrs = parser::parse_crate(&package, false, None, &[], Some(&files));
        if dep_attrs.compile_error_attrs.is_empty() {
            continue;
        }
        let dep_toml = read_manifest_toml(&dep_dir.join("Cargo.toml").display().to_string());
        let always_on = parser::edge_features_the_walk_will_leave_on(&dep_value, &dep_toml);

        for attr in dep_attrs.compile_error_attrs.iter() {
            let Some(eq) = parser::compile_error_constraint(attr, ctx, None) else {
                continue;
            };
            let (_, parsed) = parser::parse_main_attributes_direct(attr, ctx);
            let mut substitutions = Vec::new();
            let mut unreachable_atom = false;
            for feat in &parsed.features {
                let from = Bool::new_const(ctx, feat.as_str());
                let to = if always_on.contains(feat) {
                    Bool::from_bool(ctx, true)
                } else {
                    let enablers =
                        parser::local_features_enabling_dep_feature(manifest_toml, &dep_key, feat);
                    if enablers.is_empty() {
                        // No feature of this crate reaches it, so it is not a way
                        // *this* crate can answer the constraint — `false` is the
                        // only reading available here. What that reading must not
                        // do is turn the whole constraint unsatisfiable: the
                        // dependency walk rewrites dependency manifests and can
                        // turn such a feature on with no feature path from here
                        // at all (the asymmetry that made O-12(b)'s sapling entry
                        // wrong for a week), and an unsat `all_hard` costs the
                        // crate every covering run — no baseline, no solved sets,
                        // every span `AlwaysStd`. So it is substituted, and the
                        // result is kept only if it is still satisfiable.
                        unreachable_atom = true;
                        Bool::from_bool(ctx, false)
                    } else {
                        let vars: Vec<Bool> = enablers
                            .iter()
                            .map(|f| Bool::new_const(ctx, f.as_str()))
                            .collect();
                        Bool::or(ctx, &vars.iter().collect::<Vec<_>>())
                    }
                };
                substitutions.push((from, to));
            }
            let pairs: Vec<(&Bool, &Bool)> = substitutions.iter().map(|(f, t)| (f, t)).collect();
            let translated = eq.substitute(&pairs).simplify();
            if translated == Bool::from_bool(ctx, true) {
                continue;
            }
            // One unreachable arm used to discard the constraint whole. bamboo-rs-core
            // reaches four of curve25519-dalek's five backends through ed25519-dalek and
            // not `fiat_u32_backend`, and lost "no curve25519-dalek backend cargo feature
            // enabled!" — a requirement it could have met — to the fifth.
            if unreachable_atom {
                let solver = z3::Solver::new(ctx);
                solver.assert(&translated);
                if solver.check() != z3::SatResult::Sat {
                    debug!(
                        "Dependency {}'s compile_error can only be answered by a feature this \
                         crate cannot reach; leaving its constraint out",
                        package
                    );
                    continue;
                }
            }
            debug!(
                "Dependency {}'s compile_error constrains this crate: {:?}",
                package, translated
            );
            constraints.push(translated);
        }
    }
    constraints
}

/// This crate's own `compile_error!` disjunctions name feature atoms without
/// knowing which optional dependency each one resolves to, or whether that
/// dependency can build no_std at all — KI-3.
///
/// bulletproofs-bls-4.0.0's `#[cfg(all(not(feature = "rust"), not(feature =
/// "blst")))] compile_error!("At least `rust` or `blst` must be selected")`
/// names two backends: `rust` reaches `bls12_381_plus` (no_std-capable) and
/// `blst` reaches `blstrs_plus` 0.8.18 (not). Nothing in the feature solve
/// knows the backends, so Z3 is free to satisfy the disjunction with `blst`;
/// the dependency walk then severs `blstrs_plus` as not-no_std and leaves
/// `blst` on (KI-2), and the crate dies on `E0433 … unlinked crate
/// blstrs_plus` — a different failure than the one the `compile_error!` was
/// guarding against, over a configuration the tool could have avoided by
/// preferring the other backend up front.
///
/// **Which of KI-3's three options this is — read this before changing it.**
/// The KNOWN_ISSUES.md entry lists three ways to feed a dependency's no_std
/// verdict back into the branch pick:
///   1. Defer every manifest write until the whole crate — main and every
///      dependency — has been solved once as a dry pass, then re-solve with
///      the accumulated negatives. Cleanest, biggest refactor.
///   2. Solve, write, build; on failure restore `Cargo.toml` from `.orig` and
///      retry with a growing forbidden set, bounded to a couple of
///      iterations. Cheaper than (1), still a full re-analysis per retry.
///   3. Before the main solve ever runs, cheaply check only the optional
///      dependencies that a `compile_error!` disjunction actually names, and
///      forbid the ones that fail. Narrowest; handles exactly the shape that
///      bit bulletproofs-bls.
///
/// **This function is option 3**, taken first per the entry's own
/// recommendation. It is a syntactic, no-compile check (`crate_entry_file` +
/// one crate-root parse per distinct optional dependency named this way — the
/// same cost [`dependency_compile_error_constraints`] already pays per
/// dependency), it never re-solves or retries a build, and it only ever
/// forbids a feature that already appears in one of this crate's own
/// `compile_error!` conditions — an optional dependency this crate reaches
/// with no `compile_error!` naming it is untouched. **If a real case needs
/// more than this** — a dependency whose no_std-capability itself depends on
/// features only the main solve would choose, or a disjunction that does not
/// live in a crate-root file this syntactic pass reads — options 1 or 2 are
/// the fallback the entry describes; do not stretch this function to cover
/// them, extend the KI-3 entry's own reasoning instead.
///
/// Folded into `final_condition` in [`analyze_crate`] alongside the probe
/// conditions and build enablers, one candidate at a time, kept only if the
/// running conjunction stays satisfiable — the same discipline
/// [`dependency_compile_error_constraints`]'s `unreachable_atom` branch uses,
/// so a crate whose *every* backend happens to fail this check is left
/// exactly as it was rather than handed an unsat `all_hard`.
pub fn compile_error_infeasible_backend_constraints<'a>(
    ctx: &'a Context,
    manifest: &str,
    manifest_toml: &toml::Value,
) -> Vec<Bool<'a>> {
    let Some(crate_dir) = Path::new(manifest).parent() else {
        return Vec::new();
    };
    let files = compile_error_source_files(crate_dir);
    if files.is_empty() {
        return Vec::new();
    }
    let attrs = parser::parse_crate("<self>", false, None, &[], Some(&files));
    if attrs.compile_error_attrs.is_empty() {
        return Vec::new();
    }

    let optional = downloader::optional_deps_in_manifest(manifest_toml);
    if optional.is_empty() {
        return Vec::new();
    }
    let edges: HashMap<String, toml::Value> = parser::dependency_edges(manifest_toml)
        .into_iter()
        .collect();

    // dep_key -> "this dependency's crate root carries no no_std attribute",
    // cached because the same backend is often named by more than one
    // `compile_error!` (a guard arm and a `pub use`'s own cfg, say).
    let mut checked: HashMap<String, bool> = HashMap::new();
    let mut seen_atoms: HashSet<String> = HashSet::new();
    let mut forbid = Vec::new();

    for attr in &attrs.compile_error_attrs {
        let (_, parsed) = parser::parse_main_attributes_direct(attr, ctx);
        for feat in &parsed.features {
            if !seen_atoms.insert(feat.clone()) {
                continue;
            }
            let Some(dep_key) = optional.iter().find(|dep_key| {
                parser::local_features_enabling_dep(manifest_toml, dep_key).contains(feat)
            }) else {
                continue;
            };

            let lacks_no_std = *checked.entry(dep_key.clone()).or_insert_with(|| {
                let package = edges
                    .get(dep_key)
                    .and_then(|v| v.get("package"))
                    .and_then(|p| p.as_str())
                    .unwrap_or(dep_key);
                parser::find_sibling_crate_dir(manifest, package)
                    .and_then(|dir| parser::crate_entry_file(&dir))
                    .map(|entry| {
                        let dep_attrs =
                            parser::parse_crate(package, false, None, &[], Some(&[entry]));
                        // Same reading `no_std_evidence` gives a crate-root parse:
                        // files read but no attribute found is Absent, not Supported.
                        dep_attrs.files_parsed > 0
                            && !parser::parse_main_attributes(&dep_attrs, ctx, None).0
                            && !dep_attrs.unconditional_no_std
                    })
                    .unwrap_or(false)
            });

            if lacks_no_std {
                debug!(
                    "'{}' selects optional dependency '{}', named in this crate's own \
                     compile_error! and with no no_std attribute at its crate root — \
                     forbidding it up front (KI-3)",
                    feat, dep_key
                );
                forbid.push(Bool::new_const(ctx, feat.as_str()).not());
            }
        }
    }
    forbid
}

/// The files of a downloaded crate that contain a `compile_error!` at all —
/// the prefilter for [`dependency_compile_error_constraints`], and the answer
/// to "which files can hold one".
///
/// Reading is the prefilter: parsing every dependency of every analysed crate
/// with syn is not worth doing for the crates that declare no `compile_error!`,
/// which is nearly all of them. What this must not do is stop at the crate
/// root. curve25519-dalek keeps
/// ``compile_error!("no curve25519-dalek backend cargo feature enabled!")`` in
/// `src/backend/mod.rs`, spin keeps its `mutex` one in `src/mutex.rs`, and
/// reading only `lib.rs` found neither — six ed25519/x25519 crates and
/// `forkable` shipped a config their dependency refuses to compile, with the
/// constraint that says so never parsed.
///
/// `src/` only, and the declared `[lib] path` when it points elsewhere: a
/// `build.rs` emitting a `compile_error!` into generated code (swiftness) is
/// not a statement about this build's features, and `examples/` and `benches/`
/// are not compiled by the dependency edge at all.
///
/// A `compile_error!` in a module the crate itself `#[cfg]`s out is read here
/// as though the module were always compiled, since the `mod` declaration lives
/// in another file. That is the same reading the main crate's own attributes
/// get, and it errs towards *stating* a constraint the crate may not be under —
/// which for the disjunctions this finds ("pick a backend") is the harmless
/// direction, because the answer is a feature the consumer declares for exactly
/// that purpose.
fn compile_error_source_files(dep_dir: &Path) -> Vec<PathBuf> {
    let mut roots: Vec<PathBuf> = vec![dep_dir.join("src")];
    if let Some(entry) = parser::crate_entry_file(dep_dir)
        && !entry.starts_with(dep_dir.join("src"))
    {
        roots.push(entry);
    }
    let mut files = Vec::new();
    for root in roots {
        for entry in WalkDir::new(&root).into_iter().filter_map(Result::ok) {
            let path = entry.path();
            if path.extension().is_none_or(|ext| ext != "rs") {
                continue;
            }
            if fs::read_to_string(path).is_ok_and(|text| text.contains("compile_error!")) {
                files.push(path.to_path_buf());
            }
        }
    }
    files
}

/// What this crate's dependencies demand of *its* feature set, plus what its
/// own `[features]` table always implies of itself, as one constraint the
/// feature solve can be handed — R31-4, extended by KI-33.
///
/// [`dependency_compile_error_constraints`] already says the dependency half in
/// this crate's feature names; what was missing is a consumer other than the
/// covering runs. The covering runs decide which feature sets get *compiled
/// for evidence*; the edge the run finally emits is decided by `process_crate`,
/// and that solve never saw the constraint. ab_glyph 0.2.29 is the case: `libm =
/// ["owned_ttf_parser/no-std-float", …]` is exactly what ttf-parser's
/// `compile_error!` asks for, owned_ttf_parser's own solve answered `enable:
/// []` because *it* is happy without the feature, and
/// `move_unnecessary_dep_feats` then read the entry as a dep feature nobody
/// asked for and moved it to `dep_unnecessary_features` — emitting `--features
/// libm` with the one thing `libm` was for deleted out of it.
///
/// The feature implications are asserted unconditionally, not only when a
/// dependency's `compile_error!` also reaches this crate — KI-33. Any
/// disjunction this crate is handed from elsewhere (`transitive_impl_requirements`
/// / `translate_across_edge` / `dependency_compile_error_constraints`, one level
/// up) is otherwise free to be satisfied the expensive way: `sp-io` is equally
/// happy answering `std ∨ default` with `default`, and nothing but `default =>
/// std` stops a model from picking it and turning std back on — `pallet-sudo`'s
/// `custom_no_std_feature_enabled = ["sp-io/default", …]`, four direct
/// dependencies re-enabling std on all 33 targets. `sp-io` has no
/// `compile_error!` two levels down of its own, so the old gate
/// (`dep_errors.is_empty()`) never let this run for it at all. These are
/// statements about the crate's own `[features]` table, true in every
/// configuration, so asserting them constrains nothing that was free.
///
/// `None` only when nothing here has anything to say about this crate at all —
/// no dependency constrains it *and* its own `[features]` table has no
/// feature-to-feature or feature-to-optional-dep link.
pub fn dependency_feature_requirement<'a>(ctx: &'a Context, manifest: &str) -> Option<Bool<'a>> {
    let manifest_toml = read_manifest_toml(manifest);
    // Ablation study §3.4: only the compile_error!-derived slice of `parts`
    // is this mechanism's; `feature_implication_constraints` and
    // `optional_dep_implication_constraints` below are the crate's own
    // `[features]` table semantics, not compile_error! modeling, and stay
    // in even when the flag is set.
    let mut parts = if ablation::flags().no_compile_error_constraints {
        Vec::new()
    } else {
        dependency_compile_error_constraints(ctx, manifest, &manifest_toml)
    };
    let feat_map = downloader::read_local_features(&manifest_toml);
    parts.extend(solver::feature_implication_constraints(ctx, &feat_map));
    parts.extend(solver::optional_dep_implication_constraints(
        ctx,
        &downloader::optional_dep_feature_edges(&manifest_toml),
    ));
    if parts.is_empty() {
        return None;
    }
    Some(Bool::and(ctx, &parts.iter().collect::<Vec<_>>()))
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
/// An import is not the only way to name a crate, and for a whole family of
/// crates it is not the way they do it: `mutex-1.0.0` writes
/// `critical_section::with(|_| …)` and nothing else, the `icu_*` family writes
/// `icu_calendar_data::make_provider!(Baked)`, the `pallet-*-uapi` family writes
/// `#[cfg_attr(feature = "scale", derive(scale_info::TypeInfo))]`, and
/// `rp-pico` writes `rp2040_boot2::BOOT_LOADER_W25Q080`. Those references are
/// `visitor::collect_path_roots`, gated by whatever `ancestors_for_span`
/// resolves at the site, and they are read here exactly like imports.
///
/// This is what `deps_pinned_by_active_use` used to miss: 24 of the 49 crates in
/// bucket R31-2 name their dependency only this way, and `minimize` unlinked it
/// under a feature the crate's own `default` turns on — `mutex-1.0.0` builds on
/// all 26 targets with its published manifest untouched.
///
/// Resolving *plugin* spans this way was tried and is not sound, and this is not
/// that: a HIR record's span may have no item in the tree, and the "no item"
/// answer is indistinguishable from "genuinely ungated" (it pinned watchface's
/// `chrono` and cost it its build). These spans come from the same syn pass that
/// built the tree. watchface stays strippable because its only `chrono` mentions
/// outside `#[cfg(feature = "chrono")]` are in doc comments, which are string
/// literals and not paths.
///
/// When a dependency has no reference of either kind but the covering runs
/// recorded one, the reference is in a position neither pass reads and the
/// dependency is pinned outright — there is no condition to read anywhere.
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
    let path_roots = visitor::collect_path_roots(root);

    let mut pinned = HashSet::new();
    for dep in optional_deps {
        let dep_norm = dep.replace('-', "_");
        let mut references: Vec<Option<Bool>> = roots
            .iter()
            .filter(|(name, _)| *name == dep_norm)
            .map(|(_, gate)| gate.clone())
            .collect();
        // Each path reference carries its site's gate — the module chain plus
        // the innermost containing item's own `#[cfg]`, which for a `cfg_attr`
        // derive is the predicate the attribute is applied under.
        references.extend(
            path_roots
                .iter()
                .filter(|(r, _)| *r == dep_norm)
                .map(|(_, span)| {
                    visitor::ancestors_for_span(root, span)
                        .map(|conds| Bool::and(ctx, &conds.iter().collect::<Vec<_>>()))
                }),
        );

        if references.is_empty() {
            if records.iter().any(|record| record.dep == dep_norm) {
                debug!(
                    "Optional dep '{dep}' is referenced only from a position neither pass reads \
                     — no condition to read anywhere, so it must not be unlinked"
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
        for gate in &references {
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
                    "Optional dep '{dep}' is named under a gate that stays true without it \
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

    // Clear the previous crate's answer before this one's entrypoints are even
    // read: a crate whose entrypoints all turn out to be missing never reaches
    // the call below, and inheriting a dependency's predicate would be worse
    // than having none.
    set_host_no_std_applicability(None, telemetry);

    // Same reasoning, same place, for the other per-crate flag. This is the only
    // reset: `LAST_GOOD_TARGET` deliberately survives, because a triple that
    // linked once is still the right first guess, while "has *this* crate reached
    // bare metal" has to start false for every crate — main and dependency alike.
    *CRATE_REACHED_BARE_METAL.lock().unwrap() = false;
    CRATE_BARE_METAL_SETS.lock().unwrap().clear();

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
        set_host_no_std_applicability(collector.non_feature_no_std_predicate.as_ref(), telemetry);
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

        // O-12(e): a dependency's `compile_error!` fails this crate's build the
        // same way its own does, and only its own reached the solve. peniko's
        // baseline ran `[]` and died on "color requires either the `std` or
        // `libm` feature", so it had no std-off run at all.
        let dep_error_constraints =
            dependency_compile_error_constraints(ctx, manifest, &manifest_toml);

        // Ablation study §3.4: neither this crate's own compile_error! (the
        // seed veto, parsed by the visitor into `compile_error_constraints`)
        // nor a dependency's (`dep_error_constraints`) gets to constrain the
        // solve when disabled. `impl_constraints`/`optdep_constraints` are
        // unrelated (feature-table implications, not compile_error!) and
        // stay in either way.
        let mut all_hard: Vec<Bool> = if ablation::flags().no_compile_error_constraints {
            Vec::new()
        } else {
            compile_error_constraints.clone()
        };
        all_hard.extend(impl_constraints.iter().cloned());
        all_hard.extend(optdep_constraints.iter().cloned());
        if !ablation::flags().no_compile_error_constraints {
            all_hard.extend(dep_error_constraints.iter().cloned());
        }

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
                    std_inconclusive,
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
                        std_inconclusive,
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
                            std_inconclusive,
                        } => {
                            run.meta("outcome", "success");
                            run.meta("records", full_output.records.len().to_string());
                            covering_runs.push(CoveringRun {
                                features: enable.clone(),
                                output: full_output,
                                std_inconclusive,
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
                    run.meta(
                        "kind",
                        format!("cegar iter {cegar_iter} set {set_num}/{set_total}"),
                    );
                    match run_rustc_plugin_pass(manifest, crate_name, &enable, None) {
                        PassOutcome::Success {
                            macro_modules,
                            std_spans: _,
                            full_output,
                            std_inconclusive,
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
                                std_inconclusive,
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

            // Ablation study §3.2: the first iteration already ran whatever
            // covering sets `get_solved_sets` found in one partition of the
            // whole item pool — that's the combinatorial search itself, not a
            // "default features" seed. Disabling it means never re-partitioning
            // around a set that failed to compile, i.e. no failure-driven CEGAR
            // retries, while iteration 1's own runs still stand.
            if !made_progress || ablation::flags().no_combo_search {
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
                                    std_inconclusive,
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
                                        std_inconclusive,
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
    Vec<ImplRecord>,
    Vec<CrossCrateItem>,
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

/// The impls observed by the passes whose own feature set satisfies the crate's
/// no_std condition — and by every pass when not one of them does.
///
/// Which pass saw an obligation matters, and a span cannot stand in for it. uom
/// 0.38.0 is the case: its `system! { … }` at `src/si/mod.rs:10` generates the
/// whole SI module, and a pass with `std` on resolves an `f32: MulAdd` inside
/// that expansion — an obligation the no_std configuration never raises, because
/// the code that carries it is not generated. The invocation is ungated, so the
/// call site is "reachable" by any span test, and the requirement that follows
/// (`std ∨ libm` in num-traits, with `std` forbidden) forces `num-traits/libm`
/// onto a build that compiles clean without it. uom has no `libm` feature of its
/// own, so it arrives as an injected `custom_no_std_feature_enabled` entry —
/// visible, wrong, and not a build failure, which is the worst shape for a
/// mistake to take.
///
/// The fallback is not a hedge, it is the KI-27 case itself. A crate that needs
/// an impl its feature set does not provide **does not type check**, so it has
/// no no_std-consistent pass at all: multiexp 0.4.0 has zero covering runs and a
/// `default = ["std"]` pass, and that std-on pass is the only compilation in the
/// entire run that ever resolved `Vec<Vec<u8>>: Zeroize`. "No pass could be both
/// no_std and compile" is exactly the situation where a std-on pass is the only
/// evidence available, and where its evidence is worth acting on.
///
/// Note the fallback keys on whether such a *pass* exists, never on whether the
/// consistent passes happened to produce records. uom's do not, and reading that
/// as "no evidence, fall back" would put its `libm` straight back.
pub fn impls_from_no_std_passes(
    ctx: &Context,
    manifest: &str,
    hard: Option<&Bool<'_>>,
    covering_runs: &[CoveringRun],
    default_output: Option<&FeatureRunOutput>,
) -> Vec<ImplRecord> {
    let manifest_toml = read_manifest_toml(manifest);
    let feat_map = downloader::read_local_features(&manifest_toml);
    let declared = visitor::declared_features(manifest);

    // True when the crate is no_std in the configuration this pass compiled.
    // The assignment is total over the declared features, so satisfiability is
    // evaluation; atoms the condition names that cargo does not declare stay
    // free, the same latitude policy G already gives them.
    let compiled_no_std = |feats: &[String], defaults_on: bool| -> bool {
        let Some(h) = hard else {
            // Nothing to contradict: the crate is no_std whatever is enabled.
            return true;
        };
        let mut on: HashSet<String> = feats.iter().cloned().collect();
        if defaults_on {
            on.insert("default".to_string());
        }
        let on = parser::close_over_local_features(&on, &feat_map);
        let solver = z3::Solver::new(ctx);
        solver.assert(h);
        for feat in &declared {
            let var = Bool::new_const(ctx, feat.as_str());
            if on.contains(feat) {
                solver.assert(&var);
            } else {
                solver.assert(&var.not());
            }
        }
        solver.check() == z3::SatResult::Sat
    };

    // The default pass takes no `--no-default-features`; every covering run does.
    let passes: Vec<(&FeatureRunOutput, bool, &[String])> = covering_runs
        .iter()
        .map(|run| (&run.output, false, run.features.as_slice()))
        .chain(default_output.map(|out| (out, true, [].as_slice())))
        .collect();

    let mut any_consistent = false;
    let mut kept: Vec<ImplRecord> = Vec::new();
    for (output, defaults_on, feats) in &passes {
        if compiled_no_std(feats, *defaults_on) {
            any_consistent = true;
            kept.extend(output.impls.iter().cloned());
        }
    }
    if any_consistent {
        return kept;
    }
    debug!(
        "No pass compiled this crate in a no_std configuration; \
         reading trait obligations off the ones that did compile"
    );
    passes
        .iter()
        .flat_map(|(output, _, _)| output.impls.iter().cloned())
        .collect()
}

/// The cross-crate item references seen by the passes whose feature set
/// satisfies the crate's no_std condition — and by every pass that compiled when
/// not one of them does. `impls_from_no_std_passes` for plain paths (R34-6).
///
/// **The fallback is the whole case, not a hedge**, and earcut 0.4.4 is the
/// proof. `num_traits::float::Float` lives behind `#[cfg(any(feature = "std",
/// feature = "libm"))]`, so a pass that turned num-traits' defaults off does not
/// resolve the import and does not compile: earcut has **zero covering runs**,
/// exactly as multiexp does. The `default = ["std"]` pass is the only
/// compilation in the entire run that ever resolved the path, and reading it is
/// what makes the requirement derivable at all.
///
/// Reading a std-on pass costs nothing because no verdict is taken from it.
/// Which arm it resolved to is a fact about that configuration, and
/// `path_availability_requirement` asks for that item's *alternatives*, not for
/// the gate of the arm it happened to get.
///
/// Records are projected to `CrossCrateItem` as each pass is read, never
/// collected as `PathRecord`s — same reason as `CrossCrateRef`, and the same
/// web-sys memory ceiling.
pub fn paths_from_no_std_passes(
    ctx: &Context,
    manifest: &str,
    hard: Option<&Bool<'_>>,
    covering_runs: &[CoveringRun],
    default_output: Option<&FeatureRunOutput>,
) -> Vec<CrossCrateItem> {
    let manifest_toml = read_manifest_toml(manifest);
    let feat_map = downloader::read_local_features(&manifest_toml);
    let declared = visitor::declared_features(manifest);

    let compiled_no_std = |feats: &[String], defaults_on: bool| -> bool {
        let Some(h) = hard else {
            return true;
        };
        let mut on: HashSet<String> = feats.iter().cloned().collect();
        if defaults_on {
            on.insert("default".to_string());
        }
        let on = parser::close_over_local_features(&on, &feat_map);
        let solver = z3::Solver::new(ctx);
        solver.assert(h);
        for feat in &declared {
            let var = Bool::new_const(ctx, feat.as_str());
            if on.contains(feat) {
                solver.assert(&var);
            } else {
                solver.assert(&var.not());
            }
        }
        solver.check() == z3::SatResult::Sat
    };

    /// An item reference is only usable when the plugin recorded where the
    /// definition is; nothing about the item's *name* says which `#[cfg]` stands
    /// above it, and guessing is the thing this design exists to avoid.
    fn project(output: &FeatureRunOutput, out: &mut HashSet<CrossCrateItem>) {
        for r in &output.records {
            if r.definition_crate == "LOCAL" || r.is_extern_crate {
                continue;
            }
            let Some(def_span) = r.definition_span.clone() else {
                continue;
            };
            let item = r.path_text.rsplit("::").next().unwrap_or(&r.path_text);
            if item.is_empty() {
                continue;
            }
            out.insert(CrossCrateItem {
                dep: r.definition_crate.replace('-', "_"),
                item: item.to_string(),
                use_span: r.span.clone(),
                def_span,
            });
        }
    }

    // The default pass takes no `--no-default-features`; every covering run does.
    let passes: Vec<(&FeatureRunOutput, bool, &[String])> = covering_runs
        .iter()
        .map(|run| (&run.output, false, run.features.as_slice()))
        .chain(default_output.map(|out| (out, true, [].as_slice())))
        .collect();

    let mut any_consistent = false;
    let mut kept: HashSet<CrossCrateItem> = HashSet::new();
    for (output, defaults_on, feats) in &passes {
        if compiled_no_std(feats, *defaults_on) {
            any_consistent = true;
            project(output, &mut kept);
        }
    }
    if !any_consistent {
        debug!(
            "No pass compiled this crate in a no_std configuration; \
             reading cross-crate item references off the ones that did compile"
        );
        for (output, _, _) in &passes {
            project(output, &mut kept);
        }
    }
    kept.into_iter().collect()
}

/// The item references whose **use site** a no_std build can actually reach —
/// `reachable_impl_records` for plain paths.
///
/// Deliberately the same test as that one and **not**
/// `compute_valid_cross_crate_items`'s, which asks whether the gate holds in a
/// configuration where the no_std condition does *not*. That is the opposite
/// question, and it is defensible where it is used because its answer only ever
/// protects a dependency feature from removal. Here the answer *adds* a feature,
/// and a feature added on the strength of a reference the emitted configuration
/// never compiles is a dependency the crate did not need.
pub fn reachable_path_items<'a>(
    root: &ModNode<'a>,
    items: &[CrossCrateItem],
    hard: Option<&Bool<'a>>,
    ctx: &'a Context,
) -> Vec<CrossCrateItem> {
    // Keyed by span, not by item: a `use` that names three items asks the solver
    // one question, and the answer is a property of where the reference is.
    let mut by_span: HashMap<&ReadableSpan, bool> = HashMap::new();
    let mut out: Vec<CrossCrateItem> = Vec::new();
    for item in items {
        let reachable = *by_span.entry(&item.use_span).or_insert_with(|| match hard {
            None => true,
            Some(h) => match find_condition_for_span(root, &item.use_span, ctx, None) {
                None => true,
                Some(c) => {
                    let s = z3::Solver::new(ctx);
                    s.assert(&c);
                    s.assert(h);
                    s.check() == z3::SatResult::Sat
                }
            },
        });
        if reachable {
            out.push(item.clone());
        }
    }
    out.sort_by(|a, b| {
        (&a.dep, &a.item, &a.use_span.file, a.use_span.start_line).cmp(&(
            &b.dep,
            &b.item,
            &b.use_span.file,
            b.use_span.start_line,
        ))
    });
    out.dedup_by(|a, b| a.dep == b.dep && a.item == b.item);
    out
}

/// The impl records whose **call site** a no_std build can actually reach.
///
/// `hard` is the crate's own no_std condition — the thing the emitted
/// configuration has to satisfy — so a call site is in that build when its gate
/// is satisfiable *together with* it. A call under `#[cfg(feature = "std")]` is
/// not, and the impl it needs is not something to demand of a dependency. With
/// no condition at all every call site counts, which is what "no constraint on
/// the configuration" means.
///
/// Deliberately **not** `compute_valid_cross_crate_items`'s test, which asks
/// whether the gate holds in a configuration where the no_std condition does
/// *not* — the opposite question. That is defensible where it is used, because
/// its answer only ever protects a dependency feature from removal and erring
/// wide costs nothing. Here the answer *adds* a feature, and a feature added on
/// the strength of a call the emitted configuration never compiles is a
/// dependency the crate did not need.
///
/// This test is about the **gate on the call**. The other half of the same
/// question — whether the *pass* that saw the obligation was compiling a no_std
/// configuration at all — a span cannot answer; see `impls_from_no_std_passes`.
///
/// `records` is the union over every pass that compiled — the covering runs
/// *and* the default-features pass. The default pass is not an afterthought
/// here: a crate whose feature set is missing an impl does not type check, so
/// its covering runs all fail and contribute nothing, which is precisely the
/// shape KI-27 is about. multiexp 0.4.0 has **zero** covering runs, and the
/// default pass — `default = ["std"]`, which reaches `zeroize/alloc` — is the
/// only compilation in the whole run that ever resolved `Vec<Vec<u8>>: Zeroize`.
///
/// Reading a std-on pass costs nothing here because no verdict is taken from it:
/// which impl it selected is a fact about that configuration, and
/// `impl_availability_requirement` asks for the *alternatives* to that impl, not
/// for its gate.
pub fn reachable_impl_records<'a>(
    root: &ModNode<'a>,
    records: &[ImplRecord],
    hard: Option<&Bool<'a>>,
    ctx: &'a Context,
) -> Vec<ImplRecord> {
    // Keyed by span, not by record: a call site that needs three impls asks the
    // solver one question, and the answer is a property of where the call is.
    let mut by_span: HashMap<&ReadableSpan, bool> = HashMap::new();
    let mut seen: HashSet<ImplRecord> = HashSet::new();
    for record in records {
        if seen.contains(record) {
            continue;
        }
        let reachable = *by_span.entry(&record.span).or_insert_with(|| match hard {
            None => true,
            Some(h) => match find_condition_for_span(root, &record.span, ctx, None) {
                None => true,
                Some(c) => {
                    let s = z3::Solver::new(ctx);
                    s.assert(&c);
                    s.assert(h);
                    s.check() == z3::SatResult::Sat
                }
            },
        });
        if reachable {
            seen.insert(record.clone());
        }
    }
    let mut out: Vec<ImplRecord> = seen.into_iter().collect();
    out.sort_by(|a, b| {
        (
            &a.definition_crate,
            &a.trait_name,
            &a.self_ty,
            &a.span.file,
            a.span.start_line,
        )
            .cmp(&(
                &b.definition_crate,
                &b.trait_name,
                &b.self_ty,
                &b.span.file,
                b.span.start_line,
            ))
    });
    out
}

/// What a dependency's feature set has to provide so the impls its dependent's
/// calls resolved to still exist — the KI-27 requirement.
///
/// The compiler reported, for each call site, the impl it selected
/// (`ImplRecord`). Each of those is looked up in the module tree of the crate
/// that **defines** it, and:
///
/// * an impl with no `#[cfg]` requires nothing — it is there whatever the
///   feature set;
/// * a gated one requires that *some* impl with the same `(trait, self-type)`
///   key exists, which is the disjunction of their gates;
/// * a macro-generated one has no key, so the alternation is over the
///   invocations of the macro that generated it — see
///   `impl_requirement_in_crate`.
///
/// The disjunction, not the gate of the impl that happened to win, is the whole
/// point. A pass that compiled with `std` on selects the `std`-gated arm, and
/// asserting that gate would demand the one thing the run exists to remove; the
/// alternation is what lets the solve pick the arm a no_std build can have. For
/// multiexp 0.4.0 there is exactly one arm — `#[cfg(feature = "alloc")] impl<Z>
/// Zeroize for Vec<Z>` — so the requirement is `alloc`, and since no multiexp
/// feature reaches it, `final_feature_list_dep` parks it in
/// `custom_no_std_feature_enabled`. That is the configuration that builds.
///
/// The defining crate need not be this dependency. unit-sphere 0.4.0 calls
/// `.norm_squared()` on a `Vector3<f64>`, nalgebra defines that method ungated,
/// and the impl the obligation needs — `impl ComplexField for f64` — is in
/// **simba**, which unit-sphere neither names nor depends on. A requirement
/// derived in simba's feature namespace says nothing about nalgebra's until each
/// atom is re-expressed across the edge that links them, which is what
/// `translate_across_edge` does and what `dependency_compile_error_constraints`
/// already does one level up. `nalgebra` declares `libm = ["simba/libm"]`, so
/// simba's `libm` becomes nalgebra's `libm` and the constraint lands where the
/// solve can act on it.
///
/// A crate reachable through **two** direct dependencies gets the requirement
/// from both. That is redundant rather than wrong — cargo unifies features per
/// package, so one path enabling `simba/libm` gives the single simba build its
/// impl — and the alternative is to pick one edge arbitrarily.
///
/// Two kinds of requirement are dropped rather than asserted:
///
/// * one already satisfied unconditionally (`true` after simplification) — there
///   is nothing to ask for, and asking would turn a free feature into a fixed
///   one;
/// * one no feature set can meet under the dependency's own constraints. That is
///   the `unreachable_atom` discipline `dependency_compile_error_constraints`
///   applies: an unsatisfiable conjunct costs the crate *every* covering run —
///   no baseline, no solved sets, every span `AlwaysStd` — which is far worse
///   than the failure it was trying to prevent.
pub fn impl_availability_requirement<'a>(
    ctx: &'a Context,
    dep_root: &ModNode<'a>,
    dep_dir: &Path,
    dep_crate: &str,
    records: &[ImplRecord],
    dep_hard: Option<&Bool<'a>>,
) -> Option<Bool<'a>> {
    let wanted = dep_crate.replace('-', "_");

    // `(what the requirement is for, where the call was, the requirement)`, all
    // already in *this dependency's* feature namespace.
    let mut candidates: Vec<(String, &ImplRecord, Bool<'a>)> = Vec::new();

    // One hop: the impl is in this dependency, and the tree the caller already
    // built answers directly.
    let all_gates = visitor::collect_trait_impl_gates(dep_root, ctx);
    let mut foreign: BTreeMap<String, Vec<&ImplRecord>> = BTreeMap::new();
    for record in records {
        if record.definition_crate.replace('-', "_") != wanted {
            foreign
                .entry(record.definition_crate.clone())
                .or_default()
                .push(record);
            continue;
        }
        if let Some((label, requirement)) =
            impl_requirement_in_crate(ctx, dep_root, dep_dir, &all_gates, record)
        {
            candidates.push((label, record, requirement));
        }
    }

    // Two hops and further. One graph walk per dependency answers for every
    // crate the records name, so the cost does not multiply by the number of
    // distinct defining crates.
    if !foreign.is_empty() {
        let chains = edge_chains_to_crates(dep_dir, &foreign.keys().cloned().collect());
        for (definition_crate, group) in &foreign {
            let Some(chain) = chains.get(&definition_crate.replace('-', "_")) else {
                // Not below this dependency at all — some other edge of the main
                // crate reaches it, and that edge's own `process_dep_crate` call
                // is where this record gets its answer.
                continue;
            };
            candidates.extend(transitive_impl_requirements(ctx, chain, group));
        }
    }

    let mut parts: Vec<Bool<'a>> = Vec::new();
    for (label, record, requirement) in candidates {
        let s = z3::Solver::new(ctx);
        s.assert(&requirement);
        if let Some(h) = dep_hard {
            s.assert(h);
        }
        if s.check() != z3::SatResult::Sat {
            debug!(
                "[impl_req] {}: no feature set gives `{}` and keeps the crate no_std; \
                 leaving the requirement out",
                dep_crate, label
            );
            continue;
        }
        println!(
            "[impl_req] {} must provide `{}` ({} used it at {}:{}): {:?}",
            dep_crate,
            label,
            record.span.usage_crate.as_deref().unwrap_or("the crate"),
            record.span.file,
            record.span.start_line,
            requirement
        );
        if !parts.contains(&requirement) {
            parts.push(requirement);
        }
    }

    if parts.is_empty() {
        return None;
    }
    Some(Bool::and(ctx, &parts.iter().collect::<Vec<_>>()).simplify())
}

/// Does any of `records` name an impl this dependency would have to provide —
/// itself, or through a crate below it?
///
/// The `db.bin` guard. The DB answers "what does this dependency need to be
/// no_std", keyed by the dependency alone, and that is parent-independent. A
/// KI-27 requirement is not: `zeroize/alloc` is needed because *multiexp* calls
/// `.zeroize()` on a `Vec`, and `nalgebra/libm` because *unit-sphere* calls
/// `.norm_squared()` on a `Vector3<f64>` — facts about the parent, not about the
/// dependency. A cache hit would silently skip the constraint, which is the same
/// shape as a cache hit silently skipping the analysis a verification run exists
/// to test.
///
/// The two-hop case is why this is not a name comparison: the record names
/// simba, the dependency is nalgebra, and nothing about the two strings says
/// they are related. It walks the same edge graph
/// `impl_availability_requirement` does, and answers `false` fast for the common
/// case of a crate with no records at all.
pub fn dep_carries_impl_requirements(
    dep_dir: &Path,
    dep_crate: &str,
    records: &[ImplRecord],
) -> bool {
    let wanted = dep_crate.replace('-', "_");
    let mut foreign: HashSet<String> = HashSet::new();
    for record in records {
        let defined_in = record.definition_crate.replace('-', "_");
        if defined_in == wanted {
            return true;
        }
        foreign.insert(defined_in);
    }
    if foreign.is_empty() {
        return false;
    }
    !edge_chains_to_crates(dep_dir, &foreign).is_empty()
}

/// Does any of `items` name an item this dependency would have to define —
/// itself, or through a crate below it? The `db.bin` guard for R34-6, and the
/// same argument as `dep_carries_impl_requirements`.
///
/// `db.bin` answers "what does this dependency need to be no_std", keyed by the
/// dependency alone, which is parent-independent and sound for that question. A
/// path requirement is not: `num-traits/libm` is needed because *earcut* names
/// `num_traits::float::Float`, and a sibling crate that uses only `NumCast`
/// needs nothing. Without this guard the constraint is derived correctly and
/// then skipped for every dependency the cache already holds — the analysis
/// under test silently not running, which is the failure mode `db.bin` has
/// produced before.
pub fn dep_carries_path_requirements(
    dep_dir: &Path,
    dep_crate: &str,
    items: &[CrossCrateItem],
) -> bool {
    let wanted = dep_crate.replace('-', "_");
    let mut foreign: HashSet<String> = HashSet::new();
    for item in items {
        if item.dep == wanted {
            return true;
        }
        foreign.insert(item.dep.clone());
    }
    if foreign.is_empty() {
        return false;
    }
    !edge_chains_to_crates(dep_dir, &foreign).is_empty()
}

/// What the crate that **defines** a selected impl has to enable for that impl
/// to exist, in its own feature namespace — with a human-readable label for the
/// log line.
///
/// Two ways to find the alternatives, and which one applies is decided by what
/// the tree holds at the impl's span:
///
/// * a keyed `impl` item (`LocalItem::impl_trait`) — the alternatives are every
///   impl in the crate with the same `(trait, self-type)` key. Anchoring on the
///   span rather than the key is what keeps a blanket impl from answering for a
///   concrete one; see `visitor::impl_at_span`.
/// * a macro **invocation** (`LocalItem::macro_call`) — the impl does not exist
///   before expansion, so there is no keyed item to find and no key to
///   enumerate by. The alternatives are the gates of every invocation of the
///   same macro in the crate: the same macro expanded twice generates the same
///   impls twice, so the invocations *are* the arms. simba writes
///
///   ```text
///   #[cfg(all(not(feature = "std"), not(feature = "libm_force"), feature = "libm"))]
///   impl_complex!(f32, f32, Float; f64, f64, Float);
///   #[cfg(all(feature = "std", not(feature = "libm_force")))]
///   impl_complex!(f32, f32, f32; f64, f64, f64);
///   ```
///
///   giving `(¬std ∧ ¬libm_force ∧ libm) ∨ (std ∧ ¬libm_force)`, which forces
///   `libm` once `std` is off.
///
/// Guarded on `ImplRecord::via_macro`: the tree has a `LocalItem` for *every*
/// macro invocation, so without it any impl whose span happened to fall inside
/// one would be answered by the wrong alternation. The macro name comes from the
/// tree rather than from `via_macro` because the invocation is where the
/// `#[cfg]` is — a macro that expands to another macro reports the inner name
/// and carries the outer one's gate.
///
/// simba's `#[cfg(feature = "libm_force")] impl ComplexField for f32` is
/// deliberately **not** folded in as a third alternative. It is a hand-written
/// impl, not something `impl_complex!` generates, and reading it as an arm would
/// let the solve answer the requirement with `simba/libm_force` — a
/// configuration nothing in this run has compiled. The inference here is "the
/// same macro generates the same impls", and it does not extend past the macro.
fn impl_requirement_in_crate<'a>(
    ctx: &'a Context,
    root: &ModNode<'a>,
    dir: &Path,
    all_gates: &[((String, String), Option<Bool<'a>>)],
    record: &ImplRecord,
) -> Option<(String, Bool<'a>)> {
    let span = span_in_dep_tree(&record.impl_span, dir)?;

    let mut alternatives: Vec<Bool<'a>> = Vec::new();
    let mut unconditional = false;
    let label;

    match visitor::impl_at_span(root, &span, ctx) {
        // Written out, and gated: enumerate its arms by key.
        Some((key, Some(_))) => {
            label = format!("impl {} for {}", key.0, key.1);
            for (other, gate) in all_gates {
                if *other != key {
                    continue;
                }
                match gate {
                    None => unconditional = true,
                    Some(g) => alternatives.push(g.clone()),
                }
            }
        }
        // Written out and unconditional: there is nothing to ask for.
        Some((_, None)) => return None,
        // Not in the tree as an impl at all — the macro case.
        None => {
            record.via_macro.as_ref()?;
            let (macro_name, gate) = visitor::macro_invocation_at_span(root, &span, ctx)?;
            // An ungated invocation generates the impl in every configuration.
            gate.as_ref()?;
            label = format!(
                "impl {} for {} (from {}!)",
                record.trait_name, record.self_ty, macro_name
            );
            for gate in visitor::collect_macro_invocation_gates(root, &macro_name, ctx) {
                match gate {
                    None => unconditional = true,
                    Some(g) => alternatives.push(g),
                }
            }
        }
    }

    if unconditional || alternatives.is_empty() {
        return None;
    }
    let requirement = Bool::or(ctx, &alternatives.iter().collect::<Vec<_>>()).simplify();
    if requirement == Bool::from_bool(ctx, true) {
        return None;
    }
    Some((label, requirement))
}

/// What one cross-crate item requires of the crate that defines it — the
/// plain-path counterpart of `impl_requirement_in_crate` (R34-6).
///
/// Two lookups, and both are needed. `def_span` locates the definition the
/// compiler *actually resolved to*, which is the only thing that says the item
/// is in this crate and is gated at all; a name matched across the tree would
/// answer for a same-named item somewhere else entirely. The **name** then finds
/// the item's alternatives, and it is their disjunction that becomes the
/// requirement.
///
/// The alternation is the whole point, exactly as it is for impls. The pass that
/// resolved `num_traits::float::Float` compiled with `std` on and resolved it to
/// the arm `#[cfg(any(feature = "std", feature = "libm"))]` guards; asserting the
/// gate of the arm that won would demand `std`, the one thing the run exists to
/// remove. Here the cfg is itself a disjunction and the requirement comes back
/// `std ∨ libm`, which is a choice the solve can act on — `libm`.
///
/// Nothing is required when:
///
/// * the definition is not in this crate's tree — a re-export, a macro-generated
///   item, an `include!`d file the pass did not walk. There is no gate here to
///   read, and inventing one from the item's name is the guess this function
///   exists to avoid;
/// * the resolved definition is unconditional, or *some* arm of it is. The item
///   is there whatever the feature set;
/// * the name matched nothing. A gated definition whose name the tree does not
///   carry means the two lookups disagree, and the conservative answer is to ask
///   for nothing rather than to force the arm that happened to win.
fn path_requirement_in_crate<'a>(
    ctx: &'a Context,
    root: &ModNode<'a>,
    dir: &Path,
    by_file: &mut HashMap<String, Vec<(String, Bool<'a>)>>,
    item: &CrossCrateItem,
) -> Option<(String, Bool<'a>)> {
    let span = span_in_dep_tree(&item.def_span, dir)?;

    // Present and gated? `ancestors_for_span` answers `None` for a span it
    // cannot place *and* for one that is unconditional, and both mean the same
    // thing here: nothing to ask for.
    let resolved = visitor::ancestors_for_span(root, &span)?;
    if resolved.is_empty() {
        return None;
    }

    // The alternatives, scoped to the file the definition is in. Crate-wide is
    // the wrong scope: num-traits defines `abs` ungated in `sign.rs` and again
    // as a method of the `std`/`libm`-gated `Float`, and reading the first as an
    // arm of the second drops a real requirement.
    // Cached per file, not recomputed per item. The lookup walks the whole
    // module tree, and a crate naming a hundred items from one dependency asked
    // for that walk a hundred times — tween 2.0.4 and liealg 0.4.1 both stopped
    // finishing inside an hour before this cache existed.
    let named = by_file.entry(span.file.clone()).or_insert_with(|| {
        visitor::named_item_conditions_in_file(root, &span.file, ctx)
    });

    let mut alternatives: Vec<Bool<'a>> = Vec::new();
    for (name, cond) in named.iter() {
        if *name != item.item {
            continue;
        }
        // An arm that is always there settles it for every arm.
        if *cond == Bool::from_bool(ctx, true) {
            return None;
        }
        alternatives.push(cond.clone());
    }

    if alternatives.is_empty() {
        return None;
    }
    let requirement = Bool::or(ctx, &alternatives.iter().collect::<Vec<_>>()).simplify();
    if requirement == Bool::from_bool(ctx, true) {
        return None;
    }
    Some((item.item.clone(), requirement))
}

/// What a dependency's feature set has to provide so the **items** its
/// dependent's paths resolved to still exist — `impl_availability_requirement`
/// for plain paths rather than trait obligations (R34-6).
///
/// Same three-step as the impl side and the same routing: an item defined in
/// this dependency is answered from the tree the caller already built, one
/// defined below it is answered in the defining crate's own namespace and
/// carried up the edge chain by `translate_across_edge`.
///
/// earcut 0.4.4 is the case it exists for. It writes `use
/// num_traits::float::Float`, num-traits declares `libm` and is perfectly
/// no_std without it, and its own solve therefore answers `enable: []` — so the
/// tool emitted `default-features = false` on the edge and lost all 26 targets
/// to `E0432 unresolved import`, with rustc pointing at the `#[cfg(any(feature =
/// "std", feature = "libm"))]` above `pub trait Float`. That cfg is the
/// requirement, it is derivable from num-traits' own source, and this is where
/// it enters the solve.
///
/// Requirements are dropped rather than asserted under the same two conditions
/// as the impl side: one already satisfied unconditionally, and one no feature
/// set can meet while keeping the dependency no_std.
pub fn path_availability_requirement<'a>(
    ctx: &'a Context,
    dep_root: &ModNode<'a>,
    dep_dir: &Path,
    dep_crate: &str,
    items: &[CrossCrateItem],
    dep_hard: Option<&Bool<'a>>,
) -> Option<Bool<'a>> {
    if items.is_empty() {
        return None;
    }
    let wanted = dep_crate.replace('-', "_");

    let mut candidates: Vec<(String, &CrossCrateItem, Bool<'a>)> = Vec::new();

    // One hop: the item is defined in this dependency.
    debug!(
        "[path_req] {} (wanted={}): {} item(s) in, deps naming: {:?}",
        dep_crate,
        wanted,
        items.len(),
        items
            .iter()
            .map(|i| i.dep.as_str())
            .collect::<std::collections::BTreeSet<_>>()
    );
    let mut by_file: HashMap<String, Vec<(String, Bool<'a>)>> = HashMap::new();
    let mut foreign: BTreeMap<String, Vec<&CrossCrateItem>> = BTreeMap::new();
    for item in items {
        if item.dep != wanted {
            foreign.entry(item.dep.clone()).or_default().push(item);
            continue;
        }
        if let Some((label, requirement)) =
            path_requirement_in_crate(ctx, dep_root, dep_dir, &mut by_file, item)
        {
            candidates.push((label, item, requirement));
        }
    }

    // Two hops and further, through the same edge graph the impl side walks.
    if !foreign.is_empty() {
        let chains = edge_chains_to_crates(dep_dir, &foreign.keys().cloned().collect());
        for (definition_crate, group) in &foreign {
            let Some(chain) = chains.get(&definition_crate.replace('-', "_")) else {
                continue;
            };
            candidates.extend(transitive_path_requirements(ctx, chain, group));
        }
    }

    let mut parts: Vec<Bool<'a>> = Vec::new();
    for (label, item, requirement) in candidates {
        let s = z3::Solver::new(ctx);
        s.assert(&requirement);
        if let Some(h) = dep_hard {
            s.assert(h);
        }
        if s.check() != z3::SatResult::Sat {
            debug!(
                "[path_req] {}: no feature set defines `{}` and keeps the crate no_std; \
                 leaving the requirement out",
                dep_crate, label
            );
            continue;
        }
        println!(
            "[path_req] {} must define `{}` (defined at {}:{}): {:?}",
            dep_crate, label, item.def_span.file, item.def_span.start_line, requirement
        );
        if !parts.contains(&requirement) {
            parts.push(requirement);
        }
    }

    if parts.is_empty() {
        return None;
    }
    Some(Bool::and(ctx, &parts.iter().collect::<Vec<_>>()).simplify())
}

/// The item requirements a crate two or more hops down places on the direct
/// dependency at the top of `chain` — `transitive_impl_requirements` for plain
/// paths.
fn transitive_path_requirements<'a, 'r>(
    ctx: &'a Context,
    chain: &[DepHop],
    items: &[&'r CrossCrateItem],
) -> Vec<(String, &'r CrossCrateItem, Bool<'a>)> {
    let Some(last) = chain.last() else {
        return Vec::new();
    };
    let dir = &last.lower_dir;
    let manifest = dir.join("Cargo.toml").display().to_string();
    let Some(entry) = parser::crate_entry_file(dir) else {
        return Vec::new();
    };
    let name = entry
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("lib")
        .to_string();

    // Same erasure policy as any other tree: a `#[cfg]` naming a feature the
    // manifest does not declare has no solver variable, so it cannot come back
    // as a requirement.
    let known_features = visitor::declared_features(&manifest);
    let mut collector = ModCollector::with_known_features(ctx, known_features);
    let root = collector.collect(&entry, &name);
    // Read each edge once — the translation is a property of the edge, not of
    // the item, and `declared_features` shells out to `cargo metadata`.
    let hops: Vec<HopContext> = chain.iter().map(HopContext::read).collect();

    let mut out = Vec::new();
    let mut seen: HashSet<String> = HashSet::new();
    let mut by_file: HashMap<String, Vec<(String, Bool<'a>)>> = HashMap::new();
    for item in items {
        let Some((label, requirement)) =
            path_requirement_in_crate(ctx, &root, dir, &mut by_file, item)
        else {
            continue;
        };
        if !seen.insert(format!("{label}|{requirement:?}")) {
            continue;
        }
        if let Some(translated) = translate_chain(ctx, requirement, &hops) {
            out.push((label, *item, translated));
        }
    }
    out
}

/// Carry a requirement up a chain of edges, innermost first. `None` as soon as
/// one hop cannot forward it — see `translate_across_edge`.
fn translate_chain<'a>(
    ctx: &'a Context,
    mut requirement: Bool<'a>,
    hops: &[HopContext],
) -> Option<Bool<'a>> {
    for hop in hops.iter().rev() {
        requirement = translate_across_edge(ctx, &requirement, hop)?;
    }
    Some(requirement)
}

/// One dependency edge, as the two crate directories it joins plus the edge's
/// own declaration. `dep_key` is the manifest's *key* for the edge — what a
/// `features = ["key/feat"]` reference has to match, which is not the package
/// name when the edge renames it.
#[derive(Clone)]
struct DepHop {
    upper_dir: PathBuf,
    dep_key: String,
    edge: toml::Value,
    lower_dir: PathBuf,
}

/// The impl requirements a crate two or more hops down places on the direct
/// dependency at the top of `chain`.
///
/// The requirement is derived in the defining crate's namespace — its tree is
/// built here, from the copy in the `_deps` directory that already holds the
/// whole transitive closure — and then carried up one edge at a time. A hop that
/// cannot forward the requirement drops it; see `translate_across_edge`.
fn transitive_impl_requirements<'a, 'r>(
    ctx: &'a Context,
    chain: &[DepHop],
    records: &[&'r ImplRecord],
) -> Vec<(String, &'r ImplRecord, Bool<'a>)> {
    let Some(last) = chain.last() else {
        return Vec::new();
    };
    let dir = &last.lower_dir;
    let manifest = dir.join("Cargo.toml").display().to_string();
    let Some(entry) = parser::crate_entry_file(dir) else {
        return Vec::new();
    };
    let name = entry
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("lib")
        .to_string();

    // Same erasure policy as any other tree: a `#[cfg]` naming a feature the
    // manifest does not declare has no solver variable, so it cannot come back
    // as a requirement.
    let known_features = visitor::declared_features(&manifest);
    let mut collector = ModCollector::with_known_features(ctx, known_features);
    let root = collector.collect(&entry, &name);
    let all_gates = visitor::collect_trait_impl_gates(&root, ctx);

    // Read each edge once. `declared_features` shells out to `cargo metadata`,
    // and a crate that needs one impl at fifty call sites arrives as fifty
    // records — the translation is a property of the edge, not of the record.
    let hops: Vec<HopContext> = chain.iter().map(HopContext::read).collect();

    let mut out = Vec::new();
    let mut seen: HashSet<String> = HashSet::new();
    for record in records {
        let Some((label, requirement)) =
            impl_requirement_in_crate(ctx, &root, dir, &all_gates, record)
        else {
            continue;
        };
        // Same requirement from a second call site is the same requirement. The
        // caller dedups what reaches the solve anyway; this dedups the work.
        if !seen.insert(format!("{label}|{requirement:?}")) {
            continue;
        }
        if let Some(translated) = translate_chain(ctx, requirement, &hops) {
            out.push((label, *record, translated));
        }
    }
    out
}

/// One edge of a chain, with everything the translation needs read off disk —
/// the manifests on both sides, the lower crate's declared features, and the
/// features the edge itself supplies.
struct HopContext {
    upper_dir: PathBuf,
    lower_dir: PathBuf,
    dep_key: String,
    upper_toml: toml::Value,
    lower_declared: HashSet<String>,
    always_on: HashSet<String>,
}

impl HopContext {
    fn read(hop: &DepHop) -> Self {
        let upper_toml =
            read_manifest_toml(&hop.upper_dir.join("Cargo.toml").display().to_string());
        let lower_manifest = hop.lower_dir.join("Cargo.toml").display().to_string();
        let lower_toml = read_manifest_toml(&lower_manifest);
        Self {
            upper_dir: hop.upper_dir.clone(),
            lower_dir: hop.lower_dir.clone(),
            dep_key: hop.dep_key.clone(),
            always_on: parser::edge_supplied_dep_features(&hop.edge, &lower_toml),
            lower_declared: visitor::declared_features(&lower_manifest),
            upper_toml,
        }
    }
}

/// Re-express a condition over the lower crate's features in the upper crate's,
/// across one dependency edge.
///
/// The per-atom translation `dependency_compile_error_constraints` documents,
/// reused verbatim because it is the same question asked in the same direction:
///
/// * on unconditionally — the edge names the feature in `features = [...]`, or
///   does not set `default-features = false` and the feature is in the lower
///   crate's `default` closure. Cargo enables it whatever the upper crate's own
///   features do, so the atom is `true`.
/// * otherwise — the disjunction of the upper crate's features that reach
///   `<key>/<feat>` transitively. `libm` is how nalgebra reaches `simba/libm`.
/// * nothing reaches it — substituted `false`, and the result kept only if it is
///   still satisfiable. `false` is the reading available here and the dangerous
///   one: the dependency walk rewrites dependency manifests and can turn such a
///   feature on with no feature path from the parent at all, and an unsat
///   conjunct costs the crate every covering run.
///
/// `None` means "nothing left to require": either the edge already supplies it,
/// or no reachable feature can.
fn translate_across_edge<'a>(
    ctx: &'a Context,
    cond: &Bool<'a>,
    hop: &HopContext,
) -> Option<Bool<'a>> {
    let HopContext {
        upper_toml,
        lower_declared,
        always_on,
        ..
    } = hop;

    let mut atoms: Vec<String> = feature_atoms(cond, lower_declared).into_iter().collect();
    atoms.sort();

    let mut substitutions: Vec<(Bool<'a>, Bool<'a>)> = Vec::new();
    let mut unreachable_atom = false;
    for feat in atoms {
        let from = Bool::new_const(ctx, feat.as_str());
        let to = if always_on.contains(&feat) {
            Bool::from_bool(ctx, true)
        } else {
            let enablers =
                parser::local_features_enabling_dep_feature(upper_toml, &hop.dep_key, &feat);
            if enablers.is_empty() {
                unreachable_atom = true;
                Bool::from_bool(ctx, false)
            } else {
                let vars: Vec<Bool> = enablers
                    .iter()
                    .map(|f| Bool::new_const(ctx, f.as_str()))
                    .collect();
                Bool::or(ctx, &vars.iter().collect::<Vec<_>>())
            }
        };
        substitutions.push((from, to));
    }

    let pairs: Vec<(&Bool, &Bool)> = substitutions.iter().map(|(f, t)| (f, t)).collect();
    let translated = cond.substitute(&pairs).simplify();
    if translated == Bool::from_bool(ctx, true) {
        return None;
    }
    if unreachable_atom {
        let s = z3::Solver::new(ctx);
        s.assert(&translated);
        if s.check() != z3::SatResult::Sat {
            debug!(
                "An impl requirement on {} can only be answered by a feature {} cannot reach; \
                 leaving it out",
                hop.lower_dir.display(),
                hop.upper_dir.display()
            );
            return None;
        }
    }
    Some(translated)
}

/// The shortest chain of dependency edges from `from_dir` down to each crate in
/// `wanted`, keyed by the crate's underscore-normalised **lib** name — which is
/// what rustc puts in `ImplRecord::definition_crate`, and what `[lib] name` can
/// make differ from the package name.
///
/// One breadth-first walk answers for every crate at once: the alternative is a
/// walk per defining crate per dependency, and the graph being walked is the
/// same one every time. `_deps` holds the whole transitive closure, so
/// `find_sibling_crate_dir` resolves each edge without touching the index.
///
/// Optional edges are **not** skipped, and that is the difference from
/// `dependency_compile_error_constraints`. That function asserts a requirement
/// that only holds in the configurations linking the dependency, so an optional
/// edge would impose it on builds that never compile it. Here the compiler has
/// already told us it selected an impl from the crate at the far end, so the
/// edge is linked in the configuration that matters.
fn edge_chains_to_crates(
    from_dir: &Path,
    wanted: &HashSet<String>,
) -> HashMap<String, Vec<DepHop>> {
    /// Deep enough for the chains that occur (unit-sphere's is one hop past the
    /// direct dependency) without walking an entire dependency closure when the
    /// crate is not below this edge at all.
    const MAX_DEPTH: usize = 4;

    let wanted: HashSet<String> = wanted.iter().map(|w| w.replace('-', "_")).collect();
    let mut found: HashMap<String, Vec<DepHop>> = HashMap::new();
    let mut visited: HashSet<PathBuf> = HashSet::from([from_dir.to_path_buf()]);
    let mut frontier: Vec<(PathBuf, Vec<DepHop>)> = vec![(from_dir.to_path_buf(), Vec::new())];

    for _ in 0..MAX_DEPTH {
        if found.len() == wanted.len() {
            break;
        }
        let mut next: Vec<(PathBuf, Vec<DepHop>)> = Vec::new();
        for (dir, chain) in frontier {
            let manifest = dir.join("Cargo.toml").display().to_string();
            let manifest_toml = read_manifest_toml(&manifest);
            for (dep_key, edge) in parser::dependency_edges(&manifest_toml) {
                let package = edge
                    .get("package")
                    .and_then(|p| p.as_str())
                    .unwrap_or(&dep_key)
                    .to_string();
                let Some(child) = parser::find_sibling_crate_dir(&manifest, &package) else {
                    continue;
                };
                let mut extended = chain.clone();
                extended.push(DepHop {
                    upper_dir: dir.clone(),
                    dep_key: dep_key.clone(),
                    edge: edge.clone(),
                    lower_dir: child.clone(),
                });
                let lib = parser::dep_crate_name(
                    &child.join("Cargo.toml").display().to_string(),
                    &package,
                )
                .replace('-', "_");
                if wanted.contains(&lib) {
                    found.entry(lib).or_insert(extended.clone());
                }
                if visited.insert(child.clone()) {
                    next.push((child, extended));
                }
            }
        }
        if next.is_empty() {
            break;
        }
        frontier = next;
    }
    found
}

/// Re-root a span the compiler reported against the dependency source this run
/// analysed.
///
/// The impl span comes out of rustc pointing into the registry checkout the
/// build compiled (`…/registry/src/index.crates.io-<hash>/zeroize-1.9.0/src/lib.rs`),
/// while the module tree was built over the tool's own copy
/// (`…/multiexp-0.4.0_deps/zeroize-1.9.0/src/lib.rs`). Same file, two paths, and
/// `find_condition_for_span` matches on the string. The longest path suffix that
/// exists under `dep_dir` is the crate-relative path, and it is unique — no
/// other suffix of the same string names an existing file there.
fn span_in_dep_tree(span: &ReadableSpan, dep_dir: &Path) -> Option<ReadableSpan> {
    let parts: Vec<&str> = span.file.split('/').filter(|p| !p.is_empty()).collect();
    for start in 0..parts.len() {
        let candidate = dep_dir.join(parts[start..].join("/"));
        if candidate.is_file() {
            let mut mapped = span.clone();
            mapped.file = candidate.to_string_lossy().to_string();
            return Some(mapped);
        }
    }
    None
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
/// arbitrary model, so that a candidate which would switch a std gate back on is
/// dropped before it costs a compile. totsu_core is why — `std =
/// ["num-traits/std"]` and `libm = ["num-traits/libm"]` are both "features
/// without which `Float` has no `sqrt`", and offering `std` as the fix is
/// offering to give up. The bare-metal requirement is the second guard: a
/// candidate that "fixes" the build only on the host is not a fix.
///
/// ⚠ **That world is the crate's `no_std_condition`, NOT the negation of every
/// `AlwaysStd` gate** — which is what this used to assert, and which is
/// circular. `AlwaysStd` means "std in every covering run", and this search only
/// runs when no covering run compiled for a bare-metal target: with no std-off
/// run, *every* std span is `AlwaysStd` trivially. Their gates are then the
/// crate's own no_std-path gates, and negating those forbids exactly the
/// features that would have produced the std-off run.
///
/// proptest 1.6.0 is the crate that showed it. `src/arbitrary/mod.rs:39` reads
///
/// ```ignore
/// #[cfg(any(feature = "std", feature = "alloc"))]
/// mod _alloc;
/// ```
///
/// so every span in `_alloc` carries the gate `std ∨ alloc`, whose negation is
/// `¬std ∧ ¬alloc`. `alloc` was therefore UNSAT against the constraints and
/// never even reached the candidate list — though `--no-default-features
/// --features alloc,no_std` builds the crate clean on aarch64-unknown-none, and
/// in that build `_alloc`'s spans resolve to `alloc::…` rather than to std. The
/// search was being told to hold off a gate on the strength of a verdict that
/// exists only because the search had not yet succeeded.
///
/// The no_std condition is the honest constraint in its place: it is derived
/// from the crate's own `#![no_std]` and `extern crate std` structure rather
/// than from a run, and it is the same condition the baseline no_std run is
/// solved from. It still excludes `std` wherever `std` is what the crate's
/// no_std-ness turns on, which is the case the old guard was written for.
/// Nothing is lost on the std axis either way, because a configuration that
/// links std cannot compile for a bare-metal target at all, and only a
/// bare-metal success counts here (`allow_host_fallback = false`).
///
/// `avoid_gates` remains the fallback for a crate with no condition at all,
/// where it is the only restraint available.
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
    no_std_conds: &[Bool<'a>],
) -> (Vec<String>, Option<CoveringRun>) {
    let declared = visitor::declared_features(manifest);
    if declared.is_empty() {
        return (Vec::new(), None);
    }

    // The constraints the enabler has to live under: the crate's own, plus the
    // world the answer has to be valid in. That world is the crate's no_std
    // condition when it has one — see the circularity note above — and the
    // negation of each `AlwaysStd` gate only for a crate that has none.
    //
    // Either way they are added one at a time and skipped when they conflict:
    // two spans can be gated by mutually exclusive features, and a crate can
    // carry more than one no_std condition (one per entrypoint).
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
        let restraints: Vec<Bool<'a>> = if no_std_conds.is_empty() {
            debug!("[enablers] no no_std condition; falling back to negating the std gates");
            avoid_gates.iter().map(|g| g.not()).collect()
        } else {
            no_std_conds.to_vec()
        };
        for c in restraints {
            probe.push();
            probe.assert(&c);
            let ok = probe.check() == z3::SatResult::Sat;
            probe.pop(1);
            if ok {
                probe.assert(&c);
                constraints.push(c);
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
        base,
        candidates.len(),
        candidates
    );

    search_enablers(manifest, crate_name, &base, candidates, &declared)
}

/// Features the *emitted* configuration cannot build bare-metal without (KI-30).
///
/// [`discover_build_enablers`] runs during analysis, gated on
/// [`CRATE_REACHED_BARE_METAL`] — "has this crate ever compiled bare-metal". That
/// is a fact about *a* configuration, and the one the solve goes on to emit is
/// usually a different one: mavlink-core 0.13.1 compiles with
/// `--features embedded-hal-02`, which closes the gate, and then ships a set with
/// no IO prelude at all. This is the same search asked about the set that was
/// actually emitted, and it is called only after that set has failed to build on
/// every target — so the evidence is a real build, not a prediction, and a crate
/// that builds today never reaches it.
///
/// `selection` is this crate's own features as emitted, closed over its feature
/// table (and `default` when defaults are on — the trials pass
/// `--no-default-features`, so a default that is on has to be named). It decides
/// what is *not* a candidate, and it is main-crate features only because that is
/// what the compiled-set record holds. `dep_features` is the rest of the emitted
/// `--features` list — the `<dep>/<feat>` entries — which never becomes a
/// candidate but must be in every trial, or the trial is not the configuration
/// that failed. emissary-core 0.2.0 is why: its emitted set carries
/// `lazy_static/spin_no_std`, and a trial without it dies on `E0463 can't find
/// crate for std` inside lazy_static, which says nothing about the feature under
/// test.
/// `exclude` is what the solve settled and this search must not undo: the
/// features it proved false, and the ones the crate's no_std condition forbids.
/// `std` is normally in there, and where it is not, the oracle still rules it
/// out — a configuration that links std does not compile for a bare-metal
/// target, and only a bare-metal success counts (`allow_host_fallback = false`).
///
/// Returns the empty vector when there is nothing to try or nothing works, so a
/// caller can treat "no answer" and "no search" alike.
pub fn enablers_for_selection(
    manifest: &str,
    crate_name: &str,
    selection: &HashSet<String>,
    dep_features: &[String],
    exclude: &HashSet<String>,
) -> Vec<String> {
    // The tool's own three bookkeeping features are in the rewritten manifest and
    // `declared_features` reports them, but none of them is an answer to "what
    // does this crate need to build". `custom_default_features` is the parked
    // list of edge features a *proof* took off the no_std path — turning it back
    // on is undoing the one edit this tool makes on evidence — and
    // `custom_no_std_feature_enabled` is already on wherever it belongs (R34-15
    // has five rows where it is what breaks the build). Excluded by name so they
    // do not cost trials either; sma-proto 1.1.1 spent three of its six on them.
    const SYNTHETIC: [&str; 3] = [
        consts::CUSTOM_FEATURES_DISABLED,
        consts::CUSTOM_FEATURES_ENABLED,
        consts::DEP_UNNECESSARY_FEATURES,
    ];
    let declared = visitor::declared_features(manifest);
    let mut candidates: Vec<String> = declared
        .iter()
        .filter(|f| !selection.contains(*f) && !exclude.contains(*f))
        .filter(|f| !SYNTHETIC.contains(&f.as_str()))
        // KI-36: `custom` is, by ecosystem convention (getrandom;
        // confirmed here on `clock_source`'s own `custom`, a `mod custom;`
        // declaring an `extern "C"` hook only the *final binary* can
        // define), a self-registration hook rather than an ordinary
        // cfg-gated path. `cargo build --lib` never reaches the link step
        // that would catch a missing definition, so a lib-only success with
        // `custom` on "passes" on every target regardless of whether any
        // real backend exists — the same reasoning `add_synthetic_dependency`'s
        // caller already excludes it for (KI-34). Excluded by name here too,
        // conservatively: the only cost is a missed fix on whatever
        // different, benign thing a *different* crate's own `custom` feature
        // might mean, never a false one.
        .filter(|f| f.as_str() != "custom")
        .cloned()
        .collect();
    candidates.sort();
    if candidates.is_empty() {
        debug!("[enablers] emitted set has no unselected feature to try");
        return Vec::new();
    }

    let base: Vec<String> = {
        let mut b: Vec<String> = selection.iter().cloned().collect();
        b.extend(dep_features.iter().cloned());
        b.sort();
        // `deps_args` accumulates one entry per dependency pass and repeats the
        // synthetic feature they share, so without this the trial argv carries
        // `custom_no_std_feature_enabled` three times (observed on
        // emissary-core 0.2.0). Cargo does not care; the log does.
        b.dedup();
        b
    };
    debug!(
        "[enablers] emitted set {:?} built on no target; trying {} candidate feature(s): {:?}",
        base,
        candidates.len(),
        candidates
    );
    let (found, _adopted) = search_enablers(manifest, crate_name, &base, candidates, &declared);
    found
}

/// Features already in the emitted selection with no bearing on why the build
/// failed — the flip side of [`enablers_for_selection`] (KI-30). That function
/// asks what unselected feature the build needs; every retry before it in
/// `bin/main`'s chain asks the same "what should be *added*" question. This is
/// the first that asks the opposite one — is something already selected the
/// reason it fails (R34-23).
///
/// Four confirmed shapes: `bbx-0.3.1` compiles with no features at all
/// (`parser_rules`/`track_open_tags` gate a `Box` usage nothing else needs);
/// `taffy-0.8.1` compiles dropping `detailed_layout_info` alone, keeping
/// `grid`; `chf-0.3.1` compiles dropping its own `alloc`, keeping
/// `custom_no_std_feature_enabled`; `redjubjub-0.8.0` compiles dropping its
/// own `serde` (R34-15's item 11 residue — `serde` only reached
/// `non_minimalizable` because an unrelated `#[cfg(feature = "serde")]`
/// import exists somewhere in the crate, and once selected its own no_std
/// solve parked `serde/alloc`/`serde/rc` in `custom_no_std_feature_enabled`).
/// In each, the failing line sits behind a feature that *is* selected, and a
/// strict subset of the emitted selection builds clean.
///
/// `selection` is the emitted set's main-crate features, closed over the
/// crate's feature table exactly as [`enablers_for_selection`] builds it;
/// `dep_features` is the `<dep>/<feat>` half. Held fixed the same way *unless*
/// a pair's `<dep>` is itself one of `selection`'s features — cargo links
/// `<dep>` the moment any `<dep>/<feat>` reference exists on the command line,
/// so a pair like `serde/alloc` would silently re-link `serde` in every trial
/// that tries dropping the main crate's own `serde`, making that
/// configuration untestable no matter how the search shrinks `candidates`.
/// Such a pair travels with its `<dep>` candidate instead: present in a trial
/// only when `<dep>` is still in `keep`. Returns the main-crate features that
/// were dropped, and separately the `dep_features` entries tied to them —
/// both empty when the emitted set is already minimal or no subset compiles.
/// The caller has to drop both: dropping only the main feature and reusing
/// the original `dep_features` for the real rebuild reintroduces exactly the
/// dependency the search just proved removable.
///
/// **Boundary:** a flat removal only. `kitoken-0.10.1` needs `convert` (a
/// superfeature) replaced by three of its five sub-features, not dropped
/// outright — expanding a selected superfeature into its declared members and
/// searching there is future work. This function cannot reach that repair,
/// and correctly returns nothing for it: dropping `convert` whole loses
/// conversion support entirely, which is not the same repair and is not
/// attempted.
fn dep_names_reached(feat: &str, features: &[(String, TupleVec)]) -> HashSet<String> {
    if let Some((dep, _)) = feat.split_once('/') {
        return HashSet::from([dep.to_string()]);
    }
    let mut seen = HashSet::new();
    let mut reached = HashSet::new();
    let mut stack = vec![feat.to_string()];
    while let Some(f) = stack.pop() {
        if !seen.insert(f.clone()) {
            continue;
        }
        let Some((_, values)) = features.iter().find(|(name, _)| *name == f) else {
            continue;
        };
        for (k, v) in values {
            if k == v {
                // A plain feature reference (`read_local_features` reports it
                // as `(name, name)`) — recurse into what *it* reaches.
                stack.push(v.clone());
            } else {
                // `k` is the dependency name either way: `dep:` (bare link) or
                // a sub-feature both name the dependency in the tuple's first
                // slot (see `downloader::read_local_features`).
                reached.insert(k.clone());
            }
        }
    }
    reached
}

pub fn search_removals(
    manifest: &str,
    crate_name: &str,
    selection: &HashSet<String>,
    dep_features: &[String],
) -> (Vec<String>, Vec<String>) {
    // Same bookkeeping exclusion as `enablers_for_selection`: these are never
    // an answer to "what does this crate need", so never a candidate to drop
    // either — dropping `custom_no_std_feature_enabled` would undo a parked
    // repair the tool already made on evidence, not test anything new.
    const SYNTHETIC: [&str; 3] = [
        consts::CUSTOM_FEATURES_DISABLED,
        consts::CUSTOM_FEATURES_ENABLED,
        consts::DEP_UNNECESSARY_FEATURES,
    ];
    let mut candidates: Vec<String> = selection
        .iter()
        .filter(|f| !SYNTHETIC.contains(&f.as_str()))
        .cloned()
        .collect();
    candidates.sort();
    if candidates.is_empty() {
        debug!("[removals] emitted set has no removable main-crate feature");
        return (Vec::new(), Vec::new());
    }

    // A `dep_features` entry links a dependency regardless of whatever the
    // main crate's own feature of that name says — that is what `<dep>/<feat>`
    // means to cargo, whether it is spelled out directly (`rand_core/alloc`)
    // or reached through a *plain* feature name (`custom_no_std_feature_enabled
    // = ["serde/alloc", "serde/rc"]`, parked there by a dependency's own no_std
    // solve). Treating the whole of `dep_features` as a fixed base, as every
    // trial below used to, is right when the dependency it reaches is outside
    // `candidates`, but wrong when it is itself one of them: dropping `serde`
    // from `keep` while `custom_no_std_feature_enabled` stays in the base
    // re-links the exact dependency the trial is trying to remove
    // (`redjubjub-0.8.0` — `serde` only reached `non_minimalizable` because an
    // unrelated `#[cfg(feature = "serde")]` import exists somewhere in the
    // crate, and once selected its own no_std solve parked those two pairs
    // under the synthetic feature; no trial that keeps it can ever test "no
    // serde at all", which is the one configuration that builds). So an entry
    // that reaches a candidate travels with it instead of riding in every
    // trial — `dep_names_reached` resolves a literal pair directly and a plain
    // feature name by walking this crate's own `[features]` table the same way
    // `solver::all_enabled_for_feat` does for the enable direction.
    let manifest_toml = read_manifest_toml(manifest);
    let feature_table = downloader::read_local_features(&manifest_toml);
    let mut tied: HashMap<String, Vec<String>> = HashMap::new();
    let mut fixed_base: Vec<String> = Vec::new();
    for feat in dep_features {
        let reached = dep_names_reached(feat, &feature_table);
        match candidates.iter().find(|c| reached.contains(c.as_str())) {
            Some(cand) => tied.entry(cand.clone()).or_default().push(feat.clone()),
            None => fixed_base.push(feat.clone()),
        }
    }

    let base = fixed_base;
    let budget = std::cell::Cell::new(MAX_ENABLER_PROBES);
    let pinned = std::cell::Cell::new(false);
    let compiles = |keep: &[String]| -> bool {
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
        for cand in keep {
            if let Some(extra) = tied.get(cand) {
                feats.extend(extra.iter().cloned());
            }
        }
        feats.extend(keep.iter().cloned());
        let trial = timing::scope("removal_trial", keep.join(","));
        let outcome = run_rustc_plugin_pass_with(manifest, crate_name, &feats, None, false, pin);
        let ok = matches!(outcome, PassOutcome::Success { .. });
        trial.meta("compiles", ok.to_string());
        drop(trial);
        debug!(
            "[removals] {} keeping {:?}",
            if ok { "compiles" } else { "fails" },
            keep
        );
        ok
    };

    // The base alone — dep features, nothing from the main crate's own
    // selection — is the emptiest trial there is. If that already compiles,
    // none of `candidates` was needed at all: `bbx-0.3.1`'s shape.
    if compiles(&[]) {
        debug!("[removals] dep features alone compile; every selected main feature is removable");
        let removed_dep_feats: Vec<String> = tied.into_values().flatten().collect();
        return (candidates, removed_dep_feats);
    }

    // `base ∪ candidates` is the configuration that already failed — that
    // failure is the reason this runs, so it is not retried. Shrink from
    // there: halving first (cheap when one feature is the whole problem),
    // then removal one at a time.
    let mut keep = candidates.clone();
    while keep.len() > 1 && budget.get() > 0 {
        let mid = keep.len() / 2;
        let left: Vec<String> = keep[..mid].to_vec();
        let right: Vec<String> = keep[mid..].to_vec();
        if compiles(&left) {
            keep = left;
        } else if compiles(&right) {
            keep = right;
        } else {
            // The needed subset straddles the split; the removal pass below
            // finishes it.
            break;
        }
    }
    for cand in keep.clone() {
        if budget.get() == 0 {
            break;
        }
        let trial: Vec<String> = keep.iter().filter(|f| **f != cand).cloned().collect();
        if compiles(&trial) {
            keep = trial;
        }
    }

    if keep.len() == candidates.len() {
        debug!("[removals] no subset of the emitted selection compiles; nothing to drop");
        return (Vec::new(), Vec::new());
    }
    let removed: Vec<String> = candidates
        .into_iter()
        .filter(|f| !keep.contains(f))
        .collect();
    let removed_dep_feats: Vec<String> = removed
        .iter()
        .filter_map(|f| tied.get(f))
        .flat_map(|v| v.iter().cloned())
        .collect();
    debug!(
        "[removals] crate builds dropping {:?} (and dep-feature(s) {:?})",
        removed, removed_dep_feats
    );
    (removed, removed_dep_feats)
}

/// The compile-and-shrink half of [`discover_build_enablers`], over a base set
/// and candidate list the caller has already chosen.
///
/// Split out so the same search can be run from a set that is *not* a Z3 model.
/// [`discover_build_enablers`] solves its base out of the crate's constraints
/// during analysis, which is the only set available then; the post-failure retry
/// in `bin/main` starts from the configuration that was actually emitted and
/// actually failed (KI-30). Everything below — the all-on trial, the
/// optional-dep retry, the halving and removal passes, the adopted run — is
/// common to both and cares only that `base` compiles or does not.
fn search_enablers(
    manifest: &str,
    crate_name: &str,
    base: &[String],
    candidates: Vec<String>,
    declared: &HashSet<String>,
) -> (Vec<String>, Option<CoveringRun>) {
    let budget = std::cell::Cell::new(MAX_ENABLER_PROBES);
    // Only the first trial is allowed to sweep `TARGET_LIST` looking for a triple
    // that works; after that every trial is pinned to one. A trial that succeeds
    // sets `LAST_GOOD_TARGET` and pins itself; a trial that fails leaves it as it
    // was, and without this each subsequent failure could cost another 26 builds.
    // `TARGET_LIST[0]` is the arbitrary-but-fixed stand-in for a still-empty cache
    // — a crate that builds bare-metal at all almost always builds for most
    // triples, and if this one is wrong the search just reports nothing, which is
    // where it would have been anyway.
    //
    // For a dependency the cache is normally already warm, set by the main crate or
    // an earlier sibling, so even the sweeping first trial resolves to that one
    // triple inside `run_rustc_plugin_pass_with`. That is a borrowed answer rather
    // than this crate's own, and it carries the same risk `TARGET_LIST[0]` does:
    // wrong triple, empty search, no worse than not running.
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
        let mut feats = base.to_vec();
        feats.extend(extra.iter().cloned());
        let trial = timing::scope("enabler_trial", extra.join(","));
        let outcome = run_rustc_plugin_pass_with(manifest, crate_name, &feats, None, false, pin);
        let ok = matches!(outcome, PassOutcome::Success { .. });
        if let PassOutcome::Success { full_output, .. } = outcome {
            trial.meta("records", full_output.records.len().to_string());
            last_success = Some(CoveringRun {
                features: feats,
                output: full_output,
                // The search runs with `allow_host_fallback = false`, so a trial
                // that compiled did so for a bare-metal target.
                std_inconclusive: false,
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
        // that cannot be on together, or one that breaks the build by itself.
        //
        // The commonest such reason is measurable from the manifest rather than
        // guessed at: a candidate that links an OPTIONAL DEPENDENCY puts a new
        // crate into the graph, and that crate has to compile bare-metal too. One
        // std-only optional dep then vetoes the whole set. Retrying without them
        // costs one probe and keeps every candidate that cannot introduce a crate.
        //
        // proptest 1.6.0 is the crate that needed it. Its no_std build is the PAIR
        // `alloc` + `no_std` — `alloc` alone dies on `num_traits::float::Float`,
        // `no_std` alone on `cannot find macro `vec``, so no one-at-a-time search
        // can ever find it — while all-on drags in bit-vec, lazy_static,
        // rusty-fork, tempfile and fnv. Dropping the dep-linking candidates leaves
        // `[alloc, atomic64bit, no_std, unstable]`, which compiles, and the two
        // passes below shrink that to `[alloc, no_std]`.
        let dep_adding = std::fs::read_to_string(manifest)
            .ok()
            .and_then(|s| toml::from_str::<toml::Value>(&s).ok())
            .map(|toml| downloader::dep_adding_features(&toml, declared))
            .unwrap_or_default();
        let no_new_crates: Vec<String> = candidates
            .iter()
            .filter(|c| !dep_adding.contains(*c))
            .cloned()
            .collect();
        let without_deps = if no_new_crates.is_empty() || no_new_crates.len() == candidates.len() {
            // Nothing to subtract — either every candidate links a dep or none
            // does, and in both cases this probe would repeat the all-on trial.
            None
        } else {
            debug!(
                "[enablers] all-on failed; retrying without the {} candidate(s) that link an optional dep: {:?}",
                candidates.len() - no_new_crates.len(),
                candidates
                    .iter()
                    .filter(|c| dep_adding.contains(*c))
                    .collect::<Vec<_>>()
            );
            compiles(&no_new_crates).then_some(no_new_crates)
        };

        match without_deps {
            Some(set) => set,
            None => {
                // Fall back to trying each alone; the shrink then has nothing to do.
                debug!("[enablers] every candidate on does not compile; trying them one at a time");
                let single = candidates
                    .iter()
                    .find(|c| compiles(std::slice::from_ref(*c)))
                    .cloned();
                match single {
                    Some(c) => vec![c],
                    None => {
                        debug!(
                            "[enablers] no candidate makes the crate build for a bare-metal target"
                        );
                        return (Vec::new(), None);
                    }
                }
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

    // High-water mark, not an assignment and not a sum. `classify_and_split` runs
    // twice for one crate when the enabler search adopts a run (over the same set
    // plus one, so adding them would double-count), and one `Telemetry` is shared
    // by the main crate and every dependency analysed after it — a plain
    // assignment lets the last dependency's zero erase the main crate's count.
    let inconclusive = covering_runs.iter().filter(|r| r.std_inconclusive).count();
    telemetry.std_inconclusive_runs = telemetry.std_inconclusive_runs.max(inconclusive);
    if inconclusive > 0 {
        debug!(
            "{} of {} covering run(s) compiled only on the host with every bare-metal attempt \
             failing inside a dependency — their std records are not counted",
            inconclusive,
            covering_runs.len()
        );
    }

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
    Vec<ImplRecord>,
    Vec<CrossCrateItem>,
) {
    let (root, mut covering_runs, mut hard_constraints, compile_error_constraints, no_std_conds) =
        find_feature_combs_for_all_code(ctx, manifest, crate_name, telemetry);

    // A routeless bare std use (e.g. a `HashMap` brought in by a glob re-export of
    // an externally-gated `use std::collections::HashMap`) carries no route back
    // to the import `resolve_local_facade_gateways` needs. Join it to that import
    // by `def_path` and inherit the gate. Needs the module tree (`root`) to know
    // which imports are externally gated, so it runs here rather than at load time.
    if !ablation::flags().no_gateway_resolution {
        for run in &mut covering_runs {
            telemetry.routed_import_anchors +=
                resolve_import_to_use_gateways(&mut run.output, &root);
        }
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

    // KI-22, reported off this pass rather than a build of its own: a proc macro
    // the manifest has no edge to keeps its `std` whatever the parking does.
    if let Some(ref output) = default_features_output {
        report_unreachable_proc_macro_injectors(output, manifest, crate_name, telemetry);
    }

    let all_constraints = visitor::collect_all_items(&root, ctx);

    let (mut analyses, mut always_std_imports, mut always_std_others) =
        classify_and_split(&covering_runs, crate_name, telemetry);

    // `CRATE_REACHED_BARE_METAL` is still false only when not one covering run of
    // *this* crate compiled for a bare-metal target — every record above came from
    // the host fallback. Probing from here is doomed: each probe compiles the same
    // way and comes back `CompileFailed`, so look for the feature the crate needs
    // to build at all before spending them.
    //
    // This used to read `LAST_GOOD_TARGET`, which answers a different question:
    // has *anything* compiled bare-metal in this process. `analyze_crate` runs for
    // every dependency too, so the first crate that linked switched the search off
    // for every crate after it, and the main crate — analysed first, before any
    // cache exists — was in practice the only crate it ever ran for (KI-28). The
    // unit-sphere 0.4.0 run's timing shows the shape plainly: 19 `analyze` calls,
    // one `build_enablers` call, 16 ms, which was the featureless main crate
    // returning on `declared.is_empty()`.
    //
    // It used to be gated on there also being an `AlwaysStd` span to probe, on the
    // reasoning that those are the only spans a failed probe turns into `unproven`
    // — a crate with nothing to prove has nothing to gain. True of the probing
    // stage and false of the emission stage, which is the whole of R31-5's larger
    // half. euclid 0.22.11 is `#![cfg_attr(not(test), no_std)]` with no `extern
    // crate std` anywhere, so its no_std condition is `true` and it has not one std
    // span; its `libm = ["num-traits/libm"]` gates no code of its own, so no
    // covering set ever contains it, every run dies on `unresolved import
    // num_traits::real`, and the crate shipped `--no-default-features` — while
    // `--no-default-features --features libm` builds it clean on
    // `aarch64-unknown-none`. What the search finds is not evidence about a span,
    // it is a hard constraint on the feature solve, and that is worth having
    // whether or not there is anything left to prove. 45 of R31-5's 48 crates are
    // this shape (`libm`, `alloc`), none of them with a span to their name.
    //
    // Skipped outright for a crate that already reached bare metal, which is the
    // overwhelming majority. When it does run and fails, the cost is one probe's
    // worth of builds; when it succeeds it also fixes `LAST_GOOD_TARGET`, so every
    // probe after it stops sweeping all 26 targets.
    //
    // Cheaper for a dependency than for a main crate, which is why widening the
    // gate is affordable. `MAX_ENABLER_PROBES` bounds the trials either way, but
    // only the first is allowed to sweep `TARGET_LIST`, and by the time a dep is
    // analysed `LAST_GOOD_TARGET` is normally set by the main crate or a sibling —
    // so `run_rustc_plugin_pass_with` resolves even that first trial to the one
    // cached triple. With `allow_host_fallback = false` that is one build per
    // trial. A dep with no `[features]` still costs nothing: `declared.is_empty()`
    // returns before any build.
    // Local, not read back off `telemetry`: one `Telemetry` is shared by the main
    // crate and every dependency analysed after it, so recovering the list from
    // there hands the main crate's `libm` to each dep's solve — and a dep that has
    // no such feature emits it as `<dep>/libm` in `custom_no_std_feature_enabled`,
    // which cargo rejects outright (observed on totsu_core → `log/libm`).
    let mut build_enablers: Vec<String> = Vec::new();
    if !*CRATE_REACHED_BARE_METAL.lock().unwrap() {
        // The gates the prober is about to negate. Passed in so the search never
        // proposes a feature that satisfies one of them — `std` is otherwise a
        // perfectly good answer to "what makes this crate compile".
        let avoid_gates: Vec<Bool> = always_std_imports
            .iter()
            .map(|a| &a.exemplar)
            .chain(always_std_others.iter().map(|a| &a.exemplar))
            .filter_map(|ex| {
                ancestors_for_record(&root, ex).or_else(|| {
                    macro_body_cfgs_to_ancestors(ctx, &ex.macro_body_cfgs, &known_features)
                })
            })
            .flatten()
            .collect();
        let enabler_run;
        (build_enablers, enabler_run) = {
            let _t = timing::scope("build_enablers", crate_name);
            discover_build_enablers(
                ctx,
                manifest,
                crate_name,
                &hard_constraints,
                &avoid_gates,
                &no_std_conds,
            )
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
            if !ablation::flags().no_gateway_resolution {
                telemetry.routed_import_anchors +=
                    resolve_import_to_use_gateways(&mut run.output, &root);
            }
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
            ancestors: ancestors_for_record(&root, &a.exemplar).or_else(|| {
                macro_body_cfgs_to_ancestors(ctx, &a.exemplar.macro_body_cfgs, &known_features)
            }),
            externally_gated: span_externally_gated(&root, &a.exemplar),
        })
        .collect::<Vec<_>>();

    let mut hard_imports = probe_candidates(
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
            ancestors: ancestors_for_record(&root, &a.exemplar).or_else(|| {
                macro_body_cfgs_to_ancestors(ctx, &a.exemplar.macro_body_cfgs, &known_features)
            }),
            externally_gated: span_externally_gated(&root, &a.exemplar),
        })
        .collect::<Vec<_>>();

    debug!(
        "Probe candidates (other usages): {:#?}",
        probe_candidates_usages
    );

    let mut hard_usages = probe_usages(
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
            ancestors: ancestors_for_record(&root, &a.exemplar).or_else(|| {
                macro_body_cfgs_to_ancestors(ctx, &a.exemplar.macro_body_cfgs, &known_features)
            }),
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

    let (explained, conditional_targets): (Vec<_>, Vec<_>) = conditional_candidates
        .into_iter()
        .partition(|t| !t.externally_gated && t.ancestors.is_some() && explains(t).is_some());

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

    // KI-3: a feature this crate's own `compile_error!` uses to pick a backend
    // gets forbidden here if that backend's optional dependency has no no_std
    // attribute at its crate root — see
    // `compile_error_infeasible_backend_constraints` for which of the entry's
    // three options this is and why. Added one candidate at a time, keeping
    // each only if the running conjunction with `final_condition` stays
    // satisfiable: a crate whose disjunction has no reachable answer under
    // this check must be left exactly as it was (unproven, not unsat)
    // rather than handed an `all_hard` nothing can satisfy.
    // Ablation study §3.4: skip folding in the backend-forbidding
    // constraints entirely when disabled, leaving `final_condition` as the
    // probe-conditions-and-build-enablers fold above computed it.
    let final_condition = if ablation::flags().no_compile_error_constraints {
        final_condition
    } else {
        let manifest_toml = read_manifest_toml(manifest);
        compile_error_infeasible_backend_constraints(ctx, manifest, &manifest_toml)
            .into_iter()
            .fold(final_condition, |acc, forbid| {
                let candidate = match &acc {
                    Some(a) => Bool::and(ctx, &[a, &forbid]),
                    None => forbid.clone(),
                };
                let solver = z3::Solver::new(ctx);
                solver.assert(&candidate);
                if solver.check() == z3::SatResult::Sat {
                    Some(candidate.simplify())
                } else {
                    debug!(
                        "KI-3: forbidding {:?} would make this crate's hard constraints unsat; leaving it out",
                        forbid
                    );
                    acc
                }
            })
    };

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

    // Not one covering run ever put this crate in a no_std environment: every one
    // compiled only on the host, after a dependency had already failed on bare
    // metal. The probe already refuses to call such a run's "still std" proof of
    // hardness — but an *ungated* span never reaches the probe at all
    // (`initial_ungated_results` short-circuits it to `StillStd` without
    // compiling), so it would still be reported on the strength of those runs
    // alone. Where the crate did not spell `std` itself, the std in the record
    // came through a dependency's re-export or a dependency's macro, and that is
    // the dependency's configuration talking: bitstream-io's residue is 30 ×
    // `writer.write_all(…)` against `W: io::Write`, where `io` is `core2::io`,
    // and bp-wococo's is one `decl_bridge_finality_runtime_apis!(…)` that
    // bp_runtime expands to `std::result::Result`. Unproven, not hard.
    //
    // Conditioned on *every* run being inconclusive, not on any of them being so.
    // A crate with even one bare-metal run has a real witness, and discounting
    // records in that case silences genuine std — main_tests caught assertr 0.4.3
    // (ungated `use std::marker::PhantomData`) and tinywasm-parser 0.8.0
    // (`impl crate::std::error::Error`) when an earlier version of this tried it.
    // Spans downgraded below that are then excused from `unproven` entirely —
    // R34-3, `host_only_downgrade_is_safe`. The decision stays `CompileFailed`
    // (so `all_hard`/`final_condition`/`compile_failed_spans` are untouched);
    // only the later `unproven` collection subtracts this set, so a crate whose
    // only unproven spans are all excused still gets a configuration emitted
    // instead of the fatal exit at `bin/main.rs`.
    let mut host_only_excused: HashSet<ReadableSpan> = HashSet::new();
    if !covering_runs.is_empty() && covering_runs.iter().all(|r| r.std_inconclusive) {
        let mut downgraded = 0usize;
        for result in hard_imports.iter_mut().chain(hard_usages.iter_mut()) {
            if matches!(result.decision, ProbeDecision::StillStd { .. })
                && !crate_named_std_in_path(&result.target.analysis.exemplar)
            {
                debug!(
                    "'{}' at {:?}{} is std only in runs that never left the host; \
                     unproven, not hard",
                    result.target.analysis.exemplar.path_text,
                    result.target.analysis.span,
                    match result.target.analysis.exemplar.expansion_crate.as_deref() {
                        Some(krate) => format!(" (written by `{krate}`'s macro)"),
                        None => String::new(),
                    }
                );
                if host_only_downgrade_is_safe(&result.target.analysis.exemplar) {
                    debug!(
                        "'{}' excused as a don't-care: definition_crate={:?}, \
                         is_float_primitive_method={}",
                        result.target.analysis.exemplar.path_text,
                        result.target.analysis.exemplar.definition_crate,
                        result.target.analysis.exemplar.is_float_primitive_method
                    );
                    host_only_excused.insert(result.target.analysis.span.clone());
                }
                result.decision = ProbeDecision::CompileFailed {
                    reason: format!(
                        "'{}' is std only in runs that never left the host — no covering run \
                         compiled for a bare-metal target, and this crate's own source does not \
                         name std here",
                        result.target.analysis.exemplar.path_text
                    ),
                };
                downgraded += 1;
            }
        }
        if downgraded > 0 {
            debug!(
                "{} span(s) downgraded to unproven: no covering run compiled for a bare-metal \
                 target and none of them names std in this crate's own source ({} excused as \
                 don't-cares)",
                downgraded,
                host_only_excused.len()
            );
        }
    }

    let compile_failed_spans = hard_imports
        .iter()
        .chain(hard_usages.iter())
        .chain(conditional_results.iter())
        .filter(|a| matches!(a.decision, ProbeDecision::CompileFailed { .. }))
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
    // Unfiltered: `host_only_excused` spans stay in here too, so nothing that
    // was ever std-in-a-host-run vanishes from `unproven_std_usages.json` —
    // the O-16 guard (`std_a_dependencys_macro_wrote_is_not_this_crate_naming_std`)
    // is about exactly this, for a span this excusal would also cover. What the
    // excused set changes is only whether *this crate's* `unproven` count blocks
    // emission — decided at the one call site that reads it, `bin/main.rs`.
    let unproven: Vec<ReadableSpan> = hard_imports
        .iter()
        .chain(hard_usages.iter())
        .filter(|a| matches!(a.decision, ProbeDecision::CompileFailed { .. }))
        .map(|a| a.target.analysis.span.clone())
        .collect();
    telemetry.host_only_excused_spans = host_only_excused.len();
    // Why, in the compiler's own words, deduplicated: one gate's failure covers
    // every span behind it, and a crate with 300 spans behind two gates has two
    // things to say, not 300. Kept in first-seen order so the first line is the
    // first thing the probing found.
    let mut unproven_reasons: Vec<String> = Vec::new();
    for result in hard_imports.iter().chain(hard_usages.iter()) {
        if let ProbeDecision::CompileFailed { reason } = &result.decision
            && !unproven_reasons.contains(reason)
        {
            unproven_reasons.push(reason.clone());
        }
    }
    if !unproven.is_empty() {
        debug!(
            "{} std span(s) could not be proven avoidable: every feature set negating their gate failed to compile",
            unproven.len()
        );
        for reason in &unproven_reasons {
            debug!("  unproven because: {}", reason);
        }
    }
    telemetry.unproven_std_spans = unproven.len();
    telemetry.unproven_std_span_reasons = unproven_reasons;

    let all_hard: Vec<ReadableSpan> = hard_imports
        .into_iter()
        .chain(hard_usages)
        .filter(|a| matches!(a.decision, ProbeDecision::StillStd { .. }))
        .map(|f| f.target.analysis.span)
        .collect();

    // Taken before the runs are consumed below, and filtered here rather than by
    // the caller because `final_condition` — the crate's no_std condition — is
    // exactly the `hard` both of these tests need and it is live at this point.
    let observed_impls = impls_from_no_std_passes(
        ctx,
        manifest,
        final_condition.as_ref(),
        &covering_runs,
        default_features_output.as_ref(),
    );
    let impl_records =
        reachable_impl_records(&root, &observed_impls, final_condition.as_ref(), ctx);
    debug!(
        "[impl_req] {} obligation record(s) from no_std-consistent passes,          {} at call sites the emitted configuration compiles",
        observed_impls.len(),
        impl_records.len()
    );

    // The same two steps for the items the crate *names* (R34-6), taken here for
    // the same reason: `final_condition` is the `hard` both tests need and it is
    // live at this point, and the runs are consumed just below.
    let observed_paths = paths_from_no_std_passes(
        ctx,
        manifest,
        final_condition.as_ref(),
        &covering_runs,
        default_features_output.as_ref(),
    );
    let path_items = reachable_path_items(&root, &observed_paths, final_condition.as_ref(), ctx);
    debug!(
        "[path_req] {} cross-crate item reference(s) from no_std-consistent passes,          {} at use sites the emitted configuration compiles",
        observed_paths.len(),
        path_items.len()
    );

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
        impl_records,
        path_items,
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

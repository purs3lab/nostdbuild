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

fn load_plugin_output(path: &Path) -> Result<FeatureRunOutput, String> {
    let data = fs::read_to_string(path).map_err(|e| format!("read {:?}: {}", path, e))?;
    serde_json::from_str(&data).map_err(|e| format!("parse {:?}: {}", path, e))
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
/// `usage_crate`.
///
/// Only `extern crate` declarations (not glob `use` imports) are used as
/// anchors to avoid false positives from unconditional `use std::SomeType`
/// imports that happen to live in a module that also handles non-std paths.
pub fn resolve_local_facade_gateways(out: &mut FeatureRunOutput) {
    // Build: module_path → gateway crate names, from extern crate declarations.
    // A module may declare multiple extern crates; collect all so any prefix
    // match on a local_route can find the right one.
    let mut module_extern_crates: std::collections::HashMap<String, Vec<String>> =
        std::collections::HashMap::new();

    for r in &out.records {
        if r.is_extern_crate
            && let Some(dm) = r.defining_module.as_deref()
        {
            module_extern_crates
                .entry(dm.to_string())
                .or_default()
                .push(r.definition_crate.clone());
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
        let found = (1..=segments.len()).rev().any(|len| {
            let prefix = segments[..len].join("::");
            module_extern_crates
                .get(&prefix)
                .is_some_and(|crates| crates.iter().any(|c| c == "std"))
        });

        if found {
            debug!(
                "For record with local_route '{}' and span {:?}, found std gateway in ancestors",
                local_route, r.span
            );
            r.span.usage_crate = Some("std".to_string());
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

    let args = [
        "hir",
        "--",
        "--manifest-path",
        manifest,
        "--no-default-features",
        "--features",
        &feats,
    ];

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

        let mut collector = ModCollector::new(ctx);
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
                let full_path = crate_root.join(filename);
                let file_dir = full_path.parent().unwrap_or(Path::new("."));
                let rs = file_dir.join(format!("{}.rs", modname));
                let mod_rs = file_dir.join(modname).join("mod.rs");
                let mod_path = if rs.exists() { rs } else { mod_rs };
                if !mod_path.exists() {
                    debug!("No source file for module {}", modname);
                    continue;
                }

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
    Vec<PathRecord>,
) {
    let manifest = parser::determine_manifest_file(crate_name, main_name);
    analyze_crate(ctx, &manifest, crate_name, telemetry)
}

/// Converts the raw `#[cfg(…)]` strings stored in `PathRecord::macro_body_cfgs`
/// into Z3 Bool ancestors, reusing the existing `parse_main_attributes_direct`
/// path.  Returns `None` when the list is empty (so callers can chain with
/// `or_else`).
fn macro_body_cfgs_to_ancestors<'a>(ctx: &'a Context, cfgs: &[String]) -> Option<Vec<Bool<'a>>> {
    if cfgs.is_empty() {
        return None;
    }



    let bools: Vec<Bool<'a>> = cfgs
        .iter()
        .filter_map(|s| {
            use syn::parse::Parser;
            let attrs = syn::Attribute::parse_outer.parse_str(s).ok()?;
            let attr = attrs.into_iter().next()?;
            let (bool_opt, _) = parser::parse_main_attributes_direct(&attr, ctx);
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
    Vec<PathRecord>,
) {
    let (root, covering_runs, hard_constraints, compile_error_constraints) =
        find_feature_combs_for_all_code(ctx, manifest, crate_name, telemetry);

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
                .or_else(|| macro_body_cfgs_to_ancestors(ctx, &a.exemplar.macro_body_cfgs)),
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
                .or_else(|| macro_body_cfgs_to_ancestors(ctx, &a.exemplar.macro_body_cfgs)),
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
                .or_else(|| macro_body_cfgs_to_ancestors(ctx, &a.exemplar.macro_body_cfgs)),
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

    let all_hard: Vec<ReadableSpan> = hard_imports
        .into_iter()
        .chain(hard_usages)
        .filter(|a| matches!(a.decision, ProbeDecision::StillStd { .. }))
        .map(|f| f.target.analysis.span)
        .collect();

    let covering_records: Vec<PathRecord> = covering_runs
        .iter()
        .flat_map(|run| run.output.records.iter().cloned())
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

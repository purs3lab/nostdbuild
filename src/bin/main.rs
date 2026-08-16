#![feature(rustc_private)]

use std::collections::HashSet;

use anyhow::Ok;
use clap::Parser;
use log::debug;

use nostd::{Attributes, compiler, consts, db, downloader, driver, parser, solver, timing};

#[derive(Parser, Debug)]
#[command(author, about)]
struct Cli {
    #[arg(long)]
    url: Option<String>,

    #[arg(long)]
    name: Option<String>,

    #[arg(long)]
    target: Option<String>,

    #[arg(long)]
    version: Option<String>,

    #[arg(long)]
    dry_run: bool,

    #[arg(long)]
    depth: Option<u32>,

    /// Whether the final recursive dep check should run.
    #[arg(long)]
    no_recursive: bool,
}

/// Assemble the cargo flags for one feature selection: `--no-default-features`
/// when defaults are off, plus the merged `--features` list.
///
/// Returns the flags, the merged feature groups (what the `compile_error!` check
/// reads), and the feature count for telemetry. Both inputs are taken by
/// reference and copied so the same selection can be assembled twice — the
/// KI-11 retry re-assembles with a reduced `main_features` and needs the
/// original `deps_args`, not the first call's filtered leftovers.
fn assemble_final_args(
    disable_default: bool,
    main_features: &[String],
    deps_args: &[String],
) -> (Vec<String>, Vec<String>, usize) {
    let mut final_args = Vec::new();
    let mut combined_features = Vec::new();
    let mut final_features_len = main_features.len();
    let mut deps_args = deps_args.to_vec();
    let main_feature_string = main_features.join(",");

    if !deps_args.is_empty() {
        if !main_feature_string.is_empty() {
            deps_args.retain(|x| !main_feature_string.contains(x));
        }
        deps_args.sort();
        deps_args.dedup();
    }
    final_features_len += deps_args.len();

    if disable_default {
        final_args.push("--no-default-features".to_string());
    }

    if !main_feature_string.is_empty() {
        combined_features.push(main_feature_string);
    }
    if !deps_args.is_empty() {
        combined_features.push(deps_args.join(","));
    }
    if !combined_features.is_empty() {
        combined_features.sort();
        combined_features.dedup();
        final_args.push("--features".to_string());
        final_args.push(combined_features.join(","));
    }
    (final_args, combined_features, final_features_len)
}

/// Drop the features `should_skip_dep` turned off from the lists that become the
/// command line. Drains, so a second call after the retry loop only handles what
/// that loop added.
fn apply_forced_off(
    forced_off: &mut Vec<String>,
    main_features: &mut Vec<String>,
    enable: &mut Vec<String>,
) {
    if forced_off.is_empty() {
        return;
    }
    println!(
        "Features turned off because they enable a dependency the crate still names: {:?}",
        forced_off
    );
    main_features.retain(|f| !forced_off.contains(f));
    enable.retain(|f| !forced_off.contains(f));
    forced_off.clear();
}

fn process_dep_crate_wrapper(
    exchange: &mut nostd::DataExchange,
    dep: &mut Attributes,
    dep_and_feats: &mut nostd::types::TupleVec,
    main_features: &mut Vec<String>,
    disable_default: &mut bool,
    enable: &mut Vec<String>,
    deps_args: &mut Vec<String>,
    previously_disabled: &mut HashSet<String>,
    non_minimalizable: &HashSet<String>,
    deps_to_keep: &HashSet<String>,
) -> anyhow::Result<()> {
    let _t = timing::crate_scope("dep_analysis", &dep.crate_name);
    // Check the DB first: if we already have a result for this dep, skip the expensive
    // gather_crate_info + analyze_crate_wrapper + process_crate path entirely.
    let (local_dep_args, dep_disable, dep_enable) =
        if let Some(db_entry) = db::get_from_db_data(&exchange.db_data, &dep.crate_name) {
            debug!(
                "DB hit for dependency {}, skipping analysis",
                dep.crate_name
            );
            let (enable, disable) = (db_entry.features.0.clone(), db_entry.features.1.clone());
            // DB hit — no dep_root available; pass empty map (no protection check for
            // this dep). `None` for the entailed-false set for the same reason: the DB
            // stores the (enable, disable) pair only, so removals fall back to
            // `disable` and this path behaves exactly as it did before.
            parser::finalize_dep_crate(
                exchange,
                dep,
                enable,
                disable,
                None,
                std::collections::HashMap::new(),
            )?
        } else {
            parser::process_dep_crate(exchange, dep)?
        };

    println!(
        "Dependency {} enable features: {:?}, disable features: {:?}",
        dep.crate_name, dep_enable, dep_disable
    );

    deps_args.extend(local_dep_args);

    let crate_name = dep.crate_name.split(":").next().unwrap_or_default();

    // Only genuine `<dep>/<subfeat>` references. `read_local_features` renders a bare
    // `foo` as `("foo", "foo")` and `dep:foo` as `("foo", "dep:")`, both of which match
    // on the name alone — and neither names a feature of the dependency. Reading the
    // bare form as one made `final_feature_list_main` disable a feature of the *main*
    // crate to switch off a dep feature that does not exist: a7105's
    // `async = ["embedded-hal-async"]` turned into `--no-default-features`, dropping the
    // `async` gate its unconditional `impl<SPI: SpiDevice>` needs.
    let all_dep_feats: Vec<String> = exchange
        .crate_info
        .features
        .iter()
        .flat_map(|(_, feats)| feats.iter())
        .filter(|(name, feat)| *name == crate_name && feat != name && feat != "dep:")
        .map(|(_, feat)| feat.clone())
        .collect();

    debug!(
        "All features for dependency {}: {:?}",
        dep.crate_name, all_dep_feats
    );

    let (temp_disable_default, mut temp_flexible, to_disable) = solver::final_feature_list_main(
        &exchange.crate_info,
        enable,
        &dep_disable,
        Some(&all_dep_feats),
        &mut exchange.telemetry,
    );

    println!(
        "Dependency {} temp flexible features: {:?}, to disable: {:?}, temp disable default: {}",
        dep.crate_name, temp_flexible, to_disable, temp_disable_default
    );

    previously_disabled.extend(to_disable.clone());
    temp_flexible.retain(|f| !previously_disabled.contains(f));

    // What that call just re-derived is the main crate's `default` list, asked of
    // a *dependency's* disable list — it is never told what the main solve
    // decided, so it hands back every default feature the main solve had just
    // turned off. nuuid 0.5.0 solved to `¬std` and its first dependency pass
    // answered "`std` is a default feature rand_core does not disable"; the crate
    // built with `--features std` (R31-3). Filtered here rather than by widening
    // the call's disable list, which is a change in the other direction: a wider
    // list makes `disable_in_default` true where it was false, and the call then
    // re-derives `default` for a dependency it previously ignored. tarfs is what
    // that costs — `default = ["std", "builtin_devices"]`, and `builtin_devices`
    // (whose module uses std unconditionally) comes back as "a default feature
    // this dependency does not disable". Subtracting only ever removes.
    temp_flexible.retain(|f| {
        !parser::reaches_forbidden_feature(
            &exchange.crate_info,
            f,
            &exchange.main_no_std_forbidden,
        )
    });

    *disable_default = *disable_default || temp_disable_default;
    main_features.extend(temp_flexible);
    main_features.sort();
    main_features.dedup();

    main_features.retain(|f| !to_disable.contains(f));

    parser::minimize(
        &exchange.crate_info,
        dep_and_feats,
        main_features,
        non_minimalizable,
        *disable_default,
        &exchange.name_with_version,
        None,
        None,
        "dep_wrapper",
        Some(deps_to_keep),
    );

    // Empty for a dependency `finalize_dep_crate` never reached — the pass then
    // has nothing proved against any of its features and edits nothing.
    let dep_forbidden = exchange
        .dep_forbidden_features
        .get(&dep.crate_name)
        .cloned()
        .unwrap_or_default();
    parser::move_unnecessary_dep_feats(
        &exchange.name_with_version,
        enable,
        main_features,
        &dep.crate_name,
        &dep_enable,
        &mut exchange.telemetry,
        *disable_default,
        &exchange.protected_dep_features,
        &dep_forbidden,
    );
    Ok(())
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    env_logger::init();
    // Starts the clock every later scope is measured against. `AllStats::dump`
    // reads it back out, and every exit path — including the early bails below —
    // goes through `dump`.
    timing::init();

    let mut name = match cli.name {
        Some(name) => name,
        None => {
            return Err(anyhow::anyhow!("Name is required"));
        }
    };

    let target = match cli.target {
        Some(target) => {
            if !consts::is_valid_target(&target) {
                return Err(anyhow::anyhow!(
                    "Invalid target `{}`. Choose one of {:?}",
                    target,
                    consts::TARGET_LIST
                ));
            }
            // Pin the plugin record pass to this target too, not just the final
            // verification compile — otherwise it sweeps all 26 bare-metal targets
            // per covering/CEGAR run and ignores the target the user asked for.
            driver::set_explicit_target(&target);
            target
        }
        None => {
            debug!("No target provided, will use all targets");
            "".to_string()
        }
    };

    let depth = match cli.depth {
        Some(depth) if depth > 0 => depth,
        Some(_) => u32::MAX,
        None => u32::MAX,
    };

    let db_data = db::read_db_file()?;
    let mut telemetry = nostd::Telemetry::default();

    {
        let _t = timing::scope("download_main", &name);
        if let Some(url) = cli.url {
            debug!("URL provided: {}", url);
            if downloader::clone_repo(&url, &name).is_err() {
                return Err(anyhow::anyhow!("Failed to clone repo"));
            }
        } else {
            debug!("Downloading from crates.io");
            let version = cli.version.map(|version| format!("={}", version));
            name = downloader::clone_from_crates(&name, version.as_ref(), None, None)?;
            debug!("Downloaded crate: {}", name);
        }
    }

    let mut stats = nostd::AllStats::new(name.clone());
    println!("Processing crate: {}", name);
    let (temp_name, version) = name.split_once(':').unwrap_or((&name, "latest"));
    telemetry.name = temp_name.to_string();
    telemetry.version = version.to_string();

    if parser::is_proc_macro(&name, None) {
        telemetry.is_proc_macro = true;
        stats.telemetry = Some(telemetry);
        // At this point, we still did not modify any files, so no need to restore Cargo.toml
        stats.dump(false);
        return Err(anyhow::anyhow!(
            "Main crate is a proc-macro crate, which is not supported"
        ));
    }

    // A dry run only reports the `check_for_no_std` verdict below, and that check
    // reads the manifest without needing it rewritten. Gathering read-only keeps the
    // shared download dir untouched (no `Cargo.toml.bak` dance), so a dry run can be
    // run concurrently with a full evaluation over the same download cache.
    let (mut worklist, crate_name_rename, mut crate_info) = {
        let _t = timing::scope("gather_crate_info", &name);
        downloader::gather_crate_info(&name, cli.dry_run, None)?
    };
    telemetry.num_deps = crate_info.deps_and_features.len();

    debug!("Dependencies: {:?}", crate_info);

    let cfg = z3::Config::new();
    let ctx = z3::Context::new(&cfg);
    let found = {
        let _t = timing::scope("nostd_parse", &name);
        parser::check_for_no_std(&name, &ctx, Some(&mut telemetry), None)
    };

    if !found || telemetry.wrong_unconditional_setup {
        stats.telemetry = Some(telemetry);
        // Nothing was modified on a dry run, so there is no backup to restore.
        stats.dump(!cli.dry_run);
        if !found {
            return Err(anyhow::anyhow!("Main crate does not support no_std build"));
        } else {
            return Err(anyhow::anyhow!(
                "Main crate has incorrect unconditional no_std setup"
            ));
        }
    }

    // The no_std verdict is fully decided by the check above: it is what tells a
    // crate that declares `#![no_std]` (unconditionally or under some feature
    // configuration) from one that never can. Everything past this point —
    // downloading the dependency graph, the plugin record passes, the solve — exists
    // to *pick a feature set*, which a dry run does not use. Exiting here keeps a dry
    // run to parsing plus `cargo metadata --no-deps`: no cargo builds, and in
    // particular no `cargo hir` invocations, so the plugin can be reinstalled while a
    // dry run is in flight.
    if cli.dry_run {
        println!("Dry run enabled, exiting now!");
        telemetry.no_std = found;
        stats.crate_info = Some(crate_info);
        stats.telemetry = Some(telemetry);
        stats.dump(false);
        return Ok(());
    }

    let mut top_level_deps: Vec<(String, String)> = Vec::new();
    let mut proc_macro_deps: Vec<nostd::ProcMacroDep> = Vec::new();
    // Covers the download of the whole dependency graph; the transitive no_std
    // walk it ends with opens its own `dep_verify` scope inside.
    let no_std = {
        let _t = timing::scope("download_deps", &name);
        downloader::download_all_dependencies(
            &name,
            &mut worklist,
            &mut crate_info,
            depth,
            &mut telemetry,
            &mut top_level_deps,
            &mut proc_macro_deps,
        )?
    };

    let mut exchange = nostd::DataExchange {
        name_with_version: name,
        db_data,
        crate_info,
        telemetry,
        crate_name_rename,
        valid_cross_crate_items: std::collections::HashSet::new(),
        main_enable: Vec::new(),
        protected_dep_features: std::collections::HashSet::new(),
        dep_forbidden_features: std::collections::HashMap::new(),
        main_no_std_required: Vec::new(),
        main_no_std_forbidden: Vec::new(),
    };

    stats.crate_info = Some(exchange.crate_info.clone());
    exchange.telemetry.no_std = found;
    exchange.telemetry.dep_not_no_std = !no_std;

    // A non-optional dependency that supports no_std under no feature makes the
    // tree unbuildable from the root manifest — only a `[patch]` or a fork can
    // change it, and neither is something this tool emits. Stop here.
    //
    // This verdict used to be recorded and then ignored: the run went on to HIR
    // analysis, a solve, an emitted manifest and 26 target builds, all of them
    // on the dependency set as it stood when the verification pass gave up.
    // Over the 12297-crate corpus, 497 crates set this flag and **none of them
    // built** on a single target, so the work after this point had no successes
    // to protect.
    if !no_std {
        let offenders: Vec<String> = exchange
            .telemetry
            .dep_not_no_std_deps
            .iter()
            .map(|f| format!("{} (parent {}, depth {})", f.dep, f.parent, f.depth))
            .collect();
        let reason = format!(
            "Dependency does not support no_std build: {}",
            offenders.join(", ")
        );
        debug!("{}", reason);
        stats.telemetry = Some(exchange.telemetry);
        stats.dump(true);
        return Err(anyhow::anyhow!(reason));
    }

    // Before any of the analysis, and after the graph is on disk: a proc macro's
    // features choose the tokens it injects *here*, and the only way to learn
    // whether one of them injected std is to compile the crate and read the
    // expansion each std record came out of. Runs here so everything downstream —
    // the covering runs, the solve, the emitted manifest — sees the parked edge.
    {
        let manifest = parser::determine_manifest_file(&exchange.name_with_version, None);
        driver::park_injecting_proc_macros(
            &exchange.name_with_version,
            &manifest,
            &proc_macro_deps,
            &mut exchange.telemetry,
        );
    }

    let ctx = z3::Context::new(&z3::Config::new());
    let (
        all_hard,
        hard_constraints,
        coverage_comparison,
        _compile_error_constraints,
        main_root,
        covering_records,
        unproven_std,
    ) = driver::analyze_crate_wrapper(
        &ctx,
        &exchange.name_with_version,
        None,
        &mut exchange.telemetry,
    );

    // What this crate's dependencies demand of its feature set (R31-4). The
    // translation existed and only the covering runs read it: glamour 0.16.0's
    // log says `Dependency glam's compile_error constrains this crate: (default
    // ∨ std ∨ libm)` and it still shipped `--no-default-features` with none of
    // the three, losing all 26 targets to "You must specify a math backend".
    // Conjoined into the hard constraints so the feature solve answers it and
    // `hard_constraint_features` below keeps `minimize` from taking the answer
    // back. `None` for the crates no dependency constrains, which is most.
    let hard_constraints = match (
        hard_constraints,
        driver::dependency_feature_requirement(
            &ctx,
            &parser::determine_manifest_file(&exchange.name_with_version, None),
        ),
    ) {
        (Some(hard), Some(req)) => Some(z3::ast::Bool::and(&ctx, &[&hard, &req])),
        (Some(hard), None) => Some(hard),
        (None, Some(req)) => Some(req),
        (None, None) => None,
    };

    // A build enabler is load-bearing only through what it forwards. totsu_core's
    // `libm = ["num-traits/libm"]` is what gives `Float` a `sqrt`; num-traits
    // solves fine without its own `libm`, so `move_unnecessary_dep_feats` sees a
    // dep feature nobody asked for and moves it to `dep_unnecessary_features` —
    // leaving `libm` an empty feature and the emitted config back where it
    // started. Pin the forwarded values, transitively, the same way
    // `finalize_dep_crate` pins a dep feature the main crate uses items from.
    //
    // Read off telemetry here and not later: dependency analyses share this
    // `Telemetry` and append their own enablers to the same list.
    let build_enablers: HashSet<String> =
        exchange.telemetry.build_enabler_features.iter().cloned().collect();
    if !build_enablers.is_empty() {
        let closed = parser::close_over_local_features(&build_enablers, &exchange.crate_info.features);
        for (feat_name, values) in &exchange.crate_info.features {
            if !closed.contains(feat_name) {
                continue;
            }
            for (dep, sub) in values {
                // Only genuine `<dep>/<sub>` entries; `read_local_features` renders
                // a bare `foo` as `("foo", "foo")` and `dep:foo` as `("foo", "dep:")`.
                if sub != "dep:" && sub != dep {
                    exchange
                        .protected_dep_features
                        .insert((dep.replace('-', "_"), sub.clone()));
                }
            }
        }
        debug!(
            "Build enablers {:?} pin dep features {:?}",
            build_enablers, exchange.protected_dep_features
        );
    }

    // Build valid cross-crate item set while main ctx (and its Z3 Bools) is live.
    exchange.valid_cross_crate_items = driver::compute_valid_cross_crate_items(
        &main_root,
        &covering_records,
        hard_constraints.as_ref(),
        &ctx,
    );

    stats.coverage_comparison = coverage_comparison;
    // Recorded whatever the verdict — they are diagnostics for both outcomes,
    // and `dump` writes the file either way.
    stats.unproven_std_usage_matches = unproven_std;

    let mut failed = false;
    let mut reason = "";

    if !all_hard.is_empty() {
        exchange.telemetry.unguarded_std_usages = true;
        debug!("ERROR: Found unguarded std usage in the main crate");
        reason = "Found unguarded std usage in the main crate";
        stats.std_usage_matches = all_hard;
        stats.telemetry = Some(exchange.telemetry);
        stats.dump(true);
        return Err(anyhow::anyhow!(reason));
    }

    // Nothing proven unavoidable, but some spans were never proven *avoidable*:
    // they are std in every covering run and every feature set that negates their
    // gate failed to compile. Passing here would emit a config on the strength of
    // a clearance nothing verified — the quiet-clearance hole
    // `Telemetry::compile_failed_spans` only counted. Fail with a distinct reason
    // so the eval can separate "proven clean" from "not shown dirty".
    if !stats.unproven_std_usage_matches.is_empty() {
        debug!(
            "ERROR: {} std span(s) in the main crate could not be proven avoidable",
            stats.unproven_std_usage_matches.len()
        );
        // Attributed, like T5's `dep_not_no_std` exit: the probe compiled
        // something and the compiler said why it failed, so the exit says it too.
        // Unattributed, this verdict was the whole of `PROBE_SET_INFEASIBLE` —
        // 40 crates that produced a count and nothing to act on, while the
        // dominant answer ("`error[E0412]: cannot find type `Vec` in this scope`
        // — the crate does not compile with `std` off") was already in hand and
        // is not the tool's to fix.
        let reasons = exchange.telemetry.unproven_std_span_reasons.clone();
        let attributed = if reasons.is_empty() {
            "Std usage in the main crate could not be proven avoidable".to_string()
        } else {
            format!(
                "Std usage in the main crate could not be proven avoidable: {}",
                reasons.join(" | ")
            )
        };
        println!("ERROR: {attributed}");
        stats.telemetry = Some(exchange.telemetry);
        stats.dump(true);
        return Err(anyhow::anyhow!(attributed));
    }

    // Derive the file list from the resolved module tree rather than sweeping the
    // source directory: analysis has already run, so the tree is complete
    // (macro-generated modules and OUT_DIR includes are spliced in), and it holds
    // only files reachable from the entrypoint.
    let main_files = nostd::visitor::collect_source_files(&main_root);
    let mut main_attributes = parser::parse_crate(
        &exchange.name_with_version,
        true,
        None,
        &all_hard,
        Some(&main_files),
    );

    let mut dep_and_feats = parser::features_for_optional_deps(&exchange.crate_info);

    // The covering runs learned which optional dependencies the no_std half needs
    // linked (bucket 11). The final feature selection is a *separate* solve, so it
    // has to be told the same thing: `caches-0.3.0` clears the std analysis and then
    // emits `--no-default-features` with nothing else, which fails to build with
    // `can't find crate hashbrown` / `unresolved import libm` — the very configuration
    // the covering run had already rejected.
    let main_manifest = parser::determine_manifest_file(&exchange.name_with_version, None);
    let main_manifest_toml = driver::read_manifest_toml(&main_manifest);
    // Cargo's declared set, not the `[features]` table — the implicit feature of an
    // optional dependency exists only in `cargo metadata`. Read once: both this and
    // `deps_pinned_by_active_use` below need it, and it shells out to cargo.
    let main_declared_features = nostd::visitor::declared_features(&main_manifest);
    let (optdep_constraints, optdep_enablers) = driver::optional_dep_link_constraints(
        &ctx,
        &main_manifest_toml,
        &main_declared_features,
        &main_root,
    );

    // Feature names forced by the no_std hard constraints (probe-derived `final_condition`).
    // Captured here because `hard_constraints` is moved into `process_crate` below.
    // These must be protected from the later minimize passes (see `non_minimalizable`).
    let hard_constraint_features: HashSet<String> = match hard_constraints.as_ref() {
        Some(hc) => {
            let hc_solver = z3::Solver::new(&ctx);
            hc_solver.assert(hc);
            if hc_solver.check() == z3::SatResult::Sat {
                solver::model_to_features(&hc_solver.get_model())
                    .0
                    .into_iter()
                    .collect()
            } else {
                HashSet::new()
            }
        }
        None => HashSet::new(),
    };

    // The main crate's own entailed-false set is dropped: nothing on the main path
    // removes a feature from a manifest the way `finalize_dep_crate` does, and
    // `final_feature_list_main` / `minimize` are deliberately left reading the full
    // `disable` list.
    let (mut enable, mut disable, _) = parser::process_crate(
        &mut exchange,
        &ctx,
        &mut main_attributes,
        None,
        None,
        true,
        &mut dep_and_feats,
        hard_constraints,
    )?;

    // Add the optional dependencies the chosen assignment cannot link without
    // (bucket 11). `caches-0.3.0` picks `not(std)`, and its no_std half imports
    // `hashbrown` and `libm` — without their implicit features the emitted build fails
    // with `can't find crate hashbrown`, the same configuration the covering run had
    // already rejected. Additive: features the solve chose are never disturbed.
    for feat in solver::forced_optional_dep_enablers(
        &ctx,
        &optdep_constraints,
        &optdep_enablers,
        &enable,
        &disable,
    ) {
        println!("Enabling optional-dep feature '{feat}' required by the no_std feature set");
        enable.push(feat);
    }

    exchange.main_enable = enable.clone();

    // Feature names that must hold for no_std, from two sources:
    //   - compile_error conditions: avoids arbitrary Z3 picks from disjunctive constraints
    //     (e.g. uom's "at least one storage type" rule) selecting a feature that pulls in std.
    //   - hard constraints (final_condition): features a probe proved are required to avoid std
    //     (e.g. lazy_static's `spin_no_std`, which guards `extern crate std` in inline_lazy.rs).
    //     Without this the main-level minimize would strip such a feature as a droppable
    //     optional-dep enabler even though it is load-bearing for no_std.
    // Intersected with the actual feature list below — only features the build really
    // enables can be non-minimalizable.
    let ce_features = parser::compile_error_feature_names(&main_attributes, &ctx);

    println!(
        "Initial main crate features to enable: {:?}, features to disable: {:?}",
        enable, disable
    );

    let (mut disable_default, mut main_features, to_disable) = solver::final_feature_list_main(
        &exchange.crate_info,
        &enable,
        &disable,
        None,
        &mut exchange.telemetry,
    );

    println!(
        "Main crate features after solving: {:?}, to disable: {:?}, disable default: {}",
        main_features, to_disable, disable_default
    );

    debug!("Dependency and features: {:?}", dep_and_feats);

    println!("Main crate arguments: {:?}", main_features);
    main_features.extend(enable.clone());
    println!(
        "Main crate arguments after extending with enable: {:?}",
        main_features
    );

    // Intersect against `main_features`, not the solver's `enable`. A feature can be
    // load-bearing for a compile_error constraint yet never appear in `enable`: when the
    // constraint shares no feature with the crate's no_std condition it is withheld from
    // the solver (see `excluded_compile_error_eqs` in parser.rs), so the feature reaches
    // the build only because `final_feature_list_main` re-added it from `[features]
    // default`. Filtering on `enable` left it unprotected and `minimize` dropped it —
    // bulletproofs-bls lost `blst` that way and shipped a set satisfying neither `rust`
    // nor `blst`. uom is the case that must not regress: its `f32`/`f64` arrive from
    // `default` too, and are now pinned rather than surviving by luck.
    // `optdep_enablers` joins the two sources above: a feature that links an optional
    // dependency the no_std half imports. It gates no code of its own, so minimize's
    // "exists only to pull in a dep" rule would drop it — but the solver only put it in
    // `enable` because a `#[cfg]` needs that dependency linked (caches: hashbrown, libm).
    // Membership in `enable`/`main_features` is what makes this deterministic: an enabler
    // the solve did not choose is never protected.
    let mut non_minimalizable: HashSet<String> = main_features
        .iter()
        .chain(enable.iter())
        .filter(|f| {
            ce_features.contains(*f)
                || hard_constraint_features.contains(*f)
                || optdep_enablers.contains(*f)
        })
        .cloned()
        .collect();
    // The one addition not filtered through `main_features`/`enable`: a feature
    // the crate's own `#![cfg_attr(<cond>, no_std)]` *entails*. It is protected
    // whether or not the solve happened to put it in either list, because
    // without it the crate is std by the author's own statement — and its usual
    // shape (`no_std = ["ieee754"]`) is precisely the one minimize drops as
    // "exists only to pull in a dep". R31-3; robust, utm and lasso all lost the
    // feature that way.
    non_minimalizable.extend(exchange.main_no_std_required.iter().cloned());
    debug!("Non-minimalizable main features: {:?}", non_minimalizable);

    // Optional dependencies `minimize` must leave linked. `non_minimalizable` cannot
    // express this: it protects *features*, and the feature at risk here is the one
    // cargo synthesises for the dependency, which nothing in `enable`/`main_features`
    // ever names. Evaluated against `main_features` plus the `default` closure, since
    // it is the features that are actually ON — not the solver's `enable` — that decide
    // whether an import's cfg is live. The dep passes below can still add to
    // `main_features`, so this is the smallest active set the build can have; a later
    // addition can only switch more gates on, and a dep pinned here stays pinned.
    let mut active_features: HashSet<String> = main_features.iter().cloned().collect();
    if !disable_default {
        active_features.insert("default".to_string());
    }
    let active_features =
        parser::close_over_local_features(&active_features, &exchange.crate_info.features);
    let deps_to_keep = driver::deps_pinned_by_active_use(
        &ctx,
        &main_manifest_toml,
        &main_declared_features,
        &active_features,
        &main_root,
        &covering_records,
    );
    if !deps_to_keep.is_empty() {
        println!(
            "Optional deps that must stay linked (imported under a cfg the unlink would not turn off): {:?}",
            deps_to_keep
        );
    }

    parser::minimize(
        &exchange.crate_info,
        &mut dep_and_feats,
        &mut main_features,
        &non_minimalizable,
        disable_default,
        &exchange.name_with_version,
        None,
        None,
        "main:pre_deps",
        Some(&deps_to_keep),
    );

    // `minimize` rewrites the crate's `[features]` table on disk, but
    // `exchange.crate_info.features` still holds the pre-edit version. Only
    // `should_skip_dep`'s sever branch refreshed it before, so downstream
    // consumers could reconstruct a `default → std → dep` chain from an entry the
    // manifest no longer has (watchface's chrono link). Re-read the rewritten
    // manifest so every later reader of `crate_info.features` sees the truth.
    parser::refresh_crate_features(&mut exchange);

    println!(
        "Main crate arguments after minimization: {:?}",
        main_features
    );

    let deps_attrs = parser::parse_deps_crate(
        &exchange.name_with_version,
        &mut exchange.telemetry,
        &exchange.db_data,
    );
    let mut skipped = Vec::new();
    // We keep track of the features we have already disabled for dependencies.
    // This way we don't accidentally re-enable some feature for a later dependency
    // that we had to disable for an earlier dependency.
    let mut previously_disabled: HashSet<String> = HashSet::new();
    // Solve for each dependency
    // TODO: Some dependencies are from git instead of crates.io. Handle those cases.
    // TODO: There are some cleanup and refactoring to minimize the read -> mutate -> write pattern for the toml
    // TODO: Use better mechanism to get the .rs file to check for no_std (use metadata to get this).
    // TODO: Add checks to make sure all deps at all depths actually can be compiled with the given set of features in the
    // crate that depends on them. -> This is currently implemented and only checks if
    // the feature requirements can be met, not if they are actually met with the set of features enabled by that crate for
    // no_std compilation.
    // TODO: For the impossible case where there is no way to connect no_std to some feature, we try compiling, and if there are errors, we need to see what caused the error. If it was due to some unresolved import, we need to find the feature that is gating it and enabled it. Or we can also have a set of features that we know includes more things into the crate. And then when compilation fails, we can try each of those features and see if it fixes the issue. This is a last resort since it is not systematic and is expensive.
    // ADD test for yaxpeax-m16c
    // To look at: watchface-0.4.0: optional dependency getting enabled/use lock file to get the dep version here, world_magnetic_model-0.2.0: dep feature not correct, uom-0.36.0: last crate uses this but this shows std usage when there is not one requires changes to ast visitor here (chrono-0.4.19 same issue here).
    let mut deps_args = Vec::new();
    let mut enabled_optional_deps: HashSet<String> = HashSet::new();
    // Features `should_skip_dep` turned off because the dependency they enable is
    // not no_std *and* the crate still names it under a cfg the sever would leave
    // true (R31-2). The manifest edit is made there; the command line is here.
    let mut features_forced_off: Vec<String> = Vec::new();
    for mut dep in deps_attrs {
        if consts::KNOWN_SYN_FAILURES.contains(&dep.crate_name.as_str()) {
            debug!(
                "Dependency {} has known syntex failure, skipping",
                dep.crate_name
            );
            continue;
        }

        if parser::should_skip_dep(
            &dep.crate_name,
            &mut exchange,
            &mut dep_and_feats,
            &main_features,
            disable_default,
            false,
            &deps_to_keep,
            &mut features_forced_off,
        ) {
            debug!("Dependency {} is optional, skipping", dep.crate_name);
            skipped.push(dep);
            continue;
        }

        let dep_name = dep.crate_name.split(':').next().unwrap_or("").to_string();
        if parser::is_dep_optional(&exchange.crate_info, &dep_name) {
            enabled_optional_deps.insert(dep_name);
        }

        process_dep_crate_wrapper(
            &mut exchange,
            &mut dep,
            &mut dep_and_feats,
            &mut main_features,
            &mut disable_default,
            &mut enable,
            &mut deps_args,
            &mut previously_disabled,
            &non_minimalizable,
            &deps_to_keep,
        )?;
    }

    // A feature the pass above turned off in the manifest must leave the command
    // line too, or cargo enables it anyway and the code it gates comes back —
    // naming the dependency the same pass just unlinked (kitoken's `multiversion`
    // is the shape: it rode in on `--features`, not through `default`).
    apply_forced_off(&mut features_forced_off, &mut main_features, &mut enable);

    let mut temp_combined = deps_args.clone();
    temp_combined.sort();
    temp_combined.dedup();
    temp_combined.extend(main_features.clone());

    let mut dep_args_skipped = Vec::new();
    for mut dep in skipped {
        if !parser::should_skip_dep(
            &dep.crate_name,
            &mut exchange,
            &mut dep_and_feats,
            &temp_combined,
            disable_default,
            true,
            &deps_to_keep,
            &mut features_forced_off,
        ) {
            debug!(
                "Dependency {} which was skipped previously is now required",
                dep.crate_name
            );

            let dep_name = dep.crate_name.split(':').next().unwrap_or("").to_string();
            if parser::is_dep_optional(&exchange.crate_info, &dep_name) {
                enabled_optional_deps.insert(dep_name);
            }

            process_dep_crate_wrapper(
                &mut exchange,
                &mut dep,
                &mut dep_and_feats,
                &mut main_features,
                &mut disable_default,
                &mut enable,
                &mut dep_args_skipped,
                &mut previously_disabled,
                &non_minimalizable,
                &deps_to_keep,
            )?;
        }
    }

    apply_forced_off(&mut features_forced_off, &mut main_features, &mut enable);

    println!(
        "Dependecies that got enabled after processing skipped deps: {:?}",
        enabled_optional_deps
    );

    parser::minimize(
        &exchange.crate_info,
        &mut dep_and_feats,
        &mut main_features,
        &non_minimalizable,
        disable_default,
        &exchange.name_with_version,
        None,
        Some(&enabled_optional_deps),
        "main:post_deps",
        Some(&deps_to_keep),
    );

    deps_args.extend(dep_args_skipped);

    println!("Dep arguments: {:?}", deps_args);
    println!(
        "Main crate arguments after processing deps: {:?}",
        main_features
    );

    // Last check before the selection becomes a command line: does it still make
    // the crate root `#![no_std]`? Everything above is free to move a feature —
    // the dependency passes, three `minimize` calls, `should_skip_dep` — and a
    // crate whose no_std is opt-in is std again the moment one of them moves the
    // wrong one. Nothing here overrules a choice the condition left free; only
    // what it entails is restored (R31-3).
    {
        let (added, removed) = parser::enforce_no_std_polarity(
            &exchange.crate_info,
            &mut main_features,
            &mut enable,
            &mut disable_default,
            &exchange.main_no_std_required,
            &exchange.main_no_std_forbidden,
        );
        if !added.is_empty() || !removed.is_empty() {
            println!(
                "Restoring the crate's own no_std condition: enabling {:?}, disabling {:?}",
                added, removed
            );
            exchange.telemetry.no_std_polarity_restored = true;
        }
    }

    main_features.sort();
    main_features.dedup();
    let (mut final_args, mut combined_features, mut final_features_len) =
        assemble_final_args(disable_default, &main_features, &deps_args);

    println!("Final args: {:?}", final_args);
    let before_build = compiler::mark_build_records(&stats, &exchange.telemetry);
    let mut one_succeeded = if no_std {
        let t = timing::scope("verify_build", &exchange.name_with_version);
        t.meta("attempt", "initial");
        compiler::try_compile(
            &exchange.name_with_version,
            &target,
            &final_args,
            &mut stats,
            &mut exchange.telemetry,
        )
    } else {
        Ok(false)
    }?;

    // KI-11: a dependency can clear every no_std-capability check and still be
    // unbuildable for the target we chose — lazy-exclusive's `use-locks` pulls in
    // `libc`, whose `pthread_mutex_*` items do not exist on bare metal. Nothing
    // short of compiling this crate can produce that evidence, so the only place
    // to act on it is here, after a build that failed on every target. Features
    // that exist solely to link an optional dep are dropped and the build retried;
    // the retry is kept only if it succeeds, which is what makes a batch drop safe
    // even when a candidate turns out to be load-bearing. Deliberately not fed
    // back into a solve — asserting these edges there only shuffles which
    // arbitrary model Z3 returns and breaks unrelated crates.
    if no_std && !one_succeeded {
        let droppable = parser::deps_only_enable_features(
            &exchange.name_with_version,
            &exchange.crate_info,
            &main_features,
            &non_minimalizable,
            !disable_default,
        );
        let reduced: Vec<String> = main_features
            .iter()
            .filter(|feat| !droppable.contains(feat))
            .cloned()
            .collect();
        let (retry_args, retry_combined, retry_len) =
            assemble_final_args(disable_default, &reduced, &deps_args);

        if !droppable.is_empty() && retry_args != final_args {
            println!(
                "Build failed for every target; retrying without optional-dep-only feature(s) {:?}: {:?}",
                droppable, retry_args
            );
            let before_retry = compiler::mark_build_records(&stats, &exchange.telemetry);
            let retry_succeeded = {
                let t = timing::scope("verify_build", &exchange.name_with_version);
                t.meta("attempt", "retry_without_optional_dep_feats");
                compiler::try_compile(
                    &exchange.name_with_version,
                    &target,
                    &retry_args,
                    &mut stats,
                    &mut exchange.telemetry,
                )?
            };
            if retry_succeeded {
                // The retry is the emitted config now, so the failed attempt's rows
                // are dropped — one feature set per target in the results.
                compiler::discard_build_records(
                    &mut stats,
                    &mut exchange.telemetry,
                    &before_build,
                    &before_retry,
                );
                // The DB hands this crate's chosen features to any later build that
                // depends on it, so a feature the retry just proved unbuildable has
                // to change sides there too, not only in `final_args`.
                enable.retain(|feat| !droppable.contains(feat));
                for feat in &droppable {
                    if !disable.contains(feat) {
                        disable.push(feat.clone());
                    }
                }
                exchange.telemetry.optional_dep_features_dropped = droppable;
                final_args = retry_args;
                combined_features = retry_combined;
                final_features_len = retry_len;
                one_succeeded = true;
                println!("Final args after retry: {:?}", final_args);
            } else {
                compiler::rewind_build_records(
                    &mut stats,
                    &mut exchange.telemetry,
                    &before_retry,
                );
            }
        }
    }

    // Verify the feature set we built actually satisfies the crate's own
    // `compile_error!` conditions. The stage-2 check inside `process_crate` leaves
    // unselected features free and so is trivially satisfiable; this one closes the
    // world. Runs on the emitted set, so a retry above is what gets checked.
    let emitted_features = |combined: &[String]| -> Vec<String> {
        combined
            .iter()
            .flat_map(|s| s.split(','))
            .map(str::to_string)
            .collect()
    };
    let mut violated = parser::violated_compile_error_constraints(
        &ctx,
        &main_attributes,
        &exchange.crate_info,
        &emitted_features(&combined_features),
        !disable_default,
    );

    // A violated `compile_error!` is not a warning about the build — it *is* the
    // build failure: the compiler stops on the macro before anything else is
    // reached. lexical-util 1.0.6 shipped `--features floats` against a
    // `compile_error!` naming that exact case and lost all 26 targets, though
    // `write-floats` compiles clean bare metal. The constraint stays out of the
    // feature solve (`excluded_compile_error_eqs` — uom shows why), so the repair
    // is applied here in the KI-11 shape instead: only after a build that failed
    // everywhere, and kept only if the rebuild succeeds. A crate that builds today
    // cannot reach this.
    if no_std && !one_succeeded && !violated.is_empty() {
        let additions = parser::compile_error_repair_features(
            &ctx,
            &main_attributes,
            &exchange.crate_info,
            &emitted_features(&combined_features),
            !disable_default,
            &disable,
        );
        if !additions.is_empty() {
            let mut repaired = main_features.clone();
            repaired.extend(additions.iter().cloned());
            repaired.sort();
            repaired.dedup();
            let (repair_args, repair_combined, repair_len) =
                assemble_final_args(disable_default, &repaired, &deps_args);
            println!(
                "Build failed for every target and the feature set violates {:?}; \
                 retrying with compile_error repair {:?}: {:?}",
                violated, additions, repair_args
            );
            let before_repair = compiler::mark_build_records(&stats, &exchange.telemetry);
            let repair_succeeded = {
                let t = timing::scope("verify_build", &exchange.name_with_version);
                t.meta("attempt", "retry_with_compile_error_repair");
                compiler::try_compile(
                    &exchange.name_with_version,
                    &target,
                    &repair_args,
                    &mut stats,
                    &mut exchange.telemetry,
                )?
            };
            if repair_succeeded {
                compiler::discard_build_records(
                    &mut stats,
                    &mut exchange.telemetry,
                    &before_build,
                    &before_repair,
                );
                // Same reason as the retry above: the DB hands these features to
                // every later build that depends on this crate.
                for feat in &additions {
                    if !enable.contains(feat) {
                        enable.push(feat.clone());
                    }
                }
                disable.retain(|feat| !additions.contains(feat));
                exchange.telemetry.compile_error_repair_features = additions;
                final_args = repair_args;
                combined_features = repair_combined;
                final_features_len = repair_len;
                one_succeeded = true;
                // Re-derived from the set that shipped rather than cleared by
                // hand: whatever this reports is a statement about the emitted
                // config, and only the check gets to make it.
                violated = parser::violated_compile_error_constraints(
                    &ctx,
                    &main_attributes,
                    &exchange.crate_info,
                    &emitted_features(&combined_features),
                    !disable_default,
                );
                println!("Final args after compile_error repair: {:?}", final_args);
            } else {
                compiler::rewind_build_records(
                    &mut stats,
                    &mut exchange.telemetry,
                    &before_repair,
                );
            }
        }
    }

    exchange.telemetry.final_features_length = final_features_len;

    if !violated.is_empty() {
        println!(
            "WARNING: final feature set for {} violates compile_error constraint(s): {:?}",
            exchange.name_with_version, violated
        );
        exchange
            .telemetry
            .compile_error_constraint_unsatisfied
            .push(exchange.name_with_version.clone());
    }

    if one_succeeded {
        exchange.telemetry.build_success = true;
        db::add_to_db_data(
            &mut exchange.db_data,
            &exchange.name_with_version,
            (&enable, &disable),
        );
    } else {
        exchange.telemetry.hir_analysis_done = true;
        // We add no_std here but not for the previous condition becase, we want to know
        // even if some deps are not no_std compatible, whether the main would have built successfully
        // if not for the unsupported deps.
        if no_std
            && !cli.no_recursive
            && !parser::recursive_dep_requirement_check(
                &mut exchange,
                depth,
                &top_level_deps,
                &enabled_optional_deps,
            )
        {
            // This is the last resort since this has a high chance of false positives
            debug!(
                "ERROR: Some dependency at some level does not have a way to enable all its required features in no_std mode"
            );
            failed = true;
            reason = "Some dependency at some level does not have a way to enable all its required features in no_std mode";
        }
    }

    db::write_db_file(exchange.db_data)?;

    stats.telemetry = Some(exchange.telemetry);
    stats.dump(true);
    if failed {
        return Err(anyhow::anyhow!(reason));
    }
    Ok(())
}

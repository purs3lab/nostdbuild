#![feature(rustc_private)]

use std::collections::HashSet;

use anyhow::Ok;
use clap::Parser;
use log::debug;

use nostd::{
    Attributes, ablation, compiler, consts, db, downloader, driver, parser, solver, timing,
};

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

    /// Ablation study (see ablation_plan.md §3.1): ignore the HIR-verified
    /// std-finding hard constraint instead of seeding the solve with it.
    #[arg(long)]
    no_std_finding: bool,

    /// Ablation study §3.2: stop the covering-set search after the seed
    /// (default-features) run instead of iterating to full coverage.
    #[arg(long)]
    no_combo_search: bool,

    /// Ablation study §3.3: skip per-dependency no_std feature analysis;
    /// dependencies are left at their own default features.
    #[arg(long)]
    no_dep_analysis: bool,

    /// Ablation study §3.4: don't assert compile_error!-derived constraints
    /// into the solver.
    #[arg(long)]
    no_compile_error_constraints: bool,

    /// Ablation study §3.5: don't run the cfg-gate / gateway resolution
    /// passes.
    #[arg(long)]
    no_gateway_resolution: bool,

    /// Ablation study §3.6: don't check whether the main crate's usage of a
    /// dependency requires bypassing the DB cache for that dependency.
    #[arg(long)]
    no_cross_crate_propagation: bool,

    /// Ablation study §3.7: don't read or write the shared no_std result DB
    /// (`db.bin`).
    #[arg(long)]
    no_db: bool,

    /// Ablation study §3.8: don't use the in-process `cargo hir` cache (L1).
    #[arg(long)]
    no_local_cache: bool,

    /// Ablation study §3.9: don't use the cross-process `cargo hir` cache
    /// (L2).
    #[arg(long)]
    no_global_cache: bool,
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
    main_to_disable: &HashSet<String>,
) -> anyhow::Result<()> {
    let _t = timing::crate_scope("dep_analysis", &dep.crate_name);
    // The DB answers "what does this dependency need to be no_std", keyed by the
    // dependency alone. That is parent-independent and the cache is sound for it
    // — but a KI-27 requirement is not: `zeroize/alloc` is needed because
    // *multiexp* calls `.zeroize()` on a `Vec`, and a sibling crate that never
    // does needs nothing. A cached answer would silently skip the constraint,
    // the same way a cache hit silently skips the analysis a verification run is
    // testing, so a dependency this crate has impl requirements on is analysed.
    //
    // "On" includes through it: unit-sphere's records name simba, the dependency
    // is nalgebra, and it is nalgebra's `libm` that has to be forced on.
    let dep_package = dep.crate_name.split(':').next().unwrap_or(&dep.crate_name);
    let dep_manifest =
        parser::determine_manifest_file(&dep.crate_name, Some(&exchange.name_with_version));
    let dep_crate = parser::dep_crate_name(&dep_manifest, dep_package).replace('-', "_");
    let dep_dir = std::path::Path::new(&dep_manifest)
        .parent()
        .map(|p| p.to_path_buf())
        .unwrap_or_default();
    let has_impl_requirements = !ablation::flags().no_cross_crate_propagation
        && driver::dep_carries_impl_requirements(&dep_dir, &dep_crate, &exchange.impl_records);
    if has_impl_requirements {
        debug!(
            "Not using the DB for {}: the main crate's calls need impls it may gate",
            dep.crate_name
        );
    }
    // Same argument for the items the main crate *names* (R34-6). `num-traits`
    // needs `libm` because earcut imports `num_traits::float::Float`, which is a
    // fact about earcut; the DB's answer is keyed by the dependency alone and
    // would skip it.
    let has_path_requirements = !ablation::flags().no_cross_crate_propagation
        && driver::dep_carries_path_requirements(
            &dep_dir,
            &dep_crate,
            &exchange.cross_crate_items(),
        );
    if has_path_requirements {
        debug!(
            "Not using the DB for {}: the main crate names items it may gate",
            dep.crate_name
        );
    }
    // Check the DB first: if we already have a result for this dep, skip the expensive
    // gather_crate_info + analyze_crate_wrapper + process_crate path entirely.
    db::record_db_cache_attempt(has_impl_requirements || has_path_requirements);
    let (local_dep_args, dep_disable, dep_enable) = if let Some(db_entry) =
        db::get_from_db_data(&exchange.db_data, &dep.crate_name)
            .filter(|_| !has_impl_requirements && !has_path_requirements)
    {
        db::record_db_cache_hit();
        debug!(
            "DB hit for dependency {}, skipping analysis",
            dep.crate_name
        );
        let (enable, disable) = (db_entry.features.0.clone(), db_entry.features.1.clone());
        // DB hit — no dep_root available; pass empty map (no protection check for
        // this dep) and no forced optional-dep enablers (KI-12 needs the dep's
        // parsed tree to compute those, which the DB path never has). `None`
        // for the entailed-false set for the same reason: the DB stores the
        // (enable, disable) pair only, so removals fall back to `disable` and
        // this path behaves exactly as it did before.
        parser::finalize_dep_crate(
            exchange,
            dep,
            enable,
            disable,
            None,
            std::collections::HashMap::new(),
            &[],
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
        !parser::reaches_forbidden_feature(&exchange.crate_info, f, &exchange.main_no_std_forbidden)
    });

    // R34-22 sizing (measurement only): does what survived the two filters above
    // still contain a feature the main crate's own solve put in *its* `to_disable`?
    // Neither filter covers that — `previously_disabled` only ever accumulates
    // other dependencies' `to_disable`, never the main solve's own (see the field
    // doc on `Telemetry::dep_pass_reintroduced_main_disabled`). Recorded, not
    // acted on: `main_features.extend` two lines down still re-adds it exactly as
    // it does today.
    let reintroduced_main_disabled =
        solver::reintroduced_main_disabled_features(&temp_flexible, main_to_disable);
    if !reintroduced_main_disabled.is_empty() {
        exchange
            .telemetry
            .dep_pass_reintroduced_main_disabled
            .push((dep.crate_name.clone(), reintroduced_main_disabled));
    }

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
    // One stack segment for the whole run: every `syn` parse and visit below
    // asks for the same headroom, and inside this call each of those asks is a
    // pointer compare rather than another allocation. See `with_syn_stack`.
    nostd::with_syn_stack(run)
}

fn run() -> anyhow::Result<()> {
    let cli = Cli::parse();
    env_logger::init();
    // Set once, before anything below reads it — `ablation::flags()` is how
    // every mechanism's call site (in this file and in `driver.rs`) learns
    // which arm of the ablation study this process is running as.
    ablation::set_flags(ablation::AblationFlags {
        no_std_finding: cli.no_std_finding,
        no_combo_search: cli.no_combo_search,
        no_dep_analysis: cli.no_dep_analysis,
        no_compile_error_constraints: cli.no_compile_error_constraints,
        no_gateway_resolution: cli.no_gateway_resolution,
        no_cross_crate_propagation: cli.no_cross_crate_propagation,
        no_db: cli.no_db,
        no_local_cache: cli.no_local_cache,
        no_global_cache: cli.no_global_cache,
    });
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
    let mut telemetry = nostd::Telemetry {
        ablation_flags: ablation::flags().active_names(),
        ..Default::default()
    };

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
        downloader::gather_crate_info(&name, cli.dry_run, None, Some(&mut telemetry))?
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
        impl_records: Vec::new(),
        path_items: Vec::new(),
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
        impl_records,
        path_items,
    ) = driver::analyze_crate_wrapper(
        &ctx,
        &exchange.name_with_version,
        None,
        &mut exchange.telemetry,
    );

    // The feature sets that compiled bare-metal for the MAIN crate, taken now.
    // `analyze_crate` runs again for every dependency and clears the record each
    // time, so reading it after the dependency passes would hand the retry below
    // the last dependency's answer — KI-28's mistake, one flag down. Cheap: one
    // clone of a handful of short vectors.
    let main_bare_metal_sets = driver::bare_metal_compiling_sets();
    debug!(
        "Feature sets that compiled bare-metal for {}: {:?}",
        exchange.name_with_version, main_bare_metal_sets
    );

    // Ablation study §3.1: with std-finding disabled, the hard constraint
    // `analyze_crate_wrapper` derived from actually-found std usage doesn't
    // get to seed the solve below — everything else from the call above
    // (main_root, impl_records, path_items, the all_hard/unproven_std
    // fail-fast checks) is untouched.
    let hard_constraints = if ablation::flags().no_std_finding {
        None
    } else {
        hard_constraints
    };

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
    let build_enablers: HashSet<String> = exchange
        .telemetry
        .build_enabler_features
        .iter()
        .cloned()
        .collect();
    if !build_enablers.is_empty() {
        let closed =
            parser::close_over_local_features(&build_enablers, &exchange.crate_info.features);
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

    // The requirements a *call* carries rather than a name (KI-27), already
    // filtered to reachable call sites by `analyze_crate`. Read by
    // `process_dep_crate`, one dependency at a time, against that dependency's
    // own tree.
    exchange.impl_records = impl_records;
    exchange.path_items = path_items;
    if !exchange.impl_records.is_empty() {
        debug!(
            "{} trait impl(s) from dependencies are needed by reachable call sites",
            exchange.impl_records.len()
        );
    }

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
    //
    // `host_only_excused_spans` (R34-3) is a *count*, not a filter on
    // `unproven_std_usage_matches` — that list keeps every span so nothing a
    // host-only run touched vanishes from `unproven_std_usages.json`. Only the
    // decision to exit fatally subtracts it: a crate whose unproven spans are
    // all excused (a transcendental float method with no evidence beyond a host
    // build, or a core/alloc item reached through std's facade) still has
    // something real to say about the rest of its build, so it gets a chance to
    // say it instead of `[]`.
    let excused = exchange.telemetry.host_only_excused_spans;
    if stats.unproven_std_usage_matches.len() > excused {
        debug!(
            "ERROR: {} std span(s) in the main crate could not be proven avoidable ({} excused)",
            stats.unproven_std_usage_matches.len(),
            excused
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
    let (mut enable, mut disable, _, _, _) = parser::process_crate(
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
    // whether an import's cfg is live.
    //
    // Which features count as ON is `parser::active_features_for_pin_set`, and the
    // answer is *not* the selection of this moment: the set is consulted once, here,
    // then handed to the three later `minimize` calls unchanged while
    // `process_dep_crate_wrapper` grows `main_features` in between. That is R34-1 —
    // bevy_input's solve disabled `smol_str`, `bevy_utils`' pass put it back, and
    // `minimize` unlinked the dependency out of the feature that then went out on the
    // command line. The rule and its `watchface` control live on that function.
    let active_features =
        parser::active_features_for_pin_set(&main_features, &exchange.crate_info.features);
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
    // R34-22 sizing only — read by `process_dep_crate_wrapper` to detect (not yet
    // prevent) a dependency pass reintroducing a feature the main solve itself
    // already put in `to_disable` above. Not merged into `previously_disabled`:
    // that would change behavior, and this pass is measurement-only until the
    // sizing data says a fix is warranted.
    let main_to_disable: HashSet<String> = to_disable.iter().cloned().collect();
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

        // Ablation study §3.3: skip per-dependency no_std feature steering,
        // leaving main_features/deps_args/disable_default as the main
        // crate's own solve left them — the final build attempts whatever
        // that implies for this dependency.
        if !ablation::flags().no_dep_analysis {
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
                &main_to_disable,
            )?;
        }
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

            // Ablation study §3.3: same skip as the first pass above.
            if !ablation::flags().no_dep_analysis {
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
                    &main_to_disable,
                )?;
            }
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
            // No scout target. This retry drops features of the *main* crate, which
            // can be load-bearing for one target and free on another, so the whole
            // list is the question it asks. R34-11's below is the case a single
            // target settles.
            if compiler::try_alternative(
                &exchange.name_with_version,
                &target,
                &retry_args,
                "retry_without_optional_dep_feats",
                &before_build,
                None,
                &mut stats,
                &mut exchange.telemetry,
            )? {
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
            }
        }
    }

    // R34-11: `custom_no_std_feature_enabled` holds the `<dep>/<feat>` pairs a
    // dependency's own isolated solve asked for and no feature of the main crate
    // could reach. Every name in it is a real feature of the dependency it names —
    // that is what separates it from the atoms `solver::retain_selectable_features`
    // refuses to emit at all — and it is the *combination with the rest of the
    // graph* that is wrong: `encointer-primitives/full_crypto` unifies onto
    // `sp-core` and makes `Pair::sign` required of a sibling that never emits it;
    // `sp-io/with-tracing` selects a path that exists only on wasm. A per-dep solve
    // cannot see either, so this lands here rather than at the write site: 847
    // crates that build today carry an injected set, and a build that failed on
    // every target is the only evidence that this one was not free.
    //
    // Judged on one target first (`compiler::scout_target`). The A/B this repair
    // comes from was itself a single-target test, 89 of 93 rows on
    // `thumbv7em-none-eabi`, and the whole 26 cost 24.6 h across the corpus rows
    // that would reach this.
    if no_std
        && !one_succeeded
        && let Some(reduced_deps) = parser::without_injected_dep_features(&deps_args)
    {
        let (retry_args, retry_combined, retry_len) =
            assemble_final_args(disable_default, &main_features, &reduced_deps);
        let injected: Vec<String> = exchange
            .telemetry
            .custom_features_added_list
            .iter()
            .flat_map(|(dep, feats)| feats.iter().map(move |feat| format!("{dep}/{feat}")))
            .collect();
        println!(
            "Build failed for every target; retrying without the injected feature(s) {:?}: {:?}",
            injected, retry_args
        );
        let scout = compiler::scout_target(&stats, &before_build);
        if compiler::try_alternative(
            &exchange.name_with_version,
            &target,
            &retry_args,
            "retry_without_injected_dep_feats",
            &before_build,
            scout.as_deref(),
            &mut stats,
            &mut exchange.telemetry,
        )? {
            exchange.telemetry.injected_dep_features_dropped = injected;
            deps_args = reduced_deps;
            final_args = retry_args;
            combined_features = retry_combined;
            final_features_len = retry_len;
            one_succeeded = true;
            println!(
                "Final args after dropping the injected features: {:?}",
                final_args
            );
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
            // No scout target: a violated `compile_error!` stops the compiler
            // before anything target-specific is reached, so one target says
            // nothing the other 25 do not.
            if compiler::try_alternative(
                &exchange.name_with_version,
                &target,
                &repair_args,
                "retry_with_compile_error_repair",
                &before_build,
                None,
                &mut stats,
                &mut exchange.telemetry,
            )? {
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
            }
        }
    }

    // KI-30: the emitted configuration is one the crate has never been compiled
    // in, and nothing asked whether it can be.
    //
    // `driver::discover_build_enablers` is the search for "a feature this crate
    // does not build without", and during analysis it is gated on
    // `CRATE_REACHED_BARE_METAL` — has this crate ever compiled bare-metal. That
    // is a fact about *a* configuration. mavlink-core 0.13.1 selects its IO
    // prelude by feature: of 13 plugin passes exactly one compiles
    // (`--features embedded-hal-02`), which closes the gate, and the solve then
    // emits `std`, `embedded` and `embedded-hal-02` all off — a set with no arm
    // at all, 9 × `E0405 cannot find trait Read`, with rustc naming both
    // alternatives in its notes.
    //
    // The set that was emitted is only known here, so the question is asked here,
    // and only after that set has failed on every target — the same shape as the
    // three retries above. `set_is_witnessed` is the cheap half: when some set
    // that compiled bare-metal is a subset of what shipped, this build stands
    // where a build has stood before and the failure is somewhere else, so not a
    // probe is spent. Unwitnessed, the search costs at most `MAX_ENABLER_PROBES`
    // plugin passes pinned to one triple, and the answer is kept only if the
    // rebuild succeeds.
    if no_std && !one_succeeded {
        // What was actually on in the failed build: `main_features` closed over
        // the crate's own table, plus `default` only when defaults are not
        // disabled. `main_features` rather than `combined_features` because the
        // latter carries the `<dep>/<feat>` entries too and the question is which
        // of *this* crate's features were on; an earlier retry that failed left
        // it as it shipped. And deliberately not
        // `active_features_for_pin_set`, which inserts `default` unconditionally
        // — that widening is right for the question R34-1 asks (what may be on at
        // any later point) and wrong for this one, which is a statement about the
        // build that just failed. The `<dep>/<feat>` half of the emitted list goes
        // to the search separately: it is never a candidate, but every trial has
        // to carry it or the trial is not the configuration that failed.
        let mut seed: HashSet<String> = main_features.iter().cloned().collect();
        if !disable_default {
            seed.insert("default".to_string());
        }
        let selection = parser::close_over_local_features(&seed, &exchange.crate_info.features);

        if driver::set_is_witnessed(&main_bare_metal_sets, &selection) {
            debug!("Emitted feature set is one a bare-metal build has already compiled in");
        } else {
            // What the crate's own no_std condition entails false, plus every
            // feature that enabling would turn one of those on. Deliberately
            // *not* the solve's `disable` list: that is "the model left it
            // false", which mixes proof with don't-care, and the enabler is
            // exactly the kind of feature no constraint mentions — excluding
            // every unchosen feature would exclude the answer. `std` is normally
            // forbidden here anyway, and where it is not the oracle still rules
            // it out: a configuration that links std does not compile for a
            // bare-metal target, and only a bare-metal success counts.
            let exclude = parser::features_that_must_be_off(
                &exchange.crate_info.features,
                &exchange.main_no_std_forbidden,
            );

            let found = {
                let _t = timing::scope("emitted_set_enablers", &exchange.name_with_version);
                driver::enablers_for_selection(
                    &main_manifest,
                    &exchange.name_with_version,
                    &selection,
                    &deps_args,
                    &exclude,
                )
            };
            if !found.is_empty() {
                let mut repaired = main_features.clone();
                repaired.extend(found.iter().cloned());
                repaired.sort();
                repaired.dedup();
                let (repair_args, repair_combined, repair_len) =
                    assemble_final_args(disable_default, &repaired, &deps_args);
                println!(
                    "Build failed for every target and no bare-metal build of this crate \
                     stands on the emitted set; retrying with {:?}: {:?}",
                    found, repair_args
                );
                // A scout target is enough: the trials that produced `found`
                // already compiled the crate for a bare-metal triple, so what is
                // in question is whether the whole verification build agrees, not
                // which triple.
                let scout = compiler::scout_target(&stats, &before_build);
                if compiler::try_alternative(
                    &exchange.name_with_version,
                    &target,
                    &repair_args,
                    "retry_with_emitted_set_enablers",
                    &before_build,
                    scout.as_deref(),
                    &mut stats,
                    &mut exchange.telemetry,
                )? {
                    // The DB hands this crate's features to every later build that
                    // depends on it, so a feature the retry proved load-bearing has
                    // to change sides there too.
                    for feat in &found {
                        if !enable.contains(feat) {
                            enable.push(feat.clone());
                        }
                    }
                    disable.retain(|feat| !found.contains(feat));
                    exchange.telemetry.emitted_set_enabler_features = found;
                    final_args = repair_args;
                    combined_features = repair_combined;
                    final_features_len = repair_len;
                    one_succeeded = true;
                    // Re-derived from the set that shipped, like the repair above:
                    // only the check gets to make a statement about the emitted
                    // config.
                    violated = parser::violated_compile_error_constraints(
                        &ctx,
                        &main_attributes,
                        &exchange.crate_info,
                        &emitted_features(&combined_features),
                        !disable_default,
                    );
                    println!("Final args after adding the build enabler(s): {:?}", final_args);
                }
            }
        }
    }

    // R34-16: after every repair above has failed, ask whether each direct
    // dependency's own configuration is even buildable on its own — a
    // question none of them ask, since they all vary the *main* crate's
    // features or the emitted set as a whole. etime-0.1.8's `clock_source` is
    // the row this is built and guarded against: this pass never touched
    // that edge at all (nothing in `clock_source`'s own solve found anything
    // to disable), and it still does not compile — its default build
    // references `time_clock`, not one of its own dependencies, a defect in
    // the dependency's own default wiring no std/no_std judgment could have
    // found. `clock_source/custom` is the published fix, one edge feature
    // away, and only a failed build points at it.
    //
    // Bounded to a small probe budget: a crate can have many direct
    // dependencies, most of which are not the problem, and every candidate
    // here is a full retry build behind `try_alternative`'s cheap scout
    // pre-filter.
    if no_std && !one_succeeded {
        const DEP_EDGE_RETRY_BUDGET: usize = 8;
        let mut budget = DEP_EDGE_RETRY_BUDGET;
        let mut direct_deps: Vec<String> = exchange
            .crate_info
            .deps_and_features
            .iter()
            .filter(|(dep, _)| !dep.optional || enabled_optional_deps.contains(&dep.name))
            .map(|(dep, _)| format!("{}:{}", dep.name, dep.version))
            .collect();
        direct_deps.sort();
        direct_deps.dedup();

        'dep_edges: for dep in direct_deps {
            let candidates = parser::dep_edge_retry_candidates(
                &dep,
                &exchange.name_with_version,
                &exchange.crate_name_rename,
            );
            for candidate in candidates {
                if budget == 0 {
                    break 'dep_edges;
                }
                budget -= 1;

                let mut retry_deps = deps_args.clone();
                retry_deps.push(candidate.clone());
                let (retry_args, retry_combined, retry_len) =
                    assemble_final_args(disable_default, &main_features, &retry_deps);
                if retry_args == final_args {
                    continue;
                }
                println!(
                    "Build failed for every target; retrying dependency edge {} with {:?}: {:?}",
                    dep, candidate, retry_args
                );
                let scout = compiler::scout_target(&stats, &before_build);
                if compiler::try_alternative(
                    &exchange.name_with_version,
                    &target,
                    &retry_args,
                    "retry_with_dep_edge_enabler",
                    &before_build,
                    scout.as_deref(),
                    &mut stats,
                    &mut exchange.telemetry,
                )? {
                    // Nothing downstream reads `deps_args` again — this is the
                    // last retry in the chain — so unlike the earlier blocks
                    // there is no `deps_args = retry_deps` to keep in sync.
                    final_args = retry_args;
                    combined_features = retry_combined;
                    final_features_len = retry_len;
                    exchange
                        .telemetry
                        .dep_edge_enabler_features
                        .push((dep.clone(), candidate));
                    one_succeeded = true;
                    println!("Final args after dependency edge retry: {:?}", final_args);
                    // Re-derived from the set that shipped, like the repairs
                    // above: only the check gets to make a statement about the
                    // emitted config.
                    violated = parser::violated_compile_error_constraints(
                        &ctx,
                        &main_attributes,
                        &exchange.crate_info,
                        &emitted_features(&combined_features),
                        !disable_default,
                    );
                    break 'dep_edges;
                }
            }
        }
    }

    // R34-23: after every repair above has failed, ask the opposite question
    // of all of them — is something *already selected* the reason the build
    // fails, not something missing. `bbx-0.3.1`, `taffy-0.8.1` and `chf-0.3.1`
    // are the rows this is built and guarded against: each has the failing
    // line behind a feature the emitted set already turned on, and a strict
    // subset of that same set builds clean (`bbx` needs nothing at all;
    // `taffy` only loses `detailed_layout_info`; `chf` only loses its own
    // `alloc`). `driver::search_removals` is the mirror of KI-30's
    // `enablers_for_selection` two blocks up — same post-failure shape, same
    // probe budget, opposite direction.
    if no_std && !one_succeeded {
        let selection: HashSet<String> = main_features.iter().cloned().collect();
        let (removed, removed_dep_feats) = {
            let _t = timing::scope("emitted_set_removals", &exchange.name_with_version);
            driver::search_removals(&main_manifest, &exchange.name_with_version, &selection, &deps_args)
        };
        if !removed.is_empty() {
            let repaired: Vec<String> = main_features
                .iter()
                .filter(|f| !removed.contains(f))
                .cloned()
                .collect();
            // A `dep_features` entry tied to a removed candidate (e.g.
            // `custom_no_std_feature_enabled = ["serde/alloc", "serde/rc"]`
            // when `serde` is what just got dropped) has to leave with it, or
            // cargo re-links the very dependency the removal was trying to
            // shed — see `search_removals`'s own doc comment (redjubjub-0.8.0).
            let repaired_deps: Vec<String> = deps_args
                .iter()
                .filter(|f| !removed_dep_feats.contains(f))
                .cloned()
                .collect();
            let (repair_args, repair_combined, repair_len) =
                assemble_final_args(disable_default, &repaired, &repaired_deps);
            println!(
                "Build failed for every target; retrying dropping {:?} (and dep-feature(s) {:?}): {:?}",
                removed, removed_dep_feats, repair_args
            );
            let scout = compiler::scout_target(&stats, &before_build);
            if compiler::try_alternative(
                &exchange.name_with_version,
                &target,
                &repair_args,
                "retry_with_selected_feature_removal",
                &before_build,
                scout.as_deref(),
                &mut stats,
                &mut exchange.telemetry,
            )? {
                // Nothing downstream reads `main_features`/`deps_args` again —
                // this is the last retry in the chain — so unlike earlier
                // blocks there is no further state to keep in sync.
                final_args = repair_args;
                combined_features = repair_combined;
                final_features_len = repair_len;
                exchange.telemetry.selected_feature_removed = removed;
                one_succeeded = true;
                println!("Final args after selected-feature removal: {:?}", final_args);
                // Re-derived from the set that shipped, like the repairs
                // above: only the check gets to make a statement about the
                // emitted config.
                violated = parser::violated_compile_error_constraints(
                    &ctx,
                    &main_attributes,
                    &exchange.crate_info,
                    &emitted_features(&combined_features),
                    !disable_default,
                );
            }
        }
    }

    // KI-34: every repair above only ever reaches a feature one hop from the
    // manifest this tool edits (`<direct-dep>/<feat>`) — the only shape
    // cargo's `--features` flag accepts. A feature that lives on a
    // *transitive* package cannot be named that way at all: `getrandom`'s own
    // `compile_error!("target is not supported")` fires two hops behind
    // `rand_core` in `crypto-bigint-0.6.1`, and nothing above can spell "turn
    // on `getrandom`'s `rdrand` feature" from a manifest that never mentions
    // `getrandom`.
    //
    // rustc's own diagnostic already names the exact package and file
    // (`parser::implicated_transitive_package`), so instead of a wider
    // "propose any two-hop edge" search, this promotes *that* package to a
    // new, exact-pinned direct dependency and tries its declared features —
    // the same one-hop trust `dep_edge_retry_candidates` (R34-16) already
    // extends to a *direct* dependency's own features, just reached via a
    // synthetic edge for a package no direct edge names. Cargo unifies
    // feature flags across every path to the same resolved package instance,
    // so turning the feature on here turns it on everywhere else the package
    // is already reached, including the real edge nothing else can name.
    //
    // Bounded the same way R34-16 is: a small probe budget, one candidate
    // feature per retry, kept only if the rebuild succeeds and reverted
    // (`parser::restore_manifest_text`) otherwise, so a crate this does not
    // help ends this step exactly where it would have without it.
    if no_std && !one_succeeded {
        let transitive_errors: Vec<String> = compiler::errors_since(&stats, &before_build);
        let existing_dep_keys: HashSet<String> = toml::from_str::<toml::Value>(
            &std::fs::read_to_string(&main_manifest).unwrap_or_default(),
        )
        .ok()
        .and_then(|v| {
            v.get("dependencies")
                .and_then(|d| d.as_table())
                .map(|t| t.keys().cloned().collect())
        })
        .unwrap_or_default();

        if let Some((pkg_name, pkg_manifest, pkg_version)) =
            parser::implicated_transitive_package(&transitive_errors, &existing_dep_keys)
        {
            let mut candidates: Vec<String> =
                nostd::visitor::declared_features(&pkg_manifest.to_string_lossy())
                    .into_iter()
                    .filter(|f| f != "default")
                    // A feature named exactly `custom` is, by ecosystem
                    // convention (getrandom, and the same shape as
                    // `#[panic_handler]`/`#[global_allocator]`), a hook the
                    // *final binary* must register an implementation for —
                    // getrandom's own doc comment names the failure mode
                    // outright: "Attempting to register a function in a
                    // non-root crate will result in a linker error." Every
                    // build this tool runs is `cargo build --lib`, which
                    // never reaches that link step, so a lib-only success
                    // with `custom` enabled proves nothing: it would "pass"
                    // on every target regardless of whether any real backend
                    // exists (confirmed live: `custom` alone flips all 33
                    // `TARGET_LIST` members to Success for crypto-bigint,
                    // getrandom's own docs notwithstanding). Excluding it by
                    // name is conservative — the only cost is a missed fix on
                    // whatever different, benign thing a *different* package's
                    // own "custom" feature might mean — never a false one.
                    .filter(|f| f != "custom")
                    .collect();
            candidates.sort();

            const TRANSITIVE_EDGE_RETRY_BUDGET: usize = 8;
            for feat in candidates.into_iter().take(TRANSITIVE_EDGE_RETRY_BUDGET) {
                println!(
                    "Build failed for every target; promoting transitive package {} ({}), \
                     not a direct dependency, to a synthetic edge and trying feature {:?}",
                    pkg_name, pkg_version, feat
                );
                let original_manifest = parser::add_synthetic_dependency(
                    &exchange.name_with_version,
                    &pkg_name,
                    &pkg_version,
                    std::slice::from_ref(&feat),
                );
                // No scout target: `scout_target` picks the target with the
                // *most common* failure signature, which for a package that
                // fails identically everywhere (getrandom's own
                // `compile_error!` does) is an arbitrary tie-broken member of
                // `TARGET_LIST`, not necessarily one this candidate feature
                // can help — `rdrand` only fixes an x86/x86_64 target, and a
                // scout pre-check on, say, `aarch64-unknown-none` would fail
                // and abandon the retry before the one target that actually
                // works is ever tried. Every candidate needs the full sweep.
                if compiler::try_alternative(
                    &exchange.name_with_version,
                    &target,
                    &final_args,
                    "retry_with_transitive_package_edge",
                    &before_build,
                    None,
                    &mut stats,
                    &mut exchange.telemetry,
                )? {
                    exchange.telemetry.transitive_package_edge_added.push((
                        pkg_name.clone(),
                        pkg_version.clone(),
                        feat.clone(),
                    ));
                    one_succeeded = true;
                    println!(
                        "Synthetic dependency added: {} = {{ version = \"={}\", \
                         default-features = false, features = [{:?}] }}",
                        pkg_name, pkg_version, feat
                    );
                    // Re-derived from the set that shipped, like the repairs
                    // above: only the check gets to make a statement about the
                    // emitted config. `final_args` itself did not change — the
                    // fix lives entirely in the manifest edit just kept.
                    violated = parser::violated_compile_error_constraints(
                        &ctx,
                        &main_attributes,
                        &exchange.crate_info,
                        &emitted_features(&combined_features),
                        !disable_default,
                    );
                    break;
                }
                parser::restore_manifest_text(&exchange.name_with_version, &original_manifest);
            }
        }
    }

    // R34-17: after every repair above has failed, ask whether the emitted
    // argv — unchanged, not one of the repairs above — builds given an
    // operating system. `TARGET_LIST` is bare-metal only by design (an OS
    // target would hide a real std-linkage bug, since std exists there), so
    // this never repairs a HARD verdict the way the retries above do; it only
    // tells the difference apart between "nothing about this crate's std
    // usage is provably right" and "the crate is no_std, TARGET_LIST just has
    // nowhere to say so" (`sc-0.2.7`'s shape: `#[cfg(target_os = "linux")]
    // mod platform;`, no bare-metal arm at all). See the `os_target_probe`
    // field doc for what this telemetry does and does not claim.
    //
    // `consts::OS_TARGET_PROBES`, tried in order, first build wins — the
    // point is "does any OS make this compile", not which one.
    if no_std && !one_succeeded {
        for os_target in consts::OS_TARGET_PROBES.iter() {
            let probe_mark = compiler::mark_build_records(&stats, &exchange.telemetry);
            let built = {
                let _t = timing::scope("os_target_probe", &exchange.name_with_version);
                compiler::try_compile(
                    &exchange.name_with_version,
                    os_target,
                    &final_args,
                    &mut stats,
                    &mut exchange.telemetry,
                )?
            };
            // Never kept: an OS target is not a `TARGET_LIST` member, and
            // counting its record there would corrupt the success/fail
            // bookkeeping a HARD verdict is read off.
            compiler::rewind_build_records(&mut stats, &mut exchange.telemetry, &probe_mark);
            if built {
                println!(
                    "Emitted argv does not build on any TARGET_LIST member but builds on \
                     {os_target} — recording os_target_probe, not a repair (R34-17)"
                );
                exchange.telemetry.os_target_probe = Some(os_target.to_string());
                break;
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

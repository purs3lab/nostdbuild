#![feature(rustc_private)]

use std::collections::HashSet;

use anyhow::Ok;
use clap::Parser;
use log::debug;

use nostd::{Attributes, compiler, consts, db, downloader, driver, parser, solver};

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
) -> anyhow::Result<()> {
    // Check the DB first: if we already have a result for this dep, skip the expensive
    // gather_crate_info + analyze_crate_wrapper + process_crate path entirely.
    let (local_dep_args, dep_disable, dep_enable) =
        if let Some(db_entry) = db::get_from_db_data(&exchange.db_data, &dep.crate_name) {
            debug!(
                "DB hit for dependency {}, skipping analysis",
                dep.crate_name
            );
            let (enable, disable) = (db_entry.features.0.clone(), db_entry.features.1.clone());
            // DB hit — no dep_root available; pass empty map (no protection check for this dep).
            parser::finalize_dep_crate(
                exchange,
                dep,
                enable,
                disable,
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

    let all_dep_feats: Vec<String> = exchange
        .crate_info
        .features
        .iter()
        .flat_map(|(_, feats)| feats.iter())
        .filter(|(name, _)| *name == crate_name)
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
    );

    parser::move_unnecessary_dep_feats(
        &exchange.name_with_version,
        enable,
        main_features,
        &dep.crate_name,
        &dep_enable,
        &mut exchange.telemetry,
        *disable_default,
        &exchange.protected_dep_features,
    );
    Ok(())
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    env_logger::init();

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
    let (mut worklist, crate_name_rename, mut crate_info) =
        downloader::gather_crate_info(&name, cli.dry_run, None)?;
    telemetry.num_deps = crate_info.deps_and_features.len();

    debug!("Dependencies: {:?}", crate_info);

    let cfg = z3::Config::new();
    let ctx = z3::Context::new(&cfg);
    let found = parser::check_for_no_std(&name, &ctx, Some(&mut telemetry), None);

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
    let no_std = downloader::download_all_dependencies(
        &name,
        &mut worklist,
        &mut crate_info,
        depth,
        &mut telemetry,
        &mut top_level_deps,
    )?;

    let mut exchange = nostd::DataExchange {
        name_with_version: name,
        db_data,
        crate_info,
        telemetry,
        crate_name_rename,
        valid_cross_crate_items: std::collections::HashSet::new(),
        main_enable: Vec::new(),
        protected_dep_features: std::collections::HashSet::new(),
    };

    stats.crate_info = Some(exchange.crate_info.clone());
    exchange.telemetry.no_std = found;
    exchange.telemetry.dep_not_no_std = !no_std;

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
        reason = "Std usage in the main crate could not be proven avoidable";
        stats.telemetry = Some(exchange.telemetry);
        stats.dump(true);
        return Err(anyhow::anyhow!(reason));
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
    let (optdep_constraints, optdep_enablers) = driver::optional_dep_link_constraints(
        &ctx,
        &driver::read_manifest_toml(&main_manifest),
        &nostd::visitor::declared_features(&main_manifest),
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

    let (mut enable, mut disable) = parser::process_crate(
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
    let non_minimalizable: HashSet<String> = main_features
        .iter()
        .chain(enable.iter())
        .filter(|f| {
            ce_features.contains(*f)
                || hard_constraint_features.contains(*f)
                || optdep_enablers.contains(*f)
        })
        .cloned()
        .collect();
    debug!("Non-minimalizable main features: {:?}", non_minimalizable);

    parser::minimize(
        &exchange.crate_info,
        &mut dep_and_feats,
        &mut main_features,
        &non_minimalizable,
        disable_default,
        &exchange.name_with_version,
        None,
        None,
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
        )?;
    }

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
            )?;
        }
    }

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
    );

    deps_args.extend(dep_args_skipped);

    println!("Dep arguments: {:?}", deps_args);
    println!(
        "Main crate arguments after processing deps: {:?}",
        main_features
    );

    main_features.sort();
    main_features.dedup();
    let (mut final_args, mut combined_features, mut final_features_len) =
        assemble_final_args(disable_default, &main_features, &deps_args);

    println!("Final args: {:?}", final_args);
    let before_build = compiler::mark_build_records(&stats, &exchange.telemetry);
    let mut one_succeeded = if no_std {
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
            let retry_succeeded = compiler::try_compile(
                &exchange.name_with_version,
                &target,
                &retry_args,
                &mut stats,
                &mut exchange.telemetry,
            )?;
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

    exchange.telemetry.final_features_length = final_features_len;

    // Verify the feature set we built actually satisfies the crate's own
    // `compile_error!` conditions. The stage-2 check inside `process_crate` leaves
    // unselected features free and so is trivially satisfiable; this one closes the
    // world. Runs on the emitted set, so a retry above is what gets checked.
    let violated = parser::violated_compile_error_constraints(
        &ctx,
        &main_attributes,
        &exchange.crate_info,
        &combined_features
            .iter()
            .flat_map(|s| s.split(','))
            .map(str::to_string)
            .collect::<Vec<_>>(),
        !disable_default,
    );
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

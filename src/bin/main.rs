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

    let (mut worklist, crate_name_rename, mut crate_info) =
        downloader::gather_crate_info(&name, false, None)?;
    telemetry.num_deps = crate_info.deps_and_features.len();

    debug!("Dependencies: {:?}", crate_info);

    let cfg = z3::Config::new();
    let ctx = z3::Context::new(&cfg);
    let found = parser::check_for_no_std(&name, &ctx, Some(&mut telemetry), None);

    if !found || telemetry.wrong_unconditional_setup {
        stats.telemetry = Some(telemetry);
        stats.dump(true);
        if !found {
            return Err(anyhow::anyhow!("Main crate does not support no_std build"));
        } else {
            return Err(anyhow::anyhow!(
                "Main crate has incorrect unconditional no_std setup"
            ));
        }
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

    let mut failed = false;
    let mut reason = "";

    if !all_hard.is_empty() {
        exchange.telemetry.unguarded_std_usages = true;
        debug!("ERROR: Found unguarded std usage in the main crate");
        reason = "Found unguarded std usage in the main crate";
        stats.std_usage_matches = all_hard;
        stats.dump(true);
        return Err(anyhow::anyhow!(reason));
    }

    let mut main_attributes =
        parser::parse_crate(&exchange.name_with_version, true, None, &all_hard);

    let mut dep_and_feats = parser::features_for_optional_deps(&exchange.crate_info);

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

    let (mut enable, disable) = parser::process_crate(
        &mut exchange,
        &ctx,
        &mut main_attributes,
        None,
        None,
        true,
        &mut dep_and_feats,
        hard_constraints,
    )?;

    exchange.main_enable = enable.clone();

    // Derive non_minimalizable from the intersection of the solved enable list and the feature
    // names that must hold for no_std. Two sources:
    //   - compile_error conditions: avoids arbitrary Z3 picks from disjunctive constraints
    //     (e.g. uom's "at least one storage type" rule) selecting a feature that pulls in std.
    //   - hard constraints (final_condition): features a probe proved are required to avoid std
    //     (e.g. lazy_static's `spin_no_std`, which guards `extern crate std` in inline_lazy.rs).
    //     Without this the main-level minimize would strip such a feature as a droppable
    //     optional-dep enabler even though it is load-bearing for no_std.
    // Only features already needed for no_std can appear here.
    let non_minimalizable: HashSet<String> = {
        let ce_features = parser::compile_error_feature_names(&main_attributes, &ctx);
        enable
            .iter()
            .filter(|f| ce_features.contains(*f) || hard_constraint_features.contains(*f))
            .cloned()
            .collect()
    };

    println!(
        "Initial main crate features to enable: {:?}, features to disable: {:?}",
        enable, disable
    );

    if cli.dry_run {
        println!("Dry run enabled, exiting now!");
        stats.dump(true);
        return Ok(());
    }

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

    let mut final_args = Vec::new();
    let mut combined_features = Vec::new();
    let mut final_features_len = main_features.len();
    main_features.sort();
    main_features.dedup();
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
    exchange.telemetry.final_features_length = final_features_len;

    println!("Final args: {:?}", final_args);
    let one_succeeded = if no_std {
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

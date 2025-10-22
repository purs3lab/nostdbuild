#![feature(rustc_private)]

use clap::Parser;
use log::debug;

use nostd::{compiler, consts, db, downloader, hir, parser, solver};

#[derive(Parser, Debug)]
#[command(author, version, about)]
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

    #[arg(long)]
    /// Enabling this will cause the tool to try minimizing the number of features enabled
    /// to achieve a successful no_std build. Currently it only does so by removing features
    /// for optional dependencies.
    /// By default also, we don't find the maximal set of features that can be enabled. But this
    /// flag will try to minimize from the default behavior as well.
    minimize: bool,
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

    let mut db_data = db::read_db_file()?;
    let mut telemetry = nostd::Telemetry::default();

    if let Some(url) = cli.url {
        debug!("URL provided: {}", url);
        if downloader::clone_repo(&url, &name).is_err() {
            return Err(anyhow::anyhow!("Failed to clone repo"));
        }
    } else {
        debug!("Downloading from crates.io");
        name = downloader::clone_from_crates(&name, cli.version.as_ref())?;
        debug!("Downloaded crate: {}", name);
    }

    let mut stats = nostd::AllStats::new(name.clone());
    println!("Processing crate: {}", name);
    let (temp_name, version) = name.split_once(':').unwrap_or((&name, "latest"));
    telemetry.name = temp_name.to_string();
    telemetry.version = version.to_string();
    let (mut worklist, crate_name_rename, mut crate_info) =
        downloader::gather_crate_info(&name, false)?;
    telemetry.num_deps = crate_info.deps_and_features.len();

    debug!("Dependencies: {:?}", crate_info);

    let cfg = z3::Config::new();
    let ctx = z3::Context::new(&cfg);
    let found = parser::check_for_no_std(&name, &ctx);

    if !found {
        stats.telemetry = Some(telemetry);
        stats.dump();
        return Err(anyhow::anyhow!("Main crate does not support no_std build"));
    }

    let no_std = downloader::download_all_dependencies(
        &mut worklist,
        &mut crate_info,
        depth,
        &mut telemetry,
    )?;
    stats.crate_info = Some(&crate_info);
    telemetry.no_std = found;
    telemetry.dep_not_no_std = !no_std;

    let main_attributes = parser::parse_crate(&name, true);

    let readable_spans = hir::proc_macro_span_to_readable(&main_attributes.spans);
    let dep_and_feats = parser::features_for_optional_deps(&crate_info);
    let (mut enable, disable) = parser::process_crate(
        &ctx,
        &main_attributes,
        &name,
        &db_data,
        &crate_info,
        true,
        &mut telemetry,
        cli.minimize,
        &dep_and_feats,
    )?;

    if cli.dry_run {
        println!("Dry run enabled, exiting now!");
        return Ok(());
    }

    let (disable_default, mut main_features) =
        solver::final_feature_list_main(&crate_info, &mut enable, &disable, &mut telemetry);

    debug!("Dependency and features: {:?}", dep_and_feats);

    println!("Main crate arguments: {:?}", main_features);

    let deps_attrs = parser::parse_deps_crate();
    let mut skipped = Vec::new();
    // Solve for each dependency
    // TODO: Some dependencies are from git instead of crates.io. Handle those cases.
    // TODO: There are some cleanup and refactoring to minimize the read -> mutate -> write pattern for the toml
    // TODO: Use better mechanism to get the .rs file to check for no_std (use metadata to get this).
    // TODO: Add checks to make sure all deps at all depths actually can be compiled with the given set of features in the
    // crate that depends on them. -> This is currently implemented and only checks if
    // the feature requirements can be met, not if they are actually met with the set of features enabled by that crate for
    // no_std compilation.
    let mut deps_args = Vec::new();
    for dep in deps_attrs {
        if consts::KNOWN_SYN_FAILURES.contains(&dep.crate_name.as_str()) {
            debug!(
                "Dependency {} has known syntex failure, skipping",
                dep.crate_name
            );
            continue;
        }

        if parser::should_skip_dep(
            &dep.crate_name,
            &crate_info,
            &dep_and_feats,
            &main_features,
            disable_default,
            &mut telemetry,
            false,
        ) {
            debug!("Dependency {} is optional, skipping", dep.crate_name);
            skipped.push(dep);
            continue;
        }

        debug!("Processing dependency: {}", dep.crate_name);

        let local_dep_args = parser::process_dep_crate(
            &ctx,
            &dep,
            &name,
            &mut db_data,
            &crate_info,
            &crate_name_rename,
            &mut telemetry,
            cli.minimize,
        )?;
        deps_args.extend(local_dep_args);

        parser::move_unnecessary_dep_feats(
            &name,
            &enable,
            &mut main_features,
            &dep.crate_name,
            &deps_args,
            &mut telemetry,
        );
    }

    let mut temp_combined = deps_args.clone();
    temp_combined.sort();
    temp_combined.dedup();
    temp_combined.extend(main_features.clone());

    let mut dep_args_skipped = Vec::new();
    for dep in skipped {
        if !parser::should_skip_dep(
            &dep.crate_name,
            &crate_info,
            &dep_and_feats,
            &temp_combined,
            disable_default,
            &mut telemetry,
            true,
        ) {
            debug!(
                "Dependency {} which was skipped previously is now required",
                dep.crate_name
            );
            let local_dep_args = parser::process_dep_crate(
                &ctx,
                &dep,
                &name,
                &mut db_data,
                &crate_info,
                &crate_name_rename,
                &mut telemetry,
                cli.minimize,
            )?;

            dep_args_skipped.extend(local_dep_args);
            parser::move_unnecessary_dep_feats(
                &name,
                &enable,
                &mut main_features,
                &dep.crate_name,
                &dep_args_skipped,
                &mut telemetry,
            );
        }
    }
    deps_args.extend(dep_args_skipped);

    let mut final_args = Vec::new();
    let mut combined_features = Vec::new();
    let mut final_features_len = main_features.len();
    main_features.extend(enable.clone());
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
        final_args.push("--features".to_string());
        final_args.push(combined_features.join(","));
    }
    telemetry.final_features_length = final_features_len;

    println!("Final args: {:?}", final_args);
    let one_succeeded = if no_std {
        compiler::try_compile(&name, &target, &final_args, &mut stats, &mut telemetry)
    } else {
        Ok(false)
    }?;

    if one_succeeded {
        telemetry.build_success = true;
        db::add_to_db_data(&mut db_data, &name, (&enable, &disable));
    } else {
        hir::hir_visit(&name, &mut telemetry);
        telemetry.hir_analysis_done = true;
        if hir::check_for_unguarded_std_usages(&readable_spans, &mut stats) {
            telemetry.unguarded_std_usages = true;
            debug!("ERROR: Found unguarded std usage in the main crate");
        } else if !parser::recursive_dep_requirement_check(&crate_info, &db_data, depth) {
            // This is the last resort since this has a high chance of false positives
            debug!(
                "ERROR: Some dependency at some level does not have a way to enable all its required features in no_std mode"
            );
        }
    }

    db::write_db_file(db_data)?;

    stats.telemetry = Some(telemetry);
    stats.dump();
    Ok(())
}

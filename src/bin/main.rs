#![feature(rustc_private)]

use anyhow::Context;
use clap::Parser;
use log::debug;
use std::fs;

use nostd::{CrateInfo, compiler, consts, db, downloader, hir, parser, solver};

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
    dry_run: bool,

    #[arg(long)]
    depth: Option<u32>,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    env_logger::init();
    let mut worklist: Vec<(String, String)> = Vec::new();
    let mut crate_name_rename: Vec<(String, String)> = Vec::new();
    let mut crate_info: CrateInfo = CrateInfo::default();

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
            debug!("No target provided, will use either crates target or all targets");
            "".to_string()
        }
    };

    let depth = match cli.depth {
        Some(depth) if depth > 0 => depth,
        Some(_) => 4,
        None => 4,
    };

    let mut db_data = db::read_db_file()?;
    let mut results = Vec::new();

    if let Some(url) = cli.url {
        debug!("URL provided: {}", url);
        if downloader::clone_repo(&url, &name).is_err() {
            return Err(anyhow::anyhow!("Failed to clone repo"));
        }
    } else {
        debug!("Downloading from crates.io");
        name = downloader::clone_from_crates(&name, None)?;
        debug!("Downloaded crate: {}", name);
    }
    downloader::init_worklist(
        &name,
        &mut crate_name_rename,
        &mut worklist,
        &mut crate_info,
    )?;

    debug!("Dependencies: {:?}", crate_info);

    let cfg = z3::Config::new();
    let ctx = z3::Context::new(&cfg);
    let found = parser::check_for_no_std(&name, &ctx);

    if !found {
        return Err(anyhow::anyhow!("Main crate does not support no_std build"));
    }

    // Here we collect all std usages with the default features enabled.
    // This is done to check for std usages that are not behind any attribute guards.
    // If any such usage is found, we stop further processing.
    hir::do_first_pass(&name);

    downloader::download_all_dependencies(&mut worklist, &mut crate_info, depth)?;

    let main_attributes = parser::parse_crate(&name, true);
    let (enable, disable, _) = parser::process_crate(
        &ctx,
        &main_attributes,
        &name,
        &mut db_data,
        &crate_info,
        true,
    )?;

    if cli.dry_run {
        println!("Dry run enabled, exiting now!");
        return Ok(());
    }

    let (disable_default, mut main_features) =
        solver::final_feature_list_main(&crate_info, &enable, &disable);

    let dep_and_feats = parser::features_for_optional_deps(&crate_info);

    println!("Main crate arguments: {:?}", main_features,);

    let deps_attrs = parser::parse_deps_crate();
    let mut skipped = Vec::new();
    // Solve for each dependency
    // TODO: Some dependencies are from git instead of crates.io. Handle those cases (check tool_error_or_crate_issue file).
    // TODO: Should we disable default features for main crate if we update its default features list to include dependency's default features?
    // TODO: There are some cleanup and refactoring to minimize the read -> mutate -> write pattern for the toml
    // TODO: Optionally, do a cyclic check of features that gets enabled from the default list due to default disabling to make sure it does not cause issues.
    // TODO: Use better mechanism to get the .rs file to check for no_std (use metadata to get this).
    let mut deps_args = Vec::new();
    for dep in deps_attrs {
        if consts::KNOWN_SYN_FAILURES.contains(&dep.crate_name.as_str()) {
            debug!(
                "Dependency {} has known syntex failure, skipping",
                dep.crate_name
            );
            continue;
        }

        if parser::should_skip_dep(&dep.crate_name, &crate_info, &dep_and_feats, &main_features) {
            debug!("Dependency {} is optional, skipping", dep.crate_name);
            skipped.push(dep);
            continue;
        }
        let local_dep_args = parser::process_dep_crate(
            &ctx,
            &dep,
            &name,
            &mut db_data,
            &crate_info,
            &crate_name_rename,
        )?;
        deps_args.extend(local_dep_args);

        parser::move_unnecessary_dep_feats(
            &name,
            &enable,
            &mut main_features,
            &dep.crate_name,
            &deps_args,
        );
    }

    let mut dep_args_skipped = Vec::new();
    for dep in skipped {
        if !parser::should_skip_dep(&dep.crate_name, &crate_info, &dep_and_feats, &deps_args) {
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
            )?;

            dep_args_skipped.extend(local_dep_args);
            parser::move_unnecessary_dep_feats(
                &name,
                &enable,
                &mut main_features,
                &dep.crate_name,
                &dep_args_skipped,
            );
        }
    }

    let mut final_args = Vec::new();
    let mut combined_features = Vec::new();
    main_features.extend(enable.clone());
    let main_feature_string = main_features.join(",");

    if !deps_args.is_empty() {
        if !main_feature_string.is_empty() {
            deps_args.retain(|x| !main_feature_string.contains(x));
        }
        deps_args.sort();
        deps_args.dedup();
    }

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

    println!("Final args: {:?}", final_args);
    // This is temporary, we will remove it later
    let possible_archs = Vec::new();
    let one_succeeded =
        compiler::try_compile(&name, &target, &final_args, &possible_archs, &mut results)?;

    if one_succeeded {
        db::add_to_db_data(&mut db_data, &name, (&enable, &disable));
    }

    db::write_db_file(db_data)?;
    db::write_final_json(&name, &results);
    let dir = std::path::Path::new(consts::DOWNLOAD_PATH).join(name.replace(':', "-"));
    let manifest = parser::determine_manifest_file(&name);
    fs::copy(dir.join("Cargo.toml.bak"), &manifest)
        .context("Failed to restore original Cargo.toml")?;
    fs::remove_file(dir.join("Cargo.toml.bak")).context("Failed to remove backup Cargo.toml")?;
    Ok(())
}

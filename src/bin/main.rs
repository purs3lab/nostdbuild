use anyhow::Context;
use clap::Parser;
use env_logger;
use log::debug;
use std::fs;

use nostd::{compiler, consts, db, downloader, parser, solver, CrateInfo};

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
        if let Err(_) = downloader::clone_repo(&url, &name) {
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
    
    downloader::download_all_dependencies(&mut worklist, &mut crate_info, depth)?;
    
    let main_attributes = parser::parse_crate(&name, true);
    let (enable, disable, recurse, _) = parser::process_crate(
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

    let (disable_default, main_features) =
        solver::final_feature_list_main(&crate_info, &enable, &disable);

    let dep_and_feats = parser::features_for_optional_deps(&crate_info);

    println!(
        "Main crate arguments: {:?} with recurse as {}",
        main_features, recurse
    );

    let deps_attrs = parser::parse_deps_crate();
    let mut skipped = Vec::new();
    // Solve for each dependency
    // TODO: Some dependencies are from git instead of crates.io. Handle those cases (check tool_error_or_crate_issue file).
    // TODO: If a feature has ? in it before the /, handle this case.
    // TODO: Should we disable default features for main crate if we update its default features list to include dependency's default features?
    // TODO: Fix syn failure when it encounters `yield` keyword in the code.
    // TODO: There are some cleanup and refactoring to minimize the read -> mutate -> write pattern for the toml
    let mut deps_args = Vec::new();
    for dep in deps_attrs {
        if parser::should_skip_dep(&dep.crate_name, &crate_info, &dep_and_feats, &main_features) {
            debug!("Dependency {} is optional, skipping", dep.crate_name);
            skipped.push(dep);
            continue;
        }
        parser::process_dep_crate(
            &ctx,
            &dep,
            &name,
            &mut db_data,
            &crate_info,
            &mut deps_args,
            &crate_name_rename,
        )?;
    }

    let mut dep_args_skipped = Vec::new();
    for dep in skipped {
        if !parser::should_skip_dep(&dep.crate_name, &crate_info, &dep_and_feats, &deps_args) {
            debug!(
                "Dependency {} which was skipped previously is now required",
                dep.crate_name
            );
            parser::process_dep_crate(
                &ctx,
                &dep,
                &name,
                &mut db_data,
                &crate_info,
                &mut dep_args_skipped,
                &crate_name_rename,
            )?;
        }
    }

    let mut final_args = Vec::new();
    let mut combined_features = Vec::new();
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
    compiler::try_compile(&name, &target, &final_args, &possible_archs, &mut results)?;
    db::write_db_file(db_data)?;
    db::write_final_json(&name, &results);
    let dir = std::path::Path::new(consts::DOWNLOAD_PATH).join(name.replace(':', "-"));
    let filename = parser::determine_cargo_toml(&name);
    fs::copy(dir.join("Cargo.toml.bak"), &filename)
        .context("Failed to restore original Cargo.toml")?;
    fs::remove_file(dir.join("Cargo.toml.bak")).context("Failed to remove backup Cargo.toml")?;
    Ok(())
}

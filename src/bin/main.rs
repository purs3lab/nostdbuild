use clap::Parser;
use env_logger;
use log::debug;

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
    downloader::download_all_dependencies(&mut worklist, &mut crate_info)?;

    debug!("Dependencies: {:?}", crate_info);

    let cfg = z3::Config::new();
    let ctx = z3::Context::new(&cfg);
    let found = parser::check_for_no_std(&name, &ctx);

    if !found {
        return Err(anyhow::anyhow!("Main crate does not support no_std build"));
    }

    let main_attributes = parser::parse_crate(&name, true);
    let (enable, disable, recurse, mut possible_archs) = parser::process_crate(
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

    let mut main_args = solver::final_feature_list_main(&crate_info, &enable, &disable);

    println!(
        "Main crate arguments: {:?} with recurse as {}",
        main_args, recurse
    );

    let deps_attrs = parser::parse_deps_crate();
    let mut skipped = Vec::new();
    // Solve for each dependency
    // TODO: Handle optional dependencies
    // TODO: Some dependencies are from git instead of crates.io. Handle those cases (check tool_error_or_crate_issue file).
    // TODO: If a feature is not there in the cargo.toml, add it and then do the build.
    let mut deps_args = Vec::new();
    for dep in deps_attrs {
        if parser::is_dep_optional(&crate_info, &dep.crate_name.split(":").next().unwrap_or("")) {
            debug!("Dependency {} is optional, skipping", dep.crate_name);
            skipped.push(dep);
            continue;
        }

        let found = parser::check_for_no_std(&dep.crate_name, &ctx);
        assert!(
            found,
            "Dependency {} does not support no_std build",
            dep.crate_name
        );

        let (enable, disable, _, possible) = parser::process_crate(
            &ctx,
            &dep,
            &dep.crate_name,
            &mut db_data,
            &crate_info,
            false,
        )?;
        possible_archs.extend(possible);
        possible_archs.sort();
        possible_archs.dedup();

        let (args, update_default_config) = solver::final_feature_list_dep(
            &crate_info,
            &dep.crate_name.split(":").next().unwrap_or(""),
            &enable,
            &disable,
        );

        debug!(
            "Dependency requires default config update: {}",
            update_default_config
        );

        if update_default_config {
            parser::update_main_crate_default_list(&name, &dep.crate_name, &crate_name_rename);
        }

        debug!(
            "Final arguments for dependency {}: {:?}",
            dep.crate_name, args
        );

        if !args.is_empty() {
            deps_args.extend(args);
        }
    }

    if !deps_args.is_empty() {
        if !main_args.contains(&"--features".to_string()) {
            main_args.push("--features".to_string());
        } else {
            // If we already have --features, we need to add a comma to separate features
            main_args.push(",".to_string());
        }
        deps_args.sort();
        deps_args.dedup();
        main_args.push(deps_args.join(","));
    }

    println!("Final args: {:?}", main_args);
    compiler::try_compile(&name, &target, &main_args, &possible_archs, &mut results)?;
    db::write_db_file(db_data)?;
    db::write_final_json(&name, &results);
    Ok(())
}

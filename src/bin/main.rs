use clap::Parser;
use env_logger;
use log::debug;

use nostd::consts;
use nostd::db;
use nostd::downloader;
use nostd::parser;
use nostd::solver;
use nostd::CrateInfo;

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Cli {
    #[arg(long)]
    url: Option<String>,

    #[arg(long)]
    name: Option<String>,

    #[arg(long)]
    target: Option<String>,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    env_logger::init();
    let mut worklist: Vec<(String, String)> = Vec::new();
    let mut crate_info: CrateInfo = CrateInfo::default();

    let mut name = match cli.name {
        Some(name) => name,
        None => {
            return Err(anyhow::anyhow!("Name is required"));
        }
    };

    if let Some(target) = cli.target {
        if !consts::is_valid_target(&target) {
            return Err(anyhow::anyhow!(
                "Invalid target `{}`. Choose one of {:?}",
                target,
                consts::TARGET_LIST
            ));
        }
    } else {
        debug!("No target provided, will use either crates target or all targets");
    }

    let mut db_data = db::read_db_file()?;

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
    downloader::init_worklist(&name, &mut worklist, &mut crate_info)?;
    downloader::download_all_dependencies(&mut worklist, &mut crate_info)?;

    debug!("Dependencies: {:?}", crate_info);

    let cfg = z3::Config::new();
    let ctx = z3::Context::new(&cfg);

    let main_attributes = parser::parse_crate(&name);
    let (enable, disable, found, recurse) =
        parser::process_crate(&ctx, &main_attributes, &name, &mut db_data, true)?;

    if !found {
        return Err(anyhow::anyhow!("Main crate does not support no_std build"));
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
    // TODO: Handle recursing into dependencies based on unconditional no_std support
    for dep in deps_attrs {
        if parser::is_dep_optional(&crate_info, &dep.crate_name.split(":").next().unwrap_or("")) {
            debug!("Dependency {} is optional, skipping", dep.crate_name);
            skipped.push(dep);
            continue;
        }

        let (enable, disable, found, _) =
            parser::process_crate(&ctx, &dep, &dep.crate_name, &mut db_data, false)?;

        if !found {
            debug!(
                "Dependency {} does not support no_std build",
                dep.crate_name
            );
            continue;
        }

        let args = solver::final_feature_list_dep(
            &crate_info,
            &dep.crate_name.split(":").next().unwrap_or(""),
            &enable,
            &disable,
        );

        debug!(
            "Final arguments for dependency {}: {:?}",
            dep.crate_name, args
        );

        if !args.is_empty() {
            if !main_args.contains(&"--features".to_string()) {
                main_args.push("--features".to_string());
            }
            for arg in args {
                if !main_args.contains(&arg) {
                    main_args.push(arg);
                }
            }
        }
    }

    println!("Final args: {:?}", main_args);

    db::write_db_file(db_data)?;
    Ok(())
}

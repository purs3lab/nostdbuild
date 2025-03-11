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

    let db_data = db::read_db_file()?;

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
    let mut enable: Vec<String> = Vec::new();
    let mut disable: Vec<String> = Vec::new();

    let (no_std, main_equation, main_parsed_attr) =
        parser::parse_main_attributes(&main_attributes, &ctx);
    if !main_attributes.unconditional_no_std {
        if !no_std {
            debug!("No no_std found for the main crate, exiting");
            return Ok(());
        }

        let (main_equations, _possible_archs) = parser::parse_attributes(&main_attributes, &ctx);
        let filtered = parser::filter_equations(&main_equations, &main_parsed_attr.features);

        // This solves for the main crate
        let model = solver::solve(&ctx, &main_equation, &filtered);
        (enable, disable) = solver::model_to_features(&model);
        println!("Features for main create: {:?} {:?}", enable, disable);
    } else {
        // Crate should not be both conditional and unconditional no_std
        assert!(!no_std);
        debug!("Main crate is an unconditional no_std crate implmentation not yet done");
        let items = parser::parse_item_extern_crates(&name);
        if parser::if_any_item_extern_std(&items) {
            debug!("Leaf level crate reached {}", name);
        } else {
            todo!();
        }
    }

    let finals_args = solver::final_feature_list_main(&crate_info, &enable, &disable);
    debug!("Final arguments for main crate: {:?}", finals_args);

    let deps_attrs = parser::parse_deps_crate();
    // Solve for each dependency
    for dep in deps_attrs {
        let (no_std, dep_equation, dep_parsed_attr) = parser::parse_main_attributes(&dep, &ctx);
        if !dep.unconditional_no_std {
            if !no_std {
                debug!(
                    "No no_std found for the crate {}, continuing",
                    dep.crate_name
                );
                continue;
            }

            let (dep_equations, _possible_archs) = parser::parse_attributes(&dep, &ctx);
            let filtered = parser::filter_equations(&dep_equations, &dep_parsed_attr.features);

            let model = solver::solve(&ctx, &dep_equation, &filtered);
            (enable, disable) = solver::model_to_features(&model);
            println!(
                "Features for crate {}: {:?} {:?}",
                dep.crate_name, enable, disable
            );
        } else {
            // Crate should not be both conditional and unconditional no_std
            assert!(!no_std);
            debug!(
                "Dependency {} is an unconditional no_std crate implmentation not yet done",
                dep.crate_name
            );
            let items = parser::parse_item_extern_crates(&dep.crate_name);
            if parser::if_any_item_extern_std(&items) {
                debug!("Leaf level crate reached {}", dep.crate_name);
            } else {
                todo!();
            }
        }

        let dep_args = solver::final_feature_list_dep(
            &crate_info,
            &dep.crate_name.split(":").next().unwrap_or(""),
            &enable,
            &disable,
        );
        debug!(
            "Final arguments for dependency {}: {:?}",
            dep.crate_name, dep_args
        );
    }

    db::write_db_file(db_data)?;
    Ok(())
}

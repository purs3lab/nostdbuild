use clap::Parser;
use env_logger;
use log::debug;

use nostd::consts;
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
    let mut enable: Vec<String>;
    let mut disable: Vec<String>;

    // If the main crate is an unconditional no_std crate, we can skip solving.
    // TODO: Use MIR to determine if the crate has any std items by default.
    // TODO: Also we can use cfg_attr if the crate is by default std even with 
    // unconditional no_std. If the cfg_attr is also not present, we can
    // use cargo.toml default features list and turn off one by one
    // and solving for each one to see if it is no_std.
    if !main_attributes.unconditional_no_std {
        let (no_std, main_equation, main_parsed_attr) =
            parser::parse_main_attributes(&main_attributes, &ctx);

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
        debug!("Main crate is an unconditional no_std crate, nothing further to do");
        debug!("Exiting");
        return Ok(());
    }

    let finals_args = solver::final_feature_list_main(&crate_info, &enable, &disable);
    debug!("Final arguments for main crate: {:?}", finals_args);

    let deps_attrs = parser::parse_deps_crate();
    // Solve for each dependency
    for dep in deps_attrs {
        if !dep.unconditional_no_std {
            let (no_std, dep_equation, dep_parsed_attr) = parser::parse_main_attributes(&dep, &ctx);
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
            debug!(
                "Dependency {} is an unconditional no_std crate, skipping",
                dep.crate_name
            );
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
    Ok(())
}

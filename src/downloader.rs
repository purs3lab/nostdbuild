use anyhow::Context;
use crates_io_api::SyncClient;
use flate2::read::GzDecoder;
use git2::Repository;
use log::debug;
use reqwest::blocking;
use semver::VersionReq;
use std::{collections::HashSet, fs, path::Path, time::Instant};
use tar::Archive;
use toml::{self, Value, map::Map};
use walkdir::WalkDir;

use crate::{
    CrateInfo, DEPENDENCIES, Dependency, Telemetry,
    consts::{CRATE_IO, DOWNLOAD_PATH},
    parser,
};

/// Clone a git repository to the specified location
///
/// # Arguments
/// * `url` - The URL of the git repository
/// * `name` - The name of the directory to clone into
pub fn clone_repo(url: &str, name: &str) -> Result<(), git2::Error> {
    let dir = Path::new(DOWNLOAD_PATH).join(name);

    Repository::clone(url, &dir).map(|_| {
        debug!("Cloned {} into {}", url, dir.display());
    })
}

/// Download and extract a crate from crates.io
///
/// # Arguments
/// * `name` - The name of the crate to download
/// * `version` - The version of the crate to download in semver format
pub fn clone_from_crates(name: &str, version: Option<&String>) -> Result<String, anyhow::Error> {
    let dir = Path::new(DOWNLOAD_PATH);
    let filename = format!("{}.crate", name);

    let client = create_client()?;
    let (mut download_url, mut ver, mut newname): (String, String, String);
    // Try until successful
    loop {
        (download_url, ver, newname) = match get_download_url(&client, name, &version) {
            Ok((url, ver, newname)) => (url, ver, newname),
            Err(e) => {
                debug!("Failed to get download URL: {}", e);
                if e.to_string().contains("could not be found") {
                    return Err(anyhow::anyhow!("Crate not found"));
                }
                if e.to_string().contains("Known: ") {
                    return Err(anyhow::anyhow!("Version not found"));
                }
                debug!("Retrying download");
                continue;
            }
        };
        debug!("Download URL: {}", download_url);

        let crate_path = dir.join(format!("{}-{}", newname, ver));
        if Path::new(&crate_path).exists() {
            if contains_one_rs_file(crate_path.to_str().unwrap()) {
                debug!("Crate with name {} already downloaded", newname);
                return Ok(format!("{}:{}", newname, ver));
            }
            // Delete the crate if it doesn't contain any .rs files
            fs::remove_dir_all(crate_path)?;
        }

        match download_crate(&download_url, &filename) {
            Ok(_) => break,
            Err(e) => {
                debug!("Failed to download crate: {}", e);
                if e.to_string().contains("Not found") || e.to_string().contains("404") {
                    return Err(anyhow::anyhow!("Crate not found"));
                }
                debug!("Retrying download");
            }
        };
    }
    extract_crate(&filename, dir)?;
    fs::remove_file(&filename).context("Failed to delete crate file")?;

    debug!("Downloaded {} to {}", newname, dir.display());
    debug!("Name with version: {}:{}", newname, ver);
    Ok(format!("{}:{}", newname, ver))
}

/// Download all dependencies for a crate
/// This function will recursively download all dependencies for a crate
/// and add them to the worklist
/// # Arguments
/// * `worklist` - The initial worklist containing the dependencies of the crate
/// # Returns
/// * `Result` - An empty `Result` if successful, an `Error` otherwise
pub fn download_all_dependencies(
    worklist: &mut Vec<(String, String)>,
    crate_info: &mut CrateInfo,
    depth: u32,
    telemetry: &mut Telemetry,
) -> Result<bool, anyhow::Error> {
    debug!("Initial worklist length: {}", worklist.len());
    let mut initlist = Vec::new();
    while !worklist.is_empty() {
        debug!("Worklist length: {}", worklist.len());
        let (name, version) = worklist.pop().unwrap();
        debug!("Downloading {} with version {}", name, version);
        let name_with_version = match clone_from_crates(&name, Some(&version)) {
            Ok(name_with_version) => name_with_version,
            Err(e) => {
                debug!("Failed to download crate: {}", e);
                continue;
            }
        };
        let old_name = name.clone();
        let (name, new_version) = match name_with_version.split_once(':') {
            Some((n, v)) => (n.to_string(), v.to_string()),
            None => (name, "latest".to_string()),
        };

        let mut dep_lock = DEPENDENCIES.lock().unwrap();
        if !dep_lock.contains(&name_with_version) {
            dep_lock.push(name_with_version.clone());
        }
        drop(dep_lock);

        // Some crates have _ in their name when in the dependency list,
        // but the actual crate name has - instead.
        if name != old_name {
            debug!("Updating name from {} to {}", old_name, name);
            update_name(&old_name, &name, crate_info);
        }

        if parser::is_proc_macro(&name_with_version) {
            debug!("{} is a proc-macro, skipping", name_with_version);
            continue;
        }

        let cfg = z3::Config::new();
        let ctx = z3::Context::new(&cfg);
        let found = parser::check_for_no_std(&name_with_version, &ctx);
        if !parser::is_dep_optional(crate_info, &name) {
            if !found {
                debug!(
                    "ERROR: Dependency {} does not support no_std build",
                    name_with_version
                );
                return Ok(false);
            }
            initlist.push((name.clone(), new_version.to_string()));
        }

        // `clone_from_crates` gives a more accurate version.
        // Update the version in the crate_info with this version.
        traverse_and_update(&name, &version, &new_version, crate_info);

        traverse_and_add_local_features(&name, &new_version, crate_info)?;
        let dep_names = read_dep_names_and_versions(&name, &new_version, false)?;
        traverse_and_add_dep_names(&name, &new_version, crate_info, &dep_names)?;
    }
    let mut visited = HashSet::new();
    let cfg = z3::Config::new();
    let ctx = z3::Context::new(&cfg);
    let now = Instant::now();
    let (no_std, depth_traversed) =
        parser::determine_n_depth_dep_no_std(initlist, depth, 0, &mut visited, &ctx);
    telemetry.initial_dep_verification_time_ms = now.elapsed().as_millis();
    telemetry.deps_depth_traversed = depth_traversed;
    Ok(no_std)
}

/// Read the dependencies and their versions from the Cargo.toml file
/// # Arguments
/// * `name` - The name of the crate
/// * `version` - The version of the crate
/// # Returns
/// * `Result` - A vector of tuples containing the name and version of each dependency
/// * `Error` - An error if the file could not be read or parsed
pub fn read_dep_names_and_versions(
    name: &str,
    version: &str,
    skip_optional: bool,
) -> Result<Vec<(String, String)>, anyhow::Error> {
    let manifest = parser::determine_manifest_file(&format!("{}-{}", name, version));
    let mut dep_names = Vec::new();
    let toml = fs::read_to_string(&manifest).context("Failed to read Cargo.toml")?;
    let toml: toml::Value = toml::from_str(&toml).context("Failed to parse Cargo.toml")?;

    let deps = toml
        .get("dependencies")
        .and_then(Value::as_table)
        .cloned()
        .unwrap_or_else(|| {
            debug!("No dependencies found in Cargo.toml");
            Map::new()
        });
    for (name, value) in deps {
        let dep: Dependency = value
            .clone()
            .try_into()
            .context("Failed to parse dependency")?;
        let (version, optional) = match dep {
            Dependency::Simple(version) => (version, false),
            Dependency::Special { optional } => ("latest".to_string(), optional.unwrap_or(false)),
            Dependency::Detailed {
                version, optional, ..
            } => (version, optional.unwrap_or(false)),
        };
        if skip_optional && optional {
            debug!("Skipping optional dependency: {}", name);
            continue;
        }
        dep_names.push((name.to_string(), version));
    }

    Ok(dep_names)
}

/// Initialize the worklist with the dependencies of a crate.
/// This function also collects information about dependencies and
/// features that will be used later.
/// # Arguments
/// * `name` - The name of the crate to get dependencies for
/// * `crate_name_rename` - A mutable vector to store the renamed crate names
/// * `worklist` - The worklist to add the dependencies to
/// * `crate_info` - Detailed information about the crate, including its dependencies and features.
/// # Returns
/// * `Result` - An empty `Result` if successful, an `Error` otherwise
pub fn init_worklist(
    name: &str,
    crate_name_rename: &mut Vec<(String, String)>,
    worklist: &mut Vec<(String, String)>,
    crate_info: &mut CrateInfo,
) -> Result<(), anyhow::Error> {
    let dir = Path::new(DOWNLOAD_PATH).join(name.replace(':', "-"));
    let manifest = parser::determine_manifest_file(name);

    // Since we are making modifications to the Cargo.toml file,
    // we need to back it up first.
    fs::copy(&manifest, dir.join("Cargo.toml.bak")).context("Failed to copy Cargo.toml")?;
    debug!("Reading Cargo.toml from {}", manifest);

    let (name, version) = name.split_once(':').unwrap();
    crate_info.name = name.to_string();
    crate_info.version = version.to_string();
    crate_info.deps_and_features = Vec::new();
    crate_info.default_features = true;
    crate_info.features = Vec::new();

    let toml = fs::read_to_string(&manifest).context("Failed to read Cargo.toml")?;
    let mut toml: toml::Value = toml::from_str(&toml).context("Failed to parse Cargo.toml")?;
    let dependencies = toml
        .get("dependencies")
        .and_then(|val| val.as_table())
        .cloned()
        .unwrap_or_else(|| {
            debug!("No dependencies found in Cargo.toml");
            Map::new()
        });

    crate_info.features = read_local_features(&toml);
    let mut non_dev_deps: Vec<String> = Vec::new();
    for (name, value) in dependencies {
        let mut local_crate_info = CrateInfo::default();
        let mut features_to_use: Vec<String> = Vec::new();
        local_crate_info.name = name.clone();
        let dep: Dependency = value
            .clone()
            .try_into()
            .context("Failed to parse dependency")?;
        match dep {
            Dependency::Simple(version) => {
                local_crate_info.version = version;
            }
            Dependency::Special { optional } => {
                local_crate_info.optional = optional.unwrap_or(false);
                local_crate_info.version = "latest".to_string();
            }
            Dependency::Detailed {
                version,
                package,
                default_features,
                features,
                optional,
                git,
            } => {
                features_to_use = features.unwrap_or_default();
                local_crate_info = CrateInfo {
                    version,
                    default_features: default_features.unwrap_or(true),
                    optional: optional.unwrap_or(false),
                    name: package.unwrap_or(name.clone()),
                    git,
                    ..local_crate_info
                };
            }
        }
        worklist.push((
            local_crate_info.name.clone(),
            local_crate_info.version.clone(),
        ));

        non_dev_deps.push(name.clone());

        crate_name_rename.push((name, local_crate_info.name.clone()));
        crate_info
            .deps_and_features
            .push((local_crate_info, features_to_use));
    }

    parser::remove_table_from_toml("workspace", &mut toml, &manifest)?;
    parser::remove_table_from_toml("lints", &mut toml, &manifest)?;
    parser::remove_features_of_deps("dev-dependencies", &mut toml, &manifest, &non_dev_deps)?;
    parser::remove_table_from_toml("dev-dependencies", &mut toml, &manifest)?;
    parser::remove_features_of_deps("target", &mut toml, &manifest, &non_dev_deps)?;
    parser::remove_table_from_toml("target", &mut toml, &manifest)?;

    Ok(())
}

/// Check if the given path contains at least one .rs file
/// # Arguments
/// * `path` - The path to check
/// # Returns
/// True if the path contains at least one .rs file, false otherwise
pub fn contains_one_rs_file(path: &str) -> bool {
    for entry in WalkDir::new(path) {
        let entry = entry.unwrap();
        if entry.path().extension().unwrap_or_default() == "rs" {
            return true;
        }
    }
    false
}

fn read_local_features(toml: &toml::Value) -> Vec<(String, Vec<(String, String)>)> {
    let features = toml
        .get("features")
        .and_then(Value::as_table)
        .cloned()
        .unwrap_or_else(|| {
            debug!("No features found in Cargo.toml");
            Map::new()
        });

    features
        .iter()
        .map(|(k, v)| {
            (
                k.to_string(),
                v.as_array()
                    .unwrap()
                    .iter()
                    .map(|v| {
                        if v.as_str().unwrap().starts_with("dep:") {
                            return (v.as_str().unwrap()[4..].to_string(), "dep:".to_string());
                        }
                        let v: Vec<_> = v.as_str().unwrap().split("/").collect();
                        let left = v
                            .first()
                            .map(|s| s.strip_suffix("?").unwrap_or(s))
                            .unwrap_or("")
                            .to_string();
                        let right = v.last().unwrap_or(&"").to_string();
                        (left, right)
                    })
                    .collect(),
            )
        })
        .collect()
}

fn create_client() -> Result<SyncClient, anyhow::Error> {
    SyncClient::new(
        "downloader (contact@sourag.com)",
        std::time::Duration::from_secs(1),
    )
    .context("Failed to create client")
}

fn traverse_and_add_dep_names(
    name: &str,
    version: &str,
    crate_info: &mut CrateInfo,
    dep_names: &Vec<(String, String)>,
) -> anyhow::Result<(), anyhow::Error> {
    if crate_info.name == name && crate_info.version == version {
        let deps_and_features = &mut crate_info.deps_and_features;
        for dep_name in dep_names {
            let info = CrateInfo {
                name: dep_name.0.clone(),
                version: dep_name.1.clone(),
                ..CrateInfo::default()
            };
            deps_and_features.push((info, Vec::new()));
        }
        return Ok(());
    }

    for (dep, _) in &mut crate_info.deps_and_features {
        traverse_and_add_dep_names(name, version, dep, dep_names)?;
    }
    Ok(())
}

fn traverse_and_add_local_features(
    name: &str,
    version: &str,
    crate_info: &mut CrateInfo,
) -> anyhow::Result<(), anyhow::Error> {
    if crate_info.name == name && crate_info.version == version {
        let manifest = parser::determine_manifest_file(&format!("{}-{}", name, version));
        debug!("Reading Cargo.toml from {}", manifest);
        let toml = fs::read_to_string(&manifest).context("Failed to read Cargo.toml")?;
        let toml: toml::Value = toml::from_str(&toml).context("Failed to parse Cargo.toml")?;
        crate_info.features = read_local_features(&toml);
        // Once we find the crate, we don't need to traverse further.
        return Ok(());
    }
    for (dep, _) in &mut crate_info.deps_and_features {
        traverse_and_add_local_features(name, version, dep)?;
    }
    Ok(())
}

fn traverse_and_update(name: &str, version: &str, new_version: &str, crate_info: &mut CrateInfo) {
    if crate_info.name == name && crate_info.version == version {
        crate_info.version = new_version.to_string();
        // Once we find the crate, we don't need to traverse further.
        return;
    }
    for (dep, _) in &mut crate_info.deps_and_features {
        traverse_and_update(name, version, new_version, dep);
    }
}

fn update_name(old_name: &str, new_name: &str, crate_info: &mut CrateInfo) {
    if crate_info.name == old_name {
        crate_info.name = new_name.to_string();
        return;
    }
    for (dep, _) in &mut crate_info.deps_and_features {
        update_name(old_name, new_name, dep);
    }
}

fn get_download_url(
    client: &SyncClient,
    name: &str,
    version: &Option<&String>,
) -> Result<(String, String, String), anyhow::Error> {
    let crate_data = client.get_crate(name)?;

    let resolved_version = match version {
        None => crate_data.versions[0].num.clone(),
        Some(version) => {
            if *version == "latest" {
                crate_data.versions[0].num.clone()
            } else {
                let ver = VersionReq::parse(version).context("Known: Failed to parse version")?;
                let resolved_versions = crate_data
                    .versions
                    .iter()
                    .filter(|v| ver.matches(&semver::Version::parse(&v.num).unwrap()))
                    .collect::<Vec<_>>();
                if resolved_versions.is_empty() {
                    return Err(anyhow::anyhow!("Known: No matching version found"));
                }
                resolved_versions.first().unwrap().num.clone()
            }
        }
    };

    let dl_path = crate_data
        .versions
        .iter()
        .find(|v| v.num == resolved_version)
        .unwrap()
        .dl_path
        .clone();
    let download_url: String = format!("{}{}", CRATE_IO, dl_path);

    Ok((
        download_url,
        resolved_version,
        crate_data.crate_data.name.clone(),
    ))
}

fn download_crate(url: &str, filename: &str) -> Result<(), anyhow::Error> {
    debug!("Downloading crate from {}", url);
    let response = blocking::get(url)?;
    let bytes = response.bytes().context("Failed to read response")?;

    fs::write(filename, bytes).context("Failed to write crate file")
}

fn extract_crate(filename: &str, dir: &Path) -> Result<(), anyhow::Error> {
    let file = fs::File::open(filename).context("Failed to open crate file")?;
    let tar = GzDecoder::new(file);
    let mut archive = Archive::new(tar);

    archive.unpack(dir).context("Failed to unpack crate")
}

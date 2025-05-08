use anyhow::Context;
use crates_io_api::SyncClient;
use flate2::read::GzDecoder;
use git2::Repository;
use log::debug;
use reqwest::blocking;
use semver::VersionReq;
use std::{fs, path::Path};
use tar::Archive;
use toml::{self, map::Map, Value};
use walkdir::WalkDir;

use crate::{
    consts::{CRATE_IO, DOWNLOAD_PATH},
    CrateInfo, Dependency, DEPENDENCIES,
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
    let (mut download_url, mut ver): (String, String);
    // Try until successful
    loop {
        (download_url, ver) = match get_download_url(&client, name, &version) {
            Ok((url, ver)) => (url, ver),
            Err(e) => {
                debug!("Failed to get download URL: {}", e);
                if e.to_string().contains("could not be found") {
                    return Err(anyhow::anyhow!("Crate not found"));
                }
                debug!("Retrying download");
                continue;
            }
        };

        let crate_path = dir.join(format!("{}-{}", name, ver));
        if Path::new(&crate_path).exists() {
            if contains_one_rs_file(&crate_path.to_str().unwrap()) {
                debug!("Crate already downloaded");
                return Ok(format!("{}:{}", name, ver));
            }
            // Delete the crate if it doesn't contain any .rs files
            fs::remove_file(&crate_path.to_str().unwrap())?;
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

    debug!("Downloaded {} to {}", name, dir.display());
    debug!("Name with version: {}", format!("{}:{}", name, ver));
    Ok(format!("{}:{}", name, ver))
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
) -> Result<(), anyhow::Error> {
    debug!("Initial worklist length: {}", worklist.len());
    while !worklist.is_empty() {
        debug!("Worklist length: {}", worklist.len());
        let (name, version) = worklist.pop().unwrap();
        debug!("Downloading {} with version {}", name, version);
        let name_with_version = clone_from_crates(&name, Some(&version))
            .map_err(|e| anyhow::anyhow!("Failed to download crate: {}", e))?;

        let mut dep_lock = DEPENDENCIES.lock().unwrap();
        if !dep_lock.contains(&name_with_version) {
            dep_lock.push(name_with_version.clone());
        }
        drop(dep_lock);

        let new_version = name_with_version.split(':').last().unwrap_or("latest");

        // `clone_from_crates` gives a more accurate version.
        // Update the version in the crate_info with this version.
        traverse_and_update(&name, &version, new_version, crate_info);

        traverse_and_add_local_features(&name, new_version, crate_info)?;
        let dep_names = read_dep_names_and_versions(&name, new_version)?;
        traverse_and_add_dep_names(&name, &new_version, crate_info, &dep_names)?;
    }
    Ok(())
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
) -> Result<Vec<(String, String)>, anyhow::Error> {
    let dir = Path::new(DOWNLOAD_PATH).join(format!("{}-{}", name, version));
    let filename = format!("{}/Cargo.toml", dir.display());
    let mut dep_names = Vec::new();
    if !Path::new(&filename).exists() {
        debug!("Cargo.toml not found for {}", name);
        return Err(anyhow::anyhow!("Cargo.toml not found"));
    }
    let toml = fs::read_to_string(&filename).context("Failed to read Cargo.toml")?;
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
        let version = match dep {
            Dependency::Simple(version) => version,
            Dependency::Detailed { version, .. } => version,
        };
        dep_names.push((name.to_string(), version));
    }

    return Ok(dep_names);
}

/// Initialize the worklist with the dependencies of a crate.
/// This function also collects information about dependencies and
/// features that will be used later.
/// # Arguments
/// * `name` - The name of the crate to get dependencies for
/// * `worklist` - The worklist to add the dependencies to
/// # Returns
/// * `Result` - An empty `Result` if successful, an `Error` otherwise
pub fn init_worklist(
    name: &str,
    worklist: &mut Vec<(String, String)>,
    crate_info: &mut CrateInfo,
) -> Result<(), anyhow::Error> {
    let dir = Path::new(DOWNLOAD_PATH).join(name.replace(':', "-"));
    let filename = format!("{}/Cargo.toml", dir.display());
    debug!("Reading Cargo.toml from {}", filename);

    let (name, version) = name.split_once(':').unwrap();
    crate_info.name = name.to_string();
    crate_info.version = version.to_string();
    crate_info.deps_and_features = Vec::new();
    crate_info.default_features = true;
    crate_info.features = Vec::new();

    if !Path::new(&filename).exists() {
        return Err(anyhow::anyhow!("Cargo.toml not found"));
    }

    let toml = fs::read_to_string(&filename).context("Failed to read Cargo.toml")?;
    let toml: toml::Value = toml::from_str(&toml).context("Failed to parse Cargo.toml")?;
    let mut dependencies = Map::new();
    if toml
        .as_table()
        .map_or(false, |table| table.contains_key("dependencies"))
    {
        dependencies = toml["dependencies"].as_table().cloned().unwrap_or_else(|| {
            debug!("No dependencies found in Cargo.toml");
            Map::new()
        });
    }
    crate_info.features = read_local_features(toml);
    for (name, value) in dependencies {
        let mut local_crate_info = CrateInfo::default();
        let mut features_to_use: Vec<String> = Vec::new();
        let dep: Dependency = value
            .clone()
            .try_into()
            .context("Failed to parse dependency")?;
        match dep {
            Dependency::Simple(version) => {
                local_crate_info.version = version;
                local_crate_info.name = name.clone();
            }
            Dependency::Detailed {
                version,
                package,
                default_features,
                features,
                optional,
                ..
            } => {
                features_to_use = features.unwrap_or_default();
                local_crate_info = CrateInfo {
                    version,
                    default_features: default_features.unwrap_or(true),
                    optional: optional.unwrap_or(false),
                    name: package.unwrap_or(name),
                    ..local_crate_info
                };
            }
        }
        worklist.push((
            local_crate_info.name.clone(),
            local_crate_info.version.clone(),
        ));
        crate_info
            .deps_and_features
            .push((local_crate_info, features_to_use));
    }

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

fn read_local_features(toml: toml::Value) -> Vec<(String, Vec<(String, String)>)> {
    if !toml
        .as_table()
        .map_or(false, |table| table.contains_key("features"))
    {
        debug!("No features found in Cargo.toml");
        return Vec::new();
    }
    let features = toml["features"].as_table().cloned().unwrap_or_else(|| {
        debug!("No features found in Cargo.toml");
        Map::new()
    });
    let features = features
        .iter()
        .map(|(k, v)| {
            (
                k.to_string(),
                v.as_array()
                    .unwrap()
                    .iter()
                    .map(|v| {
                        // TODO: Handle optional dependencies properly
                        if v.as_str().unwrap().starts_with("dep:") {
                            return (v.as_str().unwrap()[4..].to_string(), "".to_string());
                        }
                        let v = v.as_str().unwrap().split("/");
                        let left = v.clone().next().unwrap_or("").to_string();
                        let right = v.clone().last().unwrap_or("").to_string();
                        (left, right)
                    })
                    .collect(),
            )
        })
        .collect();
    features
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
        let dir = Path::new(DOWNLOAD_PATH).join(format!("{}-{}", name, version));
        let filename = format!("{}/Cargo.toml", dir.display());
        debug!("Reading Cargo.toml from {}", filename);
        let toml = fs::read_to_string(&filename).context("Failed to read Cargo.toml")?;
        let toml: toml::Value = toml::from_str(&toml).context("Failed to parse Cargo.toml")?;
        crate_info.features = read_local_features(toml);
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

fn get_download_url(
    client: &SyncClient,
    name: &str,
    version: &Option<&String>,
) -> Result<(String, String), anyhow::Error> {
    let crate_data = client.get_crate(name)?;

    let resolved_version = match version {
        None => crate_data.versions[0].num.clone(),
        Some(version) => {
            if *version == "latest" {
                crate_data.versions[0].num.clone()
            } else {
                let ver = VersionReq::parse(&version).context("Failed to parse version")?;
                let resolved_versions = crate_data
                    .versions
                    .iter()
                    .filter(|v| ver.matches(&semver::Version::parse(&v.num).unwrap()))
                    .collect::<Vec<_>>();
                if resolved_versions.is_empty() {
                    return Err(anyhow::anyhow!("No matching version found"));
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

    Ok((download_url, resolved_version))
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

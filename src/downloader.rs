use anyhow::Context;
use crates_io_api::SyncClient;
use delete::delete_file;
use flate2::read::GzDecoder;
use git2::Repository;
use log::debug;
use reqwest::blocking;
use semver::VersionReq;
use std::fs;
use std::path::Path;
use tar::Archive;
use toml;
use walkdir::WalkDir;

use crate::consts::{CRATE_IO, DOWNLOAD_PATH};
use crate::CrateInfo;
use crate::Dependency;
use crate::DEPENDENCIES;

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

    // Download crate
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
            delete_file(&crate_path.to_str().unwrap())?;
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
    // Extract and cleanup
    extract_crate(&filename, dir)?;
    delete_file(&filename).context("Failed to delete crate file")?;

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

        // `clone_from_crates` gives a more accurate version.
        // Update the version in the crate_info with this version.
        traverse_and_update(
            &name,
            &version,
            &name_with_version.split(':').last().unwrap_or("latest"),
            crate_info,
        );
    }
    Ok(())
}

/// Initialize the worklist with the dependencies of a crate
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

    // Check if Cargo.toml exists
    if !Path::new(&filename).exists() {
        return Err(anyhow::anyhow!("Cargo.toml not found"));
    }

    let toml = fs::read_to_string(&filename).context("Failed to read Cargo.toml")?;
    let toml: toml::Value = toml::from_str(&toml).context("Failed to parse Cargo.toml")?;

    let dependencies = toml["dependencies"].as_table().unwrap();
    for (name, value) in dependencies {
        let mut name_to_use = name.to_string();
        let mut default_features_to_use: bool = true;
        let mut features_to_use: Vec<String> = Vec::new();
        let version_to_use: String;
        let dep: Dependency = value
            .clone()
            .try_into()
            .context("Failed to parse dependency")?;
        match dep {
            Dependency::Simple(version) => {
                version_to_use = version;
            }
            Dependency::Complex {
                version,
                package,
                default_features,
                features,
            } => {
                default_features_to_use = default_features.unwrap_or(true);
                name_to_use = package.unwrap_or(name_to_use);
                version_to_use = version;
                features_to_use = features.unwrap_or_default();
            }
        }
        worklist.push((name_to_use.clone(), version_to_use.clone()));
        crate_info.deps_and_features.push((
            CrateInfo {
                name: name_to_use,
                version: version_to_use,
                deps_and_features: Vec::new(),
                features: Vec::new(),
                default_features: default_features_to_use,
            },
            features_to_use,
        ));
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

// All private functions below this line

fn create_client() -> Result<SyncClient, anyhow::Error> {
    SyncClient::new(
        "downloader (contact@sourag.com)",
        std::time::Duration::from_secs(1),
    )
    .context("Failed to create client")
}

fn traverse_and_update(name: &str, version: &str, new_version: &str, crate_info: &mut CrateInfo) {
    if crate_info.name == name && crate_info.version == version {
        crate_info.version = new_version.to_string();
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
    // Fetch crate data from crates.io
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
    // let response = blocking::get(url).context("Failed to download crate")?;
    let bytes = response.bytes().context("Failed to read response")?;

    fs::write(filename, bytes).context("Failed to write crate file")
}

fn extract_crate(filename: &str, dir: &Path) -> Result<(), anyhow::Error> {
    let file = fs::File::open(filename).context("Failed to open crate file")?;
    let tar = GzDecoder::new(file);
    let mut archive = Archive::new(tar);

    archive.unpack(dir).context("Failed to unpack crate")
}

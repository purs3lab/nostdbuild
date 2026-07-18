use anyhow::Context;
use flate2::read::GzDecoder;
use git2::Repository;
use log::debug;
use reqwest::blocking;
use semver::VersionReq;
use std::{
    collections::HashSet,
    fs,
    path::{Path, PathBuf},
    sync::LazyLock,
    thread,
    time::{Duration, Instant},
};
use tar::Archive;
use toml::{self, Value, map::Map};
use walkdir::WalkDir;

use crate::types::*;
use crate::{
    CrateInfo, DEPENDENCIES, Dependency, Telemetry,
    consts::{INDEX_CRATES_IO, STATIC_CRATES_IO, DOWNLOAD_PATH},
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

const MAX_RETRIES: u32 = 3;

/// Shared HTTP client with explicit timeouts. Using a single client (instead of
/// the bare `blocking::get` convenience fn, which builds a fresh client per call)
/// gives us connection pooling and, crucially, connect/request timeouts so a
/// stalled request fails fast enough to be retried instead of hanging forever.
static HTTP_CLIENT: LazyLock<blocking::Client> = LazyLock::new(|| {
    blocking::Client::builder()
        .connect_timeout(Duration::from_secs(10))
        .timeout(Duration::from_secs(30))
        .build()
        .expect("failed to build HTTP client")
});

fn is_permanent_error(e: &anyhow::Error) -> bool {
    let s = e.to_string();
    s.contains("could not be found") || s.contains("Known:") || s.contains("404")
}

fn with_retries<T>(
    max: u32,
    mut f: impl FnMut() -> Result<T, anyhow::Error>,
) -> Result<T, anyhow::Error> {
    let mut last = anyhow::anyhow!("no attempts");
    for attempt in 0..max {
        match f() {
            Ok(v) => return Ok(v),
            Err(e) if is_permanent_error(&e) => return Err(e),
            Err(e) => {
                debug!("Attempt {}/{} failed: {}", attempt + 1, max, e);
                last = e;
                // Exponential backoff before the next attempt (skip after the last one).
                if attempt + 1 < max {
                    thread::sleep(Duration::from_millis(500 * (1 << attempt)));
                }
            }
        }
    }
    Err(last)
}

/// Download and extract a crate from crates.io
///
/// # Arguments
/// * `name` - The name of the crate to download
/// * `version` - The version of the crate to download in semver format
/// * `main_name` - The optional name of the main crate being analyzed. Used to put
///   dependencies in a crate specific directory
pub fn clone_from_crates(
    name: &str,
    version: Option<&String>,
    main_name: Option<&str>,
    parent_name: Option<&str>,
) -> Result<String, anyhow::Error> {
    let mut dir = PathBuf::from(DOWNLOAD_PATH);

    if let Some(name) = main_name {
        dir = dir.join(format!("{}_deps", name.replace(':', "-")));
    }

    let filename = format!("{}.crate", name);

    let (download_url, ver, newname) = with_retries(MAX_RETRIES, || {
        get_download_url(name, &version, main_name, parent_name)
    })?;
    debug!("Download URL: {}", download_url);

    let crate_path = dir.join(format!("{}-{}", newname, ver));
    if crate_path.exists() {
        if contains_one_rs_file(crate_path.to_str().unwrap()) {
            debug!("Crate with name {} already downloaded", newname);
            return Ok(format!("{}:{}", newname, ver));
        }
        fs::remove_dir_all(&crate_path)?;
    }

    with_retries(MAX_RETRIES, || download_crate(&download_url, &filename))?;

    extract_crate(&filename, &dir)?;
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
    main_name: &str,
    worklist: &mut TupleVec,
    crate_info: &mut CrateInfo,
    depth: u32,
    telemetry: &mut Telemetry,
    top_level_deps: &mut Vec<(String, String)>,
) -> Result<bool, anyhow::Error> {
    debug!("Initial worklist length: {}", worklist.len());
    let mut initlist = Vec::new();
    let mut opt_initlist = Vec::new();
    while !worklist.is_empty() {
        debug!("Worklist length: {}", worklist.len());
        let (name, version) = worklist.pop().unwrap();
        debug!("Downloading {} with version {}", name, version);
        let name_with_version =
            match clone_from_crates(&name, Some(&version), Some(main_name), None) {
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
        top_level_deps.push((name.clone(), new_version.clone()));

        // Some crates have _ in their name when in the dependency list,
        // but the actual crate name has - instead.
        if name != old_name {
            debug!("Updating name from {} to {}", old_name, name);
            update_name(&old_name, &name, crate_info);
        }

        if parser::is_proc_macro(&name_with_version, Some(main_name)) {
            debug!("{} is a proc-macro, skipping", name_with_version);
            continue;
        }

        let cfg = z3::Config::new();
        let ctx = z3::Context::new(&cfg);
        let found = parser::check_for_no_std(&name_with_version, &ctx, None, Some(main_name));
        if !parser::is_dep_optional(crate_info, &name) {
            if !found {
                debug!(
                    "ERROR: Dependency {} does not support no_std build",
                    name_with_version
                );
                return Ok(false);
            }
            initlist.push((name.clone(), new_version.to_string()));
        } else {
            // Optional dep: download its transitive sub-deps so recursive_dep_requirement_check
            // can inspect them, but don't fail if they lack no_std support.
            opt_initlist.push((name.clone(), new_version.to_string()));
        }

        debug!("Successfully downloaded {}", name_with_version);

        // `clone_from_crates` gives a more accurate version.
        // Update the version in the crate_info with this version.
        traverse_and_update(&name, &version, &new_version, crate_info);

        traverse_and_add_local_features(&name, &new_version, crate_info, main_name)?;
        let dep_names = read_dep_names_and_versions(&name, &new_version, false, main_name)?;
        traverse_and_add_dep_names(&name, &new_version, crate_info, &dep_names)?;
    }
    let mut visited = HashSet::new();
    let cfg = z3::Config::new();
    let ctx = z3::Context::new(&cfg);
    let now = Instant::now();
    debug!("Finished downloading dependencies. Now verifying if they support no_std build");
    let (no_std, depth_traversed) = parser::determine_n_depth_dep_no_std(
        initlist,
        depth,
        0,
        &mut visited,
        &ctx,
        main_name,
        true,
    );
    // Download transitive sub-deps of optional top-level deps so recursive_dep_requirement_check
    // can inspect them. Re-use `visited` to avoid re-downloading crates already fetched above.
    parser::determine_n_depth_dep_no_std(
        opt_initlist,
        depth,
        0,
        &mut visited,
        &ctx,
        main_name,
        false,
    );
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
    main_name: &str,
) -> Result<TupleVec, anyhow::Error> {
    let manifest =
        parser::determine_manifest_file(&format!("{}-{}", name, version), Some(main_name));
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
    for (mut name, value) in deps {
        let dep: Dependency = value
            .clone()
            .try_into()
            .context("Failed to parse dependency")?;
        let (version, optional) = match dep {
            Dependency::Simple(version) => (version, false),
            Dependency::Special { optional } => ("latest".to_string(), optional.unwrap_or(false)),
            Dependency::Detailed {
                version,
                optional,
                package,
                ..
            } => {
                if let Some(pkg) = package {
                    name = pkg;
                }
                (version, optional.unwrap_or(false))
            }
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
/// * `only_gather` - If true, only gather dependencies without modifying Cargo.toml
/// # Returns
/// * `Result` - A tuple containing the worklist, crate name renames, and crate info
pub fn gather_crate_info(
    name: &str,
    only_gather: bool,
    main_name: Option<&str>,
) -> Result<(TupleVec, TupleVec, CrateInfo), anyhow::Error> {
    let dir = Path::new(DOWNLOAD_PATH).join(name.replace(':', "-"));
    let manifest = parser::determine_manifest_file(name, main_name);
    let mut worklist: TupleVec = Vec::new();
    let mut crate_name_rename: TupleVec = Vec::new();
    let mut crate_info: CrateInfo = CrateInfo::default();

    if !only_gather {
        // Since we are making modifications to the Cargo.toml file,
        // we need to back it up first.
        fs::copy(&manifest, dir.join("Cargo.toml.bak")).context("Failed to copy Cargo.toml")?;
        debug!("Reading Cargo.toml from {}", manifest);
    }

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

    if !only_gather {
        parser::remove_table_from_toml("workspace", &mut toml, &manifest)?;
        parser::remove_table_from_toml("lints", &mut toml, &manifest)?;
        parser::remove_features_of_deps("dev-dependencies", &mut toml, &manifest, &non_dev_deps)?;
        parser::remove_table_from_toml("dev-dependencies", &mut toml, &manifest)?;
        parser::remove_features_of_deps("target", &mut toml, &manifest, &non_dev_deps)?;
        parser::remove_table_from_toml("target", &mut toml, &manifest)?;
    }

    Ok((worklist, crate_name_rename, crate_info))
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

pub(crate) fn read_local_features(toml: &toml::Value) -> Vec<(String, TupleVec)> {
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

fn index_path(name: &str) -> String {
    let lower = name.to_lowercase();
    match lower.len() {
        1 => format!("1/{}", lower),
        2 => format!("2/{}", lower),
        3 => format!("3/{}/{}", &lower[..1], lower),
        _ => format!("{}/{}/{}", &lower[..2], &lower[2..4], lower),
    }
}

pub fn fetch_index(name: &str) -> Result<Vec<serde_json::Value>, anyhow::Error> {
    let url = format!("{}/{}", INDEX_CRATES_IO, index_path(name));
    debug!("Fetching index from {}", url);
    with_retries(MAX_RETRIES, || {
        let response = HTTP_CLIENT.get(&url).send().context("Failed to fetch index")?;
        if !response.status().is_success() {
            return Err(anyhow::anyhow!("{} could not be found", name));
        }
        response
            .text()
            .context("Failed to read index")?
            .lines()
            .filter(|l| !l.is_empty())
            .map(|l| serde_json::from_str(l).context("Failed to parse index entry"))
            .collect()
    })
}

fn traverse_and_add_dep_names(
    name: &str,
    version: &str,
    crate_info: &mut CrateInfo,
    dep_names: &TupleVec,
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
    main_name: &str,
) -> anyhow::Result<(), anyhow::Error> {
    if crate_info.name == name && crate_info.version == version {
        let manifest =
            parser::determine_manifest_file(&format!("{}-{}", name, version), Some(main_name));
        debug!("Reading Cargo.toml from {}", manifest);
        let toml = fs::read_to_string(&manifest).context("Failed to read Cargo.toml")?;
        let toml: toml::Value = toml::from_str(&toml).context("Failed to parse Cargo.toml")?;
        crate_info.features = read_local_features(&toml);
        // Once we find the crate, we don't need to traverse further.
        return Ok(());
    }
    for (dep, _) in &mut crate_info.deps_and_features {
        traverse_and_add_local_features(name, version, dep, main_name)?;
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
    name: &str,
    version: &Option<&String>,
    main_name: Option<&str>,
    // No longer consulted — the main crate's Cargo.lock (see resolve_from_lock) covers
    // the whole resolved graph. Kept in the signature so clone_from_crates callers that
    // pass a parent for context don't need to change.
    _parent_name: Option<&str>,
) -> Result<(String, String, String), anyhow::Error> {
    let entries = fetch_index(name)?;
    let canonical_name = entries
        .first()
        .and_then(|e| e.get("name").and_then(|v| v.as_str()))
        .unwrap_or(name)
        .to_string();

    let mut resolved_version = String::new();

    // main_name None means this the download of the main crate itself. So there is no
    // parent or version to look for in the lock file.
    // Resolve the version from the main crate's Cargo.lock (cargo's fully-resolved
    // graph), the source of truth for both direct and transitive deps. This keeps the
    // download and parse phases in agreement. Falls back to the index below only when
    // the crate is absent from the lock. `parent_name` is no longer consulted: the
    // per-parent lock path was malformed (it never resolved) and the main lock already
    // covers the whole graph.
    if let Some(main_name) = main_name
        && let Some(ver) = resolve_from_lock(name, &version, main_name)
    {
        resolved_version = ver;
    }

    if resolved_version.is_empty() {
        debug!("Resolving version using index");
        resolved_version = resolve_version(version, &entries)?;
    } else {
        debug!(
            "Resolved version for {} using Cargo.lock: {}",
            name, resolved_version
        );
    }

    let download_url = format!(
        "{}/crates/{}/{}-{}.crate",
        STATIC_CRATES_IO, canonical_name, canonical_name, resolved_version
    );

    Ok((download_url, resolved_version, canonical_name))
}

/// Resolve the version of a crate given a version requirement
/// # Arguments
/// * `version` - The version requirement as a string
/// * `entries` - Index entries for the crate from index.crates.io
/// # Returns
/// * `Result` - The resolved version as a string if successful, an `Error` otherwise
pub fn resolve_version(
    version: &Option<&String>,
    entries: &[serde_json::Value],
) -> Result<String, anyhow::Error> {
    // Build the candidate set, optionally including yanked releases. Non-yanked is
    // always preferred, but yanked versions are kept as a fallback so an exact/lock
    // pin to a since-yanked release still resolves (cargo permits yanked versions
    // that are explicitly pinned or already locked). Filtering them out
    // unconditionally turned such pins into a fatal "No matching version found".
    let candidates = |include_yanked: bool| -> Vec<(&str, semver::Version)> {
        entries
            .iter()
            .filter(|e| {
                include_yanked || !e.get("yanked").and_then(|v| v.as_bool()).unwrap_or(false)
            })
            .filter_map(|e| {
                let vers = e.get("vers")?.as_str()?;
                let sv = semver::Version::parse(vers).ok()?;
                Some((vers, sv))
            })
            .collect()
    };

    let pick = |available: &[(&str, semver::Version)]| -> Option<String> {
        match version {
            None => available
                .iter()
                .max_by(|(_, a), (_, b)| a.cmp(b))
                .map(|(s, _)| s.to_string()),
            Some(v) if v.as_str() == "latest" => available
                .iter()
                .max_by(|(_, a), (_, b)| a.cmp(b))
                .map(|(s, _)| s.to_string()),
            Some(req_str) => {
                let req = VersionReq::parse(req_str).ok()?;
                available
                    .iter()
                    .filter(|(_, sv)| req.matches(sv))
                    .max_by(|(_, a), (_, b)| a.cmp(b))
                    .map(|(s, _)| s.to_string())
            }
        }
    };

    // Validate the requirement up front so a genuinely malformed req still errors.
    if let Some(req_str) = version
        && req_str.as_str() != "latest"
    {
        VersionReq::parse(req_str).context("Known: Failed to parse version")?;
    }

    // Prefer non-yanked; fall back to including yanked so exact/lock pins resolve.
    if let Some(v) = pick(&candidates(false)) {
        return Ok(v);
    }
    pick(&candidates(true))
        .ok_or_else(|| anyhow::anyhow!("Known: No matching version found"))
}

/// Resolve a dependency's concrete version from the main crate's Cargo.lock.
/// The main lock is cargo's fully-resolved graph, so it is the source of truth for
/// every direct and transitive dependency and keeps the download and parse phases in
/// agreement. Returns `None` when the crate is absent from the lock (callers fall back
/// to the index) or when no locked version satisfies `req`.
/// # Arguments
/// * `name` - The dependency crate name
/// * `req` - The version requirement string from the parent's manifest, if any
/// * `main_name` - The main crate `name:version`, whose Cargo.lock is consulted
pub fn resolve_from_lock(name: &str, req: &Option<&String>, main_name: &str) -> Option<String> {
    let lock_file = PathBuf::from(DOWNLOAD_PATH)
        .join(main_name.replace(':', "-"))
        .join("Cargo.lock");
    let lock_content = fs::read_to_string(&lock_file).ok()?;
    let lock_toml: Value = toml::from_str(&lock_content).ok()?;
    let packages = lock_toml.get("package").and_then(Value::as_array)?;

    // A crate can appear at several (semver-incompatible) versions in the lock;
    // collect them all and pick the newest that satisfies the requirement.
    let versions: Vec<semver::Version> = packages
        .iter()
        .filter(|p| p.get("name").and_then(Value::as_str) == Some(name))
        .filter_map(|p| p.get("version").and_then(Value::as_str))
        .filter_map(|v| semver::Version::parse(v).ok())
        .collect();

    let req_parsed = match req {
        Some(r) if r.as_str() != "latest" => VersionReq::parse(r.as_str()).ok(),
        _ => None,
    };

    let chosen = match req_parsed {
        Some(vr) => versions.iter().filter(|v| vr.matches(v)).max(),
        None => versions.iter().max(),
    };
    chosen.map(|v| v.to_string())
}

/// Resolve a dependency's version the same way the download phase does: prefer the
/// main crate's Cargo.lock, falling back to the index only when the crate is not
/// locked. Used by the parser so it references the exact versions that are on disk,
/// rather than re-deriving them from a live index (which can drift from the download).
/// # Arguments
/// * `name` - The dependency crate name
/// * `req` - The version requirement string from the parent's manifest, if any
/// * `main_name` - The main crate `name:version`, whose Cargo.lock is consulted
pub fn resolve_dep_version(
    name: &str,
    req: &Option<&String>,
    main_name: &str,
) -> Result<String, anyhow::Error> {
    if let Some(v) = resolve_from_lock(name, req, main_name) {
        return Ok(v);
    }
    let entries = fetch_index(name)?;
    resolve_version(req, &entries)
}

fn download_crate(url: &str, filename: &str) -> Result<(), anyhow::Error> {
    debug!("Downloading crate from {}", url);
    let response = HTTP_CLIENT.get(url).send().context("Failed to fetch crate")?;
    if response.status().as_u16() == 404 {
        return Err(anyhow::anyhow!("404: crate not found at {}", url));
    }
    if !response.status().is_success() {
        return Err(anyhow::anyhow!("Download failed: HTTP {}", response.status()));
    }
    let bytes = response.bytes().context("Failed to read response")?;
    fs::write(filename, bytes).context("Failed to write crate file")
}

fn extract_crate(filename: &str, dir: &Path) -> Result<(), anyhow::Error> {
    let file = fs::File::open(filename).context("Failed to open crate file")?;
    let tar = GzDecoder::new(file);
    let mut archive = Archive::new(tar);

    archive.unpack(dir).context("Failed to unpack crate")
}

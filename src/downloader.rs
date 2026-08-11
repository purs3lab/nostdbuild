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
    time::Duration,
};
use tar::Archive;
use toml::{self, Value, map::Map};
use walkdir::WalkDir;

use crate::types::*;
use crate::{
    CrateInfo, DEPENDENCIES, Dependency, ProcMacroDep, Telemetry,
    consts::{DOWNLOAD_PATH, INDEX_CRATES_IO, STATIC_CRATES_IO},
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

    let (download_url, ver, newname) = with_retries(MAX_RETRIES, || {
        get_download_url(name, &version, main_name, parent_name)
    })?;
    debug!("Download URL: {}", download_url);

    let crate_path = dir.join(format!("{}-{}", newname, ver));
    if crate_path.exists() {
        if extraction_is_complete(&crate_path) {
            debug!("Crate with name {} already downloaded", newname);
            return Ok(format!("{}:{}", newname, ver));
        }
        fs::remove_dir_all(&crate_path)?;
    }

    // Staged under a name unique to this crate version AND this process. It used
    // to be a bare `format!("{}.crate", name)` — a *relative* path, so every
    // worker wrote `./<name>.crate` into the one CWD they share. Two workers
    // wanting the same crate then raced: one read the tarball while the other was
    // still writing it, hit EOF at the writer's current offset, and `unpack`
    // stopped part-way through — leaving a directory with a garbled file and the
    // rest of the crate missing, which the old `contains_one_rs_file` check then
    // accepted forever. 13 such directories were still on disk (bp-polkadot's
    // frame-support, bdk_core's serde, …) and each one silently shrinks the
    // analysis to whatever survived. The old code also `remove_file`d that shared
    // path, deleting a tarball another worker was about to extract.
    let staging = staging_path(&newname, &ver)?;
    with_retries(MAX_RETRIES, || download_crate(&download_url, &staging))?;

    let extracted = extract_crate_checked(&staging, &dir, &crate_path);
    let _ = fs::remove_file(&staging);
    extracted?;

    debug!("Downloaded {} to {}", newname, dir.display());
    debug!("Name with version: {}:{}", newname, ver);
    Ok(format!("{}:{}", newname, ver))
}

/// Download all dependencies for a crate
/// This function will recursively download all dependencies for a crate
/// and add them to the worklist
/// # Arguments
/// * `worklist` - The initial worklist containing the dependencies of the crate
/// * `proc_macro_deps` - Out: the main crate's own proc-macro dependencies, which
///   the walk skips. `driver::park_injecting_proc_macros` probes these once the
///   crate can be compiled — a proc macro's features are the *consumer's*, and
///   whether one of them put std here is a question only a build answers.
/// # Returns
/// * `Result` - Whether every dependency reached supports no_std, an `Error`
///   otherwise
///
/// `false` ends the run in `bin/main.rs`: a non-optional dependency with no
/// no_std support cannot be fixed from the root manifest. Every offender is
/// recorded in `telemetry.dep_not_no_std_deps` first, so the verdict names the
/// dependency, its parent and its depth.
///
/// The scan of the crate's *own* dependency list runs to the end even once the
/// verdict is decided — that is one manifest's worth of crates, and the full
/// list of std-only direct deps is what makes the result readable. The
/// transitive walk below is skipped instead: it is unbounded, and nothing it
/// could find changes an answer already reached.
pub fn download_all_dependencies(
    main_name: &str,
    worklist: &mut TupleVec,
    crate_info: &mut CrateInfo,
    depth: u32,
    telemetry: &mut Telemetry,
    top_level_deps: &mut Vec<(String, String)>,
    proc_macro_deps: &mut Vec<ProcMacroDep>,
) -> Result<bool, anyhow::Error> {
    debug!("Initial worklist length: {}", worklist.len());
    let mut initlist = Vec::new();
    let mut opt_initlist = Vec::new();
    let mut all_deps_no_std = true;
    while !worklist.is_empty() {
        debug!("Worklist length: {}", worklist.len());
        let (name, version) = worklist.pop().unwrap();
        debug!("Downloading {} with version {}", name, version);
        let name_with_version =
            match clone_from_crates(&name, Some(&version), Some(main_name), None) {
                Ok(name_with_version) => name_with_version,
                Err(e) => {
                    debug!("Failed to download crate: {}", e);
                    telemetry
                        .deps_download_failed
                        .push(format!("{}:{}", name, version));
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
            // Skipping is right for the no_std *evidence*: this crate is compiled for
            // the host and run there. It is wrong for the crate's features, which
            // select the tokens it injects here — see
            // `driver::park_injecting_proc_macros`, which decides from a compile of
            // this crate whether any of them did. This loop is the main crate's own
            // dependency list (the transitive walk runs after it), so the edges it
            // names are ones the main manifest actually owns.
            proc_macro_deps.push(ProcMacroDep {
                package: name.clone(),
                manifest: parser::determine_manifest_file(&name_with_version, Some(main_name)),
            });
            continue;
        }

        let cfg = z3::Config::new();
        let ctx = z3::Context::new(&cfg);
        let evidence = parser::no_std_evidence(&name_with_version, &ctx, None, Some(main_name));
        if !parser::is_dep_optional(crate_info, &name) {
            match evidence {
                parser::NoStdEvidence::Absent => {
                    debug!(
                        "ERROR: Dependency {} does not support no_std build",
                        name_with_version
                    );
                    telemetry.dep_not_no_std_deps.push(crate::DepNoStdFailure {
                        dep: name_with_version.clone(),
                        parent: main_name.to_string(),
                        depth: 0,
                    });
                    all_deps_no_std = false;
                }
                parser::NoStdEvidence::NoSources => {
                    // Nothing was parsed, so nothing is known. Treated as
                    // unknown rather than std: its own dependencies are still
                    // worth verifying, and the crate still needs downloading
                    // and analysing like any other.
                    debug!(
                        "Dependency {} could not be parsed — no_std support unknown",
                        name_with_version
                    );
                    telemetry
                        .deps_no_sources_parsed
                        .push(name_with_version.clone());
                }
                parser::NoStdEvidence::Supported => {}
            }
            // A dep already known to be std-only contributes no sub-tree: the
            // walk below exists to find the first offender, and this one is it.
            if evidence != parser::NoStdEvidence::Absent {
                initlist.push((name.clone(), new_version.to_string()));
            }
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
    // Decided by the crate's own dependency list — the transitive walk cannot
    // change it, and it is the expensive half.
    if !all_deps_no_std {
        debug!(
            "Skipping the transitive no_std walk: {} direct dependencies already fail it",
            telemetry.dep_not_no_std_deps.len()
        );
        return Ok(false);
    }

    let mut visited = HashSet::new();
    let cfg = z3::Config::new();
    let ctx = z3::Context::new(&cfg);
    let _t = crate::timing::scope("dep_verify", main_name);
    debug!("Finished downloading dependencies. Now verifying if they support no_std build");
    let (deep_no_std, depth_traversed) = parser::determine_n_depth_dep_no_std(
        initlist,
        depth,
        0,
        &mut visited,
        &ctx,
        main_name,
        true,
        telemetry,
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
        telemetry,
    );
    telemetry.deps_depth_traversed = depth_traversed;
    Ok(deep_no_std)
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

pub fn read_local_features(toml: &toml::Value) -> Vec<(String, TupleVec)> {
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

/// Collect the names of all optional dependencies declared in a manifest.
///
/// An optional dependency creates an implicit feature of the same name, so
/// `feat = ["<optdep>/<subfeat>"]` transitively enables that implicit feature.
/// Covers both `[dependencies]` and target-specific `[target.*.dependencies]`;
/// `[build-dependencies]` are excluded because optional build deps do not create
/// features, and dev-dependencies cannot be optional.
pub fn optional_deps_in_manifest(toml: &toml::Value) -> HashSet<String> {
    let mut out = HashSet::new();

    let collect = |table: Option<&Value>, out: &mut HashSet<String>| {
        if let Some(deps) = table.and_then(Value::as_table) {
            for (name, spec) in deps {
                let optional = spec
                    .as_table()
                    .and_then(|t| t.get("optional"))
                    .and_then(Value::as_bool)
                    .unwrap_or(false);
                if optional {
                    out.insert(name.clone());
                }
            }
        }
    };

    collect(toml.get("dependencies"), &mut out);

    if let Some(targets) = toml.get("target").and_then(Value::as_table) {
        for (_, cfg) in targets {
            collect(cfg.get("dependencies"), &mut out);
        }
    }

    out
}

/// For each `feat = ["<dep>/<subfeat>"]` reference where `<dep>` is an optional
/// dependency and the reference is **not** weak (`<dep>?/...`), yield the edge
/// `(feat, dep)`.
///
/// Enabling `feat` turns on the implicit `<dep>` feature, so the solver must
/// learn `feat => dep`; otherwise it can pick a covering set with `feat` on and
/// `dep` off that Cargo will silently re-unify (bucket 3c). This parses the
/// `[features]` table directly rather than via `read_local_features` so the weak
/// `?` marker — which `read_local_features` strips — is still visible: a weak
/// `dep?/feat` enables the sub-feature only if the dep is already on, so it
/// implies nothing and must be excluded.
pub fn optional_dep_feature_edges(toml: &toml::Value) -> Vec<(String, String)> {
    let optional_deps = optional_deps_in_manifest(toml);
    let mut edges = Vec::new();

    let features = match toml.get("features").and_then(Value::as_table) {
        Some(f) => f,
        None => return edges,
    };

    for (feat_name, values) in features {
        let Some(arr) = values.as_array() else {
            continue;
        };
        for v in arr {
            let Some(entry) = v.as_str() else { continue };
            // `dep:foo` names a dependency, not a `dep/feat` reference.
            if entry.starts_with("dep:") {
                continue;
            }
            let Some((dep_part, _subfeat)) = entry.split_once('/') else {
                // No slash: a plain feature link, handled by
                // `feature_implication_constraints`.
                continue;
            };
            // Weak reference: enables the sub-feature only if the dep is already
            // on, so it does not imply the dep. Exclude.
            if dep_part.ends_with('?') {
                continue;
            }
            if optional_deps.contains(dep_part) {
                edges.push((feat_name.clone(), dep_part.to_string()));
            }
        }
    }

    edges
}

/// For every optional dependency, the feature names Cargo accepts on the command
/// line to link it: `("hashbrown", ["hashbrown"])`.
///
/// Two ways a feature links an optional dep directly:
/// * the **implicit** feature Cargo synthesises for a dep never referenced as
///   `dep:foo` — it appears in `cargo metadata` but nowhere in the `[features]`
///   table, which is why `declared` (built from metadata by
///   `visitor::declared_features`) is the authority here rather than the table;
/// * an explicit `feat = ["dep:foo"]`, which *suppresses* the implicit feature —
///   handled for free, since the suppressed name is then absent from `declared`.
///
/// Plain `feat = ["foo"]` links are deliberately not collected: they are already
/// modelled by `feature_implication_constraints`, and the implicit feature is the
/// minimal way to ask for the dependency.
///
/// Deps with no enabler are omitted — nothing can be asserted about them.
pub fn optional_dep_enablers(
    toml: &toml::Value,
    declared: &HashSet<String>,
) -> Vec<(String, Vec<String>)> {
    let optional_deps = optional_deps_in_manifest(toml);
    let features = toml.get("features").and_then(Value::as_table);

    let mut out: Vec<(String, Vec<String>)> = Vec::new();
    for dep in optional_deps {
        let mut enablers: Vec<String> = Vec::new();
        if declared.contains(&dep) {
            enablers.push(dep.clone());
        }
        if let Some(features) = features {
            let marker = format!("dep:{dep}");
            for (feat_name, values) in features {
                if !declared.contains(feat_name) {
                    continue;
                }
                let names_dep = values
                    .as_array()
                    .into_iter()
                    .flatten()
                    .filter_map(Value::as_str)
                    .any(|v| v == marker);
                if names_dep && !enablers.contains(feat_name) {
                    enablers.push(feat_name.clone());
                }
            }
        }
        if !enablers.is_empty() {
            enablers.sort();
            out.push((dep, enablers));
        }
    }
    out.sort();
    out
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
        let response = HTTP_CLIENT
            .get(&url)
            .send()
            .context("Failed to fetch index")?;
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
    pick(&candidates(true)).ok_or_else(|| anyhow::anyhow!("Known: No matching version found"))
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

/// Name of the marker `clone_from_crates` drops in a crate directory once
/// `unpack` has returned success for it.
///
/// A directory without one is not condemned — every directory downloaded before
/// this existed lacks it — it just falls back to the weaker `contains_one_rs_file`
/// test, which is what the tool did for all of them anyway. A directory *with*
/// one is known-complete, which is what lets the repair pass state which of the
/// two it is looking at.
pub const EXTRACT_MARKER: &str = ".nostd-extract-ok";

/// Where to stage a downloaded `.crate` before unpacking it.
///
/// Unique per crate version and per process, so no two workers can ever be
/// reading and writing the same tarball (see `clone_from_crates`). Lives under
/// the download root rather than the CWD both to keep the tool's own directory
/// clean and to stay on the same filesystem as the extraction.
fn staging_path(name: &str, version: &str) -> Result<String, anyhow::Error> {
    static SEQ: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
    static SWEEP: std::sync::Once = std::sync::Once::new();
    let dir = PathBuf::from(DOWNLOAD_PATH).join(".staging");
    fs::create_dir_all(&dir).context("Failed to create staging directory")?;
    // Both exits from `clone_from_crates` delete the staged tarball, but a run
    // killed outright (the eval's timeout) leaves its file behind. Sweep once
    // per process; six hours is far longer than any single download, so nothing
    // in flight is at risk.
    SWEEP.call_once(|| sweep_stale_staging(&dir));
    let unique = format!(
        "{}-{}-{}-{}.crate",
        name,
        version,
        std::process::id(),
        SEQ.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
    );
    Ok(dir.join(unique).to_string_lossy().into_owned())
}

/// Delete staged tarballs left by runs that were killed before they could clean
/// up. Best-effort throughout: anything unreadable is simply left alone.
fn sweep_stale_staging(dir: &Path) {
    const STALE_AFTER: Duration = Duration::from_secs(6 * 60 * 60);
    let Ok(entries) = fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let stale = entry
            .metadata()
            .and_then(|m| m.modified())
            .map(|t| t.elapsed().unwrap_or_default() > STALE_AFTER)
            .unwrap_or(false);
        if stale && fs::remove_file(entry.path()).is_ok() {
            debug!("Removed stale staged tarball {}", entry.path().display());
        }
    }
}

/// Unpack `staging` into `dir`, leaving behind either a complete crate
/// directory (marked as such) or no directory at all.
///
/// The middle state is what caused the damage: a torn tarball makes `unpack`
/// stop part-way, and the truncated directory it leaves behind looks exactly
/// like a good one to the "already downloaded?" check. `crate_path` is where
/// this crate's files land — `dir` is its parent, since the tarball carries the
/// `<name>-<version>/` prefix itself.
pub fn extract_crate_checked(
    staging: &str,
    dir: &Path,
    crate_path: &Path,
) -> Result<(), anyhow::Error> {
    if let Err(e) = extract_crate(staging, dir) {
        // A partial extraction must not outlive the failure that made it: the
        // next run would find a directory full of plausible `.rs` files and
        // never look again.
        debug!(
            "Discarding partial extraction at {}: {}",
            crate_path.display(),
            e
        );
        let _ = fs::remove_dir_all(crate_path);
        return Err(e);
    }
    // Best-effort: the marker only strengthens the *next* run's check, and a
    // crate we just unpacked is usable whether or not it lands.
    if let Err(e) = fs::write(crate_path.join(EXTRACT_MARKER), b"") {
        debug!(
            "Could not mark {} as completely extracted: {}",
            crate_path.display(),
            e
        );
    }
    Ok(())
}

/// Is this already-present directory usable as-is?
///
/// Marker present ⇒ a previous run unpacked it in full. Otherwise fall back to
/// the historical test (does it contain any `.rs` file at all), which keeps the
/// ~12k directories predating the marker in play; a truncated one among them is
/// caught by the repair pass (`repair_downloads.py`), not here.
pub fn extraction_is_complete(crate_path: &Path) -> bool {
    if crate_path.join(EXTRACT_MARKER).exists() {
        return true;
    }
    contains_one_rs_file(crate_path.to_str().unwrap_or_default())
}

fn download_crate(url: &str, filename: &str) -> Result<(), anyhow::Error> {
    debug!("Downloading crate from {}", url);
    let response = HTTP_CLIENT
        .get(url)
        .send()
        .context("Failed to fetch crate")?;
    if response.status().as_u16() == 404 {
        return Err(anyhow::anyhow!("404: crate not found at {}", url));
    }
    if !response.status().is_success() {
        return Err(anyhow::anyhow!(
            "Download failed: HTTP {}",
            response.status()
        ));
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

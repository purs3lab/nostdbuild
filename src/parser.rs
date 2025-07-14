use anyhow::Context;
use log::debug;
use proc_macro2::TokenStream;
// use quote::ToTokens;
use std::{
    collections::HashSet,
    fs,
    path::{Path, PathBuf},
};
use syn::{visit::Visit, Attribute, ItemExternCrate, Meta};
use walkdir::WalkDir;
use z3::{self, ast::Bool};

use crate::{consts::DOWNLOAD_PATH, db, downloader, solver, CrateInfo, DBData, DEPENDENCIES};

#[derive(Debug, Clone, PartialEq)]
enum Logic {
    And,
    Or,
    Not,
    Any,
}

#[derive(Default, Clone, Debug)]
pub struct ParsedAttr {
    constants: Vec<String>,
    pub features: Vec<String>,
    pub possible_target_archs: Vec<String>,
    pub filepath: Option<String>,
    logic: Vec<Logic>,
}

#[derive(Default, Clone, Debug)]
pub struct Attributes {
    attributes: Vec<Attribute>,
    pub crate_name: String,
    pub unconditional_no_std: bool,
}

#[derive(Default, Clone, Debug)]
pub struct ItemExternCrates {
    itemexterncrates: Vec<ItemExternCrate>,
}

#[derive(Default, Clone, Debug)]
pub struct ItemExternCratesAll {
    itemexterncrates: Vec<ItemExternCrate>,
}

impl<'a> Visit<'a> for ItemExternCrates {
    fn visit_item_extern_crate(&mut self, i: &ItemExternCrate) {
        // We will save all the extern crates that have an
        // attribute associated with them.
        if i.attrs.len() != 0 {
            self.itemexterncrates.push(i.clone());
        }
    }
}

impl<'a> Visit<'a> for ItemExternCratesAll {
    fn visit_item_extern_crate(&mut self, i: &ItemExternCrate) {
        self.itemexterncrates.push(i.clone());
    }
}

impl<'a> Visit<'a> for Attributes {
    fn visit_attribute(&mut self, i: &Attribute) {
        if let Some(ident) = i.path().get_ident() {
            if ident == "cfg" || ident == "cfg_attr" {
                self.attributes.push(i.clone());
            }
            if ident == "no_std" {
                self.unconditional_no_std = true;
            }
        }
    }

    fn visit_item_macro(&mut self, i: &syn::ItemMacro) {
        if i.mac.path.is_ident("compiler_error") {
            let attrs = i.attrs.clone();
            // Remove the attribute from the list which are
            // compiler_error attributes.
            // This is currently assuming there will be only
            // one attribute associated with compiler_error
            // and also that the attribute to remove
            // has already been visited.
            self.attributes.retain(|a| a != &attrs[0]);
        }
    }
}

/// Parse the extern crates of the main crate
/// # Arguments
/// * `crate_name` - The name of the main crate
/// # Returns
/// The extern crates of the main crate
/// that have attributes associated with them.
pub fn parse_item_extern_crates(crate_name: &str) -> ItemExternCrates {
    let mut itemexterncrates = ItemExternCrates {
        itemexterncrates: Vec::new(),
    };

    if let Err(err) = visit(&mut itemexterncrates, crate_name, true) {
        debug!(
            "Failed to parse crate {} with error:{}. Will continue...",
            crate_name, err
        );
    }
    itemexterncrates
}

/// Parse the extern crates of a crate
/// This will parse all the files separately
/// and return the extern crates that does not have
/// any attributes associated with them.
/// # Arguments
/// * `crate_name` - The name of the crate
/// # Returns
/// A vector containing the names of the files
/// that have unguarded `extern crate std`.
pub fn parse_item_extern_crates_for_files(crate_name: &str) -> Vec<String> {
    let path = format!("{}/{}", DOWNLOAD_PATH, crate_name.replace(':', "-"));
    let files = get_all_rs_files(&path, true);
    let mut files_ungaurded = Vec::new();
    for file in files {
        let mut itemexterncrates = ItemExternCratesAll {
            itemexterncrates: Vec::new(),
        };

        let filename = file.replace(DOWNLOAD_PATH, "");
        if let Err(err) = visit(&mut itemexterncrates, &filename, true) {
            debug!(
                "Failed to parse file {} with error:{}. Will continue...",
                file, err
            );
        }

        let extern_std_without_cfg = itemexterncrates
            .itemexterncrates
            .iter()
            .filter(|i| i.ident == "std")
            .any(|i| {
                !i.attrs
                    .iter()
                    .any(|a| a.path().get_ident().map_or(false, |ident| ident == "cfg"))
            });
        if extern_std_without_cfg {
            debug!("Found unguarded extern crate std in file: {}", file);
            let basename = Path::new(&filename)
                .file_name()
                .and_then(|s| s.to_str())
                .unwrap_or("");
            files_ungaurded.push(basename.to_string());
        }
    }
    files_ungaurded
}

/// Get the attribute of the extern crate std
/// We only return the cfg attribute associated with the first
/// extern crate std. This is because we assume that even if there
/// are multiple extern crate stds, they will have the same
/// cfg attribute.
/// # Arguments
/// * `itemexterncrates` - The extern crates of the main crate
/// # Returns
/// The attribute of the extern crate std
/// if it exists, otherwise None.
pub fn get_item_extern_std(itemexterncrates: &ItemExternCrates) -> Option<Attribute> {
    itemexterncrates
        .itemexterncrates
        .iter()
        .filter(|i| i.ident == "std")
        .flat_map(|i| i.attrs.iter())
        .find(|a| a.path().get_ident().map_or(false, |ident| ident == "cfg"))
        .cloned()
}

/// Parse the main crate and return the attributes
/// # Arguments
/// * `path` - The path to the main crate
/// * `crate_name` - The name of the main crate
/// # Returns
/// The attributes of the main crate
pub fn parse_crate(crate_name: &str, recurse: bool) -> Attributes {
    let mut attributes = Attributes::default();

    if let Err(err) = visit(&mut attributes, crate_name, recurse) {
        debug!(
            "Failed to parse crate {} with error:{}. Will continue...",
            crate_name, err
        );
    }
    attributes.crate_name = crate_name.to_string();
    attributes
}

/// Check if the crate has a no_std attribute.
/// # Arguments
/// * `name` - The name of the crate
/// * `ctx` - The Z3 context
/// # Returns
/// A boolean indicating whether the crate has a no_std attribute.
pub fn check_for_no_std(name: &str, ctx: &z3::Context) -> bool {
    let base_attrs = parse_crate(name, false);

    if !parse_main_attributes(&base_attrs, ctx).0 && !base_attrs.unconditional_no_std {
        debug!("No no_std found for the crate {}", name);
        return false;
    }
    true
}

/// Parse the dependencies of the main crate
/// This function will parse the dependencies of the main crate
/// and return the attributes of each dependency as a vector.
/// # Returns
/// A vector containing the attributes of each dependency
pub fn parse_deps_crate() -> Vec<Attributes> {
    let mut attributes = Vec::new();
    let deps_lock = DEPENDENCIES.lock().unwrap();
    for dep in deps_lock.iter() {
        attributes.push(parse_crate(&dep.clone(), true));
    }
    drop(deps_lock);
    attributes
}

/// Main function that does the actual processing of the crate.
/// It first starts from a `cfg_attr` is found and solves other
/// `cfg` attributes based on this.
/// If `cfg_attr` is not found, it will check for an unconditional
/// `no_std` attribute. If found, it will use the `cfg` attribute
/// guaring the `no_std` attribute to solve the other `cfg` attributes.
/// If neither is found, it will return not found.
/// # Arguments
/// * `ctx` - The Z3 context
/// * `attrs` - The attributes of the crate
/// * `name` - The name of the crate
/// * `db_data` - The database data
/// * `is_main` - A boolean indicating whether the crate is the main crate
/// # Returns
/// A tuple containing the features to enable, the features to disable,
/// a boolean indicating whether `no_std` was found, and a boolean indicating
/// whether to recurse further if it was the main crate.
pub fn process_crate(
    ctx: &z3::Context,
    attrs: &Attributes,
    name_with_version: &str,
    db_data: &mut Vec<DBData>,
    crate_info: &CrateInfo,
    is_main: bool,
) -> anyhow::Result<(Vec<String>, Vec<String>, bool, Vec<String>)> {
    let mut recurse = false;
    let mut is_leaf = false;
    let (mut enable, mut disable): (Vec<String>, Vec<String>) = (Vec::new(), Vec::new());

    let (no_std, mut equation, mut parsed_attr) = parse_main_attributes(&attrs, &ctx);
    if !attrs.unconditional_no_std {
        if !no_std {
            debug!("No no_std found for the crate");
            return Ok((Vec::new(), Vec::new(), recurse, Vec::new()));
        }
    } else {
        debug!(
            "crate {} is an unconditional no_std crate",
            name_with_version
        );
        // If the crate is both conditional and unconditional no_std,
        // we will treat it as unconditional.
        if !no_std {
            debug!("WARNING: Crate {} is both unconditional and conditional no_std, will consider on unconditional.", name_with_version);
        }

        let items = parse_item_extern_crates(name_with_version);

        // This case implies that the crate is no_std without any feature requirements.
        if items.itemexterncrates.len() == 0 {
            debug!("No extern crates found for the crate");
            return Ok((Vec::new(), Vec::new(), recurse, Vec::new()));
        }
        let std_attrs = get_item_extern_std(&items);
        if std_attrs.is_some() {
            debug!("Leaf level crate reached {}", name_with_version);
            let features = db::get_from_db_data(&db_data, name_with_version);
            if features.is_some() {
                debug!(
                    "Features to enable and disable for crate {}: {:?}",
                    name_with_version, features
                );
                (enable, disable) = features.unwrap().features.clone();
            } else {
                debug!("No features to enable for crate {}", name_with_version);
                (equation, parsed_attr) = parse_main_attributes_direct(&std_attrs.unwrap(), &ctx);
                // We need to negate the equation since we are
                // trying to remove std features.
                equation = match equation {
                    Some(eq) => Some(eq.not()),
                    None => None,
                };
                debug!("Main equation: {:?}", equation);
                is_leaf = true;
            }
        } else {
            if is_main {
                // Main crate is special since we are anyway processing the direct
                // dependencies.
                recurse = true;
            } else {
                debug!("Leaf level crate reached {}", name_with_version);
                let (name, version) = name_with_version.split_once(':').unwrap();
                if let Some(dep_and_features) = get_deps_and_features(name, version, crate_info) {
                    let names_and_versions: Vec<(String, String)> = dep_and_features
                        .iter()
                        .map(|(dep, _)| (dep.name.clone(), dep.version.clone()))
                        .collect();
                    let externs = get_item_extern_dep(&items, &names_and_versions);
                    match parse_top_level_externs(&ctx, &names_and_versions, &externs) {
                        Ok((eq, attr)) => {
                            if eq.is_some() {
                                equation = Some(eq.unwrap().not());
                                parsed_attr = attr;
                            }
                        }
                        Err(e) => {
                            debug!("Failed to parse extern crates: {}", e);
                            return Ok((Vec::new(), Vec::new(), recurse, Vec::new()));
                        }
                    }
                }
                debug!("main equation: {:?}", equation);
            }
        }
    }
    let (equations, possible_archs) = parse_attributes(&attrs, &ctx);
    let filtered = filter_equations(&equations, &parsed_attr.features);

    // This part adds equations if there are attributes that conditionally include
    // files which might contain unguarded `extern crate std`.
    let files_and_equations = get_files_in_attributes(&attrs, &ctx);
    debug!("Files in attributes: {:?}", files_and_equations);
    let files_unguared = parse_item_extern_crates_for_files(name_with_version);
    debug!(
        "Files with unguarded extern crate std: {:?}",
        files_unguared
    );

    for (file, eq) in files_and_equations {
        if files_unguared.contains(&file) {
            debug!("File {} contains unguarded extern crate std", file);
            if let Some(e) = eq {
                let neg = e.not();
                if let Some(existing_eq) = &mut equation {
                    *existing_eq = Bool::and(ctx, &[existing_eq, &neg]);
                } else {
                    equation = Some(neg);
                }
            }
        }
    }

    // Finally, we solve the equations
    let model = solver::solve(&ctx, &equation, &filtered);
    if enable.is_empty() && disable.is_empty() {
        (enable, disable) = solver::model_to_features(&model);
    }

    if is_leaf {
        db::add_to_db_data(db_data, name_with_version, (&enable, &disable));
    }

    Ok((enable, disable, recurse, possible_archs))
}

/// Process the dependency crate.
/// This function will call `process_crate` for the dependency crate.
/// But this does some additional checks and updates the main crate's
/// toml file if required.
/// # Arguments
/// * `ctx` - The Z3 context
/// * `dep` - The attributes of the dependency crate
/// * `main_name` - The name of the main crate
/// * `db_data` - The database data
/// * `crate_info` - The crate info
/// * `deps_args` - Output: The featurs for this dependency
/// * `crate_name_rename` - A list of names and their renames of crate names
/// # Returns
/// A Result indicating success or failure.
pub fn process_dep_crate(
    ctx: &z3::Context,
    dep: &Attributes,
    main_name: &str,
    db_data: &mut Vec<DBData>,
    crate_info: &CrateInfo,
    deps_args: &mut Vec<String>,
    crate_name_rename: &[(String, String)],
) -> Result<(), anyhow::Error> {
    let found = check_for_no_std(&dep.crate_name, ctx);
    assert!(
        found,
        "Dependency {} does not support no_std build",
        dep.crate_name
    );

    let (enable, disable, _, _) =
        process_crate(ctx, dep, &dep.crate_name, db_data, crate_info, false)?;

    let (args, update_default_config) = solver::final_feature_list_dep(
        crate_info,
        &dep.crate_name.split(":").next().unwrap_or(""),
        &enable,
        &disable,
    );

    debug!(
        "Dependency requires default config update: {}",
        update_default_config
    );

    if update_default_config {
        update_main_crate_default_list(&main_name, &dep.crate_name, &crate_name_rename);
    }

    debug!(
        "Final arguments for dependency {}: {:?}",
        dep.crate_name, args
    );

    if !args.is_empty() {
        deps_args.extend(args);
    }
    Ok(())
}

/// Parse the attributes of a the main crate.
/// This will verify if the crate is no_std or not and
/// whether to continue parsing the dependencies or not.
/// # Arguments
/// * `attrs` - The attributes of the main crate
/// * `ctx` - The Z3 context
/// # Returns
/// A tuple containing a boolean indicating whether the crate is no_std,
/// an optional equation for the main crate and the parsed attributes.
pub fn parse_main_attributes<'a>(
    attrs: &Attributes,
    ctx: &'a z3::Context,
) -> (bool, Option<Bool<'a>>, ParsedAttr) {
    let mut atleast_one_no_std = false;
    let mut parsed: ParsedAttr = ParsedAttr::default();
    let mut equation: Option<Bool> = None;
    for attr in &attrs.attributes {
        if attr.path().get_ident().unwrap() == "cfg_attr" {
            // println!("{}", attr.to_token_stream());
            (equation, parsed) = parse_meta_for_cfg_attr(&attr.meta, &ctx);
            if is_no_std(&parsed) {
                atleast_one_no_std = true;
                debug!("Found no_std");
                break;
            }
        }
    }
    if !atleast_one_no_std {
        equation = None;
    }
    (atleast_one_no_std, equation, parsed)
}

/// Just a wrapper around parse_meta_for_cfg_attr.
/// This is to make the interface consistent with the other functions.
/// # Arguments
/// * `attr` - The attribute to parse
/// * `ctx` - The Z3 context
/// # Returns
/// A tuple containing an optional equation and the parsed attributes.
pub fn parse_main_attributes_direct<'a>(
    attr: &Attribute,
    ctx: &'a z3::Context,
) -> (Option<Bool<'a>>, ParsedAttr) {
    parse_meta_for_cfg_attr(&attr.meta, &ctx)
}

/// Parse the attributes of a dependency crate.
/// This does not need to verify if the crate is no_std or not.
/// # Arguments
/// * `attrs` - The attributes of the dependency crate
/// * `ctx` - The Z3 context
/// # Returns
/// A tuple containing the equations for the dependency
/// crate and the parsed attributes.
pub fn parse_attributes<'a>(
    attrs: &Attributes,
    ctx: &'a z3::Context,
) -> (Vec<Option<Bool<'a>>>, Vec<String>) {
    let mut equation: Vec<Option<Bool>> = Vec::new();
    let mut possible_target_archs: Vec<String> = Vec::new();
    let mut temp_eq: Option<Bool>;
    let mut parsed: ParsedAttr;
    for attr in &attrs.attributes {
        let ident = attr.path().get_ident().unwrap();
        if ident == "cfg" {
            (temp_eq, parsed) = parse_meta_for_cfg_attr(&attr.meta, &ctx);
            possible_target_archs.extend(parsed.possible_target_archs.clone());
            // TODO: Should this check be removed?
            if parsed.features.len() == 1 || parsed.logic.is_empty() {
                // Attributes like `#[cfg (feature = "serde")]` are not interesting.
                continue;
            }
            equation.push(temp_eq);
        }
    }

    (equation, possible_target_archs)
}

/// Filter the equations based on the main features.
/// Only the equations that contain the main features will be kept.
/// # Arguments
/// * `equations` - The equations to filter
/// * `main_features` - The features of the main crate
/// # Returns
/// The filtered equations
pub fn filter_equations<'a>(
    equations: &Vec<Option<Bool<'a>>>,
    main_features: &[String],
) -> Vec<Bool<'a>> {
    let mut filtered: Vec<Bool<'_>> = Vec::new();
    let mut seen: HashSet<String> = HashSet::new();

    for eq in equations {
        if let Some(e) = eq {
            let mut found = false;
            for feature in main_features {
                if e.to_string().contains(feature) {
                    found = true;
                    break;
                }
            }
            if found {
                filtered.push(e.clone());
            }
        }
    }

    // Remove duplicates
    filtered.retain(|e: &Bool<'_>| seen.insert(e.to_string()));
    filtered
}

/// Determine the path to the Cargo.toml file in the given directory.
/// It checks for both `Cargo.toml` and `cargo.toml` (lowercase).
/// # Arguments
/// * `dir` - The directory to check for the Cargo.toml file
/// # Returns
/// The path to the Cargo.toml file if it exists, otherwise panics.
pub fn determine_cargo_toml(dir: &PathBuf) -> String {
    let path = format!("{}/Cargo.toml", dir.display());
    if Path::new(&path).exists() {
        return path;
    }
    let path = format!("{}/cargo.toml", dir.display());
    if Path::new(&path).exists() {
        return path;
    }
    unreachable!(
        "No Cargo.toml found in the directory: {}. This should not happen.",
        dir.display()
    )
}

/// Remove a table from the Cargo.toml file.
/// # Arguments
/// * `key` - The key of the table to remove
/// * `toml` - The TOML value to modify
/// * `filename` - The path to the Cargo.toml file
/// # Returns
/// A Result indicating success or failure.
pub fn remove_table_from_toml(
    key: &str,
    toml: &mut toml::Value,
    filename: &str,
) -> Result<(), anyhow::Error> {
    if let Some(table) = toml.as_table_mut() {
        if table.contains_key(key) {
            debug!("{} found in Cargo.toml, removing it", key);
            table.remove(key);
            fs::write(
                &filename,
                toml::to_string(&toml).context("Failed to write Cargo.toml")?,
            )
            .context("Failed to write Cargo.toml")?;
            debug!("Removed {} from Cargo.toml", key);
        }
    }
    Ok(())
}

/// Update the main crate's default features list
/// by adding the default features of the given dependency.
/// This function will also set the dependency to not have
/// default features set in the main crate's Cargo.toml.
/// # Arguments
/// * `main` - The name of the main crate
/// * `dep` - The name of the dependency to add to the main crate's default features
/// # Returns
/// None
fn update_main_crate_default_list(main: &str, dep: &str, crate_name_rename: &[(String, String)]) {
    let main_dir = PathBuf::from(DOWNLOAD_PATH).join(main.replace(':', "-"));
    let main_cargo_toml = determine_cargo_toml(&main_dir);
    let dep_dir = PathBuf::from(DOWNLOAD_PATH).join(dep.replace(':', "-"));
    let dep_cargo_toml = determine_cargo_toml(&dep_dir);
    let dep_name_original = dep.split(':').next().unwrap().to_string();
    let dep_name = crate_name_rename
        .iter()
        .find(|(name, _)| name == &dep_name_original)
        .map(|(_, renamed)| renamed)
        .unwrap_or_else(|| &dep_name_original);

    debug!(
        "Updating main crate default features list: {} with dependency: {}",
        main_cargo_toml, dep_cargo_toml
    );

    let mut main_toml: toml::Value =
        toml::from_str(&fs::read_to_string(&main_cargo_toml).unwrap()).unwrap();
    let mut dep_toml: toml::Value =
        toml::from_str(&fs::read_to_string(&dep_cargo_toml).unwrap()).unwrap();

    let main_dependencies = main_toml
        .get_mut("dependencies")
        .and_then(|v| v.as_table_mut())
        .expect("Failed to get dependencies table from main Cargo.toml");

    if let Some(value) = main_dependencies.get_mut(dep_name) {
        match value {
            toml::Value::Table(table) => {
                table.insert("default-features".to_string(), toml::Value::Boolean(false));
            }
            _ => {
                debug!(
                    "Dependency {} in main Cargo.toml is not a table, skipping default-features update",
                    dep_name
                );
            }
        }
    }

    let dep_features = dep_toml
        .get_mut("features")
        .and_then(|v| v.as_table_mut())
        .expect("Failed to get features table from dependency Cargo.toml");

    let dep_default_features = dep_features
        .get("default")
        .and_then(|v| v.as_array())
        .map(|v| {
            v.iter()
                .filter_map(|f| f.as_str().map(|s| format!("{}/{}", dep_name, s)))
                .collect()
        })
        .unwrap_or_else(|| Vec::new());

    let main_features = main_toml
        .get_mut("features")
        .and_then(|v| v.as_table_mut())
        .expect("Failed to get features table from main Cargo.toml");

    if let Some(default_features) = main_features.get_mut("default") {
        match default_features {
            toml::Value::Array(arr) => {
                for feature in dep_default_features {
                    if !arr.contains(&toml::Value::String(feature.clone())) {
                        debug!("Adding {} to main crate default features", feature);
                        arr.push(toml::Value::String(feature));
                    }
                }
            }
            _ => {
                debug!("Default features in main Cargo.toml is not an array, skipping update");
            }
        }
    } else {
        main_features.insert(
            "default".to_string(),
            toml::Value::Array(
                dep_default_features
                    .into_iter()
                    .map(toml::Value::String)
                    .collect(),
            ),
        );
        debug!("Added default features to main crate features");
    }

    fs::write(
        &main_cargo_toml,
        toml::to_string(&main_toml)
            .context("Failed convert Value to string")
            .unwrap(),
    )
    .unwrap();
}

/// Given a `CrateInfo`, this function finds all optional dependencies
/// and their features that are required to enable them.
/// It returns a vector of tuples where each tuple contains the dependency name
/// and the feature name.
/// # Arguments
/// * `crate_info` - The `CrateInfo` containing dependencies and features.
/// # Returns
/// A vector of tuples, each containing the dependency name and the feature name.
pub fn features_for_optional_deps(crate_info: &CrateInfo) -> Vec<(String, String)> {
    let deps_and_feats = &crate_info.deps_and_features;
    let main_feats = &crate_info.features;

    let optional_deps: Vec<String> = deps_and_feats
        .iter()
        .filter(|(dep, _)| dep.optional)
        .map(|(dep, _)| dep.name.clone())
        .collect();

    let mut direct_feat_match: Vec<(String, String)> = optional_deps
        .iter()
        .filter(|dep| main_feats.iter().any(|(feat, _)| feat == *dep))
        .map(|dep| (dep.clone(), dep.clone()))
        .collect();

    let indirect_feat_match: Vec<(String, String)> = optional_deps
        .iter()
        .filter_map(|dep| {
            main_feats.iter().find_map(|(feat_name, dep_feats)| {
                dep_feats
                    .iter()
                    .find(|f| f.0 == dep.as_str() && f.1 == "dep:")
                    .map(|_| (dep.clone(), feat_name.clone()))
            })
        })
        .collect();

    direct_feat_match.extend(indirect_feat_match);
    direct_feat_match
}

/// Determine if a dependency should be skipped.
/// This function checks if a dependency is enabled by a feature
/// of the main crate even if it is optional. If yes, it returns false,
/// indicating that the dependency should not be skipped.
/// If the dependency is optional and not enabled by any feature,
/// it returns true, indicating that the dependency should be skipped.
/// # Arguments
/// * `name` - The name of the dependency.
/// * `crate_info` - The `CrateInfo` containing the crate's dependencies and features.
/// * `deps_and_features` - A slice of tuples containing dependency names and the
/// features that enable them.
/// * `enable_features` - A slice of features that are enabled in the main crate.
/// # Returns
/// A boolean indicating whether the dependency should be skipped.
pub fn should_skip_dep(
    name: &str,
    crate_info: &CrateInfo,
    deps_and_features: &[(String, String)],
    enable_features: &[String],
) -> bool {
    let dep_name = name.split(':').next().unwrap_or("").to_string();
    let feat_of_dep = deps_and_features
        .iter()
        .find(|(dep, _)| dep == &dep_name)
        .map(|(_, feat)| feat);

    if let Some(feat) = feat_of_dep {
        if enable_features.contains(feat) {
            debug!("Dependency {} is enabled by feature {}", dep_name, feat);
            return false;
        }
    }

    if is_dep_optional(crate_info, &dep_name) {
        debug!("Dependency {} is optional, skipping", dep_name);
        return true;
    }

    false
}

fn is_dep_optional(crate_info: &CrateInfo, name: &str) -> bool {
    crate_info
        .deps_and_features
        .iter()
        .find(|(dep, _)| dep.name == name)
        .map(|(dep, _)| dep.optional)
        .unwrap_or(false)
}

fn parse_top_level_externs<'a>(
    ctx: &'a z3::Context,
    names_and_versions: &[(String, String)],
    externs: &Vec<ItemExternCrate>,
) -> Result<(Option<Bool<'a>>, ParsedAttr), anyhow::Error> {
    let mut worklist = Vec::new();
    for ex in externs {
        let (equation, parsed_attr) = parse_main_attributes_direct(ex.attrs.first().unwrap(), ctx);
        // If there is no attribute guarding the extern crate,
        // then we can't control it.
        if equation.is_none() {
            continue;
        }
        let version = names_and_versions
            .iter()
            .find(|(name, _)| name == &ex.ident.to_string())
            .map(|(_, version)| version);
        if version.is_none() {
            continue;
        }
        let name_with_version = downloader::clone_from_crates(&ex.ident.to_string(), version)?;
        let items = parse_item_extern_crates(&name_with_version);
        if items.itemexterncrates.len() == 0 {
            continue;
        }
        let std_attrs = get_item_extern_std(&items);
        if std_attrs.is_some() {
            return Ok((equation, parsed_attr));
        }
        worklist.push((name_with_version, equation, parsed_attr));
    }
    Ok(parse_n_level_externs_entry(&mut worklist))
}

fn parse_n_level_externs_entry<'a>(
    worklist: &mut Vec<(String, Option<Bool<'a>>, ParsedAttr)>,
) -> (Option<Bool<'a>>, ParsedAttr) {
    let mut worklists = Vec::new();
    worklist.iter().for_each(|(name_with_version, _, _)| {
        worklists.push((name_with_version.clone(), Vec::<String>::new()));
    });
    loop {
        if worklists.iter().all(|(_, remaining)| remaining.is_empty()) {
            return (None, ParsedAttr::default());
        }
        for (name_with_version, equation, parsed_attr) in worklist.iter() {
            let local_worklist = worklists
                .iter_mut()
                .find(|(name, _)| name == name_with_version)
                .unwrap();
            if parse_n_level_externs(&mut local_worklist.1) {
                return (equation.clone(), parsed_attr.clone());
            }
        }
    }
}

fn parse_n_level_externs<'a>(worklist: &mut Vec<String>) -> bool {
    let mut local_worklist = Vec::new();
    for name_with_version in worklist.drain(..) {
        let (mut name, version) = name_with_version.split_once(':').unwrap();
        let new_name_with_version =
            downloader::clone_from_crates(name, Some(&version.to_string())).unwrap();
        name = new_name_with_version
            .split_once(':')
            .map_or(name, |(n, _)| n);
        let names_and_versions = downloader::read_dep_names_and_versions(name, version).unwrap();
        let unfiltered = parse_item_extern_crates(&new_name_with_version);
        let std_attrs = get_item_extern_std(&unfiltered);
        if std_attrs.is_some() {
            return true;
        }
        let externs = get_item_extern_dep(&unfiltered, &names_and_versions);
        externs.iter().for_each(|ex| {
            let version = names_and_versions
                .iter()
                .find(|(name, _)| name == &ex.ident.to_string())
                .map(|(_, version)| version);
            local_worklist.push(format!(
                "{}:{}",
                ex.ident.to_string(),
                version.unwrap_or(&"latest".to_string())
            ));
        });
    }
    worklist.extend(local_worklist);
    false
}

fn get_item_extern_dep(
    itemexterncrates: &ItemExternCrates,
    names: &[(String, String)],
) -> Vec<ItemExternCrate> {
    let mut externs = Vec::new();
    for i in itemexterncrates.itemexterncrates.iter() {
        debug!("Checking ident: {}", i.ident);
        names.iter().for_each(|(name, _)| {
            if i.ident == *name.replace("-", "_") {
                debug!("Found ident: {}", i.ident);
                externs.push(i.clone());
            }
        });
    }
    externs
}

fn get_deps_and_features<'a>(
    name: &str,
    version: &str,
    crate_info: &'a CrateInfo,
) -> Option<&'a Vec<(CrateInfo, Vec<String>)>> {
    if crate_info.name == name && crate_info.version == version {
        return Some(&crate_info.deps_and_features);
    }
    for (dep, _) in &crate_info.deps_and_features {
        if let Some(res) = get_deps_and_features(name, version, &dep) {
            return Some(res);
        }
    }
    None
}

fn get_files_in_attributes<'a>(
    attrs: &Attributes,
    ctx: &'a z3::Context,
) -> Vec<(String, Option<Bool<'a>>)> {
    let mut files_and_equations = Vec::new();
    for attr in &attrs.attributes {
        if attr.path().get_ident().unwrap() == "cfg_attr" {
            let (eq, parsed_attr) = parse_main_attributes_direct(attr, ctx);
            if let Some(filepath) = parsed_attr.filepath {
                files_and_equations.push((filepath, eq));
            }
        }
    }
    files_and_equations
}

fn visit<T>(visiter_type: &mut T, crate_name: &str, recurse: bool) -> anyhow::Result<()>
where
    T: for<'a> Visit<'a>,
{
    let path = format!("{}/{}", DOWNLOAD_PATH, crate_name.replace(':', "-"));
    let files = get_all_rs_files(&path, recurse);

    for filename in files {
        debug!("Parsing file: {}", filename);
        let content = fs::read_to_string(&filename).context("Failed to read file")?;
        let file = syn::parse_file(&content).context("Failed to parse file")?;
        visiter_type.visit_file(&file);
    }
    Ok(())
}

fn is_any_logic(logic: &String) -> Option<Logic> {
    match logic.as_str() {
        "any" => Some(Logic::Any),
        "and" => Some(Logic::And),
        "all" => Some(Logic::And),
        "or" => Some(Logic::Or),
        "not" => Some(Logic::Not),
        _ => None,
    }
}

fn is_no_std(parsed: &ParsedAttr) -> bool {
    if let Some(last) = parsed.constants.last() {
        last == "no_std"
    } else {
        false
    }
}

fn parse_token_stream<'a>(
    tokens: TokenStream,
    parsed: &mut ParsedAttr,
    ctx: &'a z3::Context,
    equation: &mut Option<Bool<'a>>,
    in_any: &mut bool,
) -> Vec<Bool<'a>> {
    let mut was_feature = false;
    let mut was_filepath = false;
    let mut was_target_arch = false;
    let mut current_expr: Option<Bool> = None;
    let mut group_items: Vec<Bool> = Vec::new();
    let mut curr_logic = Logic::Any;

    for token in tokens {
        match token {
            proc_macro2::TokenTree::Group(g) => {
                let mut group_expr = None;
                let local_group_items =
                    parse_token_stream(g.stream(), parsed, ctx, &mut group_expr, in_any);

                let refs: Vec<&Bool> = local_group_items.iter().collect();
                if refs.is_empty() {
                    continue;
                }
                let local_expr = match curr_logic {
                    Logic::And => Some(Bool::and(ctx, refs.as_slice())),
                    Logic::Or | Logic::Any => Some(Bool::or(ctx, refs.as_slice())),
                    Logic::Not => {
                        current_expr = match local_group_items.first() {
                            Some(first) => Some(first.not()),
                            None => None,
                        };
                        None
                    }
                };

                if let Some(local) = local_expr {
                    current_expr = Some(
                        current_expr.map_or(local.clone(), |prev| Bool::and(ctx, &[&prev, &local])),
                    );
                }
            }
            proc_macro2::TokenTree::Ident(i) => {
                let ident_str = i.to_string();

                if ident_str == "feature" {
                    was_feature = true;
                } else if ident_str == "target_arch" {
                    was_target_arch = true;
                } else if ident_str == "path" {
                    was_filepath = true;
                } else if let Some(logic) = is_any_logic(&ident_str) {
                    parsed.logic.push(logic.clone());
                    curr_logic = logic;
                } else {
                    parsed.constants.push(ident_str);
                }
            }
            proc_macro2::TokenTree::Literal(l) => {
                if was_feature {
                    let feature_str = l.to_string()[1..l.to_string().len() - 1].to_string();
                    parsed.features.push(feature_str.clone());

                    let feature_var = Bool::new_const(ctx, feature_str);
                    group_items.push(feature_var);
                    was_feature = false;
                } else if was_target_arch {
                    let target_arch_str = l.to_string()[1..l.to_string().len() - 1].to_string();
                    parsed.possible_target_archs.push(target_arch_str);
                    was_target_arch = false;
                } else if was_filepath {
                    let filepath_str = l.to_string()[1..l.to_string().len() - 1].to_string();
                    parsed.filepath = Some(filepath_str);
                    was_filepath = false;
                }
            }
            _ => {}
        }
    }

    if let Some(expr) = current_expr {
        *equation = Some(expr.clone());
        group_items.push(expr);
    } else {
        if !group_items.is_empty() {
            match curr_logic {
                Logic::And => {
                    let refs: Vec<&Bool> = group_items.iter().collect();
                    *equation = Some(Bool::and(ctx, refs.as_slice()));
                }
                Logic::Or | Logic::Any => {
                    let refs: Vec<&Bool> = group_items.iter().collect();
                    *equation = Some(Bool::or(ctx, refs.as_slice()));
                }
                Logic::Not => {
                    *equation = Some(group_items.first().unwrap().not());
                }
            }
        }
    }

    group_items
}

fn parse_meta_for_cfg_attr<'a>(
    meta: &Meta,
    ctx: &'a z3::Context,
) -> (Option<Bool<'a>>, ParsedAttr) {
    match meta {
        Meta::List(list) => {
            let tokens = list.tokens.clone();
            let mut parsed = ParsedAttr::default();
            let mut equation = None;
            parse_token_stream(tokens, &mut parsed, ctx, &mut equation, &mut false);
            (equation, parsed)
        }
        _ => {
            debug!("Meta is not a list");
            (None, ParsedAttr::default())
        }
    }
}

fn get_all_rs_files(path: &str, recurse: bool) -> Vec<String> {
    let path_obj = Path::new(path);
    if path_obj.is_file() && path_obj.extension().unwrap_or_default() == "rs" {
        return vec![path.to_string()];
    }

    let mut files = Vec::new();

    if recurse {
        for entry in WalkDir::new(path) {
            push_to_files_vec(&entry.unwrap().path(), &mut files);
        }
    } else {
        for entry in fs::read_dir(format!("{}/src", path)).unwrap() {
            push_to_files_vec(&entry.unwrap().path(), &mut files);
        }
    }
    files
}

fn push_to_files_vec(path: &Path, files: &mut Vec<String>) {
    if path.extension().unwrap_or_default() == "rs" && !path.to_str().unwrap().contains("/tests/") {
        files.push(path.to_str().unwrap().to_string());
    }
}

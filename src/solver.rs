use anyhow::Context;
use log::debug;
use std::fs;
use toml;
use z3::{self, ast::Bool};

use crate::{CrateInfo, Telemetry, consts::CUSTOM_FEATURES_ENABLED, parser};

/// Given a context, a main equation and a list of
/// filtered equations, solve for the main equation
/// # Arguments
/// * `ctx` - The Z3 context
/// * `main_equation` - The main equation to solve for
/// * `filtered` - The list of filtered equations
/// # Returns
/// * `Option<z3::Model>` - The model if the equation is satisfiable
/// * `None` - If the equation is not satisfiable
pub fn solve<'a>(
    ctx: &'a z3::Context,
    main_equation: &Option<Bool>,
    filtered: &Vec<Bool>,
) -> (Option<z3::Model<'a>>, usize, usize) {
    let solver = z3::Solver::new(ctx);
    let possible = find_possible_equations(ctx, main_equation, filtered);
    let (mut len, mut depth) = (0, 0);
    if !possible.is_empty() {
        for p in possible {
            let (l, d) = length_and_depth(p.to_string());
            if l > len {
                len = l;
            }
            if d > depth {
                depth = d;
            }
            solver.assert(&p);
        }
    }
    if let Some(eq) = main_equation {
        solver.assert(eq);
    }
    let result = solver.check();
    let model = match result {
        z3::SatResult::Sat => solver.get_model(),
        _ => None,
    };
    assert_eq!(result, z3::SatResult::Sat);
    (model, len, depth)
}

fn length_and_depth(ast: String) -> (usize, usize) {
    let tokens: Vec<&str> = ast.split_whitespace().filter(|s| !s.is_empty()).collect();
    let node_count = tokens.iter().filter(|s| **s != "(" && **s != ")").count();
    let mut max_depth = 0;
    let mut current_depth = 0;
    for c in ast.chars() {
        match c {
            '(' => {
                current_depth += 1;
                max_depth = max_depth.max(current_depth);
            }
            ')' => current_depth -= 1,
            _ => {}
        }
    }
    (node_count, max_depth)
}

/// Given a model, convert it to a list of features
/// to enable and disable.
/// # Arguments
/// * `model` - The model to convert
/// # Returns
/// * `Vec<String>` - The list of features to enable
/// * `Vec<String>` - The list of features to disable
pub fn model_to_features(model: &Option<z3::Model>) -> (Vec<String>, Vec<String>) {
    if model.is_none() {
        return (Vec::new(), Vec::new());
    }
    let model = model.as_ref().unwrap();
    (
        model_to_enabled_features(model),
        model_to_disabled_features(model),
    )
}

/// Given the crate info, the name of the dependency,
/// the list of features to enable and disable,
/// return the final feature list to pass to cargo for the dependency.
/// # Arguments
/// * `crate_info` - The crate info
/// * `name` - The name of the dependency
/// * `enable` - The list of features to enable
/// * `disable` - The list of features to disable
/// # Returns
/// * `(Vec<String>, bool)` - The final feature list for the
///
/// dependency and a boolean indicating if the default
/// features list of main crate should be updated
pub fn final_feature_list_dep(
    crate_info: &CrateInfo,
    name: &str,
    enable: &[String],
    disable: &[String],
    crate_name_rename: &[(String, String)],
    telemetry: &mut Telemetry,
) -> (Vec<String>, bool) {
    let mut update_default_config = false;
    let (dep_crate_info, dep_already_enabled) = crate_info
        .deps_and_features
        .iter()
        .find(|(dep, _)| dep.name == name)
        .unwrap_or_else(|| {
            panic!("Dependency {} not found in the list of dependencies", name);
        });

    if disable_in_default(dep_crate_info, disable) && dep_crate_info.default_features {
        update_default_config = true;
    }

    let main_available_features = &crate_info.features;
    let mut features_to_enable = Vec::new();
    // We track the features that exist and are required by the dependency
    // to make it no_std, but the main crate does not provide a way to enable them.
    let mut not_found = Vec::new();
    for to_enable in enable {
        // If main crate added this feature when declaring the dependency,
        // we don't need to add it again.
        if dep_already_enabled.contains(to_enable) {
            continue;
        }
        match main_available_features.iter().find(|(_, features)| {
            features
                .iter()
                .any(|(dep, feature)| dep == name && feature == to_enable)
        }) {
            Some((main_feat, _)) => features_to_enable.push(main_feat.clone()),
            None => not_found.push(to_enable.clone()),
        }
    }

    let dep_feats_to_remove: Vec<String> = disable
        .iter()
        .filter(|feat| dep_already_enabled.contains(feat))
        .cloned()
        .collect();

    if !dep_feats_to_remove.is_empty() {
        telemetry
            .default_list_modified
            .push((name.to_string(), true));
    } else {
        telemetry
            .default_list_modified
            .push((name.to_string(), false));
    }

    if !not_found.is_empty() {
        telemetry
            .custom_features_added
            .push((name.to_string(), true));
        telemetry
            .custom_features_added_list
            .push((name.to_string(), not_found.clone()));
        features_to_enable.push(CUSTOM_FEATURES_ENABLED.to_string());
    } else {
        telemetry
            .custom_features_added
            .push((name.to_string(), false));
    }

    let main_name = format!("{}-{}", crate_info.name, crate_info.version);
    parser::update_feat_lists(
        &main_name,
        &name.to_string(),
        &dep_feats_to_remove,
        &not_found,
        crate_name_rename,
    );

    // TODO IMPORTANT: Do we need this here? `move_unnecessary_dep_feats` seems to
    // do the same thing.
    // parser::remove_conflicting_dep_feats(&main_name, name, disable);

    (features_to_enable, update_default_config)
}

/// Given the crate info, the list of features to enable and disable,
/// return the final feature list to pass to cargo for the main crate.
/// # Arguments
/// * `crate_info` - The crate info
/// * `enable` - The list of features to enable
/// * `disable` - The list of features to disable
/// # Returns
/// * `(bool, Option<String>)` - A boolean indicating if the default
///
/// features list of main crate should disabled and a string
/// containing the final feature list for the main crate.
pub fn final_feature_list_main(
    crate_info: &CrateInfo,
    enable: &mut Vec<String>,
    disable: &[String],
    telemetry: &mut Telemetry,
) -> (bool, Vec<String>) {
    let mut disable_default = enable.is_empty();
    let mut enable_from_default = Vec::new();

    if disable_in_default(crate_info, disable) || disable_in_default_indirect(crate_info, disable) {
        disable_default = true;
        enable_from_default = get_features_not_disabled(crate_info, disable);
    }

    let kept = new_feats_to_add(crate_info, &enable_from_default, enable);

    debug!("Main crate does not have features: {:?}", kept);
    let main_name = format!("{}-{}", crate_info.name, crate_info.version);
    let main_manifest = parser::determine_manifest_file(&main_name);
    let mut main_toml: toml::Value =
        toml::from_str(&fs::read_to_string(&main_manifest).unwrap()).unwrap();
    telemetry.new_feats_added_to_main = !kept.is_empty();
    for to_add in kept {
        telemetry.new_feats_added_to_main_list.push(to_add.clone());
        parser::add_feats_to_custom_feature(&mut main_toml, &to_add, &[]);
    }
    fs::write(
        &main_manifest,
        toml::to_string(&main_toml)
            .context("Failed convert Value to string")
            .unwrap(),
    )
    .unwrap();

    (disable_default, enable_from_default)
}

/// This returns the list of features that need to be added to the main crate's
/// manifest file. It also removes features that are implicitly added by cargo
/// for optional dependencies.
/// # Arguments
/// * `crate_info` - The crate info
/// * `enable_from_default` - The list of features to enable from default list that are not disabled
///   explicitly by the disable list.
/// * `enable` - The list of features to enable
/// # Returns
/// * `Vec<String>` - The list of features to add to the main crate's manifest file
pub fn new_feats_to_add(
    crate_info: &CrateInfo,
    enable_from_default: &[String],
    enable: &mut Vec<String>,
) -> Vec<String> {
    let optional_deps: Vec<String> = crate_info
        .deps_and_features
        .iter()
        .filter(|(dep, _)| dep.optional)
        .map(|(dep, _)| dep.name.clone())
        .collect();

    let main_available_features = &crate_info.features;
    let mut not_found = Vec::new();
    enable_from_default
        .iter()
        .chain(enable.iter())
        .for_each(|to_enable| {
            if !main_available_features
                .iter()
                .any(|(name, _)| name == to_enable)
            {
                not_found.push(to_enable.clone());
            }
        });

    // We don't want to add features that are implicitly added by cargo for
    // optional dependencies.
    let (removed, kept): (Vec<String>, Vec<String>) = not_found
        .into_iter()
        .partition(|feat| optional_deps.iter().any(|dep| feat == dep));

    enable.retain(|feat| !removed.contains(feat));

    kept
}

fn get_features_not_disabled(crate_info: &CrateInfo, disable: &[String]) -> Vec<String> {
    let mut feats: Vec<String> = Vec::new();
    if let Some((_, features)) = crate_info
        .features
        .iter()
        .find(|(name, _)| name == "default")
    {
        for feature in features {
            let current_feat = if feature.0 == feature.1 {
                feature.1.clone()
            } else {
                // If the feature is to enable a feature from a dependency, then we
                // don't need to worry about it enabling other features in the main crate.
                continue;
            };
            let mut all_enabled = vec![current_feat.clone()];
            all_enabled_for_feat(&mut all_enabled, crate_info);
            if all_enabled.iter().all(|f| !disable.contains(f)) {
                feats.push(current_feat);
            }
        }
    };
    feats
}

/// Given the crate info, the list of features to disable,
/// return a boolean indicating if any of the disabled features
/// are part of the default feature list.
/// # Arguments
/// * `crate_info` - The crate info
/// * `disable` - The list of features to disable
/// # Returns
/// * `bool` - True if any of the disabled features are part of the default feature list
pub fn disable_in_default(crate_info: &CrateInfo, disable: &[String]) -> bool {
    crate_info
        .features
        .iter()
        .find(|(name, _)| name == "default")
        .is_some_and(|(_, features)| features.iter().any(|feature| disable.contains(&feature.1)))
}

fn disable_in_default_indirect(crate_info: &CrateInfo, disable: &[String]) -> bool {
    let default_feats = crate_info
        .features
        .iter()
        .find(|(name, _)| name == "default")
        .map(|(_, features)| {
            features
                .iter()
                .map(|(_, feature)| feature.clone())
                .collect::<Vec<String>>()
        })
        .unwrap_or_default();

    let mut all_enabled = default_feats.clone();
    all_enabled_for_feat(&mut all_enabled, crate_info);

    disable.iter().any(|feat| all_enabled.contains(feat))
}

fn all_enabled_for_feat(all_enabled: &mut Vec<String>, crate_info: &CrateInfo) {
    let mut to_check = all_enabled.clone();

    while let Some(f) = to_check.pop() {
        if let Some((_, features)) = crate_info.features.iter().find(|(name, _)| name == &f) {
            for (k, v) in features {
                let full = if k == v {
                    v.clone()
                } else {
                    format!("{}/{}", k, v)
                };
                if !all_enabled.contains(&full) {
                    all_enabled.push(full.clone());
                    to_check.push(full);
                }
            }
        }
    }
}

fn model_to_enabled_features(model: &z3::Model) -> Vec<String> {
    let mut features: Vec<String> = Vec::new();

    for it in model.iter() {
        let name = it.name().to_string();

        if let Some(true) = model
            .eval(&it.apply(&[]), true)
            .and_then(|v| v.as_bool())
            .and_then(|v| v.as_bool())
        {
            features.push(name);
        }
    }

    features
}

fn model_to_disabled_features(model: &z3::Model) -> Vec<String> {
    let mut features: Vec<String> = Vec::new();

    for it in model.iter() {
        let name = it.name().to_string();

        if let Some(false) = model
            .eval(&it.apply(&[]), true)
            .and_then(|v| v.as_bool())
            .and_then(|v| v.as_bool())
        {
            features.push(name);
        }
    }
    features
}

fn find_possible_equations<'a>(
    ctx: &z3::Context,
    main_equation: &Option<Bool>,
    filtered: &Vec<Bool<'a>>,
) -> Vec<Bool<'a>> {
    let mut possible: Vec<Bool<'a>> = Vec::new();
    let solver = z3::Solver::new(ctx);
    for eq in filtered {
        if let Some(eq) = main_equation {
            solver.assert(eq);
        }
        if !possible.is_empty() {
            for p in &possible {
                solver.assert(p);
            }
        }
        solver.assert(eq);
        let result = solver.check();
        if result == z3::SatResult::Sat {
            possible.push(eq.clone());
        }
        solver.reset();
    }
    possible
}

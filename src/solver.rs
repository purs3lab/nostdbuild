use anyhow::Context;
use log::debug;
use std::fs;
use toml;
use z3::{self, ast::Bool};

use crate::{CrateInfo, consts::CUSTOM_FEATURES_ENABLED, parser};

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
) -> Option<z3::Model<'a>> {
    let solver = z3::Solver::new(ctx);
    let possible = find_possible_equations(ctx, main_equation, filtered);
    if !possible.is_empty() {
        for p in possible {
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
    model
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
    // We track that features that exist and are required by the dependency
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

    if !not_found.is_empty() {
        features_to_enable.push(CUSTOM_FEATURES_ENABLED.to_string());
    }

    let main_name = format!("{}-{}", crate_info.name, crate_info.version);
    parser::update_feat_lists(
        &main_name,
        &name.to_string(),
        &dep_feats_to_remove,
        &not_found,
        crate_name_rename,
    );

    parser::remove_conflicting_dep_feats(&main_name, name, disable);

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
    enable: &[String],
    disable: &[String],
) -> (bool, Vec<String>) {
    let mut disable_default = false;
    let mut enable_from_default = Vec::new();

    if disable_in_default(crate_info, disable) {
        disable_default = true;
        enable_from_default = get_features_not_disabled(crate_info, disable);
    }

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
    debug!("Main crate does not have features: {:?}", not_found);
    let main_name = format!("{}-{}", crate_info.name, crate_info.version);
    let main_manifest = parser::determine_manifest_file(&main_name);
    let mut main_toml: toml::Value =
        toml::from_str(&fs::read_to_string(&main_manifest).unwrap()).unwrap();
    for to_add in not_found {
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

fn get_features_not_disabled(crate_info: &CrateInfo, disable: &[String]) -> Vec<String> {
    let mut feats: Vec<String> = Vec::new();
    if let Some((_, features)) = crate_info
        .features
        .iter()
        .find(|(name, _)| name == "default")
    {
        features.iter().for_each(|feature| {
            if !disable.contains(&feature.1) {
                feats.push(feature.1.clone());
            }
        });
    };
    feats
}

fn disable_in_default(crate_info: &CrateInfo, disable: &[String]) -> bool {
    crate_info
        .features
        .iter()
        .find(|(name, _)| name == "default")
        .is_some_and(|(_, features)| features.iter().any(|feature| disable.contains(&feature.1)))
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

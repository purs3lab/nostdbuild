use z3;
use z3::ast::Bool;

use crate::CrateInfo;

/// Given a context, a main equation and a list of
/// filtered equations, solve for the main equation
/// # Arguments
/// * `ctx` - The Z3 context
/// * `main_equation` - The main equation to solve for
/// * `filtered` - The list of filtered equations
/// # Returns
/// * `SatResult` - The result of the solving
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
/// * `Vec<String>` - The final feature list for the dependency
pub fn final_feature_list_dep(
    crate_info: &CrateInfo,
    name: &str,
    enable: &[String],
    disable: &[String],
) -> Vec<String> {
    let dep_crate_info = crate_info
        .deps_and_features
        .iter()
        .find(|(dep, _)| dep.name == name)
        .map(|(dep, _)| dep)
        .unwrap_or_else(|| {
            panic!("Dependency {} not found in the list of dependencies", name);
        });

    if disable_in_default(dep_crate_info, disable) {
        // If depencency requires disabling default features
        // but the main crate does not have default_features
        // set to false, then we can't enable no_std for
        // the dependency.
        assert!(!dep_crate_info.default_features);
    }

    let main_available_features = &crate_info.features;
    let features_to_enable: Vec<String> = enable
        .iter()
        .filter_map(|to_enable| {
            main_available_features
                .iter()
                .find(|(_, features)| {
                    features
                        .iter()
                        .any(|(dep, feature)| dep == name && feature == to_enable)
                })
                .map(|(main_feat, _)| main_feat.clone())
        })
        .collect();
    features_to_enable
}

/// Given the crate info, the list of features to enable and disable,
/// return the final feature list to pass to cargo for the main crate.
/// # Arguments
/// * `crate_info` - The crate info
/// * `enable` - The list of features to enable
/// * `disable` - The list of features to disable
/// # Returns
/// * `Vec<String>` - The final feature list for the main crate
pub fn final_feature_list_main(
    crate_info: &CrateInfo,
    enable: &[String],
    disable: &[String],
) -> Vec<String> {
    let mut args = Vec::new();
    if disable_in_default(crate_info, disable) {
        args.push("--no-default-features".to_string());
    }
    if !enable.is_empty() {
        args.push("--features".to_string());
        args.push(enable.join(","));
    }
    args
}

fn disable_in_default(crate_info: &CrateInfo, disable: &[String]) -> bool {
    crate_info
        .features
        .iter()
        .find(|(name, _)| name == "default")
        .map_or(false, |(_, features)| {
            features.iter().any(|feature| disable.contains(&feature.1))
        })
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
            continue;
        }
        solver.reset();
    }
    possible
}

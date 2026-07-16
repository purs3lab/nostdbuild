use anyhow::Context;
use log::debug;
use proc_macro2::TokenStream;
// use quote::ToTokens;
use std::{
    collections::{HashMap, HashSet},
    fs,
    path::{Path, PathBuf},
    time::Instant,
};
use syn::{Attribute, ItemExternCrate, Meta, visit::Visit};
use walkdir::WalkDir;
use z3::{self, ast::Bool};

use strsim::levenshtein;

use crate::{
    Attributes, CrateInfo, DBData, DEPENDENCIES, DataExchange, Telemetry, consts, db, downloader,
    driver,
    solver::{self, model_to_features},
    visitor::{GetItemExternCrate, ItemExternCrates, ItemExternCratesAll, ParsedAttr},
};

use crate::types::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Logic {
    And,
    Or,
    Not,
    Any,
}

/// Parse the extern crates of the main crate
/// # Arguments
/// * `crate_name` - The name of the main crate
/// # Returns
/// The extern crates of the main crate
/// that have attributes associated with them.
pub fn parse_item_extern_crates(crate_name: &str, main_name: Option<&str>) -> ItemExternCrates {
    let mut itemexterncrates = ItemExternCrates {
        itemexterncrates: Vec::new(),
    };

    if let Err(err) = visit(&mut itemexterncrates, crate_name, true, false, main_name) {
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
pub fn parse_item_extern_crates_for_files(
    crate_name: &str,
    main_name: Option<&str>,
) -> Vec<String> {
    let dir = get_actual_dir(crate_name, main_name);

    let files = get_all_rs_files(&dir, true, main_name);
    let mut files_ungaurded = Vec::new();
    for file in files {
        let mut itemexterncrates = ItemExternCratesAll {
            itemexterncrates: Vec::new(),
        };

        if let Err(err) = visit(
            &mut itemexterncrates,
            file.as_os_str().to_str().unwrap_or_default(),
            true,
            true,
            main_name,
        ) {
            debug!(
                "Failed to parse file {:?} with error:{}. Will continue...",
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
                    .any(|a| a.path().get_ident().is_some_and(|ident| ident == "cfg"))
            });
        if extern_std_without_cfg {
            debug!("Found unguarded extern crate std in file: {:?}", file);
            let basename = file.file_name().and_then(|s| s.to_str()).unwrap_or("");
            files_ungaurded.push(basename.to_string());
        }
    }
    files_ungaurded
}

/// Get the attributes of the extern crate std
/// # Arguments
/// * `itemexterncrates` - The extern crates of the main crate
/// # Returns
/// The attributes of the extern crate std
/// if it exists, otherwise None.
pub fn get_item_extern_std(itemexterncrates: &ItemExternCrates) -> Vec<Attribute> {
    itemexterncrates
        .itemexterncrates
        .iter()
        .filter(|i| i.ident == "std")
        .flat_map(|i| i.attrs.iter())
        .filter(|a| a.path().get_ident().is_some_and(|ident| ident == "cfg"))
        .cloned()
        .collect()
}

/// Parse the main crate and return the attributes
/// # Arguments
/// * `crate_name` - The name of the main crate
/// # Returns
/// The attributes of the main crate
/// TODO: This should not need to take hir_spans anymore
pub fn parse_crate(
    crate_name: &str,
    recurse: bool,
    main_name: Option<&str>,
    hir_spans: &[ReadableSpan],
) -> Attributes {
    let mut attributes = Attributes {
        hir_spans: hir_spans.to_vec(),
        ..Default::default()
    };

    if let Err(err) = visit(&mut attributes, crate_name, recurse, false, main_name) {
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
pub fn check_for_no_std(
    name: &str,
    ctx: &z3::Context,
    telemetry: Option<&mut Telemetry>,
    main_name: Option<&str>,
) -> bool {
    // This is the list of known syn failure crates which are no_std
    if consts::KNOWN_SYN_FAILURES.contains(&name) {
        debug!("Skipping known syn failure crate: {}", name);
        return true;
    }

    // We need to re-parse this instead of using already existing attributes
    // since files in non root directory might have `no_std` attribute
    // and we don't want to include those.
    // TODO: Remove the file parser here to use the files returned by analyze_crate
    let base_attrs = parse_crate(name, false, main_name, &[]);

    if let Some(telemetry) = telemetry {
        telemetry.wrong_unconditional_setup = base_attrs.wrong_unconditional_setup;
    }

    if !parse_main_attributes(&base_attrs, ctx).0 && !base_attrs.unconditional_no_std {
        debug!("No no_std found for the crate {}", name);
        return false;
    }
    true
}

/// Parse the dependencies of the main crate
/// # Returns
/// A vector containing the attributes of each dependency
pub fn parse_deps_crate(
    main_name: &str,
    telemetry: &mut Telemetry,
    db_data: &[DBData],
) -> Vec<Attributes> {
    let mut attributes = Vec::new();
    let deps_lock = DEPENDENCIES.lock().unwrap();
    for dep in deps_lock.iter() {
        if is_proc_macro(dep, Some(main_name)) {
            debug!("Skipping proc macro dependency: {}", dep);
            continue;
        }

        // If this dep is already in the DB we only need its name for the later
        // finalize_dep_crate call — skip the expensive analysis and parsing.
        if db::get_from_db_data(db_data, dep).is_some() {
            debug!("Dependency {} found in DB, skipping analysis", dep);
            attributes.push(Attributes {
                crate_name: dep.clone(),
                ..Default::default()
            });
            continue;
        }

        // Create a new ctx per dependency
        let ctx = z3::Context::new(&z3::Config::new());
        let (all_hard, _, _, _, _, _) =
            driver::analyze_crate_wrapper(&ctx, &dep.clone(), Some(main_name), telemetry);
        attributes.push(parse_crate(&dep.clone(), true, Some(main_name), &all_hard));
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
/// * `exchange` - The data exchange struct that contains all the necessary data for processing the crate
/// * `attrs` - The attributes of the crate
/// * `name_with_version` - The name of the crate. Exchange crate name is used if None is provided.
/// * `crate_info` - The crate info of the main crate. Exchange crate info is used if None is provided.
/// * `is_main` - A boolean indicating whether the crate is the main crate
/// * `optional_dep_feats` - The features that enable the optional dependencies of the crate
pub fn process_crate(
    exchange: &mut DataExchange,
    ctx: &z3::Context,
    attrs: &mut Attributes,
    name_with_version: Option<&str>,
    crate_info: Option<&CrateInfo>,
    is_main: bool,
    optional_dep_feats: &mut TupleVec,
    hard_constraints: Option<Bool>,
) -> anyhow::Result<DoubleTupleVecString> {
    let (mut enable, mut disable): DoubleTupleVecString = (Vec::new(), Vec::new());

    let name_with_version = name_with_version.unwrap_or(&exchange.name_with_version);
    let crate_info = crate_info.unwrap_or(&exchange.crate_info);

    let (no_std, mut equation, mut parsed_attr) = parse_main_attributes(attrs, ctx);

    let main_name = if is_main {
        None
    } else {
        Some(exchange.name_with_version.as_ref())
    };

    if is_main {
        exchange.telemetry.main_conditional_no_std = no_std;
        exchange.telemetry.unknown_idents_in_attrs = parsed_attr.typoed_keyword;
    } else {
        exchange
            .telemetry
            .conditional_no_std_deps
            .push((name_with_version.to_string(), no_std));
        exchange
            .telemetry
            .unknown_idents_in_attrs_deps
            .push((name_with_version.to_string(), parsed_attr.typoed_keyword));
    }

    if !attrs.unconditional_no_std {
        if !no_std {
            debug!("No no_std found for the crate");
            return Ok((Vec::new(), Vec::new()));
        }
    } else {
        if is_main {
            exchange.telemetry.main_unconditional_no_std = true;
        } else {
            exchange
                .telemetry
                .unconditional_no_std_deps
                .push((name_with_version.to_string(), true));
        }

        debug!(
            "crate {} is an unconditional no_std crate",
            name_with_version
        );
        // If the crate is both conditional and unconditional no_std,
        // we will treat it as unconditional.
        if no_std {
            debug!(
                "WARNING: Crate {} is both unconditional and conditional no_std, will consider only unconditional.",
                name_with_version
            );
        }

        let items = parse_item_extern_crates(name_with_version, main_name);

        // This case implies that the crate is no_std without any feature requirements.
        if items.itemexterncrates.is_empty() {
            debug!("No extern crates found for the crate");
            return Ok((Vec::new(), Vec::new()));
        }
        let std_attrs = get_item_extern_std(&items);
        if !std_attrs.is_empty() {
            debug!("Leaf level crate reached {}", name_with_version);
            if is_main {
                exchange.telemetry.direct_extern_std_usage_main = true;
            } else {
                exchange
                    .telemetry
                    .direct_extern_std_usage_deps
                    .push(name_with_version.to_string());
            }
            debug!("No features to enable for crate {}", name_with_version);
            let (local_equation, local_parsed_attr) = std_attrs.into_iter().fold(
                (None::<Bool>, None::<ParsedAttr>),
                |(local_eq, local_attr), std_attr| {
                    let (eq, mut attr) = parse_main_attributes_direct(&std_attr, ctx);
                    if eq.is_none() {
                        debug!("No equation found for attribute: {:?}", std_attr);
                        return (local_eq, local_attr);
                    }
                    let combined_eq = match local_eq {
                        Some(prev_eq) => Some(Bool::and(ctx, &[&prev_eq, &eq.unwrap()])),
                        None => Some(eq.unwrap()),
                    };
                    let combined_attr = match local_attr {
                        Some(prev_attr) => {
                            attr.features.extend(prev_attr.features);
                            Some(ParsedAttr {
                                features: attr.features,
                                ..prev_attr
                            })
                        }
                        None => Some(attr),
                    };
                    (combined_eq, combined_attr)
                },
            );
            (equation, parsed_attr) = (local_equation, local_parsed_attr.unwrap_or_default());
            // We need to negate the equation since we are
            // trying to remove std features.
            equation = equation.map(|eq| eq.not());
        } else if !is_main {
            debug!("Leaf level crate reached {}", name_with_version);
            let (name, version) = name_with_version.split_once(':').unwrap();
            if let Some(dep_and_features) = get_deps_and_features(name, version, crate_info) {
                let names_and_versions: TupleVec = dep_and_features
                    .iter()
                    .map(|(dep, _)| (dep.name.clone(), dep.version.clone()))
                    .collect();
                let externs = get_item_extern_dep(&items, &names_and_versions);
                match parse_top_level_externs(
                    ctx,
                    &names_and_versions,
                    &externs,
                    &mut exchange.telemetry,
                    &exchange.name_with_version,
                    Some(name_with_version),
                ) {
                    Ok((eq, attr)) => {
                        if let Some(eq) = eq {
                            equation = Some(eq.not());
                            parsed_attr = attr;
                        }
                    }
                    Err(e) => {
                        debug!("Failed to parse extern crates: {}", e);
                        return Ok((Vec::new(), Vec::new()));
                    }
                }
            }
        }
    }
    let equations = parse_attributes(attrs, ctx);
    let mut filtered = filter_equations(&equations, &parsed_attr.features);

    let mut non_minimalizable_features: HashSet<String> = HashSet::new();

    // Negated compile_error attributes are added to filtered only when they share at least one
    // feature with the main no_std equation. Unrelated constraints (e.g. "at least one storage
    // type" in uom, which shares no features with `not(feature="std")`) would cause Z3 to make
    // arbitrary disjunction picks that can break the build. Track excluded equations for the
    // post-solve satisfiability check below.
    let mut excluded_compile_error_eqs: Vec<Bool> = Vec::new();
    for negated_attr in attrs.compile_error_attrs.iter() {
        let (neg_eq, neg_parsed_attr) = parse_main_attributes_direct(negated_attr, ctx);
        non_minimalizable_features.extend(neg_parsed_attr.features.iter().cloned());
        if let Some(neg_eq) = neg_eq {
            let has_overlap = neg_parsed_attr
                .features
                .iter()
                .any(|f| parsed_attr.features.contains(f));
            if has_overlap {
                filtered.push(neg_eq);
            } else {
                excluded_compile_error_eqs.push(neg_eq);
            }
        }
    }

    let hard_constraint_vec: Vec<Bool> = if let Some(hard) = &hard_constraints {
        let solver = z3::Solver::new(ctx);
        solver.assert(hard);
        if solver.check() == z3::SatResult::Sat {
            let model = solver.get_model();
            let feats = model_to_features(&model);
            non_minimalizable_features.extend(feats.0);
            // Only feed the hard constraint into the solver when it is actually satisfiable;
            // constructing `vec![hard]` in the unsat branch too (the previous bug) made
            // `solve` assert a contradiction and panic.
            vec![hard.clone()]
        } else {
            // Hard constraints alone are contradictory (e.g. the parent imposes conflicting
            // feature requirements). The crate cannot be made no_std under them — record it and
            // bail rather than feeding an unsatisfiable constraint into the solver.
            debug!(
                "Hard constraints are unsatisfiable for crate {}",
                name_with_version
            );
            if is_main {
                exchange.telemetry.hard_unsat_main = Some(hard.to_string());
            } else {
                exchange
                    .telemetry
                    .hard_unsat_deps
                    .push((name_with_version.to_string(), hard.to_string()));
            }
            return Ok((Vec::new(), Vec::new()));
        }
    } else {
        vec![]
    };

    // This part adds equations if there are attributes that conditionally include
    // files which might contain unguarded `extern crate std`.
    // TODO: Remove the following as well
    let files_and_equations = get_files_in_attributes(attrs, ctx);
    if is_main {
        files_and_equations.iter().for_each(|(f, _)| {
            attrs.files_in_cfg_attrs.push(f.clone());
        });
    }
    if !files_and_equations.is_empty() {
        if is_main {
            exchange.telemetry.conditional_file_import_main = true;
        } else {
            exchange
                .telemetry
                .conditional_file_import_deps
                .push((name_with_version.to_string(), true));
        }
    }
    let files_unguarded = parse_item_extern_crates_for_files(name_with_version, main_name);
    debug!(
        "Files with unguarded extern crate std: {:?}",
        files_unguarded
    );

    let mut imported_files: Vec<String> = Vec::new();
    for (file, eq) in files_and_equations {
        if files_unguarded.contains(&file) {
            debug!("File {} contains unguarded extern crate std", file);
            imported_files.push(file.clone());
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

    if is_main {
        exchange
            .telemetry
            .conditional_files_with_std_main
            .extend(imported_files);
    } else {
        exchange
            .telemetry
            .conditional_files_with_std_deps
            .push((name_with_version.to_string(), imported_files));
    }

    // Now that the no_std equation is fully assembled (including the file-block contributions
    // above), verify the crate is actually satisfiable: the hard constraints together with the
    // equation must have a solution. Reaching here means the hard constraints alone were sat
    // (otherwise we returned early above), so this specifically catches the case where the
    // crate's own no_std equation conflicts with the parent's requirements. Bail before the
    // solver is handed an unsatisfiable system.
    if let Some(hard) = &hard_constraints
        && let Some(eq) = &equation
    {
        let solver = z3::Solver::new(ctx);
        solver.assert(hard);
        solver.assert(eq);
        if solver.check() != z3::SatResult::Sat {
            let cond = Bool::and(ctx, &[hard, eq]).to_string();
            debug!(
                "Hard constraints together with the no_std equation are unsatisfiable for crate {}",
                name_with_version
            );
            if is_main {
                exchange.telemetry.hard_with_main_unsat_main = Some(cond);
            } else {
                exchange
                    .telemetry
                    .hard_with_main_unsat_deps
                    .push((name_with_version.to_string(), cond));
            }
            return Ok((Vec::new(), Vec::new()));
        }
    }

    let now = Instant::now();
    // Finally, we solve the equations
    let (model, len, depth) = solver::solve(ctx, &equation, &filtered, &hard_constraint_vec);
    debug!(
        "Solver result for crate {}: model={:?}, len={}, depth={}",
        name_with_version, model, len, depth
    );
    exchange
        .telemetry
        .constraint_solving_time_ms
        .push((name_with_version.to_string(), now.elapsed().as_millis()));
    exchange
        .telemetry
        .max_contraint_length
        .push((name_with_version.to_string(), len));
    exchange
        .telemetry
        .max_constrait_depth
        .push((name_with_version.to_string(), depth));
    if enable.is_empty() && disable.is_empty() {
        (enable, disable) = solver::model_to_features(&model);
    }

    // Stage 2: verify that the solved feature set satisfies excluded compile_error constraints.
    // These constraints were not added to the solver because they share no features with the
    // main no_std condition. A failure here means the compile_error requirement is not met by
    // the features the solver chose — log a warning and record it in telemetry.
    if !excluded_compile_error_eqs.is_empty() {
        let check_solver = z3::Solver::new(ctx);
        for f in &enable {
            let var = z3::ast::Bool::new_const(ctx, f.as_str());
            check_solver.assert(&var);
        }
        for eq in &excluded_compile_error_eqs {
            check_solver.push();
            check_solver.assert(eq);
            if check_solver.check() != z3::SatResult::Sat {
                println!(
                    "[process_crate] WARNING: solved feature set for {} does not satisfy \
                     a compile_error constraint: {:?}",
                    name_with_version, eq
                );
                exchange
                    .telemetry
                    .compile_error_constraint_unsatisfied
                    .push(name_with_version.to_string());
            }
            check_solver.pop(1);
        }
    }

    minimize(
        crate_info,
        optional_dep_feats,
        &mut enable,
        &non_minimalizable_features,
        true,
        name_with_version,
        main_name,
        None,
    );

    Ok((enable, disable))
}

/// Returns the Cargo.toml string representation of how `dep_name` is enabled
/// in `values` (e.g. `"dep:somedep"` or `"somedep"`), or `None` if not present.
fn dep_entry_string_in_toml(dep_name: &str, values: &TupleVec) -> Option<String> {
    values.iter().find_map(|(k, v)| {
        if k == dep_name {
            if v == "dep:" {
                Some(format!("dep:{}", dep_name))
            } else if v.as_str() == dep_name {
                Some(dep_name.to_string())
            } else {
                None
            }
        } else {
            None
        }
    })
}

/// Walks the feature chain from `feat_name` and returns the name of the feature
/// Returns `true` if `feat_name` and every feature reachable from it transitively
/// serves no purpose other than enabling optional deps — i.e. every leaf value in
/// the subtree is an optional-dep reference and no feature in the subtree is
/// non-minimalizable. When this holds, `feat_name` can be dropped from the enable
/// list entirely, regardless of how many branches the subtree has.
fn all_subtree_deps_only(
    feat_name: &str,
    crate_info: &CrateInfo,
    optional_deps: &[String],
    non_minimalizable_features: &HashSet<String>,
    visited: &mut HashSet<String>,
) -> bool {
    if !visited.insert(feat_name.to_string()) {
        return true; // already verified (cycle-safe)
    }
    if non_minimalizable_features.contains(feat_name) {
        return false;
    }
    let Some(values) = crate_info
        .features
        .iter()
        .find(|(name, _)| name == feat_name)
        .map(|(_, v)| v)
    else {
        return false;
    };
    for (k, v) in values {
        let is_dep_ref = optional_deps.contains(k) && (v == "dep:" || v.as_str() == k.as_str());
        if is_dep_ref {
            continue;
        }
        if k == v {
            // Pure feature reference — recurse into sub-feature.
            if !all_subtree_deps_only(
                k,
                crate_info,
                optional_deps,
                non_minimalizable_features,
                visited,
            ) {
                return false;
            }
        } else {
            // Something other than a dep ref or plain feature ref (e.g. crate/feat).
            return false;
        }
    }
    true
}

/// Returns `true` if `feat_name` and every feature reachable from it transitively
/// serves no purpose other than enabling features of optional deps that are NOT in
/// `enabled_optional_deps`. If a dep/feat entry points to a dep that IS enabled, or
/// if the subtree contains any dep-enabling ref (`dep:X` / `X`), returns `false` —
/// the former means the configuration is live, the latter means the existing minimize
/// loop handles it. Non-minimalizable features also return `false`.
fn all_subtree_dep_feat_only(
    feat_name: &str,
    crate_info: &CrateInfo,
    optional_deps: &[String],
    enabled_optional_deps: &HashSet<String>,
    non_minimalizable_features: &HashSet<String>,
    visited: &mut HashSet<String>,
) -> bool {
    if !visited.insert(feat_name.to_string()) {
        return true; // cycle-safe
    }
    if non_minimalizable_features.contains(feat_name) {
        return false;
    }
    let Some(values) = crate_info
        .features
        .iter()
        .find(|(name, _)| name == feat_name)
        .map(|(_, v)| v)
    else {
        return false;
    };
    if values.is_empty() {
        return false;
    }
    for (k, v) in values {
        let is_dep_enabler = optional_deps.contains(k) && (v == "dep:" || v.as_str() == k.as_str());
        if is_dep_enabler {
            // Handled by the existing dep-enabling loop; don't overlap.
            return false;
        }
        let is_dep_feat = optional_deps.contains(k) && v != "dep:" && v.as_str() != k.as_str();
        if is_dep_feat {
            if enabled_optional_deps.contains(k) {
                return false; // dep is present — this configuration is active
            }
            continue; // dep not enabled — this entry is inert, droppable
        }
        if k == v {
            // Pure feature reference — recurse.
            if !all_subtree_dep_feat_only(
                k,
                crate_info,
                optional_deps,
                enabled_optional_deps,
                non_minimalizable_features,
                visited,
            ) {
                return false;
            }
        } else {
            return false;
        }
    }
    true
}

/// that *directly* has the dep reference for `dep_name` in its value list.
fn find_direct_dep_enabler(
    feat_name: &str,
    dep_name: &str,
    crate_info: &CrateInfo,
    visited: &mut HashSet<String>,
) -> Option<String> {
    if !visited.insert(feat_name.to_string()) {
        return None;
    }
    let values = crate_info
        .features
        .iter()
        .find(|(name, _)| name == feat_name)
        .map(|(_, v)| v)?;

    if dep_entry_string_in_toml(dep_name, values).is_some() {
        return Some(feat_name.to_string());
    }

    for (k, v) in values {
        if k == v
            && let Some(found) = find_direct_dep_enabler(k, dep_name, crate_info, visited)
        {
            return Some(found);
        }
    }
    None
}

/// Returns `true` if `feat_name` has any value that is NOT an optional-dep reference
/// (i.e. the feature also enables other features or sub-crate features).
fn feat_has_non_dep_values(
    feat_name: &str,
    crate_info: &CrateInfo,
    optional_deps: &[String],
) -> bool {
    crate_info
        .features
        .iter()
        .find(|(name, _)| name == feat_name)
        .map(|(_, values)| {
            values.iter().any(|(k, v)| {
                let is_dep_ref =
                    optional_deps.contains(k) && (v == "dep:" || v.as_str() == k.as_str());
                !is_dep_ref
            })
        })
        .unwrap_or(false)
}

/// Removes `dep_entry` (e.g. `"dep:somedep"`) from `feat_name`'s array in the
/// `[features]` table of `main_toml`. Returns `true` if an entry was removed.
/// Adds to `handled` every `(dep_name, feat)` pair in `optional_dep_feats` whose
/// direct-enabler chain resolves to `leaf`. This covers both the leaf itself and
/// every feature that reaches the dep transitively through `leaf`.
fn invalidate_through_leaf(
    dep_name: &str,
    leaf: &str,
    crate_info: &CrateInfo,
    optional_dep_feats: &TupleVec,
    handled: &mut Vec<(String, String)>,
) {
    for (d, f) in optional_dep_feats {
        if d != dep_name {
            continue;
        }
        if handled.contains(&(d.clone(), f.clone())) {
            continue;
        }
        let mut vis = HashSet::new();
        if find_direct_dep_enabler(f, dep_name, crate_info, &mut vis)
            .map(|l| l == leaf)
            .unwrap_or(false)
        {
            handled.push((d.clone(), f.clone()));
        }
    }
}

fn remove_dep_from_toml_feature(
    main_toml: &mut toml::Value,
    feat_name: &str,
    dep_entry: &str,
) -> bool {
    main_toml
        .get_mut("features")
        .and_then(|f| f.as_table_mut())
        .and_then(|t| t.get_mut(feat_name))
        .and_then(|f| f.as_array_mut())
        .map(|arr| {
            let before = arr.len();
            arr.retain(|v| v.as_str() != Some(dep_entry));
            arr.len() < before
        })
        .unwrap_or(false)
}

/// If there are features that got enabled, but are the only reason an optional
/// dependency is included, we can drop those features from the main crate's
/// feature list.
/// # Arguments
/// * `crate_info` - The crate info of the crate being minimized
/// * `optional_dep_feats` - The list of (dep, feature) pairs from `features_for_optional_deps`
/// * `enable` - The list of features to enable; modified in place
/// * `non_minimalizable_features` - Features that must stay in `enable`
/// * `disable_default` - Whether Cargo's `default` feature is disabled; if `false`,
///   `"default"` is also analyzed for optional-dep enabling
/// * `crate_name` - The name-with-version of the crate whose Cargo.toml to modify
/// * `main_name` - When minimizing a dep crate, the name-with-version of the main
///   crate (needed to locate the dep's manifest); `None` when minimizing the main crate
pub fn minimize(
    crate_info: &CrateInfo,
    optional_dep_feats: &mut TupleVec,
    enable: &mut Vec<String>,
    non_minimalizable_features: &HashSet<String>,
    disable_default: bool,
    crate_name: &str,
    main_name: Option<&str>,
    enabled_optional_deps: Option<&HashSet<String>>,
) {
    debug!(
        "Non-minimalizable features for crate '{}': {:?}",
        crate_name, non_minimalizable_features
    );

    let optional_deps: Vec<String> = crate_info
        .deps_and_features
        .iter()
        .filter(|(dep, _)| dep.optional)
        .map(|(dep, _)| dep.name.clone())
        .collect();

    // Build the analysis set: explicitly enabled features + "default" if implicitly active
    // and the crate actually defines a default feature.
    let mut to_analyze: Vec<String> = enable.clone();
    let default_is_defined = crate_info
        .features
        .iter()
        .any(|(name, _)| name == "default");
    if !disable_default && default_is_defined && !to_analyze.contains(&"default".to_string()) {
        to_analyze.push("default".to_string());
    }

    let manifest = determine_manifest_file(crate_name, main_name);
    let mut main_toml: toml::Value =
        toml::from_str(&fs::read_to_string(&manifest).unwrap()).unwrap();
    let mut toml_modified = false;
    let mut custom_disabled: Vec<String> = Vec::new();
    let mut to_drop: HashSet<String> = HashSet::new();
    // (dep_name, feat_name) pairs successfully handled — removed from optional_dep_feats
    // at the end so should_skip_dep sees the updated state.
    let mut handled: Vec<(String, String)> = Vec::new();

    for feat_name in &to_analyze {
        println!(
            "\n[minimize] Analyzing feature '{}' for potential removal from enable list...",
            feat_name
        );
        // Collect optional deps that this feature (transitively) enables.
        let enabled_deps: Vec<String> = optional_dep_feats
            .iter()
            .filter(|(_, f)| f == feat_name)
            .map(|(dep, _)| dep.clone())
            .collect();

        for dep_name in enabled_deps {
            let mut visited = HashSet::new();
            let leaf = find_direct_dep_enabler(feat_name, &dep_name, crate_info, &mut visited)
                .unwrap_or_else(|| feat_name.to_string());

            let dep_entry = crate_info
                .features
                .iter()
                .find(|(name, _)| name.as_str() == leaf.as_str())
                .and_then(|(_, values)| dep_entry_string_in_toml(&dep_name, values));

            let is_direct = leaf == *feat_name;
            let leaf_only_dep_values = !feat_has_non_dep_values(&leaf, crate_info, &optional_deps);
            let can_drop =
                leaf_only_dep_values && !non_minimalizable_features.contains(leaf.as_str());

            let subtree_deps_only = enable.contains(feat_name)
                && all_subtree_deps_only(
                    feat_name,
                    crate_info,
                    &optional_deps,
                    non_minimalizable_features,
                    &mut HashSet::new(),
                );

            println!(
                "[minimize] Checking if feature '{}' can be dropped for dep '{}': is_direct={}, leaf='{}', leaf_only_dep_values={}, can_drop={}, subtree_deps_only={}, non_minimalizable={}",
                feat_name,
                dep_name,
                is_direct,
                leaf,
                leaf_only_dep_values,
                can_drop,
                subtree_deps_only,
                non_minimalizable_features.contains(leaf.as_str())
            );

            if subtree_deps_only {
                // Every branch of feat_name's subtree only enables optional deps — drop it.
                println!(
                    "[minimize] DROP feature '{}': entire subtree only enables optional deps (dep='{}')",
                    feat_name, dep_name
                );
                to_drop.insert(feat_name.clone());
                // Invalidate every pair whose chain goes through this leaf.
                invalidate_through_leaf(
                    &dep_name,
                    &leaf,
                    crate_info,
                    optional_dep_feats,
                    &mut handled,
                );
            } else if is_direct
                && leaf_only_dep_values
                && non_minimalizable_features.contains(leaf.as_str())
            {
                // The feature's sole purpose is enabling this dep, AND the feature itself must
                // stay enabled. Stripping dep:D from the feature would make it hollow while still
                // being required. Leave both the feature and the dep alone.
                debug!(
                    "[minimize] KEEP dep '{}' in feature '{}': feature is non-minimalizable and only exists to enable this dep — dep is also required",
                    dep_name, leaf
                );
            } else if let Some(entry) = dep_entry {
                // Surgical removal is safe here because:
                // - transitive chain: the leaf feature can lose dep D while the higher-level
                //   feature continues to work for other reasons, OR
                // - leaf has other values: strip dep D, keep the rest, OR
                // - leaf_only_dep_values but non-minimalizable would be caught above already
                let reason = if !is_direct {
                    format!("transitive enabler (leaf='{leaf}', not direct feat '{feat_name}')")
                } else if !leaf_only_dep_values {
                    format!("leaf '{leaf}' has non-dep values — surgical removal of entry")
                } else {
                    format!("leaf '{leaf}' is non-minimalizable")
                };
                if remove_dep_from_toml_feature(&mut main_toml, &leaf, &entry) {
                    debug!(
                        "[minimize] MOVE entry '{}' from feature '{}' to custom-disabled (dep='{}', feat='{}', reason: {})",
                        entry, leaf, dep_name, feat_name, reason
                    );
                    custom_disabled.push(entry);
                    toml_modified = true;
                    // Invalidate every pair (dep, feat) whose chain goes through leaf,
                    // including the direct (dep, leaf) pair itself.
                    invalidate_through_leaf(
                        &dep_name,
                        &leaf,
                        crate_info,
                        optional_dep_feats,
                        &mut handled,
                    );
                }
            } else {
                debug!(
                    "[minimize] SKIP dep '{}' via feat '{}' (leaf='{}'): is_direct={}, can_drop={}, dep_entry=None",
                    dep_name, feat_name, leaf, is_direct, can_drop
                );
            }
        }
    }

    // Remove handled pairs so should_skip_dep sees the updated state.
    optional_dep_feats.retain(|pair| !handled.contains(pair));

    // Second pass: drop features whose entire subtree only enables dep/feat entries
    // for optional deps that were never included in the build.
    if let Some(enabled_deps) = enabled_optional_deps {
        for feat_name in &to_analyze {
            if to_drop.contains(feat_name) || !enable.contains(feat_name) {
                continue;
            }
            if all_subtree_dep_feat_only(
                feat_name,
                crate_info,
                &optional_deps,
                enabled_deps,
                non_minimalizable_features,
                &mut HashSet::new(),
            ) {
                println!(
                    "[minimize] DROP feature '{}': subtree only enables dep/feat for non-enabled optional deps",
                    feat_name
                );
                to_drop.insert(feat_name.clone());
            }
        }
    }

    if !to_drop.is_empty() {
        debug!(
            "[minimize] Dropping features from enable list: {:?}",
            to_drop
        );
    }
    if !custom_disabled.is_empty() {
        debug!(
            "[minimize] Entries moved to custom-disabled in Cargo.toml: {:?}",
            custom_disabled
        );
    }
    enable.retain(|f| !to_drop.contains(f));
    enable.sort();
    enable.dedup();

    if toml_modified {
        add_feats_to_custom_feature(
            &mut main_toml,
            consts::CUSTOM_FEATURES_DISABLED,
            &custom_disabled,
        );
        fs::write(&manifest, toml::to_string(&main_toml).unwrap()).unwrap();
    }
}

/// Performs the post-processing for a dependency after `(enable, disable)` features are known,
/// whether from the DB or computed via `process_crate`. Calls `final_feature_list_dep`,
/// optionally updates the main crate's default feature list, and formats the disable vector
/// with the dep name prefix.
pub fn finalize_dep_crate(
    exchange: &mut DataExchange,
    dep: &Attributes,
    enable: Vec<String>,
    disable: Vec<String>,
    feature_to_items: HashMap<String, HashSet<String>>,
) -> Result<TripleTupleVecString, anyhow::Error> {
    debug!(
        "Dependency {}: enable: {:?}, disable: {:?}",
        dep.crate_name, enable, disable
    );

    let dep_original_name = dep.crate_name.split(":").next().unwrap_or("").to_string();

    let dep_crate_name_norm = exchange
        .crate_name_rename
        .iter()
        .find(|(_, pkg)| pkg.as_str() == dep_original_name.as_str())
        .map(|(cname, _)| cname.replace('-', "_"))
        .unwrap_or_else(|| dep_original_name.replace('-', "_"));

    // Print all main items that reference this dep.
    let main_items_for_dep: Vec<&String> = exchange
        .valid_cross_crate_items
        .iter()
        .filter(|(cname, _)| cname == &dep_crate_name_norm)
        .map(|(_, iname)| iname)
        .collect();
    {
        let mut sorted = main_items_for_dep
            .iter()
            .map(|s| s.as_str())
            .collect::<Vec<_>>();
        sorted.sort();
        println!(
            "[finalize] dep={} (norm={}) — main uses {} items from this dep: {:?}",
            dep.crate_name,
            dep_crate_name_norm,
            sorted.len(),
            sorted
        );
    }

    // Determine which features in the disable list must be protected because:
    // 1. Main uses items from this dep that require the feature, OR
    // 2. A main [features] entry in the enable list references dep/feat.
    let protected: HashSet<String> = disable
        .iter()
        .filter(|feat| {
            // Check 1: items usage
            if let Some(gated_items) = feature_to_items.get(*feat) {
                let has_glob = gated_items.contains("*");
                let matching_items: Vec<&String> = gated_items
                    .iter()
                    .filter(|iname| {
                        exchange
                            .valid_cross_crate_items
                            .contains(&(dep_crate_name_norm.clone(), (*iname).clone()))
                    })
                    .collect();
                println!(
                    "[finalize]   feature='{}' gates {} items — main uses {:?}{}",
                    feat,
                    gated_items.len(),
                    matching_items,
                    if has_glob { " (+ glob *)" } else { "" }
                );
                if has_glob || !matching_items.is_empty() {
                    println!(
                        "[finalize]   => PROTECT '{}' (items check: {:?})",
                        feat, matching_items
                    );
                    return true;
                }
            } else {
                println!(
                    "[finalize]   feature='{}' not in feature_to_items (no gated items known)",
                    feat
                );
            }
            // Check 2: features table
            let protected_by_table =
                exchange
                    .crate_info
                    .features
                    .iter()
                    .any(|(main_feat, tuples)| {
                        exchange.main_enable.contains(main_feat)
                            && tuples.iter().any(|(d, f)| {
                                d.replace('-', "_") == dep_crate_name_norm && f == *feat
                            })
                    });
            if protected_by_table {
                println!("[finalize]   => PROTECT '{}' (features table check)", feat);
            } else {
                println!("[finalize]   => ALLOW REMOVAL of '{}'", feat);
            }
            protected_by_table
        })
        .cloned()
        .collect();

    println!(
        "[finalize] dep={} protected features: {:?}, will remove: {:?}",
        dep.crate_name,
        {
            let mut v: Vec<_> = protected.iter().collect();
            v.sort();
            v
        },
        {
            let mut v: Vec<_> = disable.iter().filter(|f| !protected.contains(*f)).collect();
            v.sort();
            v
        }
    );

    debug!(
        "Dependency {}: protected features (not removing): {:?}",
        dep.crate_name, protected
    );

    for feat in &protected {
        exchange
            .protected_dep_features
            .insert((dep_crate_name_norm.clone(), feat.clone()));
    }

    let filtered_disable: Vec<String> = disable
        .iter()
        .filter(|f| !protected.contains(*f))
        .cloned()
        .collect();

    let (args, update_default_config) = solver::final_feature_list_dep(
        &exchange.crate_info,
        &dep_original_name,
        &enable,
        &filtered_disable,
        &exchange.crate_name_rename,
        &mut exchange.telemetry,
    );

    debug!(
        "Dependency requires default config update: {}",
        update_default_config
    );

    if update_default_config {
        update_main_crate_default_list(
            &exchange.name_with_version,
            &dep.crate_name,
            &exchange.crate_name_rename,
        );
        exchange
            .telemetry
            .default_true_unset_deps
            .push((dep.crate_name.clone(), true));
    } else {
        exchange
            .telemetry
            .default_true_unset_deps
            .push((dep.crate_name.clone(), false));
    }

    debug!(
        "Final arguments for dependency {}: {:?}",
        dep.crate_name, args
    );

    let dep_name = exchange
        .crate_name_rename
        .iter()
        .find(|(_, name)| *name == dep_original_name)
        .map(|(renamed, _)| renamed)
        .unwrap_or(&dep_original_name);

    let formatted_disable: Vec<String> = disable
        .iter()
        .map(|f| format!("{}/{}", dep_name, f))
        .collect();

    Ok((args, formatted_disable, enable))
}

/// Process the dependency crate (non-DB path).
/// Gathers crate info, runs the AST/HIR analysis, solves for features, then delegates
/// post-processing to `finalize_dep_crate`.
/// Callers are responsible for checking the DB before invoking this — see
/// `process_dep_crate_wrapper` in `main.rs`.
pub fn process_dep_crate(
    exchange: &mut DataExchange,
    dep: &mut Attributes,
) -> Result<TripleTupleVecString, anyhow::Error> {
    let (.., dep_crate_info) =
        downloader::gather_crate_info(&dep.crate_name, true, Some(&exchange.name_with_version))?;
    let mut optional_dep_feats = features_for_optional_deps(&dep_crate_info);
    let dep_crate_name = dep.crate_name.clone();
    let ctx = z3::Context::new(&z3::Config::new());
    let (_hard_std, hard_constraints, _, _, dep_root, _) = driver::analyze_crate_wrapper(
        &ctx,
        &dep.crate_name,
        Some(&exchange.name_with_version),
        &mut exchange.telemetry,
    );
    let (enable, disable) = process_crate(
        exchange,
        &ctx,
        dep,
        Some(&dep_crate_name),
        Some(&dep_crate_info),
        false,
        &mut optional_dep_feats,
        hard_constraints,
    )?;

    // Build feature→items map from the dep's tree while dep ctx is live.
    // dep_root's Z3 Bools are tied to `ctx` — must not outlive this scope.
    let named = crate::visitor::collect_named_items_with_conditions(&dep_root, &ctx);

    println!(
        "[dep_tree] All named items collected from dep {} tree ({} items):",
        dep.crate_name,
        named.len()
    );
    let mut named_sorted: Vec<_> = named.iter().map(|(n, _)| n.as_str()).collect();
    named_sorted.sort();
    named_sorted.dedup();
    for name in &named_sorted {
        println!("  {}", name);
    }

    let feature_to_items: HashMap<String, HashSet<String>> = disable
        .iter()
        .map(|feat| {
            let f_var = z3::ast::Bool::new_const(&ctx, feat.as_str());
            let gated: HashSet<String> = named
                .iter()
                .filter(|(_, cond)| {
                    let s = z3::Solver::new(&ctx);
                    s.assert(cond);
                    s.assert(&f_var.not());
                    s.check() == z3::SatResult::Unsat
                })
                .map(|(name, _)| name.clone())
                .collect();
            (feat.clone(), gated)
        })
        .collect();

    println!(
        "[dep_tree] Feature→items map for dep {} (features in disable list):",
        dep.crate_name
    );
    let mut feats_sorted: Vec<_> = feature_to_items.keys().collect();
    feats_sorted.sort();
    for feat in feats_sorted {
        let mut items: Vec<_> = feature_to_items[feat].iter().collect();
        items.sort();
        println!(
            "  feature='{}' gates {} items: {:?}",
            feat,
            items.len(),
            items
        );
    }

    finalize_dep_crate(exchange, dep, enable, disable, feature_to_items)
}

/// Sometimes main might enable a feature that enables a dependency feature
/// that is not required for no_std build and can cause build failure.
/// If such a feature exists in a main feature which is not necessary
/// for the main, it is dropped from the enabled features of main crate.
/// If the feature is part of a fixed feature list, it is moved to a
/// custom feature list called `dep-unnecessary-features`.
/// # Arguments
/// * `main_name` - The name of the main crate
/// * `fixed_main_args` - The fixed features of the main crate
/// * `flexible_main_args` - The list of features whihc are not necessary for main
/// * `dep_name` - The name of the dependency with the version
/// * `deps_args` - The features required for the dependency. This is the list of features
///   that are enabled for a dependency.
pub fn move_unnecessary_dep_feats(
    main_name: &str,
    fixed_main_args: &mut Vec<String>,
    flexible_main_args: &mut Vec<String>,
    dep_name: &str,
    deps_args: &[String],
    telemetry: &mut Telemetry,
    disable_default: bool,
    protected_dep_features: &std::collections::HashSet<(String, String)>,
) {
    let main_manifest = determine_manifest_file(main_name, None);
    let mut main_toml: toml::Value =
        toml::from_str(&fs::read_to_string(&main_manifest).unwrap()).unwrap();
    let main_features = main_toml.get_mut("features").and_then(|f| f.as_table_mut());

    let dep_name_only = dep_name.split(':').next().unwrap_or(dep_name);

    if main_features.is_none() {
        debug!("No features found for main crate {}", main_name);
        return;
    }
    let main_features = main_features.unwrap();

    let mut default_added = false;

    if !disable_default && !fixed_main_args.contains(&"default".to_string()) {
        fixed_main_args.push("default".to_string());
        default_added = true;
    }

    // List of features that are indirectly enabled and are not part of either the
    // fixed or flexible main args.
    let mut indirect_flexible_args: HashSet<String> = HashSet::new();
    let mut worklist: HashSet<String> = HashSet::from_iter(
        fixed_main_args
            .iter()
            .chain(flexible_main_args.iter())
            .cloned(),
    );

    while let Some(feature) = worklist.iter().next().cloned() {
        worklist.remove(&feature);

        let Some(arr) = main_features.get(&feature).and_then(|f| f.as_array()) else {
            debug!(
                "Feature {} in main crate {} does not have an array in Cargo.toml",
                feature, main_name
            );
            continue;
        };

        for f in arr {
            let Some(s) = f.as_str() else { continue };
            if main_features.contains_key(s)
                && !fixed_main_args.contains(&s.to_string())
                && !flexible_main_args.contains(&s.to_string())
            {
                indirect_flexible_args.insert(s.to_string());
                worklist.insert(s.to_string());
            }
        }
    }

    let prefix1 = format!("{}/", dep_name_only);
    let prefix2 = format!("{}?/", dep_name_only);

    // Let's say main has a feature that enables multiple dependency features,
    // and only some of those features are not required for a dependency. In this
    // case, we track the ones which are required so we can add it to
    // a custom feature list later. Note that we only drop it if there is atleast
    // one feature that has to be disabled.
    let mut needed_dropped: HashSet<String> = HashSet::new();
    flexible_main_args.retain(|feature| {
        main_features
            .get_mut(feature)
            .and_then(|f| f.as_array_mut())
            .is_some_and(|arr| {
                let mut has_mismatch = false;
                let mut local_needed_dropped: HashSet<String> = HashSet::new();
                for f in arr.iter().filter_map(|v| v.as_str()) {
                    if f.starts_with(&prefix1) || f.starts_with(&prefix2) {
                        let key = extract_key(f);
                        if deps_args.contains(&key.to_string()) {
                            local_needed_dropped.insert(f.to_string());
                        } else {
                            has_mismatch = true;
                        }
                    }
                }
                if has_mismatch {
                    needed_dropped.extend(local_needed_dropped);
                }
                !has_mismatch
            })
    });

    let dep_norm = dep_name_only.replace('-', "_");
    let mut removed = HashSet::new();
    for feature in fixed_main_args.iter() {
        if let Some(arr) = main_features
            .get_mut(feature)
            .and_then(|f| f.as_array_mut())
        {
            arr.retain(|v| {
                if let Some(s) = v.as_str()
                    && (s.starts_with(&prefix1) || s.starts_with(&prefix2))
                {
                    let key = extract_key(s);
                    if !deps_args.contains(&key.to_string()) {
                        if protected_dep_features.contains(&(dep_norm.clone(), key.to_string())) {
                            return true;
                        }
                        debug!("Removing unnecessary feature {} from main crate", s);
                        removed.insert(s.to_string());
                        return false;
                    }
                }
                true
            });
        }
    }

    for enabled_feat in indirect_flexible_args {
        if let Some(arr) = main_features
            .get_mut(&enabled_feat)
            .and_then(|f| f.as_array_mut())
        {
            arr.retain(|v| {
                if let Some(s) = v.as_str()
                    && (s.starts_with(&prefix1) || s.starts_with(&prefix2))
                {
                    let key = extract_key(s);
                    if !deps_args.contains(&key.to_string()) {
                        if protected_dep_features.contains(&(dep_norm.clone(), key.to_string())) {
                            return true;
                        }
                        removed.insert(s.to_string());
                        return false;
                    }
                }
                true
            });
        }
    }

    if !removed.is_empty() {
        telemetry
            .unnecessary_features_removed
            .push((dep_name.to_string(), true));
        telemetry
            .unnecessary_features_removed_list
            .push((dep_name.to_string(), removed.iter().cloned().collect()));
    } else {
        telemetry
            .unnecessary_features_removed
            .push((dep_name.to_string(), false));
    }

    if default_added {
        fixed_main_args.retain(|f| f != "default");
    }

    add_feats_to_custom_feature(
        &mut main_toml,
        consts::DEP_UNNECESSARY_FEATURES,
        &removed.iter().cloned().collect::<Vec<_>>(),
    );

    add_feats_to_custom_feature(
        &mut main_toml,
        consts::CUSTOM_FEATURES_ENABLED,
        &needed_dropped.iter().cloned().collect::<Vec<_>>(),
    );

    fs::write(
        &main_manifest,
        toml::to_string(&main_toml)
            .context("Failed convert Value to string")
            .unwrap(),
    )
    .unwrap();
}

/// Recursively determine if dependencies at a certain depth
/// support no_std.
/// # Arguments
/// * `initlist` - The initial list of dependencies to check (depth 0)
/// * `depth` - The maximum depth to check
/// * `current_depth` - The current depth in the recursion
/// * `visited` - A set to keep track of visited dependencies
/// * `ctx` - The Z3 context
/// # Returns
/// The maximum depth tested for no_std support. This can be less than
/// the requested depth if there are no more dependencies to check or
/// if a dependency does not support no_std.
pub fn determine_n_depth_dep_no_std(
    initlist: TupleVec,
    depth: u32,
    current_depth: u32,
    visited: &mut HashSet<(String, String)>,
    ctx: &z3::Context,
    main_name: &str,
    fail_on_nostd: bool,
) -> (bool, u32) {
    let mut local_initlist = Vec::new();
    if current_depth >= depth || initlist.is_empty() {
        return (true, current_depth);
    }
    for (name, version) in initlist {
        if !visited.insert((name.clone(), version.clone())) {
            debug!("Already visited dependency {}:{}", name, version);
            continue;
        }
        let names_and_versions =
            downloader::read_dep_names_and_versions(&name, &version, true, main_name)
                .expect("Failed to read dependency names and versions");
        for (dep_name, dep_version) in names_and_versions {
            debug!(
                "Processing dependency {}:{} for no_std",
                dep_name, dep_version
            );
            let name_with_version = match downloader::clone_from_crates(
                &dep_name,
                Some(&dep_version),
                Some(main_name),
                Some(&format!("{}:{}", &name, &version)),
            ) {
                Ok(name_with_version) => name_with_version,
                Err(e) => {
                    debug!("Failed to download crate: {}", e);
                    continue;
                }
            };

            if is_proc_macro(&name_with_version, Some(main_name)) {
                debug!("{} is a proc-macro, skipping", name_with_version);
                continue;
            }

            let (name_inner, version) = name_with_version
                .split_once(':')
                .unwrap_or((&name_with_version, ""));

            if fail_on_nostd && !check_for_no_std(&name_with_version, ctx, None, Some(main_name)) {
                debug!(
                    "ERROR: Dependency {} of dependency {} does not support no_std build at depth {}",
                    name_with_version, name, current_depth
                );
                return (false, current_depth);
            }

            local_initlist.push((name_inner.to_string(), version.to_string()));
        }
    }

    determine_n_depth_dep_no_std(
        local_initlist,
        depth,
        current_depth + 1,
        visited,
        ctx,
        main_name,
        fail_on_nostd,
    )
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
            (equation, parsed) = parse_meta_for_cfg_attr(&attr.meta, ctx);
            if is_no_std(&parsed, false) {
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
    parse_meta_for_cfg_attr(&attr.meta, ctx)
}

/// Collect all feature names mentioned across all compile_error attributes.
/// Used by callers that cannot access the private `compile_error_attrs` field directly.
pub fn compile_error_feature_names(attrs: &Attributes, ctx: &z3::Context) -> HashSet<String> {
    attrs
        .compile_error_attrs
        .iter()
        .flat_map(|attr| {
            let (_, parsed) = parse_main_attributes_direct(attr, ctx);
            parsed.features
        })
        .collect()
}

/// Parse the attributes of a dependency crate.
/// This does not need to verify if the crate is no_std or not.
/// # Arguments
/// * `attrs` - The attributes of the dependency crate
/// * `ctx` - The Z3 context
/// # Returns
/// A tuple containing the equations for the dependency
/// crate and the parsed attributes.
pub fn parse_attributes<'a>(attrs: &Attributes, ctx: &'a z3::Context) -> Vec<Option<Bool<'a>>> {
    let mut equation: Vec<Option<Bool>> = Vec::new();
    let mut temp_eq: Option<Bool>;
    let mut parsed: ParsedAttr;
    for attr in &attrs.attributes {
        let ident = attr.path().get_ident().unwrap();
        if ident == "cfg" {
            (temp_eq, parsed) = parse_meta_for_cfg_attr(&attr.meta, ctx);
            // TODO: Should this check be removed?
            if parsed.features.len() == 1 || parsed.logic.is_empty() {
                // Attributes like `#[cfg (feature = "serde")]` are not interesting.
                continue;
            }
            equation.push(temp_eq);
        }
    }

    equation
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

    for e in equations.iter().flatten() {
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
pub fn determine_manifest_file(name_with_version: &str, main_name: Option<&str>) -> String {
    let mut dir = PathBuf::from(consts::DOWNLOAD_PATH);

    if let Some(main_name) = main_name {
        debug!(
            "Determining manifest file for dependency {} of main crate {}",
            name_with_version, main_name
        );
        dir = dir.join(format!("{}_deps", main_name.replace(':', "-")));
    }

    dir = dir.join(name_with_version.replace(':', "-"));

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
    if let Some(table) = toml.as_table_mut()
        && table.contains_key(key)
    {
        debug!("{} found in Cargo.toml, removing it", key);

        if key == "target" {
            let target_table = table
                .get_mut(key)
                .and_then(toml::Value::as_table_mut)
                .unwrap();

            for (inner_key, inner_value) in target_table.iter_mut() {
                if let toml::Value::Table(inner_table) = inner_value {
                    let to_remove: Vec<String> = inner_table
                        .keys()
                        .filter(|&k| k != "dependencies")
                        .cloned()
                        .collect();

                    for k in to_remove {
                        inner_table.remove(&k);
                        debug!("Removed {} from {}.{}", k, key, inner_key);
                    }
                }
            }
        } else {
            table.remove(key);
        }
        fs::write(
            filename,
            toml::to_string(&toml).context("Failed to write Cargo.toml")?,
        )
        .context("Failed to write Cargo.toml")?;
        debug!("Removed {} from Cargo.toml", key);
    }
    Ok(())
}

/// For all features that refer to the `key` table,
/// remove them from the features list.
/// This is to prevent errors when we remove the `key` table
/// from the Cargo.toml file.
/// # Arguments
/// * `key` - The key of the table to remove features for
/// * `toml` - The TOML value to modify
/// * `filename` - The path to the Cargo.toml file
/// * `common` - A list of deps that appear in both dependencies and dev-dependencies
/// # Returns
/// A Result indicating success or failure.
/// This will also write the modified TOML back to the file.
pub fn remove_features_of_deps(
    key: &str,
    toml: &mut toml::Value,
    filename: &str,
    common: &[String],
) -> Result<(), anyhow::Error> {
    let table = match toml.get(key).and_then(toml::Value::as_table) {
        Some(table) => table.clone(),
        None => {
            debug!("No table found for key: {} in Cargo.toml", key);
            return Ok(());
        }
    };

    let features = match toml.get_mut("features").and_then(toml::Value::as_table_mut) {
        Some(features) => features,
        None => {
            debug!("No features table found in Cargo.toml");
            return Ok(());
        }
    };

    let mut dep_names: Vec<String> = Vec::new();

    if key == "target" {
        table.iter().for_each(|(_, value)| {
            if let toml::Value::Table(table) = value {
                for dep_type in ["dev-dependencies", "build-dependencies"] {
                    if let Some(inner_deps) = table.get(dep_type).and_then(toml::Value::as_table) {
                        for (dep_name, _) in inner_deps.iter() {
                            debug!("Found dependency: {}", dep_name);
                            let dep = dep_name.clone();
                            if !dep_names.contains(&dep) {
                                dep_names.push(dep);
                            }
                        }
                    }
                }
            }
        });
    } else {
        table.iter().for_each(|(dep_name, _)| {
            dep_names.push(dep_name.clone());
        });
    }

    dep_names.retain(|d| !common.contains(d));

    debug!("Removing features for key: {}", key);
    for dep_name in dep_names {
        debug!("Removing features for dependency: {}", dep_name);
        let prefix1 = format!("{}/", dep_name);
        let prefix2 = format!("{}?/", dep_name);
        let prefix3 = format!("dep:{}", dep_name);
        for (_, feature_value) in features.iter_mut() {
            if let toml::Value::Array(arr) = feature_value {
                arr.retain(|f| {
                    if let toml::Value::String(s) = f
                        && (s.starts_with(&prefix1)
                            || s.starts_with(&prefix2)
                            || s.as_str() == dep_name
                            || s.ends_with(&prefix3))
                    {
                        debug!("Removing {} from features", s);
                        return false;
                    }
                    true
                });
            }
        }
    }

    fs::write(
        filename,
        toml::to_string(toml).context("Failed to serialize Cargo.toml")?,
    )
    .context("Failed to write Cargo.toml")?;

    Ok(())
}

/// Check if the Cargo.toml file has a binary target.
/// # Arguments
/// * `filename` - The path to the Cargo.toml file
/// # Returns
/// A boolean indicating whether the Cargo.toml file has a binary target.
pub fn toml_has_bin_target(filename: &str) -> bool {
    let toml_content = fs::read_to_string(filename).expect("Failed to read Cargo.toml");
    let toml: toml::Value = toml::from_str(&toml_content).expect("Failed to parse Cargo.toml");
    if let Some(table) = toml.get("bin")
        && table.is_table()
    {
        return true;
    }
    false
}

/// Given a crate name with version, check if it is a procedural macro.
/// # Arguments
/// * `crate_name` - The name of the crate with version
/// # Returns
/// A boolean indicating whether the crate is a procedural macro.
pub fn is_proc_macro(crate_name: &str, main_name: Option<&str>) -> bool {
    let manifest = determine_manifest_file(crate_name, main_name);
    let toml: toml::Value = toml::from_str(&fs::read_to_string(&manifest).unwrap()).unwrap();
    if let Some(lib) = toml.get("lib")
        && let Some(proc_macro) = lib.get("proc-macro")
    {
        return proc_macro.as_bool().unwrap_or(false);
    }
    false
}

/// Update the main crate's default features list
/// by adding the default features of the given dependency.
/// This function will also set the dependency to not have
/// default features set in the main crate's Cargo.toml.
/// The goal of this is to prevent main crate's functionality
/// from being affected by the dependency's default features
/// getting disabled.
/// This also implies that to compile the main crate in
/// non no_std mode, the new feature that got added should
/// always be enabled.
/// # Arguments
/// * `main` - The name of the main crate
/// * `dep` - The name of the dependency to add to the main crate's default features
/// * `crate_name_rename` - A list of names and their renames of crate names
/// # Returns
/// None
fn update_main_crate_default_list(main: &str, dep: &str, crate_name_rename: &[(String, String)]) {
    let main_manifest = determine_manifest_file(main, None);
    let dep_manifest = determine_manifest_file(dep, Some(main));
    let dep_name_original = dep.split(':').next().unwrap().to_string();
    let dep_name = crate_name_rename
        .iter()
        .find(|(_, name)| name == &dep_name_original)
        .map(|(renamed, _)| renamed)
        .unwrap_or(&dep_name_original);

    debug!(
        "Updating main crate default features list: {} with dependency: {}",
        main_manifest, dep_manifest
    );

    let mut main_toml: toml::Value =
        toml::from_str(&fs::read_to_string(&main_manifest).unwrap()).unwrap();
    let mut dep_toml: toml::Value =
        toml::from_str(&fs::read_to_string(&dep_manifest).unwrap()).unwrap();

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

    let dep_default_features: Vec<String> = dep_features
        .get("default")
        .and_then(|v| v.as_array())
        .map(|v| {
            v.iter()
                .filter_map(|f| f.as_str().map(|s| format!("{}/{}", dep_name, s)))
                .collect()
        })
        .unwrap_or_default();

    add_feats_to_custom_feature(
        &mut main_toml,
        consts::CUSTOM_FEATURES_DISABLED,
        &dep_default_features,
    );

    fs::write(
        &main_manifest,
        toml::to_string(&main_toml)
            .context("Failed convert Value to string")
            .unwrap(),
    )
    .unwrap();
}

/// Remove a given list of features from the declared features
/// of a dependency in the main crate's Cargo.toml.
/// This will also add the features to the custom feature list
/// in the main crate's Cargo.toml.
/// This function additionally adds given features to a new
/// custom feature list in the main crate's Cargo.toml
/// which is used during the no_std build.
/// # Arguments
/// * `main_name` - The name of the main crate
/// * `name` - The name of the dependency to remove features from
/// * `feats` - The list of features to remove from the dependency
/// * `crate_name_rename` - A list of names and their renames of crate names
/// # Returns
/// None
pub fn update_feat_lists(
    main_name: &str,
    dep_original_name: &String,
    feats_to_move: &[String],
    feats_to_add: &[String],
    crate_name_rename: &[(String, String)],
) {
    let main_manifest = determine_manifest_file(main_name, None);
    let mut main_toml: toml::Value =
        toml::from_str(&fs::read_to_string(&main_manifest).unwrap()).unwrap();

    let dep_name = crate_name_rename
        .iter()
        .find(|(_, name)| name == dep_original_name)
        .map(|(renamed, _)| renamed)
        .unwrap_or(dep_original_name);

    let dependency = match main_toml
        .get_mut("dependencies")
        .and_then(|v| v.as_table_mut())
        .and_then(|table| table.get_mut(dep_name))
        .expect("Failed to get dependency from main Cargo.toml")
        .as_table_mut()
    {
        Some(table) => table,
        None => {
            debug!("Dependency {} not found in main Cargo.toml", dep_name);
            &mut toml::map::Map::new()
        }
    };

    let declared_features = match dependency
        .get_mut("features")
        .and_then(|v| v.as_array_mut())
    {
        Some(features) => features,
        None => {
            debug!("No features array found for dependency {}", dep_name);
            &mut Vec::new()
        }
    };

    declared_features.retain(|f| {
        if let toml::Value::String(s) = f {
            !feats_to_move.contains(s)
        } else {
            true
        }
    });

    let formatted_feats_to_move: Vec<String> = feats_to_move
        .iter()
        .map(|f| format!("{}/{}", dep_original_name, f))
        .collect();

    let formatted_feats_to_add: Vec<String> = feats_to_add
        .iter()
        .map(|f| format!("{}/{}", dep_original_name, f))
        .collect();

    add_feats_to_custom_feature(
        &mut main_toml,
        consts::CUSTOM_FEATURES_DISABLED,
        &formatted_feats_to_move,
    );
    add_feats_to_custom_feature(
        &mut main_toml,
        consts::CUSTOM_FEATURES_ENABLED,
        &formatted_feats_to_add,
    );

    fs::write(
        &main_manifest,
        toml::to_string(&main_toml)
            .context("Failed convert Value to string")
            .unwrap(),
    )
    .unwrap();
}

/// Sometime dependencies have features that should be disabled
/// to compile it in no_std mode. But these features maybe enabled
/// in the main crate's Cargo.toml.
/// This function will remove those features from the original feature from
/// main crate's Cargo.toml and add them to the custom feature list
/// which is used during std build.
/// # Arguments
/// * `main_name` - The name of the main crate
/// * `name` - The name of the dependency to remove features from
/// * `disable` - The list of features to disable from the dependency
/// # Returns
/// None
pub fn remove_conflicting_dep_feats(main_name: &str, name: &str, disable: &[String]) {
    let main_manifest = determine_manifest_file(main_name, None);
    let mut main_toml: toml::Value =
        toml::from_str(&fs::read_to_string(&main_manifest).unwrap()).unwrap();

    let features = main_toml
        .get_mut("features")
        .and_then(|v| v.as_table_mut())
        .expect("Failed to get features table from main Cargo.toml");

    for to_disable in disable {
        let to_remove = format!("{}/{}", name, to_disable);

        features
            .iter_mut()
            .filter_map(|(_, v)| v.as_array_mut())
            .for_each(|arr| {
                arr.retain(|f| {
                    if let toml::Value::String(s) = f
                        && s == &to_remove
                    {
                        debug!("Removing feature {} from main crate", to_remove);
                        return false;
                    }
                    true
                });
            });
    }

    let formatted_feats_to_move: Vec<String> =
        disable.iter().map(|f| format!("{}/{}", name, f)).collect();
    add_feats_to_custom_feature(
        &mut main_toml,
        consts::CUSTOM_FEATURES_DISABLED,
        &formatted_feats_to_move,
    );

    fs::write(
        &main_manifest,
        toml::to_string(&main_toml)
            .context("Failed convert Value to string")
            .unwrap(),
    )
    .unwrap();
    debug!(
        "Removed conflicting features from main crate: {}",
        main_name
    );
}

/// When a crate enables some features that inturn enable a dependency that does
/// not support no_std, we need to remove that feature from the main crate feature.
/// # Arguments
/// * `main_name` - The name of the main crate
/// * `feats` - The list of features in main crate that enable the dependency
/// * `to_drop` - The feature to drop from the main crate
/// # Returns
/// The names of the features an entry was actually removed from. Empty means the
/// manifest already had no link from `feats` to `to_drop` and was left untouched.
pub fn remove_feats_enabling_dep(
    main_name: &str,
    feats: &[String],
    to_drop: &String,
) -> Vec<String> {
    if feats.is_empty() {
        return Vec::new();
    }

    let main_manifest = determine_manifest_file(main_name, None);
    let mut main_toml: toml::Value =
        toml::from_str(&fs::read_to_string(&main_manifest).unwrap()).unwrap();

    let features = main_toml
        .get_mut("features")
        .and_then(|v| v.as_table_mut())
        .expect("Failed to get features table from main Cargo.toml");

    let formatted = format!("dep:{}", to_drop);
    let mut to_push: Vec<String> = Vec::new();
    let mut severed: Vec<String> = Vec::new();
    for feat in feats {
        if let Some(arr) = features.get_mut(feat).and_then(|v| v.as_array_mut()) {
            let before = arr.len();
            arr.retain(|f| {
                if let toml::Value::String(s) = f
                    && (s == to_drop || *s == formatted)
                {
                    // Record the entry exactly as it was written, so enabling the
                    // custom feature restores the original behaviour.
                    to_push.push(s.clone());
                    debug!("Removing feature {} from main crate", s);
                    return false;
                }
                true
            });
            if arr.len() != before {
                severed.push(feat.clone());
            }
        }
    }

    if to_push.is_empty() {
        debug!(
            "No features enabling {} found in main crate, leaving manifest untouched",
            to_drop
        );
        return severed;
    }

    add_feats_to_custom_feature(&mut main_toml, consts::CUSTOM_FEATURES_DISABLED, &to_push);

    fs::write(
        &main_manifest,
        toml::to_string(&main_toml)
            .context("Failed convert Value to string")
            .unwrap(),
    )
    .unwrap();
    severed
}

/// Get the actual directory of a crate given its name with version and the main crate name.
/// Since we use crate specific directories for dependencies of that crate, we use this
/// to construct the actual path to the crate's directory.
/// # Arguments
/// * `name_with_version` - The name of the crate with version (main or dependency)
/// * `main_name` - The name of the main crate, if the crate is a dependency
/// # Returns
/// The path to the crate's directory as a PathBuf.
pub fn get_actual_dir(name_with_version: &str, main_name: Option<&str>) -> PathBuf {
    let mut dir = PathBuf::from(consts::DOWNLOAD_PATH);

    if let Some(main_name) = main_name {
        dir = dir.join(format!("{}_deps", main_name.replace(':', "-")));
    }

    dir.join(name_with_version.replace(':', "-"))
}

/// Given a toml::Value representing the main Cargo.toml,
/// a feature name, and a list of features to add,
/// this function adds the features to the specified feature.
/// If the feature does not exist, it creates it.
/// If the feature already exists, it appends the new features to it.
/// # Arguments
/// * `main_toml` - The main Cargo.toml as a toml::Value
/// * `custom_feat` - The name of the custom feature to add to
/// * `feats_to_add` - A slice of features to add to the custom feature
/// # Returns
/// None
pub fn add_feats_to_custom_feature(
    main_toml: &mut toml::Value,
    custom_feat: &str,
    feats_to_add: &[String],
) {
    let main_features = main_toml
        .as_table_mut()
        .expect("Failed to get main Cargo.toml as table")
        .entry("features")
        .or_insert_with(|| toml::Value::Table(toml::map::Map::new()))
        .as_table_mut()
        .expect("Failed to get features table from main Cargo.toml");

    if let Some(custom) = main_features.get_mut(custom_feat) {
        if let toml::Value::Array(arr) = custom {
            for feat in feats_to_add {
                if !arr.contains(&toml::Value::String(feat.clone())) {
                    arr.push(toml::Value::String(feat.clone()));
                    debug!("Added feature {} to custom features", feat);
                }
            }
        } else {
            debug!("Custom features is not an array, skipping");
        }
    } else {
        main_features.insert(
            custom_feat.to_string(),
            toml::Value::Array(
                feats_to_add
                    .iter()
                    .cloned()
                    .map(toml::Value::String)
                    .collect(),
            ),
        );
        debug!("Added default features to main crate features");
        println!(
            "WARNING: To use the main crate in non no_std mode, you need to enable the feature `{}`",
            custom_feat
        );
    }
}

/// Re-read the main crate's `[features]` table from its manifest into
/// `exchange.crate_info.features`.
///
/// `crate_info.features` is populated once at download time, but the manifest is
/// rewritten repeatedly while features are being solved. Any caller that mutates
/// the manifest must call this afterwards, otherwise later passes make decisions
/// against the crate as it was downloaded rather than as it now stands.
///
/// The synthetic features this tool adds are filtered out: they are build knobs
/// for the generated manifest, not part of the crate's real feature graph, and
/// letting them back in would re-link optional deps that were just severed.
/// # Arguments
/// * `exchange` - The `DataExchange` whose `crate_info` should be refreshed
/// # Returns
/// None
pub fn refresh_crate_features(exchange: &mut DataExchange) {
    let main_name = format!(
        "{}:{}",
        exchange.crate_info.name, exchange.crate_info.version
    );
    let main_manifest = determine_manifest_file(&main_name, None);
    let main_toml: toml::Value = match fs::read_to_string(&main_manifest)
        .ok()
        .and_then(|s| toml::from_str(&s).ok())
    {
        Some(toml) => toml,
        None => {
            debug!(
                "Could not re-read manifest {} to refresh features",
                main_manifest
            );
            return;
        }
    };

    exchange.crate_info.features = downloader::read_local_features(&main_toml)
        .into_iter()
        .filter(|(name, _)| {
            !matches!(
                name.as_str(),
                consts::CUSTOM_FEATURES_DISABLED
                    | consts::CUSTOM_FEATURES_ENABLED
                    | consts::DEP_UNNECESSARY_FEATURES
            )
        })
        .collect();
}

/// Given a `CrateInfo`, this function finds all optional dependencies
/// and their features that are required to enable them.
/// It returns a vector of tuples where each tuple contains the dependency name
/// and the feature name.
/// # Arguments
/// * `crate_info` - The `CrateInfo` containing dependencies and features.
/// # Returns
/// A vector of tuples, each containing the dependency name and the feature name.
pub fn features_for_optional_deps(crate_info: &CrateInfo) -> TupleVec {
    let optional_deps: Vec<String> = crate_info
        .deps_and_features
        .iter()
        .filter(|(dep, _)| dep.optional)
        .map(|(dep, _)| dep.name.clone())
        .collect();

    let mut result: TupleVec = Vec::new();

    for (feat_name, _) in &crate_info.features {
        let mut all_enabled = vec![feat_name.clone()];
        solver::all_enabled_for_feat(&mut all_enabled, crate_info);

        for dep_name in &optional_deps {
            // `dep:depname` is encoded as "{dep_name}/dep:" in the expanded set.
            let explicit = all_enabled.contains(&format!("{}/dep:", dep_name));
            // `depname` (bare, enabling the implicit Cargo feature) is encoded as
            // dep_name itself. Skip index 0 to avoid a false positive when
            // feat_name == dep_name but the user overrode that feature to not enable the dep.
            let implicit = all_enabled[1..].contains(dep_name);
            if explicit || implicit {
                result.push((dep_name.clone(), feat_name.clone()));
            }
        }
    }

    result.sort();
    result.dedup();
    result
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
///   features that enable them.
/// * `enable_features` - A slice of features that are enabled in the main crate.
/// * `disable_default` - A boolean indicating whether the default features are disabled.
/// * `telemetry` - A mutable reference to the `Telemetry` struct for logging purposes.
/// * `second_round` - A boolean indicating if this is the second set of calls made to
///   this function.
/// # Returns
/// A boolean indicating whether the dependency should be skipped.
pub fn should_skip_dep(
    name: &str,
    exchange: &mut DataExchange,
    deps_and_features: &mut TupleVec,
    enable_features: &[String],
    disable_default: bool,
    second_round: bool,
) -> bool {
    if is_proc_macro(name, Some(&exchange.name_with_version)) {
        debug!("Dependency {} is a proc-macro, skipping", name);
        return true;
    }

    let dep_name = name.split(':').next().unwrap_or("").to_string();

    if !is_dep_optional(&exchange.crate_info, &dep_name) {
        debug!("Dependency {} is not optional, not skipping", name);
        return false;
    }

    let feats_of_dep: Vec<String> = deps_and_features
        .iter()
        .filter(|(dep, _)| dep == &dep_name)
        .map(|(_, feat)| feat.clone())
        .collect();

    let main_feats = &exchange.crate_info.features;
    let mut worklist: Vec<String> = enable_features.to_vec();
    if !disable_default {
        worklist.push("default".to_string());
    }
    // This is used to prevent going in circles.
    let mut all_enabled = HashSet::new();
    all_enabled.extend(worklist.iter().cloned());

    let mut features_for_dependency: Vec<String> = Vec::new();

    while let Some(item) = worklist.pop() {
        if feats_of_dep.contains(&item) {
            features_for_dependency.push(item.clone());
        }

        let enabled = main_feats
            .iter()
            .find(|(feat_name, _)| *feat_name == item)
            .map(|(_, dep_feats)| dep_feats);
        if let Some(dep_feats) = enabled {
            let possible: Vec<String> = dep_feats
                .iter()
                .filter(|(dep, feat)| feat == dep)
                .map(|(dep, _)| dep.clone())
                .collect();
            worklist.extend(
                possible
                    .iter()
                    .filter(|f| !all_enabled.contains(*f))
                    .cloned(),
            );
            all_enabled.extend(possible);
        }
    }

    if !features_for_dependency.is_empty() {
        let cfg = z3::Config::new();
        let ctx = z3::Context::new(&cfg);
        let found = check_for_no_std(name, &ctx, None, Some(&exchange.name_with_version));

        debug!(
            "Dependency: {} is enabled by features: {:?} and currently enabled list enabled {:?} from that list",
            dep_name, feats_of_dep, features_for_dependency
        );

        if !found {
            debug!(
                "Dependency {} does not support no_std. Creating a new feature and adding the conflicting features to it",
                dep_name
            );
            let main_name = &format!(
                "{}:{}",
                exchange.crate_info.name, exchange.crate_info.version
            );
            let severed = remove_feats_enabling_dep(main_name, &features_for_dependency, &dep_name);
            if !severed.is_empty() {
                // The manifest no longer links these features to this dep. Re-read it
                // and drop the matching pairs so a later round sees that, rather than
                // trying to sever a link that is already gone. Features that still
                // reach the dep stay in the list and remain re-checkable.
                refresh_crate_features(exchange);
                deps_and_features.retain(|(dep, feat)| dep != &dep_name || !severed.contains(feat));
                // Recorded on whichever round actually severed the dep: with the state
                // above kept in sync, only one round can reach this point per dep.
                exchange
                    .telemetry
                    .optional_deps_disabled
                    .push(dep_name.clone());
                exchange
                    .telemetry
                    .optional_deps_disabled_features_moved
                    .push((dep_name, features_for_dependency));
            }
            return true;
        } else {
            debug!("Dependency {} supports no_std", dep_name);
            if second_round {
                exchange
                    .telemetry
                    .optional_deps_enabled
                    .push(dep_name.clone());
                exchange
                    .telemetry
                    .optional_deps_enabled_features
                    .push((dep_name, features_for_dependency));
            }
            return false;
        }
    }
    // If the dependency is optional and not enabled by any feature,
    // we skip it.
    true
}

/// Check if a dependency is optional in the given `CrateInfo`.
/// This also checks the enabled features to determine if enabling that
/// feature caused an optional dependency to be included.
/// # Arguments
/// * `crate_info` - The `CrateInfo` containing the crate's dependencies and
///   features.
/// * `name` - The name of the dependency to check.
/// # Returns
/// A boolean indicating whether the dependency is optional.
pub fn is_dep_optional(crate_info: &CrateInfo, name: &str) -> bool {
    crate_info
        .deps_and_features
        .iter()
        .find(|(dep, _)| dep.name == name)
        .map(|(dep, _)| dep.optional)
        .unwrap_or(false)
}

/// This function checks at each level of dependencies, whether all its
/// dependencies have the required features set/have a way to set it.
/// If not, that means the crate author made a mistake and the crate cannot
/// be compiled in no_std even if it claims to be no_std.
/// # Arguments
/// * `crate_info` - The `CrateInfo` of the main crate.
/// * `db_data` - A slice of `DBData` containing database information.
/// # Returns
/// A boolean indicating whether all dependencies can satisfy their
/// no_std requirements.
/// Per-crate context cached as the dependency tree is walked: this crate's
/// own no_std solve result, plus the usage information needed to audit
/// *its* dependencies the same way `finalize_dep_crate` audits main's direct
/// dependencies, just read-only and one level removed.
struct DepUsageContext {
    /// Features this crate's own no_std solve determined it needs enabled.
    enable: Vec<String>,
    /// Features this crate's own no_std solve determined it does not need.
    disable: Vec<String>,
    /// (dep_norm_name, item_name) pairs this crate's own source actually
    /// references from each of its dependencies, restricted to call sites
    /// compatible with this crate's hard constraints. Mirrors
    /// `DataExchange::valid_cross_crate_items`, just computed for an
    /// arbitrary crate instead of only the main crate.
    valid_cross_crate_items: HashSet<(String, String)>,
    /// For each feature in this crate's own `disable` list, the named items
    /// in *this crate's own* source that become unreachable without it.
    feature_to_items: HashMap<String, HashSet<String>>,
}

pub fn recursive_dep_requirement_check(
    exchange: &mut DataExchange,
    depth: u32,
    top_level_deps: &[(String, String)],
    enabled_optional_deps: &std::collections::HashSet<String>,
) -> bool {
    exchange.telemetry.recursive_requirement_check_done = true;
    println!("Starting recursive dependency requirement check...");
    // Throwaway telemetry for the re-run analyze_crate_wrapper/process_crate calls below —
    // we don't want to duplicate their per-call stats into the main run's telemetry.
    let mut telemetry = Telemetry::default();

    let mut seen: HashSet<(String, String)> = HashSet::new();
    // Each entry carries its depth (1 = direct dep of main crate).
    let mut worklist: Vec<(String, String, u32)> = top_level_deps
        .iter()
        .map(|(n, v)| (n.clone(), v.clone(), 1u32))
        .collect();

    // For the crates that we already processed, save the requirements and usage
    // context so that we don't have to recompute them, and so a crate's context is
    // available later when it's revisited as the *parent* of its own dependencies.
    let mut dep_contexts: HashMap<String, DepUsageContext> = HashMap::new();
    // Seed with the main crate's own already-computed context.
    dep_contexts.insert(
        exchange.name_with_version.clone(),
        DepUsageContext {
            enable: exchange.main_enable.clone(),
            disable: Vec::new(),
            valid_cross_crate_items: exchange.valid_cross_crate_items.clone(),
            feature_to_items: HashMap::new(),
        },
    );

    let mut violations: Vec<String> = Vec::new();
    let mut first_failed_dep: Option<String> = None;
    let instant = std::time::Instant::now();

    while let Some((name, version, item_depth)) = worklist.pop() {
        // Optional top-level deps that were never enabled don't need checking —
        // their sub-deps may not have been downloaded and they won't appear in the build.
        if item_depth == 1
            && is_dep_optional(&exchange.crate_info, &name)
            && !enabled_optional_deps.contains(&name)
        {
            debug!(
                "Optional dep {} was not enabled, skipping recursive check",
                name
            );
            continue;
        }

        // `version` is already the exact downloaded version — do not re-resolve via
        // resolve_version, which applies semver-compat rules and can return a different
        // version than what is on disk (e.g. "0.4.19" → "0.4.45").
        let name_with_version = format!("{}:{}", name, version);
        debug!("Checking at depth {}: {}", item_depth, name_with_version);
        // Only process crates that were actually downloaded.
        let dep_dir = std::path::PathBuf::from(consts::DOWNLOAD_PATH)
            .join(format!(
                "{}_deps",
                exchange.name_with_version.replace(':', "-")
            ))
            .join(name_with_version.replace(':', "-"));
        if !dep_dir.exists() {
            panic!(
                "Dependency {} not found on disk. This should not happen since the dependency should have been downloaded if it is in the lock file.",
                name_with_version
            );
        }

        if is_proc_macro(&name_with_version, Some(&exchange.name_with_version)) {
            debug!(
                "Dependency: {} is a proc-macro crate, skipping requirement check",
                name_with_version
            );
            continue;
        }

        // Current crate's CrateInfo. We will use this to check the features the the current crate exposes.
        let (.., crate_info) = downloader::gather_crate_info(
            &name_with_version,
            true,
            Some(&exchange.name_with_version),
        )
        .unwrap();

        // This crate's own active feature set and cross-crate item usage, used below to
        // audit *its* dependencies — populated when this crate itself was first analyzed
        // (either the main-crate seed, or a previous iteration where it was `dep_name_with_version`).
        let (parent_active_enable, parent_valid_cross_crate_items) = dep_contexts
            .get(&name_with_version)
            .map(|c| (c.enable.clone(), c.valid_cross_crate_items.clone()))
            .unwrap_or_else(|| {
                debug!(
                    "No cached usage context for {} — treating it as having no active features or known item usage",
                    name_with_version
                );
                (Vec::new(), HashSet::new())
            });

        for (dep, _) in crate_info.deps_and_features.iter() {
            // fetch_index retries transient failures internally; a surviving error
            // means a permanent one (e.g. crate not found), so skip this dep rather
            // than panicking the whole run.
            let dep_index_entries = match downloader::fetch_index(&dep.name) {
                Ok(entries) => entries,
                Err(e) => {
                    debug!("Skipping dep {}: failed to fetch index: {}", dep.name, e);
                    continue;
                }
            };
            let dep_resolved_version =
                match downloader::resolve_version(&Some(&dep.version), &dep_index_entries) {
                    Ok(v) => v,
                    Err(e) => {
                        debug!("Skipping dep {}: failed to resolve version: {}", dep.name, e);
                        continue;
                    }
                };
            let dep_name_with_version = format!("{}:{}", dep.name.clone(), dep_resolved_version);

            println!("Processing dependency: {}", dep_name_with_version);

            if is_dep_optional(&crate_info, &dep.name)
                || is_proc_macro(&dep_name_with_version, Some(&exchange.name_with_version))
            {
                debug!(
                    "Dependency: {} is optional or a proc-macro crate: {}, skipping requirement check",
                    dep.name, name_with_version
                );
                continue;
            }

            let sub_dep_dir = std::path::PathBuf::from(consts::DOWNLOAD_PATH)
                .join(format!(
                    "{}_deps",
                    exchange.name_with_version.replace(':', "-")
                ))
                .join(dep_name_with_version.replace(':', "-"));
            if !sub_dep_dir.exists() {
                panic!(
                    "Dependency {} not found on disk. This should not happen since the dependency should have been downloaded if it is in the lock file.",
                    dep_name_with_version
                );
            }

            let (.., dep_crate_info) = downloader::gather_crate_info(
                &dep_name_with_version,
                true,
                Some(&exchange.name_with_version),
            )
            .unwrap();

            let mut optional_dep_feats = features_for_optional_deps(&dep_crate_info);

            let cached = dep_contexts.get(&dep_name_with_version).map(|c| {
                (c.enable.clone(), c.disable.clone(), c.feature_to_items.clone())
            });

            let (enable, disable, feature_to_items): (Vec<String>, Vec<String>, HashMap<String, HashSet<String>>) =
                if let Some((enable, disable, feature_to_items)) = cached {
                    debug!(
                        "Already visited dependency requirements for crate: {}",
                        dep_name_with_version
                    );
                    (enable, disable, feature_to_items)
                } else {
                    let ctx = z3::Context::new(&z3::Config::new());
                    let (all_hard, hard_constraints, _, _, dep_root, dep_records) =
                        driver::analyze_crate_wrapper(
                            &ctx,
                            &dep_name_with_version,
                            Some(&exchange.name_with_version),
                            &mut telemetry,
                        );
                    let mut crate_attrs = parse_crate(
                        &dep_name_with_version,
                        false,
                        Some(&exchange.name_with_version),
                        &all_hard,
                    );
                    let (enable, disable) = process_crate(
                        exchange,
                        &ctx,
                        &mut crate_attrs,
                        Some(&dep_name_with_version),
                        Some(&dep_crate_info),
                        false,
                        &mut optional_dep_feats,
                        hard_constraints.clone(),
                    )
                    .unwrap();

                    // While `ctx` (and the Z3 Bools tied to it) is still alive, compute and
                    // cache this dependency's own usage context, so it's available later if
                    // it's revisited as the *parent* of its own dependencies.
                    let named = crate::visitor::collect_named_items_with_conditions(&dep_root, &ctx);
                    let dep_valid_cross_crate_items = driver::compute_valid_cross_crate_items(
                        &dep_root,
                        &dep_records,
                        hard_constraints.as_ref(),
                        &ctx,
                    );
                    let feature_to_items: HashMap<String, HashSet<String>> = disable
                        .iter()
                        .map(|feat| {
                            let f_var = z3::ast::Bool::new_const(&ctx, feat.as_str());
                            let gated: HashSet<String> = named
                                .iter()
                                .filter(|(_, cond)| {
                                    let s = z3::Solver::new(&ctx);
                                    s.assert(cond);
                                    s.assert(&f_var.not());
                                    s.check() == z3::SatResult::Unsat
                                })
                                .map(|(name, _)| name.clone())
                                .collect();
                            (feat.clone(), gated)
                        })
                        .collect();

                    dep_contexts.insert(
                        dep_name_with_version.clone(),
                        DepUsageContext {
                            enable: enable.clone(),
                            disable: disable.clone(),
                            valid_cross_crate_items: dep_valid_cross_crate_items,
                            feature_to_items: feature_to_items.clone(),
                        },
                    );

                    (enable, disable, feature_to_items)
                };

            debug!(
                "Dependency: {} requires features: {:?} to be enabled and features: {:?} to be disabled to support no_std",
                dep_name_with_version, enable, disable
            );

            // We use the resolved version here because multiple versions of the same crate
            // can resolve to the same version and are required by different dependencies.
            // In that case, we don't want to check the same crate multiple times.
            if item_depth <= depth && seen.insert((dep.name.clone(), dep_resolved_version.clone()))
            {
                debug!(
                    "Adding dependency: {} to worklist for requirement check with version: {} at depth {}",
                    dep.name,
                    dep_resolved_version,
                    item_depth + 1
                );
                worklist.push((
                    dep.name.clone(),
                    dep_resolved_version.clone(),
                    item_depth + 1,
                ));
            }

            let dep_violations = audit_dependency_requirement(
                &crate_info,
                &dep_crate_info,
                &dep.name,
                &name_with_version,
                &dep_name_with_version,
                &enable,
                &disable,
                &parent_active_enable,
                &parent_valid_cross_crate_items,
                &feature_to_items,
            );
            if !dep_violations.is_empty() {
                for v in &dep_violations {
                    println!("[recursive_check] {}", v);
                }
                if first_failed_dep.is_none() {
                    first_failed_dep = Some(dep_name_with_version.clone());
                }
                violations.extend(dep_violations);
            }
        }
    }
    exchange.telemetry.recursive_requirement_check_time_ms = instant.elapsed().as_millis();
    exchange.telemetry.recursive_requirement_check_violations = violations;
    if let Some(failed_dep) = first_failed_dep {
        exchange.telemetry.recursive_requirement_check_failed = true;
        exchange.telemetry.recursive_requirement_check_failed_dep = Some(failed_dep);
        false
    } else {
        true
    }
}

/// Audits one parent→dependency edge in both directions without modifying any
/// Cargo.toml. `enable`/`disable` are the dependency's own minimal no_std solve
/// result (what it actually needs/doesn't need, in isolation). Returns one
/// human-readable message per problem found; an empty vec means the edge is fine.
#[allow(clippy::too_many_arguments)]
fn audit_dependency_requirement(
    main_crate_info: &CrateInfo,
    dep_crate_info: &CrateInfo,
    dep_name: &str,
    parent_name_with_version: &str,
    dep_name_with_version: &str,
    enable: &[String],
    disable: &[String],
    parent_active_enable: &[String],
    parent_valid_cross_crate_items: &HashSet<(String, String)>,
    feature_to_items: &HashMap<String, HashSet<String>>,
) -> Vec<String> {
    let mut violations = Vec::new();

    let dep_edge = main_crate_info
        .deps_and_features
        .iter()
        .find(|(dep, _)| dep.name == dep_name);
    let dep_default_feats: Vec<String> = dep_edge
        .map(|(_, feats)| feats.clone())
        .unwrap_or_default();

    // Features reachable from the parent's actual active feature set, walked
    // transitively through the parent's own [features] table.
    let mut parent_reachable = parent_active_enable.to_vec();
    solver::all_enabled_for_feat(&mut parent_reachable, main_crate_info);

    // --- Direction 1: dep requires a feature the parent has no way to enable. ---
    for feat in enable {
        if dep_default_feats.contains(feat) {
            continue;
        }
        if feat_available_for_dep(main_crate_info, dep_name, feat) {
            continue;
        }
        violations.push(format!(
            "{} (parent {}) requires feature '{}' for no_std, but '{}' is not declared on the \
             dependency edge and no [features] entry of {} maps to {}/{}",
            dep_name_with_version,
            parent_name_with_version,
            feat,
            feat,
            parent_name_with_version,
            dep_name,
            feat
        ));
    }

    // --- Direction 2: parent forces on a feature the dep doesn't need. ---
    // `disable` only means "not required in isolation," not "forbidden" — if the
    // parent's own source genuinely uses an item gated by this feature (under the
    // parent's own hard constraints), it's not a misconfiguration, skip it.
    let dep_norm = dep_name.replace('-', "_");
    for feat in disable {
        let protected = feature_to_items.get(feat).is_some_and(|items| {
            items.contains("*")
                || items
                    .iter()
                    .any(|item| parent_valid_cross_crate_items.contains(&(dep_norm.clone(), item.clone())))
        });
        if protected {
            continue;
        }

        let forced_by_edge = dep_default_feats.contains(feat);
        let forced_by_table = main_crate_info.features.iter().any(|(main_feat, tuples)| {
            parent_reachable.contains(main_feat)
                && tuples.iter().any(|(d, f)| d == dep_name && f == feat)
        });
        let forced_by_default = dep_edge.is_some_and(|(dep, _)| dep.default_features)
            && solver::disable_in_default(dep_crate_info, std::slice::from_ref(feat));

        if forced_by_edge {
            violations.push(format!(
                "{} (parent {}) does not require feature '{}', but the parent declares it \
                 explicitly on the dependency edge and {}'s own code does not use anything that \
                 feature gates",
                dep_name_with_version, parent_name_with_version, feat, parent_name_with_version
            ));
        } else if forced_by_table {
            violations.push(format!(
                "{} (parent {}) does not require feature '{}', but it is reachable from {}'s \
                 active [features] table and {}'s own code does not use anything that feature \
                 gates",
                dep_name_with_version,
                parent_name_with_version,
                feat,
                parent_name_with_version,
                parent_name_with_version
            ));
        } else if forced_by_default {
            violations.push(format!(
                "{} (parent {}) does not require feature '{}', but default-features = true on \
                 the edge and '{}' is part of {}'s own default feature set — default-features \
                 should be false here",
                dep_name_with_version, parent_name_with_version, feat, feat, dep_name
            ));
        }
    }

    violations
}

fn feat_available_for_dep(main_crate_info: &CrateInfo, dep_name: &str, feat: &str) -> bool {
    main_crate_info.features.iter().any(|(_, dep_feats)| {
        dep_feats
            .iter()
            .any(|(dep, f)| dep == dep_name && f == feat)
    })
}

fn parse_top_level_externs<'a>(
    ctx: &'a z3::Context,
    names_and_versions: &[(String, String)],
    externs: &Vec<ItemExternCrate>,
    telemetry: &mut Telemetry,
    main_name: &str,
    parent_name: Option<&str>,
) -> Result<(Option<Bool<'a>>, ParsedAttr), anyhow::Error> {
    let mut worklist = Vec::new();
    for ex in externs {
        let (equation, parsed_attr) = parse_main_attributes_direct(ex.attrs.first().unwrap(), ctx);
        // If there is no attribute gating the extern crate,
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
        let name_with_version = downloader::clone_from_crates(
            &ex.ident.to_string(),
            version,
            Some(main_name),
            parent_name,
        )?;
        let items = parse_item_extern_crates(&name_with_version, Some(main_name));
        if items.itemexterncrates.is_empty() {
            continue;
        }
        let std_attrs = get_item_extern_std(&items);
        if !std_attrs.is_empty() {
            telemetry.indirect_extern_std_usage_depth = 1;
            telemetry.indirect_extern_std_usage_crate = Some(name_with_version.clone());
            return Ok((equation, parsed_attr));
        }
        worklist.push((name_with_version, equation, parsed_attr));
    }

    Ok(parse_n_level_externs_entry(
        &mut worklist,
        telemetry,
        main_name,
    ))
}

fn parse_n_level_externs_entry<'a>(
    worklist: &mut Vec<(String, Option<Bool<'a>>, ParsedAttr)>,
    telemetry: &mut Telemetry,
    main_name: &str,
) -> (Option<Bool<'a>>, ParsedAttr) {
    let mut worklists = Vec::new();
    let mut depth = 2;

    worklist.iter().for_each(|(name_with_version, _, _)| {
        let (name, version) = name_with_version.split_once(':').unwrap();
        let dep_names = downloader::read_dep_names_and_versions(name, version, false, main_name)
            .unwrap_or_default();
        let initial_worklist = dep_names
            .iter()
            .map(|(dep_name, dep_version)| format!("{}:{}", dep_name, dep_version))
            .collect::<Vec<String>>();
        worklists.push((name_with_version.clone(), initial_worklist));
    });

    let mut visited: HashSet<String> = HashSet::new();

    loop {
        if worklists.iter().all(|(_, remaining)| remaining.is_empty()) {
            telemetry.indirect_extern_std_usage_depth = depth;
            return (None, ParsedAttr::default());
        }
        for (name_with_version, equation, parsed_attr) in worklist.iter() {
            let local_worklist = worklists
                .iter_mut()
                .find(|(name, _)| name == name_with_version)
                .unwrap();
            // TODO: BFS across all top-level crates simultaneously - returns on the first
            // extern crate std hit at the shallowest depth. A crate with a deeper violation
            // may be missed if another crate hits first at a shallower depth. Consider
            // exhaustive per-crate traversal if full coverage is needed.
            if parse_n_level_externs(
                &mut local_worklist.1,
                telemetry,
                main_name,
                Some(name_with_version),
                &mut visited,
            ) {
                telemetry.indirect_extern_std_usage_depth = depth;
                return (equation.clone(), parsed_attr.clone());
            }
        }
        depth += 1;
    }
}

fn parse_n_level_externs(
    worklist: &mut Vec<String>,
    telemetry: &mut Telemetry,
    main_name: &str,
    parent_name: Option<&str>,
    visited: &mut HashSet<String>,
) -> bool {
    let mut local_worklist = Vec::new();
    for name_with_version in worklist.drain(..) {
        if !visited.insert(name_with_version.clone()) {
            continue;
        }
        let (name, version) = name_with_version.split_once(':').unwrap();
        let new_name_with_version = downloader::clone_from_crates(
            name,
            Some(&version.to_string()),
            Some(main_name),
            parent_name,
        )
        .unwrap();
        let (name, version) = new_name_with_version
            .split_once(':')
            .unwrap_or((name, version));
        let names_and_versions =
            downloader::read_dep_names_and_versions(name, version, false, main_name).unwrap();
        let unfiltered = parse_item_extern_crates(&new_name_with_version, Some(main_name));
        let std_attrs = get_item_extern_std(&unfiltered);
        if !std_attrs.is_empty() {
            telemetry.indirect_extern_std_usage_crate = Some(new_name_with_version);
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
                ex.ident,
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
        if let Some(res) = get_deps_and_features(name, version, dep) {
            return Some(res);
        }
    }
    None
}

fn extract_key(s: &str) -> &str {
    s.split_once("/").map_or(s, |(_, value)| value)
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

fn visit<T>(
    visiter_type: &mut T,
    crate_name: &str,
    recurse: bool,
    direct_file: bool,
    main_name: Option<&str>,
) -> anyhow::Result<()>
where
    T: for<'a> Visit<'a> + GetItemExternCrate,
{
    let dir = if !direct_file {
        get_actual_dir(crate_name, main_name)
    } else {
        PathBuf::from(crate_name)
    };

    let files = get_all_rs_files(&dir, recurse, main_name);

    for filename in files {
        debug!("Parsing file: {:?}", filename);
        let content = match fs::read_to_string(&filename) {
            Ok(content) => content,
            Err(e) => {
                debug!("Failed to read file {:?}: {}", filename, e);
                continue;
            }
        };
        let file = match syn::parse_file(&content) {
            Ok(file) => file,
            Err(e) => {
                debug!("Failed to parse file {:?}: {}", filename, e);
                continue;
            }
        };
        let span_file_path = if !direct_file {
            let span_file_path = filename.strip_prefix(&dir).unwrap();
            visiter_type.set_current_file(span_file_path.display().to_string());
            span_file_path.to_path_buf()
        } else {
            filename
        };
        visiter_type.visit_file(&file);
        if let Some(spans) = visiter_type.get_spans() {
            // Newly added spans will have None as filename.
            // We fill it with the current filename.
            for span in spans {
                if span.1.is_none() {
                    span.1.replace(span_file_path.display().to_string());
                }
            }
        }
    }
    Ok(())
}

fn is_any_logic(logic: &str) -> Option<Logic> {
    match logic {
        "any" => Some(Logic::Any),
        "and" => Some(Logic::And),
        "all" => Some(Logic::And),
        "or" => Some(Logic::Or),
        "not" => Some(Logic::Not),
        _ => None,
    }
}

pub(crate) fn is_no_std(parsed: &ParsedAttr, check_all: bool) -> bool {
    let mut to_check = vec!["no_std"];
    if check_all {
        to_check.append(&mut vec!["no_core"]);
    }
    parsed
        .constants
        .iter()
        .any(|c| to_check.contains(&c.as_str()))
}

fn parse_token_stream<'a>(
    tokens: TokenStream,
    parsed: &mut ParsedAttr,
    ctx: &'a z3::Context,
    equation: &mut Option<Bool<'a>>,
) -> Vec<Bool<'a>> {
    let mut was_feature = false;
    let mut was_filepath = false;
    let mut current_expr: Option<Bool> = None;
    let mut group_items: Vec<Bool> = Vec::new();
    let mut curr_logic = Logic::Any;

    for token in tokens {
        match token {
            proc_macro2::TokenTree::Group(g) => {
                let mut group_expr = None;
                let constants_before_call = parsed.constants.len();
                let local_group_items =
                    parse_token_stream(g.stream(), parsed, ctx, &mut group_expr);

                let local_group_items_refs: Vec<&Bool> = local_group_items.iter().collect();
                if local_group_items_refs.is_empty() {
                    // Prevent false positives when feature(no_std) is present in an attribute
                    if parsed.constants.len() == constants_before_call + 1 {
                        parsed.constants.truncate(constants_before_call);
                    }
                    continue;
                }
                let local_expr = match curr_logic {
                    Logic::And => Some(Bool::and(ctx, local_group_items_refs.as_slice())),
                    Logic::Or | Logic::Any => {
                        Some(Bool::or(ctx, local_group_items_refs.as_slice()))
                    }
                    Logic::Not => local_group_items.first().map(|first| first.not()),
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
                } else if ident_str == "path" {
                    was_filepath = true;
                } else if let Some(logic) = is_any_logic(&ident_str) {
                    parsed.logic.push(logic.clone());
                    curr_logic = logic;
                } else {
                    parsed.constants.push(ident_str.clone());
                    if levenshtein(&ident_str, "feature") == 2 {
                        debug!("Possible misspelled feature: {}", ident_str);
                        parsed.typoed_keyword = true;
                    }
                }
            }
            proc_macro2::TokenTree::Literal(l) => {
                if was_feature {
                    let feature_str = l.to_string()[1..l.to_string().len() - 1].to_string();
                    parsed.features.push(feature_str.clone());

                    let feature_var = Bool::new_const(ctx, feature_str);
                    group_items.push(feature_var);
                    was_feature = false;
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
    } else if !group_items.is_empty() {
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
            parse_token_stream(tokens, &mut parsed, ctx, &mut equation);
            (equation, parsed)
        }
        _ => {
            debug!("Meta is not a list");
            (None, ParsedAttr::default())
        }
    }
}

fn get_all_rs_files(path: &Path, recurse: bool, main_name: Option<&str>) -> Vec<PathBuf> {
    if path.is_file() && path.extension().unwrap_or_default() == "rs" {
        return vec![path.to_path_buf()];
    }

    let mut files = Vec::new();

    if recurse {
        for entry in WalkDir::new(path) {
            push_to_files_vec(entry.unwrap().path(), &mut files);
        }
    } else {
        let basename = path
            .file_name()
            .unwrap_or_default()
            .to_str()
            .unwrap_or_default();
        let manifest_path = determine_manifest_file(basename, main_name);
        let toml: toml::Value = fs::read_to_string(&manifest_path)
            .ok()
            .and_then(|content| toml::from_str(&content).ok())
            .unwrap_or_else(|| {
                debug!("Failed to read or parse Cargo.toml at {}", manifest_path);
                toml::Value::Table(toml::map::Map::new())
            });

        if let Some(lib) = toml.get("lib").and_then(|l| l.as_table())
            && let Some(path_value) = lib.get("path").and_then(|p| p.as_str())
        {
            let lib_path = Path::new(path).join(path_value);
            push_to_files_vec(&lib_path, &mut files);
        }
        if let Some(bin_array) = toml.get("bin").and_then(|b| b.as_array()) {
            for bin_target in bin_array {
                if let Some(path_value) = bin_target.get("path").and_then(|p| p.as_str()) {
                    let bin_path = Path::new(path).join(path_value);
                    push_to_files_vec(&bin_path, &mut files);
                }
            }
        }
        let src_path = Path::new(path).join("src");
        let entries = if !src_path.exists() {
            debug!("No src directory found in {:?}", path);
            fs::read_dir(path).unwrap()
        } else {
            let mut push_if_path_exist = |sub_path: &str| {
                let full_path = src_path.join(sub_path);
                if full_path.exists() {
                    for entry in fs::read_dir(&full_path).unwrap() {
                        push_to_files_vec(&entry.unwrap().path(), &mut files);
                    }
                }
            };
            push_if_path_exist("bin");
            push_if_path_exist("lib");
            fs::read_dir(&src_path).unwrap()
        };
        for entry in entries {
            push_to_files_vec(&entry.unwrap().path(), &mut files);
        }
    }
    files
}

fn push_to_files_vec(path: &Path, files: &mut Vec<PathBuf>) {
    if path.extension().unwrap_or_default() == "rs" && !path.to_str().unwrap().contains("/tests/") {
        files.push(path.to_path_buf());
    }
}

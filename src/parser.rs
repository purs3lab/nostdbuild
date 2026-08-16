use anyhow::Context;
use log::debug;
use proc_macro2::TokenStream;
// use quote::ToTokens;
use std::{
    collections::{HashMap, HashSet},
    fs,
    path::{Path, PathBuf},
};
use syn::{Attribute, ItemExternCrate, Meta, visit::Visit};
use walkdir::WalkDir;
use z3::{self, ast::Bool};

use strsim::levenshtein;

use crate::{
    Attributes, CrateInfo, DBData, DEPENDENCIES, DataExchange, DepNoStdFailure, Telemetry, consts,
    db, downloader, driver,
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

    if let Err(err) = visit(&mut itemexterncrates, crate_name, true, false, main_name, None) {
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
            None,
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
///
/// `files`, when supplied, restricts parsing to that exact list — callers with a
/// resolved `ModNode` tree should pass `visitor::collect_source_files(&root)` so
/// only entrypoint-reachable files are parsed. `None` falls back to the naive
/// directory sweep in `get_all_rs_files`.
pub fn parse_crate(
    crate_name: &str,
    recurse: bool,
    main_name: Option<&str>,
    hir_spans: &[ReadableSpan],
    files: Option<&[PathBuf]>,
) -> Attributes {
    let mut attributes = Attributes {
        hir_spans: hir_spans.to_vec(),
        ..Default::default()
    };

    match visit(
        &mut attributes,
        crate_name,
        recurse,
        false,
        main_name,
        files,
    ) {
        Ok(parsed) => attributes.files_parsed = parsed,
        Err(err) => {
            debug!(
                "Failed to parse crate {} with error:{}. Will continue...",
                crate_name, err
            );
        }
    }
    attributes.crate_name = crate_name.to_string();
    attributes
}

/// What a crate-root parse established about a crate's no_std support.
///
/// The three cases used to be two: anything that was not `Supported` came back
/// as `false`, so "we read this crate and it declares no `no_std`" and "we read
/// nothing at all" were the same answer. Only the first is a verdict.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NoStdEvidence {
    /// A crate-root `#![no_std]` or `#![cfg_attr(…, no_std)]` was found.
    Supported,
    /// The crate root was parsed and carries no `no_std` attribute.
    Absent,
    /// Nothing was parsed — cargo reported no lib/bin target, or every
    /// candidate file failed to read or failed `syn`. Says nothing either way.
    NoSources,
}

/// Check if the crate has a no_std attribute.
/// # Arguments
/// * `name` - The name of the crate
/// * `ctx` - The Z3 context
/// # Returns
/// A boolean indicating whether the crate has a no_std attribute. Callers that
/// act on a *negative* answer should use [`no_std_evidence`] instead, so a crate
/// whose sources were never parsed is not reported as std-only.
pub fn check_for_no_std(
    name: &str,
    ctx: &z3::Context,
    telemetry: Option<&mut Telemetry>,
    main_name: Option<&str>,
) -> bool {
    no_std_evidence(name, ctx, telemetry, main_name) == NoStdEvidence::Supported
}

/// [`check_for_no_std`], keeping the reason for a negative answer.
pub fn no_std_evidence(
    name: &str,
    ctx: &z3::Context,
    telemetry: Option<&mut Telemetry>,
    main_name: Option<&str>,
) -> NoStdEvidence {
    // This is the list of known syn failure crates which are no_std
    if consts::KNOWN_SYN_FAILURES.contains(&name) {
        debug!("Skipping known syn failure crate: {}", name);
        return NoStdEvidence::Supported;
    }

    // We need to re-parse this instead of using already existing attributes
    // since files in non root directory might have `no_std` attribute
    // and we don't want to include those.
    //
    // This runs before any analysis, so there is no ModNode tree to derive files
    // from. `#![no_std]` / `#![cfg_attr(…, no_std)]` are crate-root inner
    // attributes though, so the entrypoints are the only files that matter — and
    // asking cargo for them also applies the `is_lib || (is_bin && !has_lib)`
    // rule, keeping a bin target's missing `#![no_std]` from being read as the
    // library's.
    let manifest = determine_manifest_file(name, main_name);
    let mut entrypoints: Vec<PathBuf> = Vec::new();
    crate::visitor::find_entrypoints(&manifest, &mut entrypoints);
    entrypoints.retain(|p| p.exists());
    let entry_files = (!entrypoints.is_empty()).then_some(entrypoints.as_slice());

    let base_attrs = parse_crate(name, false, main_name, &[], entry_files);

    if let Some(telemetry) = telemetry {
        telemetry.wrong_unconditional_setup = base_attrs.wrong_unconditional_setup;
    }

    if !parse_main_attributes(&base_attrs, ctx).0 && !base_attrs.unconditional_no_std {
        // No attribute found — but an empty attribute list from a parse that
        // read no files is not the same statement as one from a parse that read
        // the crate root. `nb:0.1.3` is in `KNOWN_SYN_FAILURES` for exactly this
        // reason; report the case instead of needing a name per crate.
        if base_attrs.files_parsed == 0 {
            debug!(
                "No source parsed for the crate {} — no_std support is unknown, not absent",
                name
            );
            return NoStdEvidence::NoSources;
        }
        debug!("No no_std found for the crate {}", name);
        return NoStdEvidence::Absent;
    }
    NoStdEvidence::Supported
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
        let (all_hard, _, _, _, _, _, _) =
            driver::analyze_crate_wrapper(&ctx, &dep.clone(), Some(main_name), telemetry);
        attributes.push(parse_crate(&dep.clone(), true, Some(main_name), &all_hard, None));
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
) -> anyhow::Result<TripleTupleVecString> {
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
            return Ok((Vec::new(), Vec::new(), Vec::new()));
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
            return Ok((Vec::new(), Vec::new(), Vec::new()));
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
                        return Ok((Vec::new(), Vec::new(), Vec::new()));
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
        let (_, neg_parsed_attr) = parse_main_attributes_direct(negated_attr, ctx);
        non_minimalizable_features.extend(neg_parsed_attr.features.iter().cloned());
        // `None` when the cfg names an atom policy G erases — its negation
        // cannot be modelled soundly, so it constrains nothing. The features it
        // named stay non-minimalizable above: that only pins, it asserts nothing.
        if let Some(neg_eq) = compile_error_constraint(negated_attr, ctx, None) {
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
            return Ok((Vec::new(), Vec::new(), Vec::new()));
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
            return Ok((Vec::new(), Vec::new(), Vec::new()));
        }
    }

    // The equation is complete here, so this is the last point at which the
    // crate's own statement of when it is `#![no_std]` can be read as a
    // statement rather than as one satisfying assignment. What it *entails* has
    // to survive every pass after the solve: `minimize` drops a feature whose
    // whole subtree only links optional deps, which is exactly the shape of
    // `robust`'s `no_std = ["ieee754"]`, `utm`'s `no_std = ["num"]` and
    // `lasso`'s `no-std` — the solve turned each on and minimize took it back
    // off, and the crate compiled as std. Protecting them here is the same
    // treatment `compile_error!`'s features already get two blocks above.
    let (no_std_required, no_std_forbidden) = match &equation {
        Some(eq) => solver::no_std_forced_features(ctx, eq, crate_info),
        None => (Vec::new(), Vec::new()),
    };
    non_minimalizable_features.extend(no_std_required.iter().cloned());
    if is_main {
        exchange.main_no_std_required = no_std_required;
        exchange.main_no_std_forbidden = no_std_forbidden;
    }

    // Finally, we solve the equations
    let (model, len, depth, entailed_false) = {
        let t = crate::timing::scope("feature_solve", name_with_version);
        t.meta("constraint_len", filtered.len().to_string());
        solver::solve(ctx, &equation, &filtered, &hard_constraint_vec)
    };
    debug!(
        "Solver result for crate {}: model={:?}, len={}, depth={}, entailed false={:?}",
        name_with_version, model, len, depth, entailed_false
    );
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

    // The pin set (`driver::deps_pinned_by_active_use`) cannot exist yet for the main
    // crate: it needs the active feature set, which needs `final_feature_list_main`,
    // which needs the `enable` this function is still computing. Passing an empty set
    // here read as "nothing is pinned" and let the surgical branch unlink a dependency
    // the crate still imports, before `bin/main.rs` ever got to arm the check — the T1
    // signature, resurfaced (KI-14). `None` says *unknown*, so this pass leaves the
    // entries alone and the armed call in `bin/main.rs` decides.
    // Dependency crates keep the empty set: their own active feature set is equally
    // unknown here, but no later pass revisits their manifest, so deferring would mean
    // never minimizing them at all.
    let empty_pins = HashSet::new();
    let deps_to_keep = if is_main { None } else { Some(&empty_pins) };
    let call_site = if is_main {
        "process_crate:main"
    } else {
        "process_crate:dep"
    };
    minimize(
        crate_info,
        optional_dep_feats,
        &mut enable,
        &non_minimalizable_features,
        true,
        name_with_version,
        main_name,
        None,
        call_site,
        deps_to_keep,
    );

    Ok((enable, disable, entailed_false))
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
        // An undeclared feature is normally opaque — we cannot see what it pulls
        // in, so it is not droppable. The one exception is the feature Cargo
        // synthesises for an optional dependency: it is absent from `[features]`
        // precisely *because* its only value is `dep:<itself>`, which is the
        // deps-only condition this function tests for.
        return is_implicit_optional_dep_feature(feat_name, crate_info, optional_deps);
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

/// Returns the features in `enable` whose *sole* effect is linking an optional
/// dependency, so dropping them from `--features` changes nothing else about the
/// build.
///
/// This is the retry material for the case where a dependency passes every
/// no_std-capability check and still cannot build for the target we picked:
/// lazy-exclusive's `use-locks` pulls in `libc`, which is a perfectly no_std
/// crate, but the `pthread_mutex_*` items the crate imports from it do not exist
/// on bare metal. No dependency-level probe can see that — `libc` alone compiles
/// for the target fine — so the only evidence is a failed compile of the crate
/// itself, and the only safe use of this list is as material for a retry whose
/// result is verified by a build. It must never be asserted into a solve: doing
/// so just reshuffles which arbitrary model Z3 returns.
///
/// A feature is a candidate only if it actually reaches an optional dep
/// (`features_for_optional_deps`), which keeps marker features like `foo = []`
/// out, and only if `all_subtree_deps_only` holds for its whole subtree.
/// Features `default` already pulls in are excluded when defaults are on:
/// removing those from the command line would not turn them off.
pub fn deps_only_enable_features(
    crate_name: &str,
    crate_info: &CrateInfo,
    enable: &[String],
    non_minimalizable_features: &HashSet<String>,
    default_features_on: bool,
) -> Vec<String> {
    let mut optional_deps: Vec<String> = crate_info
        .deps_and_features
        .iter()
        .filter(|(dep, _)| dep.optional)
        .map(|(dep, _)| dep.name.clone())
        .collect();
    // `gather_crate_info` only reads `[dependencies]`, so an optional dep under
    // `[target.'cfg(…)'.dependencies]` is missing from `deps_and_features` —
    // which is exactly lazy-exclusive's shape, where both deps `use-locks` pulls
    // in are target-scoped. Read them off the manifest as well.
    let manifest = determine_manifest_file(crate_name, None);
    if let Ok(toml) = fs::read_to_string(&manifest)
        .map_err(anyhow::Error::from)
        .and_then(|s| toml::from_str::<toml::Value>(&s).map_err(anyhow::Error::from))
    {
        for dep in downloader::optional_deps_in_manifest(&toml) {
            if !optional_deps.contains(&dep) {
                optional_deps.push(dep);
            }
        }
    }
    if optional_deps.is_empty() {
        return Vec::new();
    }

    let dep_enablers = features_for_optional_deps_with(crate_info, &optional_deps);
    let via_default: HashSet<String> =
        if default_features_on && crate_info.features.iter().any(|(name, _)| name == "default") {
            close_over_local_features(
                &HashSet::from(["default".to_string()]),
                &crate_info.features,
            )
        } else {
            HashSet::new()
        };

    let mut candidates: Vec<String> = enable
        .iter()
        .filter(|feat| !via_default.contains(feat.as_str()))
        .filter(|feat| dep_enablers.iter().any(|(_, f)| f == *feat))
        .filter(|feat| {
            all_subtree_deps_only(
                feat,
                crate_info,
                &optional_deps,
                non_minimalizable_features,
                &mut HashSet::new(),
            )
        })
        .cloned()
        .collect();
    candidates.sort();
    candidates.dedup();
    candidates
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
    let Some(values) = crate_info
        .features
        .iter()
        .find(|(name, _)| name == feat_name)
        .map(|(_, v)| v)
    else {
        // Cargo's implicit feature for an optional dep enables that dep directly
        // and has no manifest entry to walk, so it is its own enabler.
        let this_dep = [dep_name.to_string()];
        return (feat_name == dep_name
            && is_implicit_optional_dep_feature(feat_name, crate_info, &this_dep))
        .then(|| feat_name.to_string());
    };

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
/// * `call_site` - Which of the four call sites this is; logged so a manifest edit
///   can be traced back to the pass that made it
/// * `deps_to_keep` - Optional dependencies that must stay linked because the crate
///   imports from them under a cfg that survives the unlink
///   (`driver::deps_pinned_by_active_use`). `None` means the set is not computable
///   yet: unlinking a dep cannot be shown safe, so this pass leaves the entries
///   alone and a later, armed pass decides.
pub fn minimize(
    crate_info: &CrateInfo,
    optional_dep_feats: &mut TupleVec,
    enable: &mut Vec<String>,
    non_minimalizable_features: &HashSet<String>,
    disable_default: bool,
    crate_name: &str,
    main_name: Option<&str>,
    enabled_optional_deps: Option<&HashSet<String>>,
    call_site: &str,
    deps_to_keep: Option<&HashSet<String>>,
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
            "\n[minimize/{call_site}] Analyzing feature '{}' for potential removal from enable list...",
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
                "[minimize/{call_site}] Checking if feature '{}' can be dropped for dep '{}': is_direct={}, leaf='{}', leaf_only_dep_values={}, can_drop={}, subtree_deps_only={}, non_minimalizable={}",
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
                    "[minimize/{call_site}] DROP feature '{}': entire subtree only enables optional deps (dep='{}')",
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
            } else if deps_to_keep.is_none_or(|keep| keep.contains(dep_name.as_str())) {
                // The crate imports items from this dep under a cfg that stays true
                // once the dep is unlinked — stripping the entry below would leave
                // those imports resolving against a crate cargo never links
                // (a7105-0.1.0: `default = ["async"]`, `async = ["embedded-hal-async"]`,
                // `#[cfg(feature = "async")] use embedded_hal_async::…` → E0433).
                // The dep stays. `None` lands here too: with no pin set there is no
                // evidence the unlink is safe, so the entry is left for the armed pass.
                let why = match deps_to_keep {
                    Some(_) => "the crate imports it under a cfg that survives unlinking",
                    None => "pin set not computable here — deferring to the armed pass",
                };
                println!("[minimize/{call_site}] KEEP dep '{dep_name}' in feature '{leaf}': {why}");
            } else if is_direct
                && leaf_only_dep_values
                && non_minimalizable_features.contains(leaf.as_str())
            {
                // The feature's sole purpose is enabling this dep, AND the feature itself must
                // stay enabled. Stripping dep:D from the feature would make it hollow while still
                // being required. Leave both the feature and the dep alone.
                debug!(
                    "[minimize/{call_site}] KEEP dep '{}' in feature '{}': feature is non-minimalizable and only exists to enable this dep — dep is also required",
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
                        "[minimize/{call_site}] MOVE entry '{}' from feature '{}' to custom-disabled (dep='{}', feat='{}', reason: {})",
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
                    "[minimize/{call_site}] SKIP dep '{}' via feat '{}' (leaf='{}'): is_direct={}, can_drop={}, dep_entry=None",
                    dep_name, feat_name, leaf, is_direct, can_drop
                );
            }
        }
    }

    // Cargo's implicit `D = ["dep:D"]` feature is a consequence of D being reachable,
    // never an independent enabler: the only way to switch it on is to name it in the
    // enable list. So once this pass has invalidated D's declared enablers, the (D, D)
    // pair is stale — and it is not inert, because `should_skip_dep` re-derives the
    // link by walking the *in-memory* `[features]` table, which still holds the entry
    // we just stripped from the manifest. watchface's `std = ["chrono"]` is the case:
    // the entry was moved to custom-disabled, but the walk `default -> std -> chrono`
    // still matched the implicit pair and the dep was treated as live, pulling chrono
    // into a no_std build with only `alloc` — enough to switch the crate's own
    // `#[cfg(feature = "chrono")]` code on while withholding the `clock` it needs.
    let stale_implicit: Vec<(String, String)> = optional_dep_feats
        .iter()
        .filter(|(dep, feat)| dep == feat)
        .filter(|(dep, _)| !handled.contains(&(dep.clone(), dep.clone())))
        // Only when this pass actually cut a declared enabler of the dep. A dep nobody
        // touched keeps its pair.
        .filter(|(dep, _)| handled.iter().any(|(d, _)| d == dep))
        // If the implicit feature is genuinely in the enable list and survived the pass,
        // the dep really is on and must not be skipped.
        .filter(|(dep, _)| !enable.contains(dep) || to_drop.contains(dep.as_str()))
        .filter(|(dep, _)| is_implicit_optional_dep_feature(dep, crate_info, &optional_deps))
        .cloned()
        .collect();
    if !stale_implicit.is_empty() {
        debug!(
            "[minimize/{call_site}] Evicting stale implicit optional-dep features: {:?}",
            stale_implicit
        );
    }
    handled.extend(stale_implicit);

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
                    "[minimize/{call_site}] DROP feature '{}': subtree only enables dep/feat for non-enabled optional deps",
                    feat_name
                );
                to_drop.insert(feat_name.clone());
            }
        }
    }

    if !to_drop.is_empty() {
        debug!(
            "[minimize/{call_site}] Dropping features from enable list: {:?}",
            to_drop
        );
    }
    if !custom_disabled.is_empty() {
        debug!(
            "[minimize/{call_site}] Entries moved to custom-disabled in Cargo.toml: {:?}",
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
/// `entailed_false` is the subset of `disable` the dependency's solve proved cannot be
/// on — `solver::solve`'s fourth value. It is consulted *only* where a feature is about
/// to be taken away from the emitted manifest, never for the feature-selection passes,
/// which keep reading the full `disable`. `None` means the caller has no such set (the
/// DB-cache path, which stores only the pair), and every removal falls back to
/// `disable` exactly as before.
pub fn finalize_dep_crate(
    exchange: &mut DataExchange,
    dep: &Attributes,
    enable: Vec<String>,
    disable: Vec<String>,
    entailed_false: Option<Vec<String>>,
    feature_to_items: HashMap<String, HashSet<String>>,
) -> Result<TripleTupleVecString, anyhow::Error> {
    // The list the removal sites use: the proven-false subset when the caller has one,
    // the whole disable list when it does not.
    //
    // `entailed_known` keeps the two apart where the *difference* is what carries
    // the meaning. Overriding protection below is sound only for a feature the
    // solve proved cannot be on; on the DB-cache path `removable` is a copy of
    // `disable`, and reading that as proof would switch protection off for every
    // cached dependency.
    let entailed_known = entailed_false.is_some();
    let removable: Vec<String> = entailed_false.unwrap_or_else(|| disable.clone());
    debug!(
        "Dependency {}: enable: {:?}, disable: {:?}, removable: {:?}",
        dep.crate_name, enable, disable, removable
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
            // Protection guards the *don't-cares* — the features the model left
            // false with no reason either way, which is where a removal takes
            // away a choice the author made (KI-8's `chrono/clock`). It cannot
            // guard a feature in `removable`: that one the dependency's own
            // solve proved cannot be on if the crate is to be no_std, so keeping
            // it emits a configuration the analysis has already refuted.
            //
            // uom is where the difference shows. Its `std` gates
            // `#[cfg(feature = "std")] pub use std::*` — a glob, which the items
            // check below reads as "gates everything" and protects on sight,
            // although `main uses []`. `afe4404` then never got
            // `default-features = false` on its uom edge and pulled in
            // `uom/std`. The glob it tripped over *is* the std re-export.
            if entailed_known && removable.contains(*feat) {
                println!(
                    "[finalize]   => ALLOW REMOVAL of '{}' (entailed false: the dep cannot be no_std with it on)",
                    feat
                );
                return false;
            }
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
    let filtered_removable: Vec<String> = removable
        .iter()
        .filter(|f| !protected.contains(*f))
        .cloned()
        .collect();

    let (args, update_default_config) = solver::final_feature_list_dep(
        &exchange.crate_info,
        &dep_original_name,
        &enable,
        &filtered_disable,
        &filtered_removable,
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
            &removable,
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
/// Record the optional-dep enablers a dependency's solved feature set makes
/// mandatory but which its emitted feature list does not contain (KI-12).
///
/// Pure observation — nothing is enabled. A non-empty result is the repro KI-12
/// has been waiting for: a dependency with the bucket-11 shape
/// (`#[cfg(not(feature = "std"))] use <optional dep>::…`) whose emitted config
/// therefore cannot link. Enablers already implied by the dep's `[features]`
/// table are expected and harmless — the redundant edge, e.g. rand 0.8's
/// `serde1 = ["serde", …]` — so entries need reading before they are believed.
fn record_missing_optional_dep_enablers<'a>(
    ctx: &'a z3::Context,
    dep_crate_name: &str,
    main_name: &str,
    dep_root: &crate::visitor::ModNode<'a>,
    enable: &[String],
    disable: &[String],
    telemetry: &mut Telemetry,
) {
    let manifest = determine_manifest_file(dep_crate_name, Some(main_name));
    let Ok(text) = fs::read_to_string(&manifest) else {
        debug!("KI-12 probe: no manifest at {manifest} for dep {dep_crate_name}");
        return;
    };
    let Ok(manifest_toml) = toml::from_str::<toml::Value>(&text) else {
        debug!("KI-12 probe: unparsable manifest at {manifest} for dep {dep_crate_name}");
        return;
    };

    let known_features = crate::visitor::declared_features(&manifest);
    let (edges, enablers) =
        driver::optional_dep_link_constraints(ctx, &manifest_toml, &known_features, dep_root);
    let forced = solver::forced_optional_dep_enablers(ctx, &edges, &enablers, enable, disable);
    if forced.is_empty() {
        return;
    }

    println!(
        "[KI-12] Dependency {dep_crate_name} needs optional-dep feature(s) {forced:?} for the \
         chosen no_std feature set, and the emitted list does not have them (not applied)"
    );
    telemetry
        .dep_missing_optional_dep_enablers
        .push((dep_crate_name.to_string(), forced));
}

pub fn process_dep_crate(
    exchange: &mut DataExchange,
    dep: &mut Attributes,
) -> Result<TripleTupleVecString, anyhow::Error> {
    let (.., dep_crate_info) =
        downloader::gather_crate_info(&dep.crate_name, true, Some(&exchange.name_with_version))?;
    let mut optional_dep_feats = features_for_optional_deps(&dep_crate_info);
    let dep_crate_name = dep.crate_name.clone();
    let main_name = exchange.name_with_version.clone();
    let ctx = z3::Context::new(&z3::Config::new());
    let (_hard_std, hard_constraints, _, _, dep_root, _, _) = driver::analyze_crate_wrapper(
        &ctx,
        &dep.crate_name,
        Some(&exchange.name_with_version),
        &mut exchange.telemetry,
    );

    // What *this dependency's* dependencies demand of it (R31-4). Its own
    // `compile_error!`s stay out of the solve for the reason `uom` documents —
    // a wide disjunction of storage types is not the solver's to pick from —
    // but a constraint arriving from one level down is not a choice this crate
    // is offering: ttf-parser refuses to compile without `std` or
    // `no-std-float`, so owned_ttf_parser answering "I need no features" is
    // wrong, and the parent's `libm = ["owned_ttf_parser/no-std-float"]` is
    // deleted as unnecessary on the strength of that answer.
    //
    // Conjoined with the probe-derived condition rather than replacing it: a
    // feature forced on here also lands in `non_minimalizable_features` via the
    // hard-constraint model, which is what keeps `minimize` from taking it back
    // one pass later (R31-3).
    let dep_manifest = determine_manifest_file(&dep_crate_name, Some(&main_name));
    let dep_requirement = driver::dependency_feature_requirement(&ctx, &dep_manifest);
    let hard_constraints = match (hard_constraints, dep_requirement) {
        (Some(hard), Some(req)) => Some(Bool::and(&ctx, &[&hard, &req])),
        (Some(hard), None) => Some(hard),
        (None, Some(req)) => Some(req),
        (None, None) => None,
    };

    let (enable, disable, entailed_false) = process_crate(
        exchange,
        &ctx,
        dep,
        Some(&dep_crate_name),
        Some(&dep_crate_info),
        false,
        &mut optional_dep_feats,
        hard_constraints,
    )?;

    // KI-12 observation. The optional-dep link edges are built for dependencies
    // exactly as they are for the main crate, but they never leave
    // `find_feature_combs_for_all_code`, so nothing adds back the enablers this
    // assignment makes mandatory — `bin/main.rs` does that step for the main
    // crate only. Records what the missing step would have added; the feature
    // set is left untouched.
    record_missing_optional_dep_enablers(
        &ctx,
        &dep_crate_name,
        &main_name,
        &dep_root,
        &enable,
        &disable,
        &mut exchange.telemetry,
    );

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

    finalize_dep_crate(
        exchange,
        dep,
        enable,
        disable,
        Some(entailed_false),
        feature_to_items,
    )
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
    let dep_norm = dep_name_only.replace('-', "_");
    flexible_main_args.retain(|feature| {
        // A feature with no `[features]` array is Cargo's implicit per-optional-dep
        // feature (`hashbrown = ["dep:hashbrown"]`, synthesised, never written down).
        // It references no dependency features, so it cannot mismatch this dep's build
        // — the question this retain asks does not apply. Dropping it on a `None` was
        // what removed `hashbrown`/`libm` from caches-0.3.0's args after the solver had
        // put them there, emitting a config that fails with `can't find crate hashbrown`.
        let Some(arr) = main_features.get_mut(feature).and_then(|f| f.as_array_mut()) else {
            return true;
        };
        let mut has_mismatch = false;
        let mut local_needed_dropped: HashSet<String> = HashSet::new();
        for f in arr.iter().filter_map(|v| v.as_str()) {
            if f.starts_with(&prefix1) || f.starts_with(&prefix2) {
                let key = extract_key(f);
                if deps_args.contains(&key.to_string()) {
                    local_needed_dropped.insert(f.to_string());
                } else if !protected_dep_features.contains(&(dep_norm.clone(), key.to_string())) {
                    // A protected value is one the *main* crate needs even though
                    // the dep's own solve did not ask for it — totsu_core needs
                    // `libm = ["num-traits/libm"]` to have a `sqrt` at all, while
                    // num-traits alone is happy without it. Counting that as a
                    // mismatch drops `libm` from the args, and the loop below then
                    // empties the feature's array, so the emitted config has
                    // neither. The removal loops below already honour this set;
                    // this one did not.
                    has_mismatch = true;
                }
            }
        }
        if has_mismatch {
            needed_dropped.extend(local_needed_dropped);
        }
        !has_mismatch
    });

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
/// * `telemetry` - Where each violation is recorded; only written when
///   `fail_on_nostd` is set, since the optional-dependency sweep is a download
///   pass and its verdicts are not the tree's.
/// # Returns
/// Whether every dependency reached supports no_std, and the maximum depth
/// tested. The depth can be less than the requested one when there are no more
/// dependencies to check.
///
/// The first dependency that is not no_std ends the traversal, as it always
/// did — the verdict is decided at that point and the caller exits the run on
/// it. What is new is that the offender is recorded (`telemetry
/// .dep_not_no_std_deps`) before returning, so the verdict names the crate,
/// its parent and the depth instead of being an anonymous `false`.
pub fn determine_n_depth_dep_no_std(
    initlist: TupleVec,
    depth: u32,
    current_depth: u32,
    visited: &mut HashSet<(String, String)>,
    ctx: &z3::Context,
    main_name: &str,
    fail_on_nostd: bool,
    telemetry: &mut Telemetry,
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
        // Bound before the loop: `version` is shadowed inside it by the
        // dependency's own version.
        let parent = format!("{}:{}", name, version);
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
                Some(&parent),
            ) {
                Ok(name_with_version) => name_with_version,
                Err(e) => {
                    debug!("Failed to download crate: {}", e);
                    if fail_on_nostd {
                        telemetry
                            .deps_download_failed
                            .push(format!("{}:{}", dep_name, dep_version));
                    }
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

            if fail_on_nostd {
                match no_std_evidence(&name_with_version, ctx, None, Some(main_name)) {
                    NoStdEvidence::Absent => {
                        debug!(
                            "ERROR: Dependency {} of dependency {} does not support no_std build at depth {}",
                            name_with_version, name, current_depth
                        );
                        telemetry.dep_not_no_std_deps.push(DepNoStdFailure {
                            dep: name_with_version.clone(),
                            parent: parent.clone(),
                            depth: current_depth + 1,
                        });
                        // Decided: no root-manifest edit makes this tree
                        // no_std, and the caller ends the run here. Walking
                        // the rest of a transitive tree only to report more
                        // offenders is not worth what it downloads.
                        return (false, current_depth);
                    }
                    NoStdEvidence::NoSources => {
                        debug!(
                            "Dependency {} of dependency {} could not be parsed at depth {}",
                            name_with_version, name, current_depth
                        );
                        telemetry
                            .deps_no_sources_parsed
                            .push(name_with_version.clone());
                        // Unknown, not std: keep walking its subtree.
                    }
                    NoStdEvidence::Supported => {}
                }
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
        telemetry,
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
            (equation, parsed) = parse_meta_for_cfg_attr(&attr.meta, ctx, None);
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
    parse_meta_for_cfg_attr(&attr.meta, ctx, None)
}

/// As `parse_main_attributes_direct`, but erases `feature = "X"` atoms naming
/// features Cargo cannot enable for this crate.
///
/// Such a cfg is only ever satisfied by something outside the feature system —
/// in practice a build script emitting `cargo:rustc-cfg=feature="X"` based on
/// the target (blst does this for `std` on any non-embedded target). Modelling
/// X as a solver variable is wrong twice over: the gate is reported unguarded
/// because negating X never removes the code, and X leaks into `--features`,
/// where cargo rejects it outright ("does not have the feature `std`").
///
/// Erasing it instead reuses the bucket-G treatment of non-feature cfgs: the
/// atom becomes a constant, so a gate naming only undeclared features is
/// externally gated, and a mixed gate projects onto the features that remain.
///
/// Safe by construction — if nothing ever set X the gated code would be dead
/// and emit no HIR records, so this only takes effect where X is genuinely
/// forced on from outside.
pub fn parse_main_attributes_direct_with<'a>(
    attr: &Attribute,
    ctx: &'a z3::Context,
    known_features: Option<&HashSet<String>>,
) -> (Option<Bool<'a>>, ParsedAttr) {
    parse_meta_for_cfg_attr(&attr.meta, ctx, known_features)
}

/// The negated cfg of a `#[cfg(C)] compile_error!(…)` as a solver constraint —
/// or `None` when `C` names an atom that policy G erases.
///
/// Erasure drops the operand from its combinator, so an erased atom reads as
/// **false** inside `any(…)` and **true** inside `all(…)`. Both directions push
/// `C` toward true, which is the safe side wherever `C` gates code — and the
/// wrong side here, because the constraint this position emits is `¬C`. A `C`
/// made more likely makes `¬C` forbid configurations that build perfectly well:
///
/// ```text
/// #[cfg(not(any(feature = "std", error_in_core)))] compile_error!(…)   // miden-thiserror
///   error_in_core erased out of the or  ⇒  C = not(feature="std")
///   constraint ¬C = (not (not (or std)))  ⇒  "std is mandatory"
///
/// #[cfg(all(not(feature = "std"), not(target_family = "wasm")))] compile_error!(…)  // midenc-hir-symbol
///   not(target_family="wasm") erased out of the and  ⇒  C = and(not std)
///   constraint ¬C = (not (and (not std)))  ⇒  "std is mandatory"
/// ```
///
/// Both crates build clean with `--no-default-features` (aarch64-unknown-none
/// and wasm32v1-none respectively), yet every std-off covering seed was skipped
/// as "unsatisfiable with hard constraints", so no std-off run ever survived and
/// `classify_spans` marked every std span `AlwaysStd`.
///
/// There is no sound repair by re-assigning the atom: the opposite choice makes
/// `¬C` unsatisfiable outright. An erased atom is *unknown*, and under a
/// negation an unknown cannot be given a truth value. Dropping the constraint
/// costs at most a feature set that fails to compile, which the CEGAR loop
/// already handles; keeping it costs the crate.
///
/// `known_features` is threaded through so a `feature = "X"` Cargo cannot enable
/// (bucket I) counts as erased here too — it becomes a constant by exactly the
/// same mechanism.
pub fn compile_error_constraint<'a>(
    negated_attr: &Attribute,
    ctx: &'a z3::Context,
    known_features: Option<&HashSet<String>>,
) -> Option<Bool<'a>> {
    let (eq, parsed) = parse_meta_for_cfg_attr(&negated_attr.meta, ctx, known_features);
    if !parsed.constants.is_empty() {
        debug!(
            "Dropping compile_error constraint {:?}: cfg names erased atom(s) {:?}, \
             so its negation cannot be modelled soundly",
            eq.as_ref().map(|e| e.to_string()),
            parsed.constants
        );
        return None;
    }
    eq
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

/// Transitively close a set of enabled feature names over the crate's own
/// `[features]` table, so that `blst = ["blstrs_plus"]` marks `blstrs_plus`
/// enabled once `blst` is.
///
/// Only plain feature-to-feature links are followed — the same rule
/// `feature_implication_constraints` uses. `dep:foo` suppresses the implicit
/// feature, and `dep/feat` references a *dependency's* feature, which is not a
/// name a `compile_error!` in this crate can be testing.
pub fn close_over_local_features(
    enabled: &HashSet<String>,
    features: &[(String, TupleVec)],
) -> HashSet<String> {
    let mut closed = enabled.clone();
    loop {
        let mut grew = false;
        for (feat_name, deps) in features {
            if !closed.contains(feat_name) {
                continue;
            }
            for (dep_name, qualifier) in deps {
                if qualifier != "dep:" && dep_name == qualifier && closed.insert(dep_name.clone()) {
                    grew = true;
                }
            }
        }
        if !grew {
            return closed;
        }
    }
}

/// Does enabling `feat` turn on a feature the crate's no_std condition forbids?
///
/// Reachability, not membership. `dlopen-rs-0.7.3` never passes `std` on the
/// command line — it passes `debug` and `tls`, and its own `[features]` table
/// says `debug = ["std"]`, `tls = ["std"]`. A feature that turns `std` on is
/// `std` as far as the crate root is concerned.
pub fn reaches_forbidden_feature(
    crate_info: &CrateInfo,
    feat: &str,
    forbidden: &[String],
) -> bool {
    if forbidden.is_empty() {
        return false;
    }
    let closed = close_over_local_features(
        &HashSet::from([feat.to_string()]),
        &crate_info.features,
    );
    forbidden.iter().any(|f| closed.contains(f))
}

/// Make the emitted feature selection satisfy the crate's own no_std condition.
///
/// R31-3: a solve is not allowed to finish while the crate root still evaluates
/// to std. `solver::no_std_forced_features` reads what the author's
/// `#![cfg_attr(<cond>, no_std)]` *entails*; this applies it to the selection
/// that is about to become the command line, after every pass that could have
/// moved a feature since the solve:
///
/// * a **forbidden** feature comes off `main_features` and `enable`, and if it is
///   reachable from `default` the defaults come off with it — leaving it in
///   `default` while dropping it from `--features` turns nothing off. nuuid
///   0.5.0 is the case: the solve chose `¬std`, then the first dependency pass
///   recomputed "the default features this dep does not disable" from `default =
///   ["getrandom", "std"]` and handed `std` straight back, so the crate built
///   with `--features std` and rustc answered *`std` is required by `nuuid`
///   because it does not declare `#![no_std]`*.
/// * a **required** feature goes back on. It is the feature that carries the
///   attribute — `lasso`'s `no-std`, `robust`/`utm`'s `no_std` — and without it
///   the crate is std no matter what else the selection says.
///
/// Returns `(added, removed)`, both empty when the selection already agreed with
/// the condition — which is the normal case, and the only case for a crate that
/// builds today.
///
/// This runs after the dependency passes, so a feature added back here links an
/// optional dependency nothing analysed. That is the deliberate trade: an
/// unanalysed dependency *might* not build, a crate that is still std certainly
/// does not. It should also be rare — the entailed-true features are
/// non-minimalizable from `process_crate` onwards, so the ordinary path keeps
/// them all the way here and this adds nothing.
pub fn enforce_no_std_polarity(
    crate_info: &CrateInfo,
    main_features: &mut Vec<String>,
    enable: &mut Vec<String>,
    disable_default: &mut bool,
    required: &[String],
    forbidden: &[String],
) -> (Vec<String>, Vec<String>) {
    let reaches_forbidden = |feat: &String| reaches_forbidden_feature(crate_info, feat, forbidden);

    let mut removed: Vec<String> = main_features
        .iter()
        .chain(enable.iter())
        .filter(|f| reaches_forbidden(f))
        .cloned()
        .collect();
    removed.sort();
    removed.dedup();
    main_features.retain(|f| !reaches_forbidden(f));
    enable.retain(|f| !reaches_forbidden(f));

    // Cargo enables the whole `default` closure unless told otherwise, so a
    // forbidden feature anywhere in it survives every list edit above.
    if !*disable_default
        && (solver::disable_in_default(crate_info, forbidden)
            || solver::disable_in_default_indirect(crate_info, forbidden))
    {
        debug!(
            "Turning defaults off: {:?} is reachable from `default` and the crate's no_std condition forbids it",
            forbidden
        );
        *disable_default = true;
        // The rest of `default` is not forbidden and was on until now; keep it.
        for feat in solver::features_not_disabled(crate_info, forbidden) {
            if !main_features.contains(&feat) {
                main_features.push(feat);
            }
        }
        // Report only what turning the defaults off actually took away, so the
        // log line and `no_std_polarity_restored` name the real repair.
        let default_closure = close_over_local_features(
            &HashSet::from(["default".to_string()]),
            &crate_info.features,
        );
        removed.extend(
            forbidden
                .iter()
                .filter(|f| default_closure.contains(*f))
                .cloned(),
        );
        removed.sort();
        removed.dedup();
    }

    let mut added: Vec<String> = Vec::new();
    for feat in required {
        if !main_features.contains(feat) {
            main_features.push(feat.clone());
            added.push(feat.clone());
        }
        if !enable.contains(feat) {
            enable.push(feat.clone());
        }
    }

    (added, removed)
}

/// Check a concrete, final feature set against every `compile_error!` constraint
/// the crate declares, and return the ones it violates (rendered for logging).
///
/// This is the closed-world counterpart to the stage-2 check inside
/// `process_crate`. That one asserts only the *enabled* features as true and
/// leaves every other feature free, so a disjunctive constraint like uom's
/// "at least one storage type" is trivially satisfiable and the check never
/// fires — it could not detect bulletproofs-bls shipping a feature set with
/// neither `rust` nor `blst`. Here a feature the build will not pass to cargo is
/// asserted **false**, which is what actually happens at compile time.
///
/// The closed world is restricted to the atoms appearing in the constraint
/// being checked. Closing over every declared feature would need the implicit
/// per-optional-dependency features cargo synthesises, and nothing outside the
/// constraint's own atoms can change its truth value.
///
/// Runs against the post-minimize feature list, not the solver's `enable`:
/// features re-added by `final_feature_list_main` from `[features] default` are
/// genuinely on, and uom's `f32`/`f64` arrive that way. Checking `enable` alone
/// would report uom as violated when its build is fine.
///
/// # Arguments
/// * `ctx` - The Z3 context
/// * `attrs` - The main crate's attributes, holding the compile_error conditions
/// * `crate_info` - Supplies the `[features]` table for the transitive closure
/// * `enabled` - Features the build passes to cargo via `--features`
/// * `default_features_on` - False when the build passes `--no-default-features`
/// # Returns
/// The violated constraints, rendered; empty when the feature set is consistent.
pub fn violated_compile_error_constraints(
    ctx: &z3::Context,
    attrs: &Attributes,
    crate_info: &CrateInfo,
    enabled: &[String],
    default_features_on: bool,
) -> Vec<String> {
    if attrs.compile_error_attrs.is_empty() {
        return Vec::new();
    }

    let mut on: HashSet<String> = enabled.iter().cloned().collect();
    if default_features_on {
        on.insert("default".to_string());
    }
    let on = close_over_local_features(&on, &crate_info.features);
    debug!("Effective feature set for compile_error check: {:?}", on);

    let mut violated = Vec::new();
    for attr in attrs.compile_error_attrs.iter() {
        let (_, parsed) = parse_main_attributes_direct(attr, ctx);
        // Skipped when the cfg names an erased atom: the constraint we would
        // check against is the over-strong one, so a "violation" here would be
        // an artefact of the erasure, not of the feature set.
        let Some(eq) = compile_error_constraint(attr, ctx, None) else {
            continue;
        };
        let solver = z3::Solver::new(ctx);
        solver.assert(&eq);
        // Close the world over this constraint's own atoms: on => true, off => false.
        for feat in &parsed.features {
            let var = Bool::new_const(ctx, feat.as_str());
            if on.contains(feat) {
                solver.assert(&var);
            } else {
                solver.assert(&var.not());
            }
        }
        if solver.check() != z3::SatResult::Sat {
            violated.push(format!("{:?}", eq));
        }
    }
    violated
}

/// The features to switch on so a feature set stops violating the crate's own
/// `compile_error!` conditions — empty when nothing is violated, and empty when
/// no set of the crate's declared features would satisfy them.
///
/// **This is a repair, not a constraint, and the distinction is the whole
/// design.** A `compile_error!` whose features are disjoint from the no_std
/// condition is deliberately withheld from the feature solve
/// (`excluded_compile_error_eqs` in `process_crate`): uom's is a 21-way
/// `any(feature = "usize", …, "f64")`, and asserting it lets Z3 satisfy it with
/// an arbitrary disjunct — 5 of uom's 21 pull in a dependency that links std, so
/// the constraint that was supposed to protect the build breaks it instead. That
/// filter stays. Rule 10's reason for it (never add a conjunct to a solve whose
/// arbitrary model you then read) is unchanged too.
///
/// What was missing is the other half: when the emitted set *does* violate a
/// constraint, the crate cannot compile — the `compile_error!` is what the
/// compiler stops on — and the run reported it only as a warning. lexical-util
/// 1.0.6 emitted `--no-default-features --features floats` against
///
/// ```ignore
/// #[cfg(all(feature = "floats",
///           not(any(feature = "write-floats", feature = "parse-floats"))))]
/// compile_error!("Do not use the `floats` feature directly. …");
/// ```
///
/// and lost all 26 targets to it, though `--features write-floats` compiles clean
/// on bare metal.
///
/// So the repair is computed here and *applied by a retry* (`bin/main.rs`), the
/// KI-11 shape: only for a build that already failed, and kept only if the
/// rebuild succeeds. A crate that builds today never reaches it, which is what
/// makes this unable to do what asserting the constraint would.
///
/// The world is closed exactly as [`violated_compile_error_constraints`] closes
/// it, and three kinds of atom cannot be part of a repair:
/// * one already on — a repair only *adds*. Turning `floats` back off would
///   satisfy the constraint too, but the feature solve chose it to cover code,
///   and this pass has no standing to overrule that.
/// * one in `forbidden` — the disable list, i.e. the features the no_std verdict
///   turned off. Re-enabling `std` to satisfy a `compile_error!` would trade the
///   build for the property the whole run exists to establish.
/// * one the crate does not declare — a `compile_error!` may test a cfg a build
///   script emits (bucket I), and `--features` naming it makes cargo error out.
///
/// The result is subset-minimal: each candidate is forced off in turn and kept
/// off whenever the system stays satisfiable.
///
/// # Arguments
/// * `ctx` - The Z3 context
/// * `attrs` - The main crate's attributes, holding the compile_error conditions
/// * `crate_info` - Supplies the `[features]` table: the closure, and which
///   names are declared
/// * `enabled` - Features the build passes to cargo via `--features`
/// * `default_features_on` - False when the build passes `--no-default-features`
/// * `forbidden` - Features that must stay off (the no_std disable list)
/// # Returns
/// The features to add, sorted; empty when there is nothing to repair or no
/// repair exists.
pub fn compile_error_repair_features(
    ctx: &z3::Context,
    attrs: &Attributes,
    crate_info: &CrateInfo,
    enabled: &[String],
    default_features_on: bool,
    forbidden: &[String],
) -> Vec<String> {
    // "Is anything violated" is a closed-world question and this function's own
    // solver deliberately leaves the repair candidates free, which would answer
    // it Sat every time. The check the run already trusts is the one to ask.
    if violated_compile_error_constraints(ctx, attrs, crate_info, enabled, default_features_on)
        .is_empty()
    {
        return Vec::new();
    }

    let mut on: HashSet<String> = enabled.iter().cloned().collect();
    if default_features_on {
        on.insert("default".to_string());
    }
    let on = close_over_local_features(&on, &crate_info.features);

    let declared: HashSet<&str> = crate_info
        .features
        .iter()
        .map(|(name, _)| name.as_str())
        .collect();
    let forbidden: HashSet<&str> = forbidden.iter().map(String::as_str).collect();

    let solver = z3::Solver::new(ctx);
    let mut candidates: Vec<String> = Vec::new();
    let mut constrained = false;
    for attr in attrs.compile_error_attrs.iter() {
        let (_, parsed) = parse_main_attributes_direct(attr, ctx);
        // Same skip as the check: a constraint over an erased atom is the
        // over-strong one, so neither its violation nor its repair is real.
        let Some(eq) = compile_error_constraint(attr, ctx, None) else {
            continue;
        };
        solver.assert(&eq);
        constrained = true;
        for feat in &parsed.features {
            let var = Bool::new_const(ctx, feat.as_str());
            if on.contains(feat) {
                solver.assert(&var);
            } else if forbidden.contains(feat.as_str()) || !declared.contains(feat.as_str()) {
                solver.assert(&var.not());
            } else if !candidates.contains(feat) {
                candidates.push(feat.clone());
            }
        }
    }
    // No constraint survived the erasure skip, or every atom that could repair
    // one is pinned: nothing to offer.
    if !constrained || candidates.is_empty() {
        return Vec::new();
    }

    candidates.sort();
    // Subset-minimal: force each candidate off and keep it off if the system is
    // still satisfiable. A rejected push is popped immediately, so the stack
    // stays LIFO-consistent.
    for feat in &candidates {
        solver.push();
        solver.assert(&Bool::new_const(ctx, feat.as_str()).not());
        if solver.check() != z3::SatResult::Sat {
            solver.pop(1);
        }
    }
    if solver.check() != z3::SatResult::Sat {
        debug!("No set of declared features satisfies the compile_error constraints");
        return Vec::new();
    }
    let Some(model) = solver.get_model() else {
        return Vec::new();
    };

    let mut additions: Vec<String> = candidates
        .into_iter()
        .filter(|feat| {
            model
                .eval(&Bool::new_const(ctx, feat.as_str()), true)
                .and_then(|v| v.as_bool())
                == Some(true)
        })
        .collect();
    additions.sort();
    additions.dedup();

    // The model satisfies the constraints one at a time; the authoritative check
    // is the one the run already trusts, over the whole set with the closure
    // applied. A repair that does not pass it is not offered.
    let repaired: Vec<String> = enabled.iter().chain(additions.iter()).cloned().collect();
    if !violated_compile_error_constraints(ctx, attrs, crate_info, &repaired, default_features_on)
        .is_empty()
    {
        debug!("Candidate compile_error repair {:?} does not clear the check", additions);
        return Vec::new();
    }
    additions
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
            (temp_eq, parsed) = parse_meta_for_cfg_attr(&attr.meta, ctx, None);
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

/// Every dependency this manifest declares an edge for, as crate names with
/// `-`/`_` folded to `-`.
///
/// Covers the renamed form (`foo = { package = "bar" }` yields both, because a
/// record can name either) and the target-specific and build sections, so
/// "the manifest has no edge to this crate" means it over the whole manifest.
/// Dev-dependencies count: they are edges the manifest owns even though the
/// no_std build never uses them, and reporting one as unreachable would be
/// wrong.
pub fn declared_dependency_crate_names(manifest_toml: &toml::Value) -> HashSet<String> {
    const SECTIONS: [&str; 3] = ["dependencies", "build-dependencies", "dev-dependencies"];
    let mut names = HashSet::new();
    let mut collect = |table: Option<&toml::Value>| {
        let Some(table) = table.and_then(|t| t.as_table()) else {
            return;
        };
        for (key, value) in table {
            names.insert(key.replace('_', "-"));
            if let Some(package) = value.get("package").and_then(|p| p.as_str()) {
                names.insert(package.replace('_', "-"));
            }
        }
    };
    for section in SECTIONS {
        collect(manifest_toml.get(section));
    }
    if let Some(targets) = manifest_toml.get("target").and_then(|t| t.as_table()) {
        for cfg in targets.values() {
            for section in SECTIONS {
                collect(cfg.get(section));
            }
        }
    }
    names
}

/// The downloaded source directory of the crate named `krate`, whatever its
/// version, looked up beside `manifest`.
///
/// Two layouts, because `analyze_crate` runs for the main crate and for every
/// dependency: a main crate's own dependencies live in `<dir>_deps/`, and a
/// dependency's siblings are its own parent directory. Both are tried, so the
/// caller does not have to know which crate it is looking at.
///
/// The name is folded (`derive-new` the package is `derive_new` the crate) and
/// the version suffix ignored, which is what makes this usable from a record —
/// `PathRecord::expansion_crate` carries a crate name and no version.
pub fn find_sibling_crate_dir(manifest: &str, krate: &str) -> Option<PathBuf> {
    let wanted = krate.replace('_', "-");
    let own_dir = Path::new(manifest).parent()?;
    let deps_dir = PathBuf::from(format!("{}_deps", own_dir.display()));
    let search_dirs = [deps_dir, own_dir.parent()?.to_path_buf()];

    for dir in search_dirs {
        let Ok(entries) = fs::read_dir(&dir) else {
            continue;
        };
        for entry in entries.flatten() {
            let name = entry.file_name().to_string_lossy().replace('_', "-");
            // `<name>-<version>`: split at the last `-` so a hyphenated crate
            // name survives.
            let Some((stem, _version)) = name.rsplit_once('-') else {
                continue;
            };
            if stem == wanted && entry.path().join("Cargo.toml").exists() {
                return Some(entry.path());
            }
        }
    }
    None
}

/// Every dependency edge the manifest declares, as `(key, spec)`.
///
/// The key is the manifest's own name for the edge — what a `feature = ["k/f"]`
/// reference has to match — and the spec is normalised to a table so a plain
/// `foo = "1.0"` and a `foo = { version = "1.0" }` read the same. `dev` edges
/// are excluded: they are not in the build whose constraints are being derived.
pub fn dependency_edges(manifest_toml: &toml::Value) -> Vec<(String, toml::Value)> {
    const SECTIONS: [&str; 2] = ["dependencies", "build-dependencies"];
    let mut edges = Vec::new();
    let mut collect = |table: Option<&toml::Value>| {
        let Some(table) = table.and_then(|t| t.as_table()) else {
            return;
        };
        for (key, value) in table {
            let spec = if value.is_table() {
                value.clone()
            } else {
                toml::Value::Table(toml::map::Map::new())
            };
            edges.push((key.clone(), spec));
        }
    };
    for section in SECTIONS {
        collect(manifest_toml.get(section));
    }
    if let Some(targets) = manifest_toml.get("target").and_then(|t| t.as_table()) {
        for cfg in targets.values() {
            for section in SECTIONS {
                collect(cfg.get(section));
            }
        }
    }
    edges
}

/// A downloaded crate's library entrypoint: the declared `[lib] path`, else
/// `src/lib.rs`. `None` when neither exists.
///
/// Deliberately not `visitor::find_entrypoints`, which shells out to `cargo
/// metadata` — this is called once per dependency of every analysed crate, and
/// the file it is looking for is the crate root, which is where a
/// feature-guarding `compile_error!` sits.
pub fn crate_entry_file(dir: &Path) -> Option<PathBuf> {
    let declared = fs::read_to_string(dir.join("Cargo.toml"))
        .ok()
        .and_then(|text| toml::from_str::<toml::Value>(&text).ok())
        .and_then(|t| {
            t.get("lib")
                .and_then(|lib| lib.get("path"))
                .and_then(|p| p.as_str())
                .map(|p| dir.join(p))
        });
    declared
        .into_iter()
        .chain([dir.join("src/lib.rs")])
        .find(|p| p.exists())
}

/// The dependency's features cargo turns on because of the *edge itself*,
/// whatever the consumer's own features do: the ones named in
/// `features = [...]`, plus the dependency's `default` closure unless the edge
/// says `default-features = false`.
///
/// These are the atoms a dependency-derived constraint must read as `true`
/// rather than looking for a feature of this crate that reaches them.
pub fn edge_supplied_dep_features(
    dep_value: &toml::Value,
    dep_toml: &toml::Value,
) -> HashSet<String> {
    let mut on: HashSet<String> = dep_value
        .get("features")
        .and_then(|f| f.as_array())
        .map(|arr| {
            arr.iter()
                .filter_map(|v| v.as_str())
                .map(str::to_string)
                .collect()
        })
        .unwrap_or_default();

    let defaults_on = dep_value
        .get("default-features")
        .and_then(|v| v.as_bool())
        .unwrap_or(true);
    if defaults_on {
        on.insert("default".to_string());
    }

    let features = crate::downloader::read_local_features(dep_toml);
    close_over_local_features(&on, &features)
}

/// This crate's features that reach `<dep_key>/<dep_feature>`, directly or by
/// enabling another feature that does.
///
/// A weak `dep?/feat` reference is excluded — it enables the sub-feature only
/// when something else already linked the dependency, so it is not on its own a
/// way for this crate to turn that feature on.
pub fn local_features_enabling_dep_feature(
    manifest_toml: &toml::Value,
    dep_key: &str,
    dep_feature: &str,
) -> Vec<String> {
    let Some(features) = manifest_toml.get("features").and_then(|f| f.as_table()) else {
        return Vec::new();
    };
    let target = format!("{}/{}", dep_key, dep_feature);

    let mut reaching: HashSet<String> = HashSet::new();
    loop {
        let mut grew = false;
        for (name, values) in features {
            if reaching.contains(name) {
                continue;
            }
            let Some(arr) = values.as_array() else {
                continue;
            };
            let hit = arr.iter().filter_map(|v| v.as_str()).any(|entry| {
                entry == target || (!entry.contains('/') && reaching.contains(entry))
            });
            if hit && reaching.insert(name.clone()) {
                grew = true;
            }
        }
        if !grew {
            break;
        }
    }
    let mut out: Vec<String> = reaching.into_iter().collect();
    out.sort();
    out
}

/// Whether a downloaded crate directory holds a proc macro. The non-panicking,
/// version-free counterpart to [`is_proc_macro`], which needs `name:version` and
/// `unreachable!`s when the manifest is not where it expects.
pub fn crate_dir_is_proc_macro(dir: &Path) -> bool {
    let Ok(text) = fs::read_to_string(dir.join("Cargo.toml")) else {
        return false;
    };
    let Ok(toml) = toml::from_str::<toml::Value>(&text) else {
        return false;
    };
    toml.get("lib")
        .and_then(|lib| lib.get("proc-macro"))
        .and_then(|v| v.as_bool())
        .unwrap_or(false)
}

/// Which downloaded crates beside `manifest` declare an edge to `krate` — the
/// dependencies through which an unreachable proc macro entered the graph.
///
/// Best effort by design: it answers "who could have brought this in", not
/// "who resolved it", and a manifest that fails to parse is skipped rather than
/// guessed at. Only ever called once a record has already proved the macro ran.
pub fn crate_edge_owners(manifest: &str, krate: &str) -> Vec<String> {
    let wanted = krate.replace('_', "-");
    let Some(own_dir) = Path::new(manifest).parent() else {
        return Vec::new();
    };
    let deps_dir = PathBuf::from(format!("{}_deps", own_dir.display()));
    let search_dir = if deps_dir.is_dir() {
        deps_dir
    } else {
        match own_dir.parent() {
            Some(parent) => parent.to_path_buf(),
            None => return Vec::new(),
        }
    };

    let Ok(entries) = fs::read_dir(&search_dir) else {
        return Vec::new();
    };
    let mut owners: Vec<String> = entries
        .flatten()
        .filter(|entry| entry.path().join("Cargo.toml").exists())
        .filter(|entry| {
            let Ok(text) = fs::read_to_string(entry.path().join("Cargo.toml")) else {
                return false;
            };
            let Ok(parsed) = toml::from_str::<toml::Value>(&text) else {
                return false;
            };
            declared_dependency_crate_names(&parsed).contains(&wanted)
        })
        .map(|entry| entry.file_name().to_string_lossy().to_string())
        .collect();
    owners.sort();
    owners
}

/// Every optional dependency declared anywhere in `manifest_toml`, named the way the
/// manifest names it.
///
/// Two things this does that reading `CrateInfo::deps_and_features` does not:
///
/// * it returns the **manifest key**, not the package name. `simd = { package =
///   "ppv-lite86", optional = true }` is the feature `simd`, never `ppv-lite86`;
///   `deps_and_features` stores only the package, so every renamed optional dep looks
///   like a name nobody declared.
/// * it covers `[build-dependencies]` and `[target.<cfg>.dependencies]`, which
///   `gather_crate_info` never walks. `mfio-rt`'s `mio` and `io-uring` are optional and
///   target-gated, so they were invisible.
///
/// Both gaps end the same way: the tool declares `<dep> = []` for what was already
/// cargo's implicit feature, which *replaces* that implicit feature with an empty one
/// and leaves the dependency enabled by nothing — "optional dependency `simd` is not
/// included in any feature", manifest rejected (bucket T3, 8 crates).
///
/// `dev-dependencies` are excluded: cargo does not allow them to be optional.
pub fn optional_dep_keys(manifest_toml: &toml::Value) -> HashSet<String> {
    fn collect(table: Option<&toml::Value>, out: &mut HashSet<String>) {
        let Some(table) = table.and_then(|t| t.as_table()) else {
            return;
        };
        for (key, value) in table {
            if value
                .get("optional")
                .and_then(toml::Value::as_bool)
                .unwrap_or(false)
            {
                out.insert(key.clone());
            }
        }
    }

    let mut keys = HashSet::new();
    collect(manifest_toml.get("dependencies"), &mut keys);
    collect(manifest_toml.get("build-dependencies"), &mut keys);

    if let Some(targets) = manifest_toml.get("target").and_then(toml::Value::as_table) {
        for (_cfg, target_table) in targets {
            collect(target_table.get("dependencies"), &mut keys);
            collect(target_table.get("build-dependencies"), &mut keys);
        }
    }

    keys
}

/// Whether any value in `manifest_toml`'s `[features]` table spells `dep:<dep_key>`.
///
/// That spelling is what suppresses cargo's implicit `<dep_key> = ["dep:<dep_key>"]`
/// feature. Absent it the implicit feature exists and must not be redeclared; present
/// it, a reference to `<dep_key>` only resolves if the manifest declares the feature
/// itself.
pub fn features_reference_dep_explicitly(manifest_toml: &toml::Value, dep_key: &str) -> bool {
    let Some(features) = manifest_toml.get("features").and_then(toml::Value::as_table) else {
        return false;
    };
    let spelling = format!("dep:{}", dep_key);
    features.values().any(|values| {
        values
            .as_array()
            .is_some_and(|arr| arr.iter().any(|v| v.as_str() == Some(spelling.as_str())))
    })
}

/// Park one default feature of a proc-macro dependency on the main crate's edge.
///
/// A proc-macro crate is exempt from the no_std walk for a good reason: it is
/// compiled for the *host* and run there, so its own `use std::collections::HashMap`
/// says nothing about the crate being analysed. But its `[features]` are not host-only
/// — they select which tokens it injects into its consumer:
///
/// ```ignore
/// // displaydoc 0.2.6, src/expand.rs — `default = ["std"]`
/// #[cfg(feature = "std")]
/// fn path_specialization() -> TokenStream {
///     quote! {
///         extern crate std;                                  // ← lands in the CONSUMER
///         impl PathToDisplayDoc for std::path::Path { … }     // ← std::path::Display
///     }
/// }
/// #[cfg(not(feature = "std"))]                                // ← emits nothing
/// ```
///
/// Skipping the dependency everywhere therefore left that default on, and every
/// `#[derive(Display)]` in a `#![no_std]` crate got `extern crate std` injected at the
/// derive's span: unguarded std the consumer cannot gate away, on an item it never
/// wrote. Measured on a two-item fixture against `aarch64-unknown-none`: with the
/// default on, one std record at the derive and the crate fails `E0463 can't find
/// crate for std`; with `default-features = false` on that one edge, no std records
/// and it compiles. So it is not only false evidence — the emitted config could not
/// have built either. dfu-core 0.7.0 (4 spans), embedded-exfat 0.2.4 (5) and
/// tftp 0.1.0 (1) are the displaydoc half of it; `sp-api-proc-macro` and
/// `sp-debug-derive` (`#[cfg(feature = "std")]` / `#[cfg(not(…))]` emissions, both
/// `default = ["std"]`) are the same mechanism in the Substrate family.
///
/// **Which default to park is decided by evidence, never by its name** — see
/// `driver::park_injecting_proc_macros`, which calls this once per trial and keeps
/// the trial that made the injected std records disappear without breaking the
/// build. The name test this replaced was wrong in both directions: it missed a
/// macro that spells the feature something else, and — measured over the corpus —
/// it parked the `std` default of 8 of the 115 macros that have one where that
/// feature guards the macro's *own host code*, so the macro stopped compiling and
/// every target build of the consumer failed inside it (bebytes 0.7.1 →
/// bebytes_derive 0.8.1, `use std::vec::Vec` at `src/bit_validation.rs:5`).
///
/// Everything else the macro's author set is left exactly as it is: the parked
/// feature is re-declared in `custom_default_features` like any other removed
/// default, and the remaining defaults are re-declared on the edge — the same
/// policy as `update_main_crate_default_list`, including its refusal to touch an
/// edge whose default list names something a dependency edge cannot name
/// (`dep/feat`, `dep:x`).
///
/// # Arguments
/// * `main_manifest` - Path to the main crate's `Cargo.toml`, which is rewritten
/// * `dep_manifest` - Path to the proc-macro dependency's `Cargo.toml`
/// * `dep_package` - The proc-macro's package name, as its edge names it
/// * `feature` - The default feature to park
/// # Returns
/// Whether the main manifest was rewritten.
///
/// Paths rather than `name:version` throughout: the caller has already resolved
/// them, and taking them here is what lets a fixture drive the whole rule instead
/// of only its manifest arithmetic.
pub fn park_proc_macro_default(
    main_manifest: &str,
    dep_manifest: &str,
    dep_package: &str,
    feature: &str,
) -> bool {
    let Ok(dep_text) = fs::read_to_string(dep_manifest) else {
        debug!("Proc-macro parking: cannot read {}", dep_manifest);
        return false;
    };
    let Ok(dep_toml) = toml::from_str::<toml::Value>(&dep_text) else {
        debug!("Proc-macro parking: cannot parse {}", dep_manifest);
        return false;
    };

    let Ok(main_text) = fs::read_to_string(main_manifest) else {
        debug!("Proc-macro parking: cannot read {}", main_manifest);
        return false;
    };
    let Ok(mut main_toml) = toml::from_str::<toml::Value>(&main_text) else {
        debug!("Proc-macro parking: cannot parse {}", main_manifest);
        return false;
    };

    if !park_proc_macro_default_in_manifest(&mut main_toml, &dep_toml, dep_package, feature) {
        return false;
    }

    if let Err(e) = fs::write(
        main_manifest,
        toml::to_string(&main_toml).expect("Failed to convert Value to string"),
    ) {
        debug!("Proc-macro parking: cannot write {}: {}", main_manifest, e);
        return false;
    }
    true
}

/// The name rustc knows a dependency by, which is its `[lib] name` when it sets one
/// and its package name otherwise.
///
/// The two differ for 5 of the corpus's 1719 proc-macro crates (`ethereum_ssz_derive`
/// builds `ssz_derive`, `sea-strum_macros` builds `strum_macros`), and only the lib
/// name ever appears in a `PathRecord::expansion_crate`. The *package* name stays the
/// right key for the dependency edge, so the two are read separately rather than
/// conflated.
pub fn dep_crate_name(dep_manifest: &str, package: &str) -> String {
    fs::read_to_string(dep_manifest)
        .ok()
        .and_then(|text| toml::from_str::<toml::Value>(&text).ok())
        .and_then(|toml| {
            toml.get("lib")?
                .get("name")?
                .as_str()
                .map(|name| name.to_string())
        })
        .unwrap_or_else(|| package.to_string())
}

/// The default features a proc-macro dependency's manifest declares, in declaration
/// order. The candidate list `driver::park_injecting_proc_macros` probes; empty
/// means there is nothing a consumer's edge could turn off.
pub fn proc_macro_default_features(dep_manifest: &str) -> Vec<String> {
    let Ok(dep_text) = fs::read_to_string(dep_manifest) else {
        return Vec::new();
    };
    let Ok(dep_toml) = toml::from_str::<toml::Value>(&dep_text) else {
        return Vec::new();
    };
    declared_default_features(&dep_toml)
}

fn declared_default_features(dep_toml: &toml::Value) -> Vec<String> {
    dep_toml
        .get("features")
        .and_then(toml::Value::as_table)
        .and_then(|features| features.get("default"))
        .and_then(toml::Value::as_array)
        .map(|values| {
            values
                .iter()
                .filter_map(|v| v.as_str().map(str::to_string))
                .collect()
        })
        .unwrap_or_default()
}

/// The manifest surgery behind `park_proc_macro_default`, on values rather than
/// files. Returns whether `main_toml` was changed.
///
/// Every reason to leave the edge alone lives here: `feature` is not one of the
/// macro's defaults (nothing to park), its default list names something an edge
/// cannot name, or the main manifest has no table entry for it.
pub fn park_proc_macro_default_in_manifest(
    main_toml: &mut toml::Value,
    dep_toml: &toml::Value,
    dep_package: &str,
    feature: &str,
) -> bool {
    let defaults = declared_default_features(dep_toml);

    let to_park: Vec<String> = defaults
        .iter()
        .filter(|f| f.as_str() == feature)
        .cloned()
        .collect();
    if to_park.is_empty() {
        return false;
    }

    let keep_on_edge: Vec<String> = defaults
        .iter()
        .filter(|f| f.as_str() != feature)
        .cloned()
        .collect();
    if keep_on_edge.iter().any(|f| !is_own_feature_name(f)) {
        debug!(
            "Proc-macro {} has non-local entries in its default list ({:?}); leaving the edge alone",
            dep_package, defaults
        );
        return false;
    }

    let Some(edge_key) = proc_macro_edge_key(main_toml, dep_package) else {
        debug!(
            "Proc-macro {} has no dependency table entry to park on",
            dep_package
        );
        return false;
    };

    let Some(toml::Value::Table(edge)) = main_toml
        .get_mut("dependencies")
        .and_then(toml::Value::as_table_mut)
        .and_then(|deps| deps.get_mut(&edge_key))
    else {
        return false;
    };

    edge.insert("default-features".to_string(), toml::Value::Boolean(false));
    if !keep_on_edge.is_empty() {
        let edge_feats = edge
            .entry("features".to_string())
            .or_insert_with(|| toml::Value::Array(Vec::new()));
        if let Some(arr) = edge_feats.as_array_mut() {
            for feat in &keep_on_edge {
                if !arr.iter().any(|v| v.as_str() == Some(feat.as_str())) {
                    arr.push(toml::Value::String(feat.clone()));
                }
            }
        }
    }

    debug!(
        "Proc-macro {}: parking default(s) {:?}, keeping {:?} on the edge",
        dep_package, to_park, keep_on_edge
    );
    let parked: Vec<String> = to_park
        .iter()
        .map(|feat| format!("{}/{}", edge_key, feat))
        .collect();
    add_feats_to_custom_feature(main_toml, consts::CUSTOM_FEATURES_DISABLED, &parked);
    true
}

/// The order `driver::park_injecting_proc_macros` tries a macro's defaults in.
///
/// **Ordering only — it decides nothing.** A trial is accepted because the injected
/// std records went away and the crate still compiled, so a wrong order costs builds
/// and never a wrong answer. It is worth having because the answer is a `std`-named
/// feature almost every time: measured over all 1719 distinct proc-macro crates in
/// the corpus, a `#[cfg]`-gated std-mentioning region whose gate names one of the
/// macro's own defaults is spelled `std` at 343 sites and `use_std` at exactly one
/// (`bf-impl`), so trying those first usually settles it in one build.
///
/// This is what is left of the name *test* that used to decide the parking outright.
/// Note that `alloc` is no longer special-cased away: under the old rule excluding it
/// was a guess about intent, and under this one a macro's `alloc` default is parked
/// only if turning it off is what removes std from the consumer.
pub fn std_feature_name_first(feature: &str) -> bool {
    matches!(feature, "std" | "use_std" | "use-std")
}

/// The key under `[dependencies]` that carries a package, following a rename.
///
/// The edge is keyed by the *dependency name*, which is the package name unless the
/// manifest renames it (`[dependencies.foo] package = "bar"`), and cargo's published
/// manifests normalise `_`/`-` inconsistently between the dependency list and the
/// package name. Only table entries are considered: a bare `foo = "1"` cannot carry
/// `default-features`, and rewriting it into a table is a manifest change with a
/// blast radius of its own.
fn proc_macro_edge_key(main_toml: &toml::Value, dep_package: &str) -> Option<String> {
    let deps = main_toml.get("dependencies")?.as_table()?;
    let renamed = deps.iter().find(|(_, value)| {
        value
            .as_table()
            .and_then(|t| t.get("package"))
            .and_then(toml::Value::as_str)
            == Some(dep_package)
    });
    if let Some((key, _)) = renamed {
        return Some(key.clone());
    }
    [
        dep_package.to_string(),
        dep_package.replace('-', "_"),
        dep_package.replace('_', "-"),
    ]
    .into_iter()
    .find(|candidate| deps.get(candidate).is_some_and(toml::Value::is_table))
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
/// * `entailed_false` - The dependency's features that provably cannot be on if it is
///   to be no_std (`solver::solve`'s fourth return value). Only these are parked; the
///   dependency's other defaults are re-declared on the edge.
/// # Returns
/// None
fn update_main_crate_default_list(
    main: &str,
    dep: &str,
    crate_name_rename: &[(String, String)],
    entailed_false: &[String],
) {
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
    let dep_toml: toml::Value =
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
        .get("features")
        .and_then(|v| v.as_table())
        .expect("Failed to get features table from dependency Cargo.toml");

    let dep_defaults: Vec<String> = dep_features
        .get("default")
        .and_then(|v| v.as_array())
        .map(|v| {
            v.iter()
                .filter_map(|f| f.as_str().map(str::to_string))
                .collect()
        })
        .unwrap_or_default();

    // A dependency's own `default` list does not only name its own features: it can
    // name a feature of *its* dependencies (`regex`'s default carries
    // `regex-syntax/default`) or turn on an optional dep (`dep:foo`). Prefixing those
    // yields `regex/regex-syntax/default`, which cargo refuses outright — "multiple
    // slashes in feature ... are not allowed" — and it refuses the whole manifest with
    // it, so the analysis never gets tested (bucket T3, 7 crates: `matchable`,
    // `binator_nom`, `ryml`, `odem-rs`, …). A transitive feature cannot be named from
    // the main manifest at all, so when any entry is out of reach fall back to the one
    // value that means exactly "whatever this dependency's defaults are".
    // Of the defaults that *are* nameable, only the ones the dependency's own solve
    // entailed false have any business being turned off here. Parking the rest is a
    // deletion, not a move: `custom_default_features` is on no path from `default`.
    // `afe4404` lost uom's whole `default = ["autoconvert", "f32", "f64", "si",
    // "std"]` that way and left uom with no storage type at all, tripping its own
    // `compile_error!` (bucket T4) — where only `std` was ever the problem. The rest
    // are re-declared on the edge, so the dependency keeps behaving as its author's
    // defaults said it would while `default-features = false` still takes std out.
    let has_unreachable_default = dep_defaults.iter().any(|f| !is_own_feature_name(f));
    let (to_park, keep_on_edge): (Vec<String>, Vec<String>) = if has_unreachable_default {
        (Vec::new(), Vec::new())
    } else {
        dep_defaults
            .iter()
            .cloned()
            .partition(|f| entailed_false.contains(f))
    };

    let dep_default_features: Vec<String> = if has_unreachable_default {
        debug!(
            "Dependency {} has non-local entries in its default list ({:?}); parking it as {}/default",
            dep_name, dep_defaults, dep_name
        );
        vec![format!("{}/default", dep_name)]
    } else {
        to_park
            .iter()
            .map(|f| format!("{}/{}", dep_name, f))
            .collect()
    };

    if !keep_on_edge.is_empty() {
        debug!(
            "Dependency {}: keeping defaults {:?} the solve did not forbid on the edge, parking {:?}",
            dep_name, keep_on_edge, to_park
        );
        if let Some(toml::Value::Table(table)) = main_toml
            .get_mut("dependencies")
            .and_then(|v| v.as_table_mut())
            .and_then(|deps| deps.get_mut(dep_name))
        {
            let edge_feats = table
                .entry("features".to_string())
                .or_insert_with(|| toml::Value::Array(Vec::new()));
            if let Some(arr) = edge_feats.as_array_mut() {
                for f in &keep_on_edge {
                    if !arr.iter().any(|v| v.as_str() == Some(f.as_str())) {
                        arr.push(toml::Value::String(f.clone()));
                    }
                }
            }
        }
    }

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

    // Prefix with the *manifest key*, never the package name. Cargo resolves a
    // `<dep>/<feat>` value against the keys of the dependency tables, so a renamed
    // dependency — `hex = { package = "hex-conservative" }` — can only ever be named
    // `hex/alloc`; `hex-conservative/alloc` is *"feature `custom_no_std_feature_enabled`
    // includes `hex-conservative/alloc`, but `hex-conservative` is not a dependency"*,
    // and cargo refuses the whole manifest, so nothing about the analysis behind it is
    // ever tested (bucket R31-1, 17 crates — every one of them a renamed dep:
    // `chf`/`groestlcoin*` → `hex`, `serde-feature-hack` → `real_serde`, `ovmi`/
    // `hydra-dx-math` → `codec`, `sapio-bitcoin` → `secp256k1`, …). `dep_name` is
    // already the key the table lookup above used; the two lists just did not use it.
    let formatted_feats_to_move: Vec<String> = feats_to_move
        .iter()
        .map(|f| format!("{}/{}", dep_name, f))
        .collect();

    let formatted_feats_to_add: Vec<String> = feats_to_add
        .iter()
        .map(|f| format!("{}/{}", dep_name, f))
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

/// Whether `value` names a feature of the crate whose `[features]` table it appears
/// in, as opposed to something belonging to one of that crate's dependencies —
/// `otherdep/feat`, `otherdep?/feat`, `dep:otherdep`.
///
/// Used when a value is about to be re-prefixed with a dependency name: only a
/// crate's *own* feature names survive that.
fn is_own_feature_name(value: &str) -> bool {
    !value.contains('/') && !value.starts_with("dep:")
}

/// Every dependency key the manifest declares, in every table cargo resolves a feature
/// value against: `[dependencies]`, `[build-dependencies]`, `[dev-dependencies]` and
/// the `[target.<cfg>]` form of each. Dev-dependencies are in the set because cargo
/// accepts `<dev-dep>/<feat>` in a feature value — verified against cargo directly,
/// not assumed.
///
/// Keyed the way the manifest keys them, which is the only name a feature value may
/// use: for `hex = { package = "hex-conservative" }` this yields `hex`.
pub fn declared_dep_keys(manifest_toml: &toml::Value) -> HashSet<String> {
    fn collect(table: Option<&toml::Value>, out: &mut HashSet<String>) {
        let Some(table) = table.and_then(|t| t.as_table()) else {
            return;
        };
        out.extend(table.keys().cloned());
    }

    const TABLES: [&str; 3] = ["dependencies", "build-dependencies", "dev-dependencies"];

    let mut keys = HashSet::new();
    for table in TABLES {
        collect(manifest_toml.get(table), &mut keys);
    }

    if let Some(targets) = manifest_toml.get("target").and_then(toml::Value::as_table) {
        for target_table in targets.values() {
            for table in TABLES {
                collect(target_table.get(table), &mut keys);
            }
        }
    }

    keys
}

/// The dependency `value` names, if it names one: `dep/feat`, `dep?/feat` and
/// `dep:name` all do, a bare feature name does not.
fn feature_value_dep(value: &str) -> Option<&str> {
    match value.split_once('/') {
        Some((dep, _)) => Some(dep.strip_suffix('?').unwrap_or(dep)),
        None => value.strip_prefix("dep:"),
    }
}

/// Whether cargo will accept `value` as an entry of a `[features]` array of the
/// manifest `declared_deps` was taken from.
///
/// Two independent ways to be refused, and neither is a wrong-but-buildable choice —
/// cargo rejects the manifest before resolving anything, so the emitted configuration
/// is never even tried:
///
/// * **Grammar.** A feature value is `feat`, `dep/feat`, `dep?/feat` or `dep:name` —
///   at most one slash, and no `dep:` on the right of one.
/// * **Reference.** The dependency a value names has to be one the manifest declares,
///   under the key the manifest declares it by. This is the half the T3 fix left out,
///   and it is what `custom_no_std_feature_enabled` was failing on: `hex-conservative`
///   is a perfectly well-formed name for a dependency this manifest calls `hex`.
fn is_valid_feature_value(value: &str, declared_deps: &HashSet<String>) -> bool {
    if let Some((dep, feat)) = value.split_once('/')
        && (feat.contains('/') || feat.is_empty() || feat.starts_with("dep:") || dep.is_empty())
    {
        return false;
    }

    match feature_value_dep(value) {
        Some(dep) => declared_deps.contains(dep),
        None => true,
    }
}

/// Given a toml::Value representing the main Cargo.toml,
/// a feature name, and a list of features to add,
/// this function adds the features to the specified feature.
/// If the feature does not exist, it creates it.
/// If the feature already exists, it appends the new features to it.
///
/// Values cargo would refuse are dropped here rather than written out. This is the
/// single funnel for every custom-feature write, and one bad value costs the whole
/// manifest, so the trade is deliberate: a lossy parking list still builds, a rejected
/// manifest builds nothing. Anything dropped here is a producer bug — it is logged as
/// such, not silently swallowed.
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
    let declared_deps = declared_dep_keys(main_toml);
    let feats_to_add: Vec<String> = feats_to_add
        .iter()
        .filter(|f| {
            if is_valid_feature_value(f, &declared_deps) {
                return true;
            }
            log::warn!(
                "Refusing to write feature value {:?} into `{}`: cargo would reject the manifest",
                f,
                custom_feat
            );
            false
        })
        .cloned()
        .collect();
    let feats_to_add = feats_to_add.as_slice();

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
    features_for_optional_deps_with(crate_info, &optional_deps)
}

/// `features_for_optional_deps` against a caller-supplied set of optional
/// dependencies, for callers that know of optional deps `crate_info` does not.
/// `gather_crate_info` only walks `[dependencies]`, so a dep declared under
/// `[target.'cfg(…)'.dependencies]` is absent from `deps_and_features` even
/// though its implicit feature is perfectly real.
pub fn features_for_optional_deps_with(
    crate_info: &CrateInfo,
    optional_deps: &[String],
) -> TupleVec {
    let optional_deps = optional_deps.to_vec();
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

    // The loop above can only see features the manifest declares. Cargo also
    // synthesises one feature per optional dependency (see
    // `is_implicit_optional_dep_feature`), and that feature is a legal
    // `cfg(feature = "…")` atom — rand_core's
    // `#[cfg(all(feature = "getrandom", not(feature = "std")))]` makes
    // `getrandom` a solver variable, and the solver switches it on because it is
    // compatible with `not(std)`. Without a pair here `minimize` has no way to
    // learn the feature exists only to pull in a dependency, so it survives into
    // the enable list and drags the dependency into a build that never needed it.
    for dep_name in &optional_deps {
        if is_implicit_optional_dep_feature(dep_name, crate_info, &optional_deps) {
            result.push((dep_name.clone(), dep_name.clone()));
        }
    }

    result.sort();
    result.dedup();
    result
}

/// Returns `true` when `feat_name` is the feature Cargo synthesises for an
/// optional dependency of the same name, rather than one the manifest declares.
///
/// Cargo creates an implicit `foo = ["dep:foo"]` for every optional dependency
/// `foo`, unless the manifest either declares a `[features] foo` entry of its own
/// or references the dependency as `dep:foo` somewhere in the feature table —
/// either suppresses the implicit feature. Enabling it does exactly one thing:
/// pull in the dependency.
fn is_implicit_optional_dep_feature(
    feat_name: &str,
    crate_info: &CrateInfo,
    optional_deps: &[String],
) -> bool {
    if !optional_deps.iter().any(|d| d == feat_name) {
        return false;
    }
    let declared = crate_info
        .features
        .iter()
        .any(|(name, _)| name == feat_name);
    let suppressed = crate_info.features.iter().any(|(_, values)| {
        values
            .iter()
            .any(|(k, v)| k == feat_name && v.as_str() == "dep:")
    });
    !declared && !suppressed
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
/// * `deps_to_keep` - `driver::deps_pinned_by_active_use`: the optional dependencies
///   the crate still *names* under a cfg that severing the enabler would leave true.
///   Severing one of those is what R31-2 is — see below.
/// * `features_forced_off` - Out: features this pass turned off in the manifest and
///   that the caller must therefore drop from the command line too.
/// # Returns
/// A boolean indicating whether the dependency should be skipped.
pub fn should_skip_dep(
    name: &str,
    exchange: &mut DataExchange,
    deps_and_features: &mut TupleVec,
    enable_features: &[String],
    disable_default: bool,
    second_round: bool,
    deps_to_keep: &HashSet<String>,
    features_forced_off: &mut Vec<String>,
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
        // Severing is a manifest edit made *because* the answer is negative, so
        // it needs the strong form of the answer: a crate whose sources never
        // parsed has not been shown to be std-only, and cutting the features
        // that link it would remove a dependency the crate still imports.
        let evidence = no_std_evidence(name, &ctx, None, Some(&exchange.name_with_version));
        if evidence == NoStdEvidence::NoSources {
            exchange
                .telemetry
                .deps_no_sources_parsed
                .push(name.to_string());
        }

        debug!(
            "Dependency: {} is enabled by features: {:?} and currently enabled list enabled {:?} from that list",
            dep_name, feats_of_dep, features_for_dependency
        );

        if evidence == NoStdEvidence::Absent {
            debug!(
                "Dependency {} does not support no_std. Creating a new feature and adding the conflicting features to it",
                dep_name
            );
            let main_name = &format!(
                "{}:{}",
                exchange.crate_info.name, exchange.crate_info.version
            );
            // Severing takes the dependency out of the feature that enables it and
            // leaves that feature *on*. Sound only while nothing the feature gates
            // still names the dependency — and for a dependency in the pin set,
            // something does: `bromberg_sl2-0.6.0` emitted `default = ["std"]`,
            // `std = []` over `#[cfg(feature = "std")] use rayon::prelude::*`, i.e.
            // `E0433 rayon` on all 26 targets (R31-2, 7 crates).
            //
            // The dependency cannot stay either — it is not no_std — so the feature
            // that enables it is the thing that has to go off. Removing *it* from
            // whatever enables it is the same edit one level up, and it takes the
            // code that names the dependency out of the build along with the
            // dependency itself. Nothing is deleted: the entry is parked in
            // `custom_default_features` like every other, so a std build restores it.
            let target = if deps_to_keep.contains(&dep_name) {
                let leaf = features_for_dependency
                    .iter()
                    .find_map(|feat| {
                        find_direct_dep_enabler(
                            feat,
                            &dep_name,
                            &exchange.crate_info,
                            &mut HashSet::new(),
                        )
                    })
                    .unwrap_or_else(|| dep_name.clone());
                debug!(
                    "Dependency {} is still named under a cfg that severing would leave true; \
                     turning off the feature that enables it ('{}') instead",
                    dep_name, leaf
                );
                // The caller's `--features` list can name it directly (kitoken's
                // `multiversion`), in which case there is no manifest entry to cut
                // and dropping it from the command line is the whole edit.
                features_forced_off.push(leaf.clone());
                leaf
            } else {
                dep_name.clone()
            };
            let severed = remove_feats_enabling_dep(main_name, &features_for_dependency, &target);
            // A feature named directly on the command line has no manifest entry to
            // cut, so `severed` is empty and the edit is real all the same.
            if !severed.is_empty() || target != dep_name {
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
            debug!("Dependency {} is kept: {:?}", dep_name, evidence);
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
    let _t = crate::timing::scope("recursive_req_check", &exchange.name_with_version);
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
            // Optional sub-deps are never fetched by the download phase
            // (determine_n_depth_dep_no_std reads deps with skip_optional=true), so they
            // are not on disk. Skip them up front, before doing any resolution work.
            if is_dep_optional(&crate_info, &dep.name) {
                debug!(
                    "Dependency: {} of {} is optional, skipping requirement check",
                    dep.name, name_with_version
                );
                continue;
            }

            // Resolve the version exactly the way the download phase did: from the main
            // crate's Cargo.lock (index fallback), so it matches what is on disk instead
            // of re-deriving a possibly-newer version from a live index.
            let dep_resolved_version = match downloader::resolve_dep_version(
                &dep.name,
                &Some(&dep.version),
                &exchange.name_with_version,
            ) {
                Ok(v) => v,
                Err(e) => {
                    debug!(
                        "Skipping dep {}: failed to resolve version: {}",
                        dep.name, e
                    );
                    continue;
                }
            };
            let dep_name_with_version = format!("{}:{}", dep.name.clone(), dep_resolved_version);

            println!("Processing dependency: {}", dep_name_with_version);

            // With optional deps skipped and the traversal depth aligned with the
            // download, this should always be on disk; guard defensively so a stray
            // missing dir skips the dep instead of panicking the whole run (and so the
            // is_proc_macro manifest read below never hits a missing Cargo.toml).
            let sub_dep_dir = std::path::PathBuf::from(consts::DOWNLOAD_PATH)
                .join(format!(
                    "{}_deps",
                    exchange.name_with_version.replace(':', "-")
                ))
                .join(dep_name_with_version.replace(':', "-"));
            if !sub_dep_dir.exists() {
                debug!(
                    "Dependency {} not found on disk, skipping requirement check",
                    dep_name_with_version
                );
                continue;
            }

            if is_proc_macro(&dep_name_with_version, Some(&exchange.name_with_version)) {
                debug!(
                    "Dependency: {} is a proc-macro crate, skipping requirement check",
                    dep_name_with_version
                );
                continue;
            }

            let (.., dep_crate_info) = downloader::gather_crate_info(
                &dep_name_with_version,
                true,
                Some(&exchange.name_with_version),
            )
            .unwrap();

            let mut optional_dep_feats = features_for_optional_deps(&dep_crate_info);

            let cached = dep_contexts.get(&dep_name_with_version).map(|c| {
                (
                    c.enable.clone(),
                    c.disable.clone(),
                    c.feature_to_items.clone(),
                )
            });

            let (enable, disable, feature_to_items): (
                Vec<String>,
                Vec<String>,
                HashMap<String, HashSet<String>>,
            ) = if let Some((enable, disable, feature_to_items)) = cached {
                debug!(
                    "Already visited dependency requirements for crate: {}",
                    dep_name_with_version
                );
                (enable, disable, feature_to_items)
            } else {
                let ctx = z3::Context::new(&z3::Config::new());
                let (all_hard, hard_constraints, _, _, dep_root, dep_records, _) =
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
                    None,
                );
                let (enable, disable, _) = process_crate(
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
            //
            // Guard is `item_depth < depth` (not `<=`): a crate popped at `item_depth`
            // has its deps checked at `item_depth + 1`, so pushing only while
            // `item_depth < depth` makes the deepest *checked* dep sit at `depth + 1` —
            // exactly the depth the download phase fetches to. `<=` would check one level
            // deeper than anything was downloaded and panic on the missing directory.
            if item_depth < depth && seen.insert((dep.name.clone(), dep_resolved_version.clone())) {
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
    let dep_default_feats: Vec<String> =
        dep_edge.map(|(_, feats)| feats.clone()).unwrap_or_default();

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
                || items.iter().any(|item| {
                    parent_valid_cross_crate_items.contains(&(dep_norm.clone(), item.clone()))
                })
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

/// TODO: this and `parse_top_level_externs` still reach `get_all_rs_files`
/// through `parse_item_extern_crates`, so they parse bin sources, `examples/`,
/// `benches/` and unreachable files. They walk deps by name with no `ModNode` in
/// scope, which is why they were left on the old sweep — see the comment on
/// `get_all_rs_files` for what converting them would take.
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

/// Drive `visiter_type` over a crate's source files.
///
/// When `files` is `Some`, that list is used verbatim — this is the preferred
/// path, since callers derive it from the resolved `ModNode` tree (see
/// [`visitor::collect_source_files`]) and so only touch files actually
/// reachable from the crate's entrypoint. When it is `None` the list falls back
/// to `get_all_rs_files`, whose directory sweep is naive; see that function's
/// comment for what it gets wrong.
///
/// Returns the number of files that were read *and* parsed. A file that cannot
/// be read or that `syn` rejects is skipped, so a zero return means the visitor
/// saw no source at all and whatever it did not collect says nothing about the
/// crate.
fn visit<T>(
    visiter_type: &mut T,
    crate_name: &str,
    recurse: bool,
    direct_file: bool,
    main_name: Option<&str>,
    files: Option<&[PathBuf]>,
) -> anyhow::Result<usize>
where
    T: for<'a> Visit<'a> + GetItemExternCrate,
{
    let dir = if !direct_file {
        get_actual_dir(crate_name, main_name)
    } else {
        PathBuf::from(crate_name)
    };

    let files = match files {
        Some(f) => f.to_vec(),
        None => get_all_rs_files(&dir, recurse, main_name),
    };

    let mut parsed_count = 0usize;
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
            // Tree-derived file lists can include paths outside the crate
            // directory — `include!(concat!(env!("OUT_DIR"), …))` files live in
            // the build directory. Keep those absolute rather than panicking.
            let span_file_path = filename.strip_prefix(&dir).unwrap_or(&filename);
            visiter_type.set_current_file(span_file_path.display().to_string());
            span_file_path.to_path_buf()
        } else {
            filename
        };
        parsed_count += 1;
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
    Ok(parsed_count)
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

/// `known_features` is the set of features Cargo can actually enable for the
/// crate these attributes belong to (the `[features]` table plus the implicit
/// features of optional dependencies). When supplied, a `feature = "X"` naming
/// an X outside that set is treated as a bare constant rather than a Z3 Bool —
/// see `parse_main_attributes_direct_with` for why.
///
/// Pass `None` to keep every `feature = "…"` a Bool. Callers that parse a
/// *dependency's* attributes must pass `None` (or that dependency's own set):
/// filtering a dependency's features against the main crate's list would erase
/// real, controllable features.
fn parse_token_stream<'a>(
    tokens: TokenStream,
    parsed: &mut ParsedAttr,
    ctx: &'a z3::Context,
    equation: &mut Option<Bool<'a>>,
    known_features: Option<&HashSet<String>>,
) -> Vec<Bool<'a>> {
    let mut was_feature = false;
    let mut was_filepath = false;
    let mut group_items: Vec<Bool> = Vec::new();
    // The entries of `group_items` that came from a nested group rather than
    // from a bare `feature = "…"`. Each is already folded under the operator
    // that introduced it, so the final fold below must not fold it again.
    let mut folded_operands: Vec<Bool> = Vec::new();
    let mut curr_logic = Logic::Any;

    for token in tokens {
        match token {
            proc_macro2::TokenTree::Group(g) => {
                let mut group_expr = None;
                let constants_before_call = parsed.constants.len();
                let local_group_items =
                    parse_token_stream(g.stream(), parsed, ctx, &mut group_expr, known_features);

                let local_group_items_refs: Vec<&Bool> = local_group_items.iter().collect();
                if local_group_items_refs.is_empty() {
                    // Prevent false positives when feature(no_std) is present in an
                    // attribute — i.e. a group directly following a `feature` ident,
                    // as in `#![cfg_attr(feature = "nightly", feature(no_std))]`,
                    // whose `no_std` would otherwise read as a no_std declaration.
                    //
                    // A group in any *other* position is a non-feature cfg —
                    // `not(has_std)`, `all(target_os = "…")` — and its constant is
                    // the only evidence `is_externally_gated` has that a gate was
                    // written at all. Truncating unconditionally erased exactly the
                    // single-atom groups (`not(backtrace_in_libstd)`), leaving them
                    // indistinguishable from an ungated item; groups contributing
                    // two or more constants were spared only by the `+ 1` test.
                    if was_feature && parsed.constants.len() == constants_before_call + 1 {
                        parsed.constants.truncate(constants_before_call);
                    }
                    was_feature = false;
                    continue;
                }
                let local_expr = match curr_logic {
                    Logic::And => Some(Bool::and(ctx, local_group_items_refs.as_slice())),
                    Logic::Or | Logic::Any => {
                        Some(Bool::or(ctx, local_group_items_refs.as_slice()))
                    }
                    Logic::Not => local_group_items.first().map(|first| first.not()),
                };

                // One operand of *this* level, beside any `feature = "…"` atom.
                // What combines the operands is the operator of the level that
                // owns them — `any` in
                //
                // ```ignore
                // #[cfg(any(not(any(feature = "std", feature = "spin")),
                //           all(feature = "std", feature = "spin")))]
                // ```
                //
                // — and that operator lives one frame up, which is why the
                // caller folds the returned list rather than this frame. ANDing
                // the operands together here instead read mtxgroup 0.1.1's
                // "exactly one of `std` and `spin`" as
                // `¬(¬(std ∨ spin) ∧ std ∧ spin)`: a tautology, so the
                // `compile_error!` it guards could never be violated and the
                // crate shipped the one feature set it forbids. Sibling groups
                // under `all` were right only because AND was the answer there.
                if let Some(local) = local_expr {
                    group_items.push(local.clone());
                    folded_operands.push(local);
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
                    was_feature = false;

                    // A `feature = "X"` that Cargo cannot enable is not this
                    // tool's axis to solve over: something outside the feature
                    // system (typically a build script emitting
                    // `cargo:rustc-cfg=feature="X"` off the target) decides it.
                    // Record it as a constant so it is erased exactly like a
                    // `target_os = "…"` atom, which both excuses the gate and
                    // keeps X out of the `--features` lists we hand to cargo.
                    if known_features.is_some_and(|known| !known.contains(&feature_str)) {
                        debug!("cfg names undeclared feature {feature_str:?}; treating as external");
                        parsed.constants.push(feature_str);
                        continue;
                    }

                    parsed.features.push(feature_str.clone());

                    let feature_var = Bool::new_const(ctx, feature_str);
                    group_items.push(feature_var);
                } else if was_filepath {
                    let filepath_str = l.to_string()[1..l.to_string().len() - 1].to_string();
                    parsed.filepath = Some(filepath_str);
                    was_filepath = false;
                }
            }
            _ => {}
        }
    }

    if !folded_operands.is_empty() {
        // At least one operand came from a nested group, so `curr_logic` has
        // already been spent folding it and must not be applied a second time —
        // `not(feature = "std")` arrives here as `¬std`, and folding it under
        // `Not` again would say `std`. Only the caller knows what combines the
        // operands of *this* level, and it folds the list this function returns;
        // `equation` is read by the top-level call alone, where a well-formed
        // cfg is a single predicate. Conjunction for the rest, which is what
        // this branch has always done.
        let refs: Vec<&Bool> = folded_operands.iter().collect();
        *equation = Some(if refs.len() == 1 {
            refs[0].clone()
        } else {
            Bool::and(ctx, refs.as_slice())
        });
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
    known_features: Option<&HashSet<String>>,
) -> (Option<Bool<'a>>, ParsedAttr) {
    match meta {
        Meta::List(list) => {
            let tokens = list.tokens.clone();
            let mut parsed = ParsedAttr::default();
            let mut equation = None;
            parse_token_stream(tokens, &mut parsed, ctx, &mut equation, known_features);
            (equation, parsed)
        }
        _ => {
            debug!("Meta is not a list");
            (None, ParsedAttr::default())
        }
    }
}

/// Sweep a crate directory for `.rs` files.
///
/// TODO: retire this in favour of `visitor::collect_source_files` on a resolved
/// `ModNode` tree. This sweep is naive and wrong in ways the tree is not:
///
///   * it re-derives targets by hand instead of asking cargo, so it picks up
///     bin sources (explicit `[[bin]]` paths, `src/bin/`, and any `src/main.rs`
///     caught by the bare `read_dir` below) even when the crate has a lib —
///     `find_entrypoints` analyses only the lib in that case;
///   * `recurse: true` ignores the manifest entirely and `WalkDir`s everything,
///     picking up `examples/`, `benches/`, `build.rs` and any test directory not
///     literally under `/tests/`;
///   * it has no notion of reachability, so dead files that no `mod`
///     declaration references are parsed anyway.
///
/// The blocker for the remaining callers (`parse_top_level_externs` /
/// `parse_n_level_externs`) is that they have no `ModNode` in scope, and the
/// tree is only complete after a covering run — macro-expansion-generated
/// modules arrive via the plugin's `macro_modules` and OUT_DIR `include!` files
/// via `resolve_pending_includes`. Converting them means either threading a root
/// through or accepting the syn-reachable subset.
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

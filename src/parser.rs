use anyhow::Context;
use log::debug;
use proc_macro2::{Span, TokenStream};
// use quote::ToTokens;
use std::{collections::HashSet, fs, path::Path, time::Instant};
use syn::{
    Attribute, ExprBlock, Item, ItemExternCrate, Meta, Stmt, spanned::Spanned, visit::Visit,
};
use walkdir::WalkDir;
use z3::{self, ast::Bool};

use crate::{CrateInfo, DBData, DEPENDENCIES, Telemetry, TupleVec, consts, db, downloader, solver};

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
    pub filepath: Option<String>,
    logic: Vec<Logic>,
}

#[derive(Default, Clone, Debug)]
pub struct Attributes {
    attributes: Vec<Attribute>,
    // This will be a list of attributes associated with
    // compiler_error macros. Note that the attributes present
    // here will also be present in `attributes` field.
    compile_error_attrs: Vec<Attribute>,
    pub crate_name: String,
    pub unconditional_no_std: bool,
    // Stores the filename as well since we can't recover
    // it later from the Span.
    pub spans: Vec<(Span, Option<String>)>,
}

#[derive(Default, Clone, Debug)]
pub struct ItemExternCrates {
    itemexterncrates: Vec<ItemExternCrate>,
}

#[derive(Default, Clone, Debug)]
pub struct ItemExternCratesAll {
    itemexterncrates: Vec<ItemExternCrate>,
}

trait GetItemExternCrate {
    fn get_item_extern_crate(&mut self) -> Option<&mut Vec<ItemExternCrate>> {
        None
    }
    fn get_spans(&mut self) -> Option<&mut Vec<(Span, Option<String>)>> {
        None
    }
}

impl GetItemExternCrate for Attributes {
    fn get_spans(&mut self) -> Option<&mut Vec<(Span, Option<String>)>> {
        Some(&mut self.spans)
    }
}

impl GetItemExternCrate for ItemExternCrates {
    fn get_item_extern_crate(&mut self) -> Option<&mut Vec<ItemExternCrate>> {
        Some(&mut self.itemexterncrates)
    }
}
impl GetItemExternCrate for ItemExternCratesAll {
    fn get_item_extern_crate(&mut self) -> Option<&mut Vec<ItemExternCrate>> {
        Some(&mut self.itemexterncrates)
    }
}

impl<'a> Visit<'a> for ItemExternCrates {
    fn visit_item_extern_crate(&mut self, i: &ItemExternCrate) {
        // We will save all the extern crates that have an
        // attribute associated with them.
        if !i.attrs.is_empty() {
            self.itemexterncrates.push(i.clone());
        }
    }

    fn visit_expr_block(&mut self, i: &'a ExprBlock) {
        visit_expr_block_common(self, i);
    }
}

impl<'a> Visit<'a> for ItemExternCratesAll {
    fn visit_item_extern_crate(&mut self, i: &ItemExternCrate) {
        self.itemexterncrates.push(i.clone());
    }

    fn visit_expr_block(&mut self, i: &'a syn::ExprBlock) {
        visit_expr_block_common(self, i);
    }
}

/// Visit ExprBlocks of the form
/// ```ignore
/// #[cfg(feature = "use-locks")]
/// {
///     self.source.lock.unlock();
///
///     #[cfg(feature = "std")]
///     {
///         extern crate std;
///         if std::thread::panicking() {
///             self.source.state.set(State::Poisoned);
///         }
///     }
/// }
/// ```
/// and recurse until it finds an extern crate std.
/// Once this is found, all the attributes in its path in the
/// ast will be added to the `item_extern_crate`
/// # Arguments
/// * `typ` - The type which we update with the new extern crate std
/// * `expr_block` - The expression block to visit
fn visit_expr_block_common<'a, T: Visit<'a> + GetItemExternCrate>(
    typ: &mut T,
    expr_block: &'a ExprBlock,
) {
    let old_len = typ.get_item_extern_crate().unwrap_or(&mut Vec::new()).len();
    syn::visit::visit_expr_block(typ, expr_block);
    let get_item_extern_crate = typ.get_item_extern_crate().unwrap();
    let changed = get_item_extern_crate.len() > old_len;
    if !expr_block.attrs.is_empty() {
        let stmts = &expr_block.block.stmts;
        let attrs = expr_block.attrs.clone();
        for stmt in stmts {
            match stmt {
                Stmt::Item(Item::ExternCrate(item)) => {
                    get_item_extern_crate.retain(|i| i != item);
                    let new = ItemExternCrate {
                        attrs: attrs.clone(),
                        ..item.clone()
                    };
                    get_item_extern_crate.push(new);
                }
                _ if changed => {
                    let extern_item = get_item_extern_crate.last().unwrap();
                    let new = ItemExternCrate {
                        attrs: attrs.clone(),
                        ..extern_item.clone()
                    };
                    get_item_extern_crate.push(new);
                }
                _ => {
                    debug!(
                        "Found unexpected statement in extern crate block: {:?}",
                        stmt
                    );
                }
            }
        }
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
        if i.mac.path.is_ident("compile_error") {
            let attrs = i.attrs.clone();
            if attrs.is_empty() {
                debug!("No attributes found for compile_error macro");
                return;
            }
            // Remove the attribute from the list which are
            // compiler_error attributes.
            // This is currently assuming there will be only
            // one attribute associated with compiler_error
            // and also that the attribute to remove
            // has already been visited.
            self.attributes.retain(|a| a != &attrs[0]);
            // Negate the attrs[0] and add it to the attributes
            match attrs[0].meta.clone() {
                Meta::List(meta_list) => {
                    let path = meta_list.path.get_ident();
                    if path.is_some() && path.unwrap() == "cfg" {
                        let tokens = meta_list.tokens.clone();
                        let negated: Attribute = syn::parse_quote!(
                        #[cfg(not(#tokens))]
                        );
                        self.compile_error_attrs.push(negated.clone());
                        self.attributes.push(negated);
                    }
                }
                _ => {
                    debug!(
                        "Unexpected meta type for compiler_error: {:?}",
                        attrs[0].meta
                    );
                }
            }
        }
    }

    fn visit_item_mod(&mut self, i: &'a syn::ItemMod) {
        if i.ident != "test" {
            debug!("Visiting module: {}", i.ident);
            syn::visit::visit_item_mod(self, i);
        }
    }

    // All visitors that collect Span
    fn visit_item(&mut self, i: &'a Item) {
        let attrs: &Vec<Attribute>;
        let span: Span;
        match i {
            Item::Fn(func) => {
                attrs = &func.attrs;
                span = func.span();
            }
            Item::Struct(struc) => {
                attrs = &struc.attrs;
                span = struc.span();
            }
            Item::Enum(enm) => {
                attrs = &enm.attrs;
                span = enm.span();
            }
            Item::Const(konst) => {
                attrs = &konst.attrs;
                span = konst.span();
            }
            Item::Static(stat) => {
                attrs = &stat.attrs;
                span = stat.span();
            }
            Item::Type(ty) => {
                attrs = &ty.attrs;
                span = ty.span();
            }
            Item::Union(un) => {
                attrs = &un.attrs;
                span = un.span();
            }
            Item::Trait(trt) => {
                attrs = &trt.attrs;
                span = trt.span();
            }
            Item::Impl(imp) => {
                attrs = &imp.attrs;
                span = imp.span();
            }
            Item::Mod(m) => {
                attrs = &m.attrs;
                span = m.span();
            }
            Item::Use(u) => {
                attrs = &u.attrs;
                span = u.span();
            }
            _ => {
                syn::visit::visit_item(self, i);
                return;
            }
        }
        check_attr_save_span(self, attrs, span);
        syn::visit::visit_item(self, i);
    }
    // TODO: Add visit_field and visit_variant if required
}

fn check_attr_save_span(attributes: &mut Attributes, attr: &[Attribute], span: Span) {
    if attr.iter().any(|a| a.path().is_ident("cfg")) {
        attributes.spans.push((span, None));
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
    let path = format!("{}/{}", consts::DOWNLOAD_PATH, crate_name.replace(':', "-"));
    let files = get_all_rs_files(&path, true);
    let mut files_ungaurded = Vec::new();
    for file in files {
        let mut itemexterncrates = ItemExternCratesAll {
            itemexterncrates: Vec::new(),
        };

        let filename = file.replace(consts::DOWNLOAD_PATH, "");
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
                    .any(|a| a.path().get_ident().is_some_and(|ident| ident == "cfg"))
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
    // This is the list of known syn failure crates which are no_std
    if consts::KNOWN_SYN_FAILURES.contains(&name) {
        debug!("Skipping known syn failure crate: {}", name);
        return true;
    }

    // We need to re-parse this instead of using already existing attributes
    // since files in non root directory might have `no_std` attribute
    // and we don't want to include those.
    let base_attrs = parse_crate(name, false);

    if !parse_main_attributes(&base_attrs, ctx).0 && !base_attrs.unconditional_no_std {
        debug!("No no_std found for the crate {}", name);
        return false;
    }
    true
}

/// Parse the dependencies of the main crate
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
/// * `crate_info` - The crate info of the main crate
/// * `is_main` - A boolean indicating whether the crate is the main crate
/// * `telemetry` - The global telemetry data
/// # Returns
/// A tuple containing the features to enable, the features to disable,
/// a boolean indicating whether `no_std` was found, and a boolean indicating
/// whether to recurse further if it was the main crate.
pub fn process_crate(
    ctx: &z3::Context,
    attrs: &Attributes,
    name_with_version: &str,
    db_data: &[DBData],
    crate_info: &CrateInfo,
    is_main: bool,
    telemetry: &mut Telemetry,
    optional_dep_feats: &TupleVec,
) -> anyhow::Result<(Vec<String>, Vec<String>)> {
    let (mut enable, mut disable): (Vec<String>, Vec<String>) = (Vec::new(), Vec::new());

    let (no_std, mut equation, mut parsed_attr) = parse_main_attributes(attrs, ctx);

    if is_main {
        telemetry.main_conditional_no_std = no_std;
    } else {
        telemetry
            .conditional_no_std_deps
            .push((name_with_version.to_string(), no_std));
    }

    if !attrs.unconditional_no_std {
        if !no_std {
            debug!("No no_std found for the crate");
            return Ok((Vec::new(), Vec::new()));
        }
    } else {
        if is_main {
            telemetry.main_unconditional_no_std = true;
        } else {
            telemetry
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

        let items = parse_item_extern_crates(name_with_version);

        // This case implies that the crate is no_std without any feature requirements.
        if items.itemexterncrates.is_empty() {
            debug!("No extern crates found for the crate");
            return Ok((Vec::new(), Vec::new()));
        }
        let std_attrs = get_item_extern_std(&items);
        if !std_attrs.is_empty() {
            debug!("Leaf level crate reached {}", name_with_version);
            if is_main {
                telemetry.direct_extern_std_usage_main = true;
            } else {
                telemetry
                    .direct_extern_std_usage_deps
                    .push(name_with_version.to_string());
            }
            let features = db::get_from_db_data(db_data, name_with_version);
            if let Some(feats) = features {
                debug!(
                    "Features to enable and disable for crate {} from db: {:?}",
                    name_with_version, feats
                );
                (enable, disable) = feats.features.clone();
            } else {
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
                debug!("Main equation: {:?}", equation);
            }
        } else if !is_main {
            debug!("Leaf level crate reached {}", name_with_version);
            let (name, version) = name_with_version.split_once(':').unwrap();
            if let Some(dep_and_features) = get_deps_and_features(name, version, crate_info) {
                let names_and_versions: TupleVec = dep_and_features
                    .iter()
                    .map(|(dep, _)| (dep.name.clone(), dep.version.clone()))
                    .collect();
                let externs = get_item_extern_dep(&items, &names_and_versions);
                match parse_top_level_externs(ctx, &names_and_versions, &externs, telemetry) {
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
            debug!("main equation: {:?}", equation);
        }
    }
    let equations = parse_attributes(attrs, ctx);
    let mut filtered = filter_equations(&equations, &parsed_attr.features);

    for negated_attr in attrs.compile_error_attrs.iter() {
        let (neg_eq, neg_parsed_attr) = parse_main_attributes_direct(negated_attr, ctx);
        if let Some(neg_eq) = neg_eq {
            filtered.push(neg_eq);
        }
    }

    // This part adds equations if there are attributes that conditionally include
    // files which might contain unguarded `extern crate std`.
    let files_and_equations = get_files_in_attributes(attrs, ctx);
    if !files_and_equations.is_empty() {
        if is_main {
            telemetry.conditional_file_import_main = true;
        } else {
            telemetry
                .conditional_file_import_deps
                .push((name_with_version.to_string(), true));
        }
    }
    debug!("Files in attributes: {:?}", files_and_equations);
    let files_unguared = parse_item_extern_crates_for_files(name_with_version);
    debug!(
        "Files with unguarded extern crate std: {:?}",
        files_unguared
    );

    let mut imported_files: Vec<String> = Vec::new();
    for (file, eq) in files_and_equations {
        if files_unguared.contains(&file) {
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
        telemetry
            .conditional_files_with_std_main
            .extend(imported_files);
    } else {
        telemetry
            .conditional_files_with_std_deps
            .push((name_with_version.to_string(), imported_files));
    }

    let now = Instant::now();
    // Finally, we solve the equations
    let (model, len, depth) = solver::solve(ctx, &equation, &filtered);
    telemetry
        .constraint_solving_time_ms
        .push((name_with_version.to_string(), now.elapsed().as_millis()));
    telemetry
        .max_contraint_length
        .push((name_with_version.to_string(), len));
    telemetry
        .max_constrait_depth
        .push((name_with_version.to_string(), depth));
    if enable.is_empty() && disable.is_empty() {
        (enable, disable) = solver::model_to_features(&model);
    }

    minimize(crate_info, optional_dep_feats, &mut enable);

    Ok((enable, disable))
}

/// If there are features that got enabled, but are the only reason an optional
/// dependency is included, we can drop those features from the main crate's
/// feature list.
/// # Arguments
/// * `crate_info` - The crate info of the main crate
/// * `optional_dep_feats` - The list of features that enable optional dependencies
/// * `enable` - The list of features to enable for the main crate
pub fn minimize(crate_info: &CrateInfo, optional_dep_feats: &TupleVec, enable: &mut Vec<String>) {
    let optional_deps: Vec<String> = crate_info
        .deps_and_features
        .iter()
        .filter(|(dep, _)| dep.optional)
        .map(|(dep, _)| dep.name.clone())
        .collect();
    enable.retain(|f| optional_dep_feats.iter().all(|(_, feat)| feat != f));
    enable.retain(|f| !optional_deps.contains(f));
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
/// * `crate_name_rename` - A list of names and their renames of crate names
/// # Returns
/// A Result indicating success or failure.
pub fn process_dep_crate(
    ctx: &z3::Context,
    dep: &Attributes,
    main_name: &str,
    db_data: &mut [DBData],
    crate_info: &CrateInfo,
    crate_name_rename: &[(String, String)],
    telemetry: &mut Telemetry,
) -> Result<Vec<String>, anyhow::Error> {
    let (enable, disable) = match db::get_from_db_data(db_data, &dep.crate_name) {
        Some(dbdata) => (dbdata.features.0.clone(), dbdata.features.1.clone()),
        None => {
            let (.., dep_crate_info) = downloader::gather_crate_info(&dep.crate_name, true)?;
            let optional_dep_feats = features_for_optional_deps(&dep_crate_info);
            let (enable, disable) = process_crate(
                ctx,
                dep,
                &dep.crate_name,
                db_data,
                &dep_crate_info,
                false,
                telemetry,
                &optional_dep_feats,
            )?;
            (enable, disable)
        }
    };

    debug!(
        "Dependency {}: enable: {:?}, disable: {:?}",
        dep.crate_name, enable, disable
    );

    let (args, update_default_config) = solver::final_feature_list_dep(
        crate_info,
        dep.crate_name.split(":").next().unwrap_or(""),
        &enable,
        &disable,
        crate_name_rename,
        telemetry,
    );

    debug!(
        "Dependency requires default config update: {}",
        update_default_config
    );

    if update_default_config {
        update_main_crate_default_list(main_name, &dep.crate_name, crate_name_rename);
        telemetry
            .default_true_unset_deps
            .push((dep.crate_name.clone(), true));
    } else {
        telemetry
            .default_true_unset_deps
            .push((dep.crate_name.clone(), false));
    }

    debug!(
        "Final arguments for dependency {}: {:?}",
        dep.crate_name, args
    );

    Ok(args)
}

/// Sometimes main might enable a feature that enables a dependency feature
/// that is not required for no_std build and can cause build failure.
/// If such a feature exists in a main feature which is not necessary,
/// for the main, it is dropped from the enabled features of main crate.
/// If the feature is part of a fixed feature list, it is moved to a
/// custom feature list called `dep-unnecessary-features`.
/// # Arguments
/// * `main_name` - The name of the main crate
/// * `fixed_main_args` - The fixed features of the main crate
/// * `flexible_main_args` - The list of features whihc are not necessary for main
/// * `dep_name` - The name of the dependency
/// * `deps_args` - The features required for the dependency
pub fn move_unnecessary_dep_feats(
    main_name: &str,
    fixed_main_args: &[String],
    flexible_main_args: &mut Vec<String>,
    dep_name: &str,
    deps_args: &[String],
    telemetry: &mut Telemetry,
) {
    let main_manifest = determine_manifest_file(main_name);
    let mut main_toml: toml::Value =
        toml::from_str(&fs::read_to_string(&main_manifest).unwrap()).unwrap();
    let main_features = main_toml.get_mut("features").and_then(|f| f.as_table_mut());

    if main_features.is_none() {
        debug!("No features found for main crate {}", main_name);
        return;
    }
    let main_features = main_features.unwrap();

    let prefix1 = format!("{}/", dep_name);
    let prefix2 = format!("{}?/", dep_name);

    flexible_main_args.retain(|feature| {
        main_features
            .get_mut(feature)
            .and_then(|f| f.as_array_mut())
            .is_some_and(|arr| {
                arr.iter()
                    .filter_map(|v| v.as_str())
                    .filter(|&f| f.starts_with(&prefix1) || f.starts_with(&prefix2))
                    .map(extract_key)
                    .any(|f| !deps_args.contains(&f.to_string()))
            })
    });

    let mut removed = HashSet::new();
    for feature in fixed_main_args {
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
                        debug!("Removing unnecessary feature {} from main crate", s);
                        removed.insert(feature.to_string());
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

    add_feats_to_custom_feature(
        &mut main_toml,
        consts::DEP_UNNECESSARY_FEATURES,
        &removed.iter().cloned().collect::<Vec<_>>(),
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
        let names_and_versions = downloader::read_dep_names_and_versions(&name, &version, true)
            .expect("Failed to read dependency names and versions");
        for (dep_name, dep_version) in names_and_versions {
            debug!(
                "Processing dependency {}:{} for no_std",
                dep_name, dep_version
            );
            let name_with_version =
                match downloader::clone_from_crates(&dep_name, Some(&dep_version)) {
                    Ok(name_with_version) => name_with_version,
                    Err(e) => {
                        debug!("Failed to download crate: {}", e);
                        continue;
                    }
                };

            if is_proc_macro(&name_with_version) {
                debug!("{} is a proc-macro, skipping", name_with_version);
                continue;
            }

            let (name_inner, version) = name_with_version
                .split_once(':')
                .unwrap_or((&name_with_version, ""));

            if !check_for_no_std(&name_with_version, ctx) {
                debug!(
                    "ERROR: Dependency {} of dependency {} does not support no_std build at depth {}",
                    name_with_version, name, current_depth
                );
                return (false, current_depth);
            }

            local_initlist.push((name_inner.to_string(), version.to_string()));
        }
    }

    determine_n_depth_dep_no_std(local_initlist, depth, current_depth + 1, visited, ctx)
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
    parse_meta_for_cfg_attr(&attr.meta, ctx)
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
pub fn determine_manifest_file(name_with_version: &str) -> String {
    let dir = Path::new(consts::DOWNLOAD_PATH).join(name_with_version.replace(':', "-"));
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
pub fn is_proc_macro(crate_name: &str) -> bool {
    let manifest = determine_manifest_file(crate_name);
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
    let main_manifest = determine_manifest_file(main);
    let dep_manifest = determine_manifest_file(dep);
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
    let main_manifest = determine_manifest_file(main_name);
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
    let main_manifest = determine_manifest_file(main_name);
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
/// None
pub fn remove_feats_enabling_dep(main_name: &str, feats: &[String], to_drop: &String) {
    if feats.is_empty() {
        return;
    }

    let main_manifest = determine_manifest_file(main_name);
    let mut main_toml: toml::Value =
        toml::from_str(&fs::read_to_string(&main_manifest).unwrap()).unwrap();

    let features = main_toml
        .get_mut("features")
        .and_then(|v| v.as_table_mut())
        .expect("Failed to get features table from main Cargo.toml");

    let formatted = format!("dep:{}", to_drop);
    let mut to_push = String::new();
    for feat in feats {
        if let Some(arr) = features.get_mut(feat).and_then(|v| v.as_array_mut()) {
            arr.retain(|f| {
                if let toml::Value::String(s) = f
                    && (s == to_drop || *s == formatted)
                {
                    if s == &formatted {
                        to_push = to_drop.clone();
                    } else {
                        to_push = formatted.clone();
                    }
                    debug!("Removing feature {} from main crate", s);
                    return false;
                }
                true
            });
        }
    }

    add_feats_to_custom_feature(&mut main_toml, consts::CUSTOM_FEATURES_DISABLED, &[to_push]);

    fs::write(
        &main_manifest,
        toml::to_string(&main_toml)
            .context("Failed convert Value to string")
            .unwrap(),
    )
    .unwrap();
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

/// Given a `CrateInfo`, this function finds all optional dependencies
/// and their features that are required to enable them.
/// It returns a vector of tuples where each tuple contains the dependency name
/// and the feature name.
/// # Arguments
/// * `crate_info` - The `CrateInfo` containing dependencies and features.
/// # Returns
/// A vector of tuples, each containing the dependency name and the feature name.
pub fn features_for_optional_deps(crate_info: &CrateInfo) -> TupleVec {
    let deps_and_feats = &crate_info.deps_and_features;
    let main_feats = &crate_info.features;

    let optional_deps: Vec<String> = deps_and_feats
        .iter()
        .filter(|(dep, _)| dep.optional)
        .map(|(dep, _)| dep.name.clone())
        .collect();

    // There are the two most common ways to enable an optional dependency
    // via features. `somefeat = ["depname"]` or `somefeat = ["dep:depname"]`
    // `read_local_features` function already handles parsing these correctly.
    // Cargo by default generates an implicit `depname = ["dep:depname"]` feature
    // for each optional dependency. But if the user has explicitly defined
    // a feature with "dep:depname", then that will override the implicit one.
    let common = |deps: &Vec<String>, implicit: bool| {
        {
            deps.iter().flat_map(|dep| {
                main_feats.iter().filter_map(|(feat_name, dep_feats)| {
                    let has_dep = dep_feats.iter().any(|f| {
                        f.0 == dep.as_str()
                            && if implicit {
                                f.1 == "dep:"
                            } else {
                                f.1 == dep.as_str()
                            }
                    });
                    if has_dep {
                        Some((dep.clone(), feat_name.clone()))
                    } else {
                        None
                    }
                })
            })
        }
        .collect::<Vec<_>>()
    };

    // We first find all optional dependencies that have the implicit feature
    // overridden by the user.
    let mut direct_feat_match: TupleVec = common(&optional_deps, true);

    let found: Vec<String> = direct_feat_match
        .iter()
        .map(|(dep, _)| dep.clone())
        .collect();

    // Now for the features that are not overridden, we find the implicit ones.
    let not_found: Vec<String> = optional_deps
        .iter()
        .filter(|dep| !found.contains(dep))
        .cloned()
        .collect();

    let indirect_feat_match: TupleVec = common(&not_found, false);

    direct_feat_match.extend(indirect_feat_match);
    direct_feat_match.sort();
    direct_feat_match.dedup();
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
    crate_info: &CrateInfo,
    deps_and_features: &[(String, String)],
    enable_features: &[String],
    disable_default: bool,
    telemetry: &mut Telemetry,
    second_round: bool,
) -> bool {
    if is_proc_macro(name) {
        debug!("Dependency {} is a proc-macro, skipping", name);
        return true;
    }

    let dep_name = name.split(':').next().unwrap_or("").to_string();

    if !is_dep_optional(crate_info, &dep_name) {
        debug!("Dependency {} is not optional, not skipping", name);
        return false;
    }

    let feats_of_dep: Vec<String> = deps_and_features
        .iter()
        .filter(|(dep, _)| dep == &dep_name)
        .map(|(_, feat)| feat.clone())
        .collect();

    let main_feats = &crate_info.features;
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
        let found = check_for_no_std(name, &ctx);

        debug!(
            "Dependency: {} is enabled by features: {:?} and currently enabled list enabled {:?} from that list",
            dep_name, feats_of_dep, features_for_dependency
        );

        if !found {
            debug!(
                "Dependency {} does not support no_std. Creating a new feature and adding the conflicting features to it",
                dep_name
            );
            let main_name = &format!("{}:{}", crate_info.name, crate_info.version);
            remove_feats_enabling_dep(main_name, &features_for_dependency, &dep_name);
            if second_round {
                telemetry.optional_deps_disabled.push(dep_name.clone());
                telemetry
                    .optional_deps_disabled_features_moved
                    .push((dep_name, features_for_dependency));
            }
            return true;
        } else {
            debug!("Dependency {} supports no_std", dep_name);
            if second_round {
                telemetry.optional_deps_enabled.push(dep_name.clone());
                telemetry
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
pub fn recursive_dep_requirement_check(
    crate_info: &CrateInfo,
    db_data: &[DBData],
    depth: u32,
    telemetry: &mut Telemetry,
) -> bool {
    telemetry.recursive_requirement_check_done = true;
    println!("Starting recursive dependency requirement check...");
    let top_level_deps: TupleVec = crate_info
        .deps_and_features
        .iter()
        .filter(|(info, _)| !info.optional)
        .map(|(dep, _)| (dep.name.clone(), dep.version.clone()))
        .collect();

    let ctx = z3::Context::new(&z3::Config::new());
    // Throwaway telemetry
    let mut telemetry = Telemetry::default();

    let mut seen: HashSet<(String, String)> = HashSet::new();
    let mut worklist: TupleVec = top_level_deps.clone();
    let mut threshold = top_level_deps.len();
    let mut current_threshold = 0;
    let mut current_depth = 1;

    debug!("Original worklist: {:?}", worklist);

    let client = downloader::create_client().unwrap();
    // For the crates that we already processed, save the requirements
    // so that we don't have to recompute them.
    let mut visited_dep_require: Vec<(String, (Vec<String>, Vec<String>))> = Vec::new();
    let instant = std::time::Instant::now();

    while let Some((name, version)) = worklist.pop() {
        current_threshold += 1;
        if current_threshold > threshold {
            current_depth += 1;
            threshold = worklist.len();
            current_threshold = 0;
            debug!("Recursion depth increased to: {}", depth);
        }
        println!("Checking dependency: {}:{}", name, version);
        let crate_data = client.get_crate(&name).unwrap();
        let resolved_version = downloader::resolve_version(&Some(&version), &crate_data).unwrap();
        let name_with_version = format!("{}:{}", name, resolved_version);

        if is_proc_macro(&name_with_version) {
            debug!(
                "Dependency: {} is a proc-macro crate, skipping requirement check",
                name_with_version
            );
            continue;
        }

        // Current crate's CrateInfo. We will use this to check the features the the current crate exposes.
        let (.., crate_info) = downloader::gather_crate_info(&name_with_version, true).unwrap();

        for (dep, _) in crate_info.deps_and_features.iter() {
            let dep_crate_data = client.get_crate(&dep.name).unwrap();
            let dep_resolved_version =
                downloader::resolve_version(&Some(&dep.version), &dep_crate_data).unwrap();
            let dep_name_with_version = format!("{}:{}", dep.name.clone(), dep_resolved_version);

            println!("Processing dependency: {}", dep_name_with_version);
            if is_dep_optional(&crate_info, &dep.name) || is_proc_macro(&dep_name_with_version) {
                debug!(
                    "Dependency: {} is optional or a proc-macro crate: {}, skipping requirement check",
                    dep.name, name_with_version
                );
                continue;
            }

            debug!("Seen so far: {:?}", seen);

            let (.., dep_crate_info) =
                downloader::gather_crate_info(&dep_name_with_version, true).unwrap();

            let optional_dep_feats = features_for_optional_deps(&dep_crate_info);

            let (mut enable, disable): (Vec<String>, Vec<String>) = if let Some(reqs) =
                visited_dep_require
                    .iter()
                    .find(|(visited_name, _)| visited_name == &dep_name_with_version)
                    .map(|(_, reqs)| reqs)
            {
                debug!(
                    "Already visited dependency requirements for crate: {}",
                    dep_name_with_version
                );
                reqs.clone()
            } else {
                let crate_attrs = parse_crate(&dep_name_with_version, false);
                let (enable, disable) = process_crate(
                    &ctx,
                    &crate_attrs,
                    &dep_name_with_version,
                    db_data,
                    &dep_crate_info,
                    true,
                    &mut telemetry,
                    &optional_dep_feats,
                )
                .unwrap();

                visited_dep_require.push((
                    dep_name_with_version.clone(),
                    (enable.clone(), disable.clone()),
                ));
                debug!(
                    "Already visited dependencies requirements: {:?}",
                    visited_dep_require
                );
                (enable, disable)
            };

            solver::new_feats_to_add(&dep_crate_info, &Vec::new(), &mut enable);

            debug!(
                "Dependency: {} requires features: {:?} to be enabled and features: {:?} to be disabled to support no_std",
                dep_name_with_version, enable, disable
            );

            // We use the resolved version here because multiple versions of the same crate
            // can resolve to the same version and are required by different dependencies.
            // In that case, we don't want to check the same crate multiple times.
            if current_depth <= depth
                && seen.insert((dep.name.clone(), dep_resolved_version.clone()))
            {
                debug!(
                    "Adding dependency: {} to worklist for requirement check with version: {}",
                    dep.name, dep_resolved_version
                );
                worklist.push((dep.name.clone(), dep_resolved_version.clone()));
            }

            if !dependency_requirement_possible(
                &crate_info,
                &dep_crate_info,
                &dep.name,
                &enable,
                &disable,
            ) {
                debug!(
                    "Dependency: {} cannot satisfy its no_std requirements, failing",
                    dep_name_with_version
                );
                telemetry.recursive_requirement_check_time_ms = instant.elapsed().as_millis();
                telemetry.recursive_requirement_check_failed = true;
                telemetry.recursive_requirement_check_failed_dep =
                    Some(dep_name_with_version.clone());
                return false;
            }
        }
    }
    telemetry.recursive_requirement_check_time_ms = instant.elapsed().as_millis();
    true
}

fn dependency_requirement_possible(
    main_crate_info: &CrateInfo,
    dep_crate_info: &CrateInfo,
    dep_name: &str,
    enable: &[String],
    disable: &[String],
) -> bool {
    if solver::disable_in_default(dep_crate_info, disable)
        && let Some((dep, _)) = main_crate_info
            .deps_and_features
            .iter()
            .find(|(dep, _)| dep.name == dep_name)
        && dep.default_features
    {
        println!(
            "Dependency: {} has default features enabled in main crate, cannot disable required features: {:?}",
            dep_name, disable
        );
        return false;
    }

    let dep_default_feats: Vec<String> = main_crate_info
        .deps_and_features
        .iter()
        .find(|(dep, _)| dep.name == dep_name)
        .map(|(_, feats)| feats.clone())
        .unwrap_or(Vec::new());

    for feat in enable {
        if !dep_default_feats.contains(feat)
            && !feat_available_for_dep(main_crate_info, dep_name, feat)
        {
            println!(
                "Dependency: {} cannot enable required feature: {}",
                dep_name, feat
            );
            return false;
        }
    }
    true
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

    Ok(parse_n_level_externs_entry(&mut worklist, telemetry))
}

fn parse_n_level_externs_entry<'a>(
    worklist: &mut Vec<(String, Option<Bool<'a>>, ParsedAttr)>,
    telemetry: &mut Telemetry,
) -> (Option<Bool<'a>>, ParsedAttr) {
    let mut worklists = Vec::new();
    let mut depth = 2;
    worklist.iter().for_each(|(name_with_version, _, _)| {
        worklists.push((name_with_version.clone(), Vec::<String>::new()));
    });
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
            if parse_n_level_externs(&mut local_worklist.1, telemetry) {
                telemetry.indirect_extern_std_usage_depth = depth;
                return (equation.clone(), parsed_attr.clone());
            }
        }
        depth += 1;
    }
}

fn parse_n_level_externs(worklist: &mut Vec<String>, telemetry: &mut Telemetry) -> bool {
    let mut local_worklist = Vec::new();
    for name_with_version in worklist.drain(..) {
        let (mut name, version) = name_with_version.split_once(':').unwrap();
        let new_name_with_version =
            downloader::clone_from_crates(name, Some(&version.to_string())).unwrap();
        name = new_name_with_version
            .split_once(':')
            .map_or(name, |(n, _)| n);
        let names_and_versions =
            downloader::read_dep_names_and_versions(name, version, false).unwrap();
        let unfiltered = parse_item_extern_crates(&new_name_with_version);
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
    s.split('/')
        .collect::<Vec<_>>()
        .first()
        .unwrap_or(&"")
        .strip_suffix("?")
        .unwrap_or(s)
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
    T: for<'a> Visit<'a> + GetItemExternCrate,
{
    let path = format!("{}/{}", consts::DOWNLOAD_PATH, crate_name.replace(':', "-"));
    let files = get_all_rs_files(&path, recurse);

    for filename in files {
        debug!("Parsing file: {}", filename);
        let content = match fs::read_to_string(&filename) {
            Ok(content) => content,
            Err(e) => {
                debug!("Failed to read file {}: {}", filename, e);
                continue;
            }
        };
        let file = match syn::parse_file(&content) {
            Ok(file) => file,
            Err(e) => {
                debug!("Failed to parse file {}: {}", filename, e);
                continue;
            }
        };
        visiter_type.visit_file(&file);
        if let Some(spans) = visiter_type.get_spans() {
            // Newly added spans will have None as filename.
            // We fill it with the current filename.
            for span in spans {
                if span.1.is_none() {
                    span.1.replace(
                        filename
                            .clone()
                            .strip_prefix(&(path.clone() + "/"))
                            .unwrap()
                            .to_string(),
                    );
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
                    Logic::Not => {
                        current_expr = local_group_items.first().map(|first| first.not());
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

fn get_all_rs_files(path: &str, recurse: bool) -> Vec<String> {
    let path_obj = Path::new(path);
    if path_obj.is_file() && path_obj.extension().unwrap_or_default() == "rs" {
        return vec![path.to_string()];
    }

    let mut files = Vec::new();

    if recurse {
        for entry in WalkDir::new(path) {
            push_to_files_vec(entry.unwrap().path(), &mut files);
        }
    } else {
        let manifest_path = determine_manifest_file(path);
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
            debug!("No src directory found in {}", path);
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

fn push_to_files_vec(path: &Path, files: &mut Vec<String>) {
    if path.extension().unwrap_or_default() == "rs" && !path.to_str().unwrap().contains("/tests/") {
        files.push(path.to_str().unwrap().to_string());
    }
}

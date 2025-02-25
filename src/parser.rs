use crate::consts::DOWNLOAD_PATH;
use crate::DEPENDENCIES;
use log::debug;
use proc_macro2::TokenStream;
use quote::ToTokens;
use std::collections::HashSet;
use std::fs;
use syn::{visit::Visit, Attribute, Meta};
use walkdir::WalkDir;
use z3;
use z3::ast::Bool;

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
    logic: Vec<Logic>,
}

#[derive(Default, Clone, Debug)]
pub struct Attributes {
    attributes: Vec<Attribute>,
    pub crate_name: String,
}

impl<'a> Visit<'a> for Attributes {
    fn visit_attribute(&mut self, i: &Attribute) {
        if let Some(ident) = i.path().get_ident() {
            if ident == "cfg" || ident == "cfg_attr" {
                self.attributes.push(i.clone());
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

/// Parse the main crate and return the attributes
/// # Arguments
/// * `path` - The path to the main crate
/// * `crate_name` - The name of the main crate
/// # Returns
/// The attributes of the main crate
pub fn parse_crate(path: &str, crate_name: String) -> Attributes {
    let files = get_all_rs_files(path);

    let mut attributes = Attributes::default();

    for file in files {
        debug!("Parsing file: {}", file);
        let content = fs::read_to_string(&file).unwrap();
        let file = syn::parse_file(&content).unwrap();
        attributes.visit_file(&file);
    }
    attributes.crate_name = crate_name;
    attributes
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
        let path = format!("{}/{}/", DOWNLOAD_PATH, dep.replace(':', "-"));
        attributes.push(parse_crate(&path, dep.clone()));
    }
    drop(deps_lock);
    attributes
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
            (parsed, equation) = parse_meta_for_cfg_attr(&attr.meta, &ctx);
            if is_no_std(&parsed) {
                atleast_one_no_std = true;
                debug!("Found no_std");
                break;
            }
        }
    }
    (atleast_one_no_std, equation, parsed)
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
    let mut temp_eq: Option<Bool>;
    let mut parsed: ParsedAttr = ParsedAttr::default();
    for attr in &attrs.attributes {
        let ident = attr.path().get_ident().unwrap();
        if ident == "cfg" {
            // println!("{}", attr.to_token_stream());
            (parsed, temp_eq) = parse_meta_for_cfg_attr(&attr.meta, &ctx);
            if parsed.features.len() == 1 || parsed.logic.is_empty() {
                // Attributes like `#[cfg (feature = "serde")]` are not interesting.
                continue;
            }
            equation.push(temp_eq);
        }
    }

    (equation, parsed.possible_target_archs)
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
    main_features: &Vec<String>,
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
    let mut was_target_arch = false;
    let mut current_expr: Option<Bool> = None;
    let mut group_items: Vec<Bool> = Vec::new();
    let mut curr_logic = Logic::And;

    for token in tokens {
        match token {
            proc_macro2::TokenTree::Group(g) => {
                let mut group_expr = None;
                let local_group_items =
                    parse_token_stream(g.stream(), parsed, ctx, &mut group_expr, in_any);

                let refs: Vec<&Bool> = local_group_items.iter().collect();
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
                }
            }
            _ => {}
        }
    }

    if let Some(expr) = current_expr {
        *equation = Some(expr.clone());
        group_items.push(expr);
    }

    group_items
}

fn parse_meta_for_cfg_attr<'a>(
    meta: &Meta,
    ctx: &'a z3::Context,
) -> (ParsedAttr, Option<Bool<'a>>) {
    match meta {
        Meta::List(list) => {
            let tokens = list.tokens.clone();
            let mut parsed = ParsedAttr::default();
            let mut equation = None;
            parse_token_stream(tokens, &mut parsed, ctx, &mut equation, &mut false);
            (parsed, equation)
        }
        _ => {
            unreachable!("cfg_attr and cfg attributes must have a meta list");
        }
    }
}

fn get_all_rs_files(path: &str) -> Vec<String> {
    let mut files = Vec::new();
    for entry in WalkDir::new(path) {
        let entry = entry.unwrap();
        if entry.path().extension().unwrap_or_default() == "rs" {
            files.push(entry.path().to_str().unwrap().to_string());
        }
    }
    files
}

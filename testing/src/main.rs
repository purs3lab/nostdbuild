#![cfg_attr(any(feature = "bar", feature = "no_std"), no_std)]

use itertools::Itertools;
use proc_macro2;
use proc_macro2::TokenStream;
use regex::Regex;
use serde_json::json;
use std::collections::HashSet;
use std::ffi::OsStr;
use std::fs;
use std::io;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::vec;
use syn::parse_quote;
use syn::Attribute;
use syn::Item;
use z3::SatResult;

#[derive(Clone, Debug)]
enum PLogic {
    And,
    Or,
    Not,
}

impl PLogic {
    fn combine<'a>(
        self,
        ctx: &'a z3::Context,
        values: Vec<z3::ast::Bool<'a>>,
    ) -> z3::ast::Bool<'a> {
        match self {
            PLogic::And => {
                let final_val = z3::ast::Bool::and(ctx, &values);
                final_val
            }
            PLogic::Or => {
                let final_val = z3::ast::Bool::or(ctx, &values);
                final_val
            }
            PLogic::Not => {
                let final_val = !values[0].clone();
                final_val
            }
        }
    }
}

impl TryFrom<&str> for PLogic {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let value = value.to_lowercase();
        if value == "all" {
            Ok(PLogic::And)
        } else if value == "any" {
            Ok(PLogic::Or)
        } else if value == "not" {
            Ok(PLogic::Not)
        } else {
            Err(())
        }
    }
}

fn find_all_features_used(
    ctx: &z3::Context,
    stream: proc_macro2::TokenTree,
) -> (Vec<String>, Vec<z3::ast::Bool>, Vec<z3::ast::Bool>) {
    if let proc_macro2::TokenTree::Group(group) = stream {
        let mut constants: Vec<z3::ast::Bool> = vec![];
        let mut bools: Vec<z3::ast::Bool> = vec![];
        let mut new_features: Vec<String> = vec![];
        let mut last_ident_was_feature = true;
        let mut last_logic: Option<PLogic> = None;
        for val in group.stream().into_iter() {
            let (mut new_list, mut equations, mut cs) = match val {
                proc_macro2::TokenTree::Group(group) => {
                    let (new_group_list, new_b, new_cs) = find_all_features_used(ctx, group.into());
                    let new_b = if last_logic.is_some() {
                        vec![last_logic.clone().unwrap().combine(ctx, new_b.clone())]
                    } else {
                        new_b
                    };
                    (new_group_list, new_b, new_cs)
                }
                proc_macro2::TokenTree::Ident(ident) => {
                    let temp = PLogic::try_from(ident.to_string().as_str());
                    if let Ok(new_val) = temp {
                        last_logic = Some(new_val);
                    }

                    last_ident_was_feature = ident.to_string() == "feature";
                    last_ident_was_feature = last_ident_was_feature
                        || !vec![
                            "clippy",
                            "debug_assertions",
                            "doc",
                            "doctest",
                            "fmt_debug",
                            "miri",
                            "overflow_checks",
                            "panic",
                            "proc_macro",
                            "relocation_model",
                            "rustfmt",
                            "sanitize",
                            "sanitizer_cfi_generalize_pointers",
                            "sanitizer_cfi_normalize_integers",
                            "target_abi",
                            "target_arch",
                            "target_endian",
                            "target_env",
                            "target_family",
                            "target_feature",
                            "target_has_atomic",
                            "target_has_atomic_equal_alignment",
                            "target_has_atomic_load_store",
                            "target_os",
                            "target_pointer_width",
                            "target_thread_local",
                            "target_vendor",
                            "test",
                            "ub_checks",
                            "unix",
                            "windows",
                        ]
                        .contains(&ident.to_string().as_str());
                    (vec![], vec![], vec![])
                }
                proc_macro2::TokenTree::Punct(_) => (vec![], vec![], vec![]),
                proc_macro2::TokenTree::Literal(literal) => {
                    if last_ident_was_feature {
                        let literal = literal.to_string();
                        let literal = literal[1..literal.len() - 1].to_string();
                        let new_variable = z3::ast::Bool::new_const(ctx, literal.clone());
                        (
                            vec![literal],
                            vec![new_variable.clone()],
                            vec![new_variable.clone()],
                        )
                    } else {
                        (vec![], vec![], vec![])
                    }
                }
            };
            bools.append(&mut equations);
            new_features.append(&mut new_list);
            constants.append(&mut cs);
        }
        (new_features, bools, constants)
    } else {
        (vec![], vec![], vec![])
    }
}

fn is_no_std(a: Attribute) -> bool {
    a.path().is_ident("cfg_attr")
        && a.parse_args().ok().map_or(false, |x: TokenStream| {
            x.into_iter().any(|y| match y {
                proc_macro2::TokenTree::Ident(ident) => ident.to_string() == "no_std",
                _ => false,
            })
        })
}

fn find_no_std_predicate(precondition: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    precondition
        .into_iter()
        .take_while(|x| {
            if let proc_macro2::TokenTree::Punct(punct) = x {
                punct.as_char() != ','
            } else {
                true
            }
        })
        .collect()
}

fn stream_contains_no_std_predicate(
    no_std_predicate: proc_macro2::TokenStream,
    stream: proc_macro2::TokenStream,
) -> bool {
    dbg!("contains? ");
    dbg!(&no_std_predicate.to_string());
    dbg!(&stream.to_string());
    let stream_vec: Vec<proc_macro2::TokenTree> = stream
        .into_iter()
        .map(|x| {
            // dbg!(x.clone().to_string());
            x
        })
        .collect();
    let stream_vec_slice: &[proc_macro2::TokenTree] = &stream_vec;

    let no_std_predicate_vec: Vec<proc_macro2::TokenTree> =
        no_std_predicate.clone().into_iter().collect();

    let res = stream_vec_slice
        .windows(no_std_predicate_vec.len())
        .any(|window| {
            let window = proc_macro2::TokenStream::from_iter(window.to_vec().into_iter());
            // dbg!(&window.to_string());
            window.to_string() == no_std_predicate.to_string()
        });

    let mut temp = false;
    if !res {
        for v in stream_vec.into_iter() {
            if let proc_macro2::TokenTree::Group(group) = v {
                temp = temp
                    || stream_contains_no_std_predicate(no_std_predicate.clone(), group.stream());
            }
        }
        dbg!(&temp);
        return temp;
    }

    dbg!(&res);
    res
}

fn ast_to_features(
    ctx: &z3::Context,
    syntax: syn::File,
    features: HashSet<String>,
    no_std_predicate: proc_macro2::TokenStream,
) -> (HashSet<String>, Vec<z3::ast::Bool>, Vec<z3::ast::Bool>) {
    let (attributes, auto_include) = find_all_attributes(syntax);
    let attributes = attributes.unwrap_or_default();

    let mut new_features: HashSet<String> = features;
    let mut all_bools: Vec<z3::ast::Bool> = vec![];
    let mut all_variables: Vec<z3::ast::Bool> = vec![];
    for a in attributes {
        // TODO: can we remove the cfg_attr part
        if !(a.path().is_ident("cfg") || a.path().is_ident("cfg_attr")) {
            continue;
        }

        let precondition: proc_macro2::TokenStream = a.parse_args().unwrap();
        dbg!(&precondition.to_string());
        if !(stream_contains_no_std_predicate(no_std_predicate.clone(), precondition.clone())) {
            if !auto_include.contains(&a) {
                dbg!("continuing ");
                dbg!(&no_std_predicate.to_string());
                dbg!(&precondition.to_string());
                continue;
            } else {
                dbg!("not continuing because compile error things");
                dbg!(&precondition.to_string());
            }
        } else {
            dbg!("not continuing");
            dbg!(&no_std_predicate.to_string());
            dbg!(&precondition.to_string());
        }

        let group = proc_macro2::Group::new(proc_macro2::Delimiter::Parenthesis, precondition);
        let (temp_features, mut bools, mut constants) =
            find_all_features_used(ctx, proc_macro2::TokenTree::Group(group));
        let temp_features: HashSet<String> = HashSet::from_iter(temp_features);
        if temp_features.intersection(&new_features).count() > 0 {
            new_features = temp_features.union(&new_features).cloned().collect();
        }
        all_bools.append(&mut bools);
        all_variables.append(&mut constants);
    }
    (new_features, all_bools, all_variables)
}

fn file_to_ast(file: &Path) -> syn::File {
    let src = fs::read_to_string(file).expect("unable to read file");
    syn::parse_file(&src).expect("unable to parse file")
}

fn find_all_rust_files(dir: &Path) -> io::Result<Vec<PathBuf>> {
    let mut all_projects: Vec<PathBuf> = vec![];

    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            all_projects.append(&mut find_all_rust_files(&path).unwrap_or(vec![]));
        } else if path.is_file() && path.extension().unwrap_or(OsStr::new("t")) == "rs" {
            all_projects.append(&mut vec![path.to_path_buf()]);
        }
    }
    Ok(all_projects)
}

fn is_src_dir(path: &PathBuf) -> bool {
    let pattern = Regex::new("src").unwrap();
    if let Some(file_name) = path.file_name().and_then(|os_str| os_str.to_str()) {
        if pattern.is_match(file_name) {
            return true;
        }
    }
    false
}

fn find_all_projects(dir: &Path) -> io::Result<Vec<PathBuf>> {
    if !dir.is_dir() {
        return Ok(vec![]);
    }

    let mut all_projects: Vec<PathBuf> = vec![];

    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            if is_src_dir(&path) {
                return Ok(vec![path]);
            } else {
                all_projects.append(&mut find_all_projects(&path).unwrap_or_default());
            }
        }
    }

    Ok(all_projects)
}

fn find_cfg_attr_no_std_features(
    ctx: &z3::Context,
    files: Vec<PathBuf>,
) -> (
    Option<Vec<String>>,
    Option<z3::ast::Bool>,
    Vec<proc_macro2::TokenStream>,
    Vec<z3::ast::Bool>,
) {
    let mut all_features: Vec<String> = vec![];
    let mut all_bools: Vec<z3::ast::Bool> = vec![];
    let mut all_streams: Vec<proc_macro2::TokenStream> = vec![];
    let mut all_constants: Vec<z3::ast::Bool> = vec![];
    for file in files {
        let ast = file_to_ast(&file);
        let (attributes, _) = find_all_attributes(ast);
        if let Some(attributes) = attributes {
            for a in attributes {
                if a.path().is_ident("cfg_attr") && is_no_std(a.clone()) {
                    let precondition: proc_macro2::TokenStream = a.parse_args().unwrap();
                    dbg!(&precondition.to_string());
                    let stream = find_no_std_predicate(precondition.clone());
                    all_streams.push(stream);
                    let group =
                        proc_macro2::Group::new(proc_macro2::Delimiter::Parenthesis, precondition);
                    let (mut features, mut bools, mut cs) =
                        find_all_features_used(ctx, proc_macro2::TokenTree::Group(group));
                    all_features.append(&mut features);
                    all_bools.append(&mut bools);
                    all_constants.append(&mut cs);
                }
            }
        }
    }
    let all_features = if all_features.is_empty() {
        None
    } else {
        Some(all_features)
    };
    let all_bools = if all_bools.is_empty() {
        None
    } else if all_bools.len() == 1 {
        Some(all_bools[0].clone())
    } else {
        Some(PLogic::And.combine(ctx, all_bools))
    };

    dbg!(&all_streams.len());
    (all_features, all_bools, all_streams, all_constants)
}

fn find_all_attributes(file: syn::File) -> (Option<Vec<Attribute>>, Vec<Attribute>) {
    let mut all_attributes: Vec<Attribute> = file.attrs.into_iter().collect();
    let mut auto_include: Vec<Attribute> = vec![];
    let attrs: Vec<Vec<Attribute>> = file
        .items
        .into_iter()
        .map(|a| match a {
            Item::Const(item_const) => item_const.attrs.clone(),
            Item::Enum(item_enum) => item_enum.attrs.clone(),
            Item::ExternCrate(item_extern_crate) => item_extern_crate.attrs.clone(),
            Item::Fn(item_fn) => item_fn.attrs.clone(),
            Item::ForeignMod(item_foreign_mod) => item_foreign_mod.attrs.clone(),
            Item::Impl(item_impl) => item_impl.attrs.clone(),
            Item::Macro(item_macro) => {
                let mut attributes: Vec<Attribute> = vec![];
                if item_macro.mac.path.is_ident("compile_error") {
                    for a in item_macro.attrs.clone().into_iter() {
                        let debug_stuff: proc_macro2::TokenStream = a.clone().parse_args().unwrap();
                        dbg!(&debug_stuff.to_string());
                        dbg!(&a.meta);
                        if !a.path().is_ident("cfg") {
                            attributes.push(a);
                            continue;
                        }

                        let args: proc_macro2::TokenStream = a.parse_args().unwrap();
                        dbg!(&args.clone().to_string());
                        // let nested_meta: syn::Meta = syn::parse2(args.clone()).expect("Failed to parse args as NestedMeta");
                        // dbg!(&nested_meta);
                        let new_thing: proc_macro2::TokenStream = parse_quote!(not(#args));
                        dbg!(&new_thing.to_string());
                        let new_meta = syn::Meta::List(syn::MetaList {
                            path: syn::Path::from(syn::Ident::new(
                                "cfg",
                                proc_macro2::Span::call_site(),
                            )),
                            delimiter: syn::MacroDelimiter::Paren(syn::token::Paren::default()),
                            tokens: new_thing.clone(),
                        });
                        dbg!(&new_thing);
                        dbg!(&new_meta);

                        // Create a new Attribute with the negated `cfg`
                        let new_attribute = Attribute {
                            pound_token: a.pound_token,
                            style: a.style.clone(),
                            bracket_token: a.bracket_token,
                            meta: new_meta,
                        };
                        attributes.push(new_attribute.clone());

                        auto_include.push(new_attribute.clone());

                        let debug_stuff: proc_macro2::TokenStream =
                            new_attribute.clone().parse_args().unwrap();
                        dbg!(&debug_stuff.to_string());
                    }
                    attributes
                } else {
                    item_macro.attrs.clone()
                }
            }
            Item::Mod(item_mod) => item_mod.attrs.clone(),
            Item::Static(item_static) => item_static.attrs.clone(),
            Item::Struct(item_struct) => item_struct.attrs.clone(),
            Item::Trait(item_trait) => item_trait.attrs.clone(),
            Item::TraitAlias(item_trait_alias) => item_trait_alias.attrs.clone(),
            Item::Type(item_type) => item_type.attrs.clone(),
            Item::Union(item_union) => item_union.attrs.clone(),
            Item::Use(item_use) => item_use.attrs.clone(),
            Item::Verbatim(_) => vec![],
            _ => vec![],
        })
        .collect();
    for mut val in attrs {
        all_attributes.append(&mut val);
    }

    if all_attributes.is_empty() {
        (None, auto_include)
    } else {
        (Some(all_attributes), auto_include)
    }
}

fn feature_options(features: HashSet<String>) -> Vec<String> {
    let length = features.len();
    let mut combinations: Vec<String> = vec![];
    for i in 1..length + 1 {
        let combination: Vec<Vec<String>> = features.clone().into_iter().combinations(i).collect();
        dbg!(&combination);
        let mut combination: Vec<String> = combination.into_iter().map(|a| a.join(",")).collect();
        combinations.append(&mut combination);
    }
    dbg!(&combinations);
    combinations
}

fn execute_cargo_build_command(args: Vec<String>, project: String) -> (serde_json::Value, bool) {
    dbg!(&args);
    let mut binding = Command::new("cargo");
    let output = binding.args(args.clone());
    output.current_dir(project);
    let output = output.output().unwrap();
    let status = output.status.code().unwrap();
    let stdout = String::from_utf8(output.stdout).unwrap();
    let stderr = String::from_utf8(output.stderr).unwrap();

    (
        json!({
            "features": args,
            "code": status,
            "stdout": stdout,
            "stderr": stderr,
        }),
        status == 0,
    )
}

fn solve_and_build_project(
    project: PathBuf,
    solver: z3::Solver,
    constants: HashSet<z3::ast::Bool>,
) -> Result<(serde_json::Value, Option<String>), ()> {
    let result = solver.check();
    if result != SatResult::Sat {
        return Err(());
    }

    dbg!(&result);
    let model = solver.get_model();
    if model.is_none() {
        return Err(());
    }
    let model = model.unwrap();
    let mut features_to_use: HashSet<String> = HashSet::new();
    for c in constants.into_iter() {
        let res = model.eval(&c, false).unwrap().as_bool().unwrap();
        if res {
            features_to_use.insert(c.to_string());
        }
    }
    dbg!(&features_to_use);

    Ok(build_once(project, features_to_use))
}

fn build_once(project: PathBuf, features: HashSet<String>) -> (serde_json::Value, Option<String>) {
    let project_string: String = String::from(project.to_str().unwrap_or(""));
    dbg!(&project_string);

    let mut args: Vec<String> = vec!["build".to_string()];
    args.push("--target".to_string());
    args.push("thumbv7m-none-eabi".to_string());
    let mut correct_features: Option<String>;

    if features.is_empty() {
        correct_features = Some("default".to_string());
    } else {
        args.push("--no-default-features".to_string());
        args.push("--features".to_string());
        let features: String = features.into_iter().join(",");
        args.push(features.clone());
        correct_features = Some(features);
    }
    let (command, is_successful) =
        execute_cargo_build_command(args.clone(), project_string.clone());
    if !is_successful {
        correct_features = None
    }

    (json!(command), correct_features)
}

fn build_project(project: PathBuf, features: HashSet<String>) -> (serde_json::Value, Option<String>) {
    let project_string: String = String::from(project.to_str().unwrap_or(""));
    dbg!(&project_string);

    let mut args: Vec<String> = vec!["build".to_string()];
    args.push("--target".to_string());
    args.push("thumbv7m-none-eabi".to_string());

    let mut commands: Vec<serde_json::Value> = vec![];
    let mut correct_features: Option<String> = None;

    if features.is_empty() {
        let (command, is_successful) =
            execute_cargo_build_command(args.clone(), project_string.clone());
        commands.push(command);
        if is_successful {
            correct_features = Some("default".to_string());
        }
    } else {
        args.push("--no-default-features".to_string());
        let (command, is_successful) =
            execute_cargo_build_command(args.clone(), project_string.clone());
        commands.push(command);
        if is_successful {
            correct_features = None;
        }

        args.push("--features".to_string());
        for option in feature_options(features) {
            args.push(option.clone());
            let (command, is_successful) =
                execute_cargo_build_command(args.clone(), project_string.clone());
            commands.push(command);
            if is_successful {
                correct_features = Some(option);
            }
            args.pop();
        }
    }

    (json!(commands), correct_features)
}

fn main() {
    let cfg = z3::Config::new();
    let ctx = z3::Context::new(&cfg);
    let solver = z3::Solver::new(&ctx);

    let folder = std::env::args().nth(1).expect("no directory given");
    let path = Path::new(&folder);
    let projects: Vec<PathBuf> = find_all_projects(path).unwrap_or_default();
    dbg!(&projects);

    let mut filename = String::from(path.file_name().unwrap().to_str().unwrap());
    filename.push_str(".json");
    let mut directory = "results/".to_string();
    directory.push_str(filename.as_str());

    let mut file = fs::File::create(directory).expect("bad");
    let mut projects_json: Vec<serde_json::Value> = vec![];
    let mut all_constants: HashSet<z3::ast::Bool> = HashSet::new();

    for project in projects {
        dbg!(&project);
        let rust_files = find_all_rust_files(&project).unwrap_or_default();
        let (initial_features, initial_bools, predicate, cs) =
            find_cfg_attr_no_std_features(&ctx, rust_files.clone());
        for c in cs {
            all_constants.insert(c);
        }
        if initial_features.is_none() {
            dbg!("no features for this project");
            let (commands, correct_features) = build_once(project.clone(), HashSet::new());
            projects_json.push(json!({
                "project": String::from(project.clone().to_str().unwrap()),
                "features": [],
                "commands": commands,
                "built_with_features": correct_features,
            }));
            continue;
        }

        let predicate = predicate[0].clone();
        dbg!(&initial_bools);
        solver.assert(&initial_bools.unwrap());

        let mut initial_features: HashSet<String> = HashSet::from_iter(initial_features.unwrap());

        for file in rust_files {
            let ast = file_to_ast(&file);
            dbg!("finding all attributes for file: ");
            dbg!(&file);
            let (more_features, bools, constants) =
                ast_to_features(&ctx, ast, initial_features, predicate.clone());
            initial_features = more_features;
            dbg!(&bools);
            for b in bools.into_iter() {
                solver.assert(&b);
            }
            for c in constants {
                all_constants.insert(c);
            }
        }
        dbg!(&solver);
        dbg!(&project);
        dbg!(&initial_features);

        let r = solve_and_build_project(project.clone(), solver.clone(), all_constants.clone());
        if r.is_err() {
            dbg!(&solver);
            return;
        }
        let (commands, correct_features) = r.unwrap();

        projects_json.push(json!({
            "project": String::from(project.clone().to_str().unwrap()),
            "features": initial_features.clone(),
            "commands": commands,
            "built_with_features": correct_features,
        }));
    }

    let value = json!(projects_json);
    let value = serde_json::to_string_pretty(&value).unwrap();
    let result = file.write_all(value.as_bytes());
    return;
}

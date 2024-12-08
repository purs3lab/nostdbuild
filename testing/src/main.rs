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
use std::io::Bytes;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::vec;
use syn::ext::IdentExt;
use syn::token::Not;
use syn::token::Plus;
use syn::Attribute;
use syn::Item;

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

fn print_type<T>(_: &T) {
    println!("{:?}", std::any::type_name::<T>());
}

fn find_all_features_used(
    ctx: &z3::Context,
    stream: proc_macro2::TokenTree,
) -> (Vec<String>, Vec<z3::ast::Bool>) {
    if let proc_macro2::TokenTree::Group(group) = stream {
        let mut bools: Vec<z3::ast::Bool> = vec![];
        let mut new_features: Vec<String> = vec![];
        let mut last_ident_was_feature = true;
        let mut last_logic: Option<PLogic> = None;
        for val in group.stream().into_iter() {
            let (mut new_list, mut b) = match val {
                proc_macro2::TokenTree::Group(group) => {
                    let (new_group_list, new_b) = find_all_features_used(ctx, group.into());
                    let new_b = if last_logic.is_some() {
                        vec![last_logic.clone().unwrap().combine(ctx, new_b)]
                    } else {
                        new_b
                    };
                    (new_group_list, new_b)
                }
                proc_macro2::TokenTree::Ident(ident) => {
                    // dbg!(ident.to_string().as_str());
                    let temp = PLogic::try_from(ident.to_string().as_str());
                    // dbg!(&temp);
                    if let Ok(new_val) = temp {
                        // dbg!(&new_val);
                        last_logic = Some(new_val);
                    }

                    last_ident_was_feature = ident.to_string() == "feature";
                    last_ident_was_feature = last_ident_was_feature
                        || !vec![
                            // TODO: update white list, if ident is not one of these, consider it
                            "target_arch",
                            "target_feature",
                        ]
                        .contains(&ident.to_string().as_str());
                    (vec![], vec![])
                }
                proc_macro2::TokenTree::Punct(punct) => (vec![], vec![]),
                proc_macro2::TokenTree::Literal(literal) => {
                    if last_ident_was_feature {
                        let literal = literal.to_string();
                        let literal = literal[1..literal.len() - 1].to_string();
                        let new_variable = z3::ast::Bool::new_const(ctx, literal.clone());
                        (vec![literal], vec![new_variable])
                    } else {
                        (vec![], vec![])
                    }
                }
            };
            bools.append(&mut b);
            new_features.append(&mut new_list);
        }
        (new_features, bools)
    } else {
        (vec![], vec![])
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

fn ast_to_features(
    ctx: &z3::Context,
    syntax: syn::File,
    features: HashSet<String>,
) -> (HashSet<String>, Vec<z3::ast::Bool>) {
    let attributes = find_all_attributes(syntax).unwrap_or(vec![]);

    let mut new_features: HashSet<String> = features;
    let mut all_bools: Vec<z3::ast::Bool> = vec![];
    for a in attributes {
        if !(a.path().is_ident("cfg") || a.path().is_ident("cfg_attr")) {
            continue;
        }

        let t: proc_macro2::TokenStream = a.parse_args().unwrap();
        let group =
            proc_macro2::Group::new(proc_macro2::Delimiter::Parenthesis, t);
        let (temp_features, mut bools) =
            find_all_features_used(ctx, proc_macro2::TokenTree::Group(group));
        // for x in t.into_iter() {
        //     let (mut f, b) = find_all_features_used(&ctx, x);
        //     temp_features.append(&mut f);
        //     if b.len() > 1 {
        //         all_bools.push(PLogic::And.combine(ctx, b));
        //     } else if b.len() == 1 {
        //         all_bools.push(b[0].clone());
        //     }
        // }
        let temp_features: HashSet<String> = HashSet::from_iter(temp_features);
        if temp_features.intersection(&new_features).count() > 0 {
            new_features = temp_features.union(&new_features).cloned().collect();
        }
        all_bools.append(& mut bools);
    }
    (new_features, all_bools)

    //     find_all_attributes(syntax).map(| attributes | {
    //         attributes.into_iter()
    //         .filter(| a | {
    //             a.path().is_ident("cfg") || a.path().is_ident("cfg_attr")
    //         })
    //         .map(| a | {
    //             let t: proc_macro2::TokenStream = a.parse_args().unwrap();
    //             let features: Vec<String> = t.into_iter().map(|tree | {
    //                 find_all_features_used(tree)
    //             }).flatten().collect();
    //             features
    //         }).flatten().collect()
    //    }).unwrap_or(HashSet::new())
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

fn visit_dirs(dir: &Path, cb: &mut dyn FnMut(&fs::DirEntry)) -> io::Result<()> {
    if dir.is_dir() {
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir()
                && !path.ends_with("examples")
                && !path.ends_with("benches")
                && !path.ends_with("tests")
            {
                visit_dirs(&path, cb)?;
            } else {
                cb(&entry);
            }
        }
    }
    Ok(())
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
                all_projects.append(&mut find_all_projects(&path).unwrap_or(vec![]));
            }
        }
    }

    Ok(all_projects)
}

fn find_cfg_attr_no_std_features(
    ctx: &z3::Context,
    files: Vec<PathBuf>,
) -> (Option<Vec<String>>, Option<z3::ast::Bool>) {
    let mut all_features: Vec<String> = vec![];
    let mut all_bools: Vec<z3::ast::Bool> = vec![];
    for file in files {
        let ast = file_to_ast(&file);
        let attributes = find_all_attributes(ast);
        if let Some(attributes) = attributes {
            for a in attributes {
                if a.path().is_ident("cfg_attr") && is_no_std(a.clone()) {
                    let precondition: proc_macro2::TokenStream = a.parse_args().unwrap();
                    // dbg!(&precondition);
                    let group =
                        proc_macro2::Group::new(proc_macro2::Delimiter::Parenthesis, precondition);
                    let (mut features, mut bools) =
                        find_all_features_used(ctx, proc_macro2::TokenTree::Group(group));
                    // let mut features: Vec<String> = vec![];
                    // let mut bools: Vec<z3::ast::Bool> = vec![];
                    // for token in precondition.into_iter() {
                    //     let (mut f, mut b) = find_all_features_used(ctx, token);
                    //     features.append(&mut f);
                    //     bools.append(&mut b);
                    // }
                    all_features.append(&mut features);
                    all_bools.append(&mut bools);
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

    (all_features, all_bools)
}

fn find_all_attributes(file: syn::File) -> Option<Vec<Attribute>> {
    let mut all_attributes: Vec<Attribute> = file.attrs.into_iter().collect();
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
                dbg!(&item_macro.mac.path.get_ident());
                if item_macro.mac.path.is_ident("compile_error") {
                    dbg!("found compiler error macro");
                    vec![]
                } else {
                    item_macro.attrs.clone()
                }
            },
            Item::Mod(item_mod) => item_mod.attrs.clone(),
            Item::Static(item_static) => item_static.attrs.clone(),
            Item::Struct(item_struct) => item_struct.attrs.clone(),
            Item::Trait(item_trait) => item_trait.attrs.clone(),
            Item::TraitAlias(item_trait_alias) => item_trait_alias.attrs.clone(),
            Item::Type(item_type) => item_type.attrs.clone(),
            Item::Union(item_union) => item_union.attrs.clone(),
            Item::Use(item_use) => item_use.attrs.clone(),
            Item::Verbatim(token_stream) => vec![],
            _ => vec![],
        })
        .collect();
    for mut val in attrs {
        all_attributes.append(&mut val);
    }

    if all_attributes.is_empty() {
        None
    } else {
        Some(all_attributes)
    }
}

// fn cd_into_dir(project: &Path) {
//     let project_string: String = String::from(project.to_str().unwrap_or(""));
//     let output = Command::new("cd")
//         .arg(project_string)
//         .output();
//     dbg!(output);
// }

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
    // dbg!(&output);
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

fn build_project(project: PathBuf, features: HashSet<String>) -> (serde_json::Value, String) {
    let project_string: String = String::from(project.to_str().unwrap_or(""));
    dbg!(&project_string);

    let mut args: Vec<String> = vec!["build".to_string()];
    args.push("--target".to_string());
    args.push("thumbv7m-none-eabi".to_string());

    let mut commands: Vec<serde_json::Value> = vec![];
    let mut correct_features: String = "no correct features".to_string();

    if features.is_empty() {
        let (command, is_successful) =
            execute_cargo_build_command(args.clone(), project_string.clone());
        commands.push(command);
        if is_successful {
            correct_features = "default".to_string();
        }
    } else {
        args.push("--no-default-features".to_string());
        let (command, is_successful) =
            execute_cargo_build_command(args.clone(), project_string.clone());
        commands.push(command);
        if is_successful {
            correct_features = "none".to_string();
        }

        args.push("--features".to_string());
        for option in feature_options(features) {
            args.push(option.clone());
            let (command, is_successful) =
                execute_cargo_build_command(args.clone(), project_string.clone());
            commands.push(command);
            if is_successful {
                correct_features = option;
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

    let var1 = z3::ast::Bool::new_const(&ctx, "variable1");
    let var2 = z3::ast::Bool::new_const(&ctx, "variable2");
    let var3 = z3::ast::Bool::new_const(&ctx, "variable3");
    let var4 = z3::ast::Bool::new_const(&ctx, "variable4");

    // let formula = z3::ast::Bool::and(&ctx, &[&and_var1_var2_bit, &not_var3]);
    let formula = &var1 & &var2 & (!&var3 | &var4);
    let another_formula = (&var1 & !&var4) | &var3;
    let hi = var1.clone();

    // all ar z3::ast::Bool
    print_type(&another_formula);
    print_type(&formula);
    print_type(&var1);
    print_type(&var2);
    print_type(&var3);
    solver.assert(&formula);
    solver.assert(&another_formula);
    let result = solver.check();
    dbg!(&result);
    let model = solver.get_model().unwrap();
    dbg!(&model);
    println!(
        "var1 -> {}",
        model.eval(&var1, false).unwrap().as_bool().unwrap()
    );
    println!(
        "var2 -> {}",
        model.eval(&var2, false).unwrap().as_bool().unwrap()
    );
    println!(
        "var3 -> {}",
        model.eval(&var3, false).unwrap().as_bool().unwrap()
    );
    println!(
        "var4 -> {}",
        model.eval(&var4, false).unwrap().as_bool().unwrap()
    );
    println!(
        "hi -> {}",
        model.eval(&hi, false).unwrap().as_bool().unwrap()
    );

    // for i in model.iter() {
    //     dbg!(&i);
    // }

    let folder = std::env::args().nth(1).expect("no directory given");
    let path = Path::new(&folder);
    let mut rust_files: Vec<String> = vec![];
    let projects: Vec<PathBuf> = find_all_projects(path).unwrap_or(vec![]);
    dbg!(&projects);

    let mut filename = String::from(path.file_name().unwrap().to_str().unwrap());
    filename.push_str(".json");
    let mut file = fs::File::create(filename).expect("bad");

    let mut projects_json: Vec<serde_json::Value> = vec![];

    for project in projects {
        dbg!(&project);
        let rust_files = find_all_rust_files(&project).unwrap_or(vec![]);
        // dbg!(&rust_files);
        let (initial_features, initial_bools) =
            find_cfg_attr_no_std_features(&ctx, rust_files.clone());
        // dbg!(&initial_features);
        if initial_features.is_none() {
            dbg!("no features for this project");
            let (commands, correct_features) = build_project(project.clone(), HashSet::new());
            projects_json.push(json!({
                "project": String::from(project.clone().to_str().unwrap()),
                "features": [],
                "commands": commands,
                "built_with_features": correct_features,
            }));
            continue;
        }
        dbg!(&initial_bools);
        solver.assert(&initial_bools.unwrap());

        let mut initial_features: HashSet<String> = HashSet::from_iter(initial_features.unwrap());

        for file in rust_files {
            let ast = file_to_ast(&file);
            dbg!("finding all attributes for file: ");
            dbg!(&file);
            let (more_features, bools) = ast_to_features(&ctx, ast, initial_features);
            initial_features = more_features;
            dbg!(&bools);
            for b in bools.into_iter() {
                solver.assert(&b);
            }
            // dbg!(file);
            // dbg!(&initial_features);
            // let intersection: HashSet<String> = initial_features.intersection(&features).cloned().collect();
            // if !intersection.is_empty() {
            //     initial_features = initial_features.union(&features).cloned().collect();
            //     dbg!(initial_features);
            // }
        }
        dbg!(&solver);
        dbg!(&project);
        dbg!(&initial_features);
        let (commands, correct_features) = build_project(project.clone(), initial_features.clone());

        projects_json.push(json!({
            "project": String::from(project.clone().to_str().unwrap()),
            "features": initial_features.clone(),
            "commands": commands,
            "built_with_features": correct_features,
        }));
    }

    let value = json!(projects_json);
    let value = serde_json::to_string_pretty(&value).unwrap();
    // dbg!(&value);
    let result = file.write_all(value.as_bytes());
    // dbg!(result);

    // let res = visit_dirs(path, &mut |d: &DirEntry| {
    //     let p = d.path();
    //     if p.is_file() && p.extension().unwrap_or(OsStr::new("t")) == "rs" {
    //         rust_files.push(p.to_str().unwrap().to_string());
    //     }
    // });
    // dbg!(&rust_files);

    // for path in rust_files {
    //     let src = fs::read_to_string(&path).expect("unable to read file");
    //     let syntax = syn::parse_file(&src).expect("unable to parse file");
    // };
    return;
}

// Debug impl is available if Syn is built with "extra-traits" feature.
// println!("{:#?}", syntax);

/*
Steps:
1. Identify all cfg commands
2. Convert features to variables
3. Parse predicates and input them as equations in Z3
4. Solve Z3
*/

//     for i in syntax.items.iter() {
//         // TODO: iterate through all items and exclude those that are not cfg
//         let type_of = match i {
//             Item::Const(item_const) => "const",
//             Item::Enum(item_enum) => "enum",
//             Item::ExternCrate(item_extern_crate) => "extern crate",
//             Item::Fn(item_fn) => "item func",
//             Item::ForeignMod(item_foreign_mod) => "foreign mod",
//             Item::Impl(item_impl) => "item_impl",
//             Item::Macro(item_macro) => "item_macro",
//             Item::Mod(item_mod) => {
//                 println!("this is an item mod");
//                 println!("{:#?}", item_mod.attrs);
//                 "item_mod"
//             },
//             // Item::Static(item_static) => todo!(),
//             // Item::Struct(item_struct) => todo!(),
//             // Item::Trait(item_trait) => todo!(),
//             // Item::TraitAlias(item_trait_alias) => todo!(),
//             // Item::Type(item_type) => todo!(),
//             // Item::Union(item_union) => todo!(),
//             Item::Use(item_use) => "item use",
//             // Item::Verbatim(token_stream) => todo!(),
//             _ => {
//                 "done"
//             },
//         };
//         println!("{:?}", type_of);
//     }

//     let mut all_attributes: Vec<Attribute> = syntax.attrs.clone().into_iter().collect();
//     let attrs: Vec<Vec<Attribute>> = syntax.items.clone().into_iter().map(|a | {
//         match a {
//             Item::Const(item_const) => item_const.attrs.clone(),
//             Item::Enum(item_enum) => item_enum.attrs.clone(),
//             Item::ExternCrate(item_extern_crate) => item_extern_crate.attrs.clone(),
//             Item::Fn(item_fn) => item_fn.attrs.clone(),
//             Item::ForeignMod(item_foreign_mod) => item_foreign_mod.attrs.clone(),
//             Item::Impl(item_impl) => item_impl.attrs.clone(),
//             Item::Macro(item_macro) => item_macro.attrs.clone(),
//             Item::Mod(item_mod) => item_mod.attrs.clone(),
//             Item::Static(item_static) => item_static.attrs.clone(),
//             Item::Struct(item_struct) => item_struct.attrs.clone(),
//             Item::Trait(item_trait) => item_trait.attrs.clone(),
//             Item::TraitAlias(item_trait_alias) => item_trait_alias.attrs.clone(),
//             Item::Type(item_type) => item_type.attrs.clone(),
//             Item::Union(item_union) => item_union.attrs.clone(),
//             Item::Use(item_use) => item_use.attrs.clone(),
//             Item::Verbatim(token_stream) => vec![],
//             _ => vec![],
//         }
//     }).collect();
//     for mut val in attrs {
//         all_attributes.append(&mut val);
//     }

//     let mut a = vec![];
//     for val in all_attributes.clone() {
//         if !(val.path().is_ident("cfg_attr") && is_no_std(val.clone())) {
//             continue;
//         }
//         dbg!(&val);
//         let precondition: proc_macro2::TokenStream = val.parse_args().unwrap();
//         dbg!(&precondition);
//         let precondition: proc_macro2::TokenStream = precondition.into_iter().map(| t | {
//             dbg!(t.clone());
//             a.append(& mut find_all_features_used(t.clone(), vec![]));
//             t
//         }).collect();
//     }
//     println!("a: {:?}", a);
//     let mut a: HashSet<String> = HashSet::from_iter(a.iter().cloned());
//     dbg!(&a);

//     for val in all_attributes {
//         if !(val.path().is_ident("cfg") || val.path().is_ident("cfg_attr")) {
//             continue;
//         }
//         let precondition: proc_macro2::TokenStream = val.parse_args().unwrap();
//         let mut features: Vec<String> = vec![];
//         let precondition: proc_macro2::TokenStream = precondition.into_iter().map(| t | {
//             features.append(&mut find_all_features_used(t.clone(), vec![]));
//             t
//         }).collect();

//         let set: HashSet<String> = HashSet::from_iter(features.iter().cloned());
//         let intersection: HashSet<_> = a.intersection(&set).collect();
//         if !intersection.is_empty() {
//             a = a.union(&set).cloned().collect();
//         }
//     }
//     dbg!(&a);

//     println!("\nfile-level attributes");
//     for a in syntax.attrs.clone().into_iter() {
//         if !(a.path().is_ident("cfg") || (a.path().is_ident("cfg_attr") && is_no_std(a.clone()))) {
//             continue;
//         }
//         let precondition: proc_macro2::TokenStream = a.parse_args().unwrap();
//         let mut features: Vec<String> = vec![];
//         // dbg!(a.clone());
//         let precondition: proc_macro2::TokenStream = precondition.into_iter().map(| a | {
//             features.append(&mut find_all_features_used(a.clone(), vec![]));
//             a
//         }).collect();
//         println!("\ntoken tree: {:?}, features: {:?}", precondition.to_string(), features);
//         println!("the condition: {:?}", precondition.to_string());
//         println!("is_no_std: {:?}", is_no_std(a.clone()))
//     }

//     println!("\nitem-level attributes");
//     for i in syntax.items.iter() {
//         // TODO: iterate through all items and exclude those that are not cfg
//         let attributes = match i {
//             Item::Const(item_const) => &item_const.attrs,
//             Item::Enum(item_enum) => &item_enum.attrs,
//             Item::ExternCrate(item_extern_crate) => &item_extern_crate.attrs,
//             Item::Fn(item_fn) => &item_fn.attrs,
//             Item::ForeignMod(item_foreign_mod) => &item_foreign_mod.attrs,
//             Item::Impl(item_impl) => &item_impl.attrs,
//             Item::Macro(item_macro) => &item_macro.attrs,
//             Item::Mod(item_mod) => &item_mod.attrs,
//             Item::Static(item_static) => &item_static.attrs,
//             Item::Struct(item_struct) => &item_struct.attrs,
//             Item::Trait(item_trait) => &item_trait.attrs,
//             Item::TraitAlias(item_trait_alias) => &item_trait_alias.attrs,
//             Item::Type(item_type) => &item_type.attrs,
//             Item::Union(item_union) => &item_union.attrs,
//             Item::Use(item_use) => &item_use.attrs,
//             Item::Verbatim(token_stream) => &vec![],
//             _ => &vec![],
//         };

//         for a in attributes.iter() {
//             if !(a.path().is_ident("cfg") || (a.path().is_ident("cfg_attr") && is_no_std(a.clone()))) {
//                 continue;
//             }
//             match a.style {
//                 syn::AttrStyle::Outer => {
//                     print!("outer attribute, ");
//                 },
//                 syn::AttrStyle::Inner(not) => {
//                     print!("innner attribute, ");
//                 },
//             }
//             let precondition: proc_macro2::TokenStream = a.parse_args().unwrap();
//             let precondition: proc_macro2::TokenStream = precondition.into_iter().map(| a | {
//                 let features = find_all_features_used(a.clone(), vec![]);
//                 println!("token tree: {:?}, features: {:?}", a.to_string(), features);
//                 a
//             }).collect();
//             println!("is_no_std: {:?}", is_no_std(a.clone()))
//         }
//     }
// }

// // no_std is an attribute (#![no_std]), and if we include it we cannot build bc println not in scope
// // // in the case that either "bar" or "no_std" is defined, this line becomes #![no_std]
// // #![cfg_attr(any(feature = "bar", feature = "no_std"), no_std)]

// // use std::array;

// // // even if we build for no_std, we can still bring in no_std with this line
// // extern crate std;

// // use core::panic::PanicInfo;

// // // #[cfg(...)] says include what is under this code only if the predicate is true
// // #[cfg(not(feature = "bar"))]
// // #[allow(dead_code)]
// // fn main() {

// //     // this is at runtime, not compile time
// //     if cfg!(not(feature = "bar")) {
// //         println!("Hello, world!");
// //     } else {
// //         println!("hi there, world");
// //     }

// //     // must include either feature "foo" or feature "bar" in cargo.toml
// //     #[cfg(any(feature = "bar", feature = "foo"))]
// //     std::println!("hi");

// //     #[cfg(all(target_family="unix", target_pointer_width = "64"))]
// //     std::println!("unix?");

// //     let mut vec = std::vec![0, 1, 2];

// //     // because this is not specified in cargo.toml as an option, we must pass it via "billy"
// //     #[cfg(feature = "billy")]
// //     println!("this must be specified at the command line: rustc --cfg=feature=\"billy\" src/main.rs && ./main");

// // }

// // #[cfg(feature = "bar")]
// // fn main() {}

// // // if we misspell feature, it will give a warning but still compile
// // #[cfg(featue = "no_std")]
// // fn no_std() {
// //     weofj
// // }

// // /*
// // Learnings:
// // must put feature = "foo" and then define that feature in cargo.toml
// // we set the default feature to "foo", so the "hi" printline is not grayed out
// // but we can pass "--no-default-features" to disable that
// // this command adds the configuration to the compilation: rustc --cfg=feature=\"bar\" src/main.rs
// // */

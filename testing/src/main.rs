#![cfg_attr(any(feature = "bar", feature = "no_std"), no_std)]

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
use proc_macro2;
use proc_macro2::TokenStream;
use serde_json::json;
use syn::Attribute;
use regex::Regex;
use syn::Item;
use itertools::Itertools;

fn find_all_features_used(stream: proc_macro2::TokenTree) -> Vec<String> {
    if let proc_macro2::TokenTree::Group(group) = stream {
        let mut new_features: Vec<String> = vec![];
        let mut last_ident_was_feature = true;
        for val in group.stream().into_iter() {
            let mut new_list: Vec<String> = match val {
                proc_macro2::TokenTree::Group(group) => {
                    find_all_features_used(group.into())
                },
                proc_macro2::TokenTree::Ident(ident) => {
                    last_ident_was_feature = ident.to_string() == "feature";
                    vec![]
                },
                proc_macro2::TokenTree::Punct(punct) => vec![],
                proc_macro2::TokenTree::Literal(literal) => {
                    if last_ident_was_feature {
                        let literal = literal.to_string();
                        vec![literal[1..literal.len() - 1].to_string()]
                    } else {
                        vec![]
                    }
                },
            };
            new_features.append(& mut new_list);
        };
        new_features
    } else {
        vec![]
    }
}

fn is_no_std(a: Attribute) -> bool {
    a.path().is_ident("cfg_attr") 
    && a.parse_args().ok().map_or(false, | x: TokenStream | {
        x.into_iter().any(| y | {
            match y {
                proc_macro2::TokenTree::Ident(ident) => ident.to_string() == "no_std",
                _ => false,
            }
        })
    })
}

fn ast_to_features(syntax: syn::File, features: HashSet<String>) -> HashSet<String> {
    let attributes = find_all_attributes(syntax).unwrap_or(vec![]);

    let mut new_features: HashSet<String> = features;
    for a in attributes {
        if !(a.path().is_ident("cfg") || a.path().is_ident("cfg_attr")) {
            continue;
        }

        let t: proc_macro2::TokenStream = a.parse_args().unwrap();
        let temp_features: Vec<String> = t.into_iter().map(|tree | {
            find_all_features_used(tree)
        }).flatten().collect();
        let temp_features: HashSet<String> = HashSet::from_iter(temp_features);
        if temp_features.intersection(&new_features).count() > 0 {
            new_features = temp_features.union(&new_features).cloned().collect();
        }
    }
    new_features

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
            all_projects.append(& mut find_all_rust_files(&path).unwrap_or(vec![]));
        } else if path.is_file() && path.extension().unwrap_or(OsStr::new("t")) == "rs" {
            all_projects.append(& mut vec![path.to_path_buf()]);
        } 
    }
    Ok(all_projects)
}

fn visit_dirs(dir: &Path, cb: &mut dyn FnMut(&fs::DirEntry)) -> io::Result<()> {
    if dir.is_dir() {
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() && !path.ends_with("examples") && !path.ends_with("benches") && !path.ends_with("tests") {
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
                all_projects.append(& mut find_all_projects(&path).unwrap_or(vec![]));
            }
        }
    }

    Ok(all_projects)
}

fn find_cfg_attr_no_std_features(files: Vec<PathBuf>) -> Option<Vec<String>> {
    let mut all_features: Vec<String> = vec![];
    for file in files {
        let ast = file_to_ast(&file);
        let attributes = find_all_attributes(ast);
        if let Some(attributes) = attributes {
            for a in attributes {
                if a.path().is_ident("cfg_attr") && is_no_std(a.clone()) {
                    let precondition: proc_macro2::TokenStream = a.parse_args().unwrap();
                    let mut features: Vec<String> = vec![];
                    for token in precondition.into_iter() {
                        features.append(& mut find_all_features_used(token));
                    }
                    all_features.append(&mut features);
                }
            }
        } 
    }
    if all_features.is_empty() {
        None
    } else {
        Some(all_features)
    }
}

fn find_all_attributes(file: syn::File) -> Option<Vec<Attribute>> {
    let mut all_attributes: Vec<Attribute> = file.attrs.into_iter().collect();
    let attrs: Vec<Vec<Attribute>> = file.items.into_iter().map(|a | {
        match a {
            Item::Const(item_const) => item_const.attrs.clone(),
            Item::Enum(item_enum) => item_enum.attrs.clone(),
            Item::ExternCrate(item_extern_crate) => item_extern_crate.attrs.clone(),
            Item::Fn(item_fn) => item_fn.attrs.clone(),
            Item::ForeignMod(item_foreign_mod) => item_foreign_mod.attrs.clone(),
            Item::Impl(item_impl) => item_impl.attrs.clone(),
            Item::Macro(item_macro) => item_macro.attrs.clone(),
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
        }
    }).collect();
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
    for i in 1..length+1 {
        let combination: Vec<Vec<String>> = features.clone().into_iter().combinations(i).collect();
        dbg!(&combination);
        let mut combination: Vec<String> = combination.into_iter().map(| a | {
            a.join(",")
        }).collect();
        combinations.append(&mut combination);
    } 
    dbg!(&combinations);
    combinations
}

fn execute_cargo_build_command(args: Vec<String>, project: String) -> serde_json::Value {
    dbg!(&args);
    let mut binding = Command::new("cargo");
    let output = binding
        .args(args.clone());
    output.current_dir(project);
    let output = output.output().unwrap();
    let status = output.status.code().unwrap();
    dbg!(&output);
    let stdout = String::from_utf8(output.stdout).unwrap();
    let stderr  = String::from_utf8(output.stderr).unwrap();

    json!({
        "features": args,
        "code": status,
        "stdout": stdout,
        "stderr": stderr,
    })
}

fn build_project(project: PathBuf, features: HashSet<String>) -> serde_json::Value {
    let project_string: String = String::from(project.to_str().unwrap_or(""));
    dbg!(&project_string);

    let mut args: Vec<String> = vec!["build".to_string()];
    args.push("--target".to_string());
    args.push("thumbv7m-none-eabi".to_string());

    let mut commands: Vec<serde_json::Value> = vec![];

    if features.is_empty() {
        let command = execute_cargo_build_command(args.clone(), project_string.clone());
        commands.push(command);
    } else {
        args.push("--no-default-features".to_string());
        let command = execute_cargo_build_command(args.clone(), project_string.clone());
        commands.push(command);

        args.push("--features".to_string());
        for option in feature_options(features) {
            args.push(option);
            let command = execute_cargo_build_command(args.clone(), project_string.clone());
            commands.push(command);
            args.pop();
        }
    }

    json!(commands)
}

fn main() {
    let folder = std::env::args().nth(1).expect("no directory given");
    let path = Path::new(&folder);
    let mut rust_files: Vec<String> = vec![];
    let projects: Vec<PathBuf> = find_all_projects(path).unwrap_or(vec![]);
    dbg!(&projects);

    let mut file = fs::File::create("out.json").expect("bad");

    let mut projects_json: Vec<serde_json::Value> = vec![];

    for project in projects {
        dbg!(&project);
        let rust_files = find_all_rust_files(&project).unwrap_or(vec![]);
        // dbg!(&rust_files);
        let initial_features = find_cfg_attr_no_std_features(rust_files.clone());
        // dbg!(&initial_features);
        if initial_features.is_none() {
            dbg!("no features for this project");
            continue;
        }
        let mut initial_features: HashSet<String> = HashSet::from_iter(initial_features.unwrap());

        for file in rust_files {
            let ast = file_to_ast(&file);
            initial_features = ast_to_features(ast, initial_features);
            // dbg!(file);
            // dbg!(&initial_features);
            // let intersection: HashSet<String> = initial_features.intersection(&features).cloned().collect();
            // if !intersection.is_empty() {
            //     initial_features = initial_features.union(&features).cloned().collect();
            //     dbg!(initial_features);
            // }
        }
        dbg!(&project);
        dbg!(&initial_features);
        let commands = build_project(project.clone(), initial_features.clone());

        projects_json.push(json!({
            "project": String::from(project.clone().to_str().unwrap()),
            "features": initial_features.clone(),
            "commands": commands,
        }));
    }

    let value = json!(projects_json);
    let value = serde_json::to_string_pretty(&value).unwrap();
    dbg!(&value);
    let result = file.write_all(value.as_bytes());
    dbg!(result);

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
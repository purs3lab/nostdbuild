#![cfg_attr(any(feature = "bar", feature = "no_std"), no_std)]

use std::collections::HashSet;
use std::env;
use std::fs;
use std::process;
use std::vec;
use proc_macro2;
use proc_macro2::Literal;
use proc_macro2::TokenStream;
use syn::Attribute;
use syn::Item;


fn find_all_features_used(stream: proc_macro2::TokenTree, features: Vec<String>) -> Vec<String> {
    if let proc_macro2::TokenTree::Group(group) = stream {
        let mut new_features: Vec<String> = vec![];
        let mut last_ident_was_feature = true;
        for val in group.stream().into_iter() {
            let mut new_list: Vec<String> = match val {
                proc_macro2::TokenTree::Group(group) => {
                    find_all_features_used(group.into(), features.clone())
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
            // dbg!(&new_list);
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

fn main() {
    #![cfg(not(feature = "bar"))]
    let mut args = env::args();
    let _ = args.next(); // executable name

    let filename = match (args.next(), args.next()) {
        (Some(filename), None) => filename,
        _ => {
            eprintln!("Usage: dump-syntax path/to/filename.rs");
            process::exit(1);
        }
    };

    let src = fs::read_to_string(&filename).expect("unable to read file");
    let syntax = syn::parse_file(&src).expect("unable to parse file");

    // Debug impl is available if Syn is built with "extra-traits" feature.
    // println!("{:#?}", syntax);


    /*
    Steps:
    1. Identify all cfg commands
    2. Convert features to variables
    3. Parse predicates and input them as equations in Z3
    4. Solve Z3
    */

    for i in syntax.items.iter() {
        // TODO: iterate through all items and exclude those that are not cfg
        let type_of = match i {
            Item::Const(item_const) => "const",
            Item::Enum(item_enum) => "enum",
            Item::ExternCrate(item_extern_crate) => "extern crate",
            Item::Fn(item_fn) => "item func",
            Item::ForeignMod(item_foreign_mod) => "foreign mod",
            Item::Impl(item_impl) => "item_impl",
            Item::Macro(item_macro) => "item_macro",
            Item::Mod(item_mod) => {
                println!("this is an item mod");
                println!("{:#?}", item_mod.attrs);
                "item_mod"
            },
            // Item::Static(item_static) => todo!(),
            // Item::Struct(item_struct) => todo!(),
            // Item::Trait(item_trait) => todo!(),
            // Item::TraitAlias(item_trait_alias) => todo!(),
            // Item::Type(item_type) => todo!(),
            // Item::Union(item_union) => todo!(),
            Item::Use(item_use) => "item use",
            // Item::Verbatim(token_stream) => todo!(),
            _ => {
                "done"
            },
        };
        println!("{:?}", type_of);
    }

    let mut all_attributes: Vec<Attribute> = syntax.attrs.clone().into_iter().collect();
    let attrs: Vec<Vec<Attribute>> = syntax.items.clone().into_iter().map(|a | {
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

    let mut a = vec![];
    for val in all_attributes.clone() {
        if !(val.path().is_ident("cfg_attr") && is_no_std(val.clone())) {
            continue;
        }
        dbg!(&val);
        let precondition: proc_macro2::TokenStream = val.parse_args().unwrap();
        dbg!(&precondition);
        let precondition: proc_macro2::TokenStream = precondition.into_iter().map(| t | {
            dbg!(t.clone());
            a.append(& mut find_all_features_used(t.clone(), vec![]));
            t
        }).collect();
    }
    println!("a: {:?}", a);
    let mut a: HashSet<String> = HashSet::from_iter(a.iter().cloned());
    dbg!(&a);

    for val in all_attributes {
        if !(val.path().is_ident("cfg") || val.path().is_ident("cfg_attr")) {
            continue;
        }
        let precondition: proc_macro2::TokenStream = val.parse_args().unwrap();
        let mut features: Vec<String> = vec![];
        let precondition: proc_macro2::TokenStream = precondition.into_iter().map(| t | {
            features.append(&mut find_all_features_used(t.clone(), vec![]));
            t
        }).collect();
        
        let set: HashSet<String> = HashSet::from_iter(features.iter().cloned());
        let intersection: HashSet<_> = a.intersection(&set).collect();
        if !intersection.is_empty() {
            a = a.union(&set).cloned().collect();
        }
    }
    dbg!(&a);

    println!("\nfile-level attributes");
    for a in syntax.attrs.clone().into_iter() {
        if !(a.path().is_ident("cfg") || (a.path().is_ident("cfg_attr") && is_no_std(a.clone()))) {
            continue;
        }
        let precondition: proc_macro2::TokenStream = a.parse_args().unwrap();
        let mut features: Vec<String> = vec![];
        // dbg!(a.clone());
        let precondition: proc_macro2::TokenStream = precondition.into_iter().map(| a | {
            features.append(&mut find_all_features_used(a.clone(), vec![]));
            a
        }).collect();
        println!("\ntoken tree: {:?}, features: {:?}", precondition.to_string(), features);
        println!("the condition: {:?}", precondition.to_string());
        println!("is_no_std: {:?}", is_no_std(a.clone()))
    }

    println!("\nitem-level attributes");
    for i in syntax.items.iter() {
        // TODO: iterate through all items and exclude those that are not cfg
        let attributes = match i {
            Item::Const(item_const) => &item_const.attrs,
            Item::Enum(item_enum) => &item_enum.attrs,
            Item::ExternCrate(item_extern_crate) => &item_extern_crate.attrs,
            Item::Fn(item_fn) => &item_fn.attrs,
            Item::ForeignMod(item_foreign_mod) => &item_foreign_mod.attrs,
            Item::Impl(item_impl) => &item_impl.attrs,
            Item::Macro(item_macro) => &item_macro.attrs,
            Item::Mod(item_mod) => &item_mod.attrs,
            Item::Static(item_static) => &item_static.attrs,
            Item::Struct(item_struct) => &item_struct.attrs,
            Item::Trait(item_trait) => &item_trait.attrs,
            Item::TraitAlias(item_trait_alias) => &item_trait_alias.attrs,
            Item::Type(item_type) => &item_type.attrs,
            Item::Union(item_union) => &item_union.attrs,
            Item::Use(item_use) => &item_use.attrs,
            Item::Verbatim(token_stream) => &vec![],
            _ => &vec![],
        };

        for a in attributes.iter() {
            if !(a.path().is_ident("cfg") || (a.path().is_ident("cfg_attr") && is_no_std(a.clone()))) {
                continue;
            }
            match a.style {
                syn::AttrStyle::Outer => {
                    print!("outer attribute, ");
                },
                syn::AttrStyle::Inner(not) => {
                    print!("innner attribute, ");
                },
            }
            let precondition: proc_macro2::TokenStream = a.parse_args().unwrap();
            let precondition: proc_macro2::TokenStream = precondition.into_iter().map(| a | {
                let features = find_all_features_used(a.clone(), vec![]);
                println!("token tree: {:?}, features: {:?}", a.to_string(), features);
                a
            }).collect();
            println!("is_no_std: {:?}", is_no_std(a.clone()))
        }
    }
}

// no_std is an attribute (#![no_std]), and if we include it we cannot build bc println not in scope
// // in the case that either "bar" or "no_std" is defined, this line becomes #![no_std]
// #![cfg_attr(any(feature = "bar", feature = "no_std"), no_std)]

// use std::array;

// // even if we build for no_std, we can still bring in no_std with this line
// extern crate std;

// use core::panic::PanicInfo;

// // #[cfg(...)] says include what is under this code only if the predicate is true
// #[cfg(not(feature = "bar"))]
// #[allow(dead_code)]
// fn main() {

//     // this is at runtime, not compile time
//     if cfg!(not(feature = "bar")) {
//         println!("Hello, world!");
//     } else {
//         println!("hi there, world");
//     }

//     // must include either feature "foo" or feature "bar" in cargo.toml
//     #[cfg(any(feature = "bar", feature = "foo"))]
//     std::println!("hi");

//     #[cfg(all(target_family="unix", target_pointer_width = "64"))]
//     std::println!("unix?");

//     let mut vec = std::vec![0, 1, 2];

//     // because this is not specified in cargo.toml as an option, we must pass it via "billy"
//     #[cfg(feature = "billy")]
//     println!("this must be specified at the command line: rustc --cfg=feature=\"billy\" src/main.rs && ./main");


// }

// #[cfg(feature = "bar")]
// fn main() {}

// // if we misspell feature, it will give a warning but still compile
// #[cfg(featue = "no_std")]
// fn no_std() {
//     weofj
// }

// #[cfg_attr(all(any(feature = "foo", feature = "bar"), feature = "joe"), allow(dead_code))]
// struct HStderr {}

// impl HStderr {
//     fn new() -> HStderr {
//         return HStderr{}
//     }
// }


// #[cfg(feature = "bar")]
// #[panic_handler]
// fn panic(info: &PanicInfo) -> ! {
//     let mut host_stderr = HStderr::new();

//     // logs "panicked at '$reason', src/main.rs:27:4" to the host stderr
//     // writeln!(host_stderr, "{}", info).ok();

//     loop {}
// }

// /*
// Learnings: 
// must put feature = "foo" and then define that feature in cargo.toml
// we set the default feature to "foo", so the "hi" printline is not grayed out
// but we can pass "--no-default-features" to disable that
// this command adds the configuration to the compilation: rustc --cfg=feature=\"bar\" src/main.rs
// */
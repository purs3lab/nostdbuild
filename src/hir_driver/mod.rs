extern crate rustc_ast;
extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_resolve;
extern crate rustc_session;
extern crate rustc_span;

use rustc_ast::{
    node_id::NodeId,
    visit::{self, Visitor as AstVisitor},
};
use rustc_driver::Compilation;
// use rustc_hir::{
//     self as hir, Item, def,
// intravisit::{self, Visitor},
// };
use rustc_hir::def;
use rustc_interface::interface;
// use rustc_middle::hir::nested_filter;
use rustc_middle::ty::{ResolverAstLowering, TyCtxt};
// use rustc_span::{ExpnKind, FileNameDisplayPreference, MacroKind, Span, def_id::DefId};
use rustc_span::{FileNameDisplayPreference, Span, def_id::DefId};

use rustc_plugin::{CrateFilter, RustcPlugin, RustcPluginArgs, Utf8Path};

use std::env;
use std::process::Command;
use std::{borrow::Cow, collections::HashSet};

use clap::Parser;
use log::debug;
use serde::{Deserialize, Serialize};

use crate::ReadableSpan;
use crate::consts;

// struct MyVisitor<'tcx> {
//     tcx: TyCtxt<'tcx>,
//     spans: HashSet<ReadableSpan>,
//     skipped_spans: Vec<Span>,
// }

// impl<'tcx> MyVisitor<'tcx> {
//     fn new(tcx: TyCtxt<'tcx>) -> Self {
//         MyVisitor {
//             tcx,
//             spans: HashSet::new(),
//             skipped_spans: Vec::new(),
//         }
//     }
// }

struct PathResolutionInfo {
    path_text: String,
    // final_def_id: DefId,
    // The DefId of the first segment (e.g., 'std' or 'core')
    root_segment_def_id: Option<DefId>,
    // is_macro: bool,
    span: Span,
}

struct MyVisitor<'r> {
    // We hold a reference to the data struct
    resolver: &'r ResolverAstLowering,
    results: Vec<PathResolutionInfo>,
    skipped_spans: Vec<Span>,
}

fn res_to_def_id(res: &def::Res<NodeId>) -> Option<DefId> {
    match res {
        def::Res::Def(_, def_id) => Some(*def_id),
        def::Res::SelfTyAlias { alias_to, .. } => Some(*alias_to),
        def::Res::SelfTyParam { trait_ } => Some(*trait_),
        _ => None,
    }
}

impl<'r, 'a> AstVisitor<'a> for MyVisitor<'r> {
    fn visit_path(&mut self, path: &'a rustc_ast::Path) -> Self::Result {
        // println!("AST Path: {:?}", path);
        // let is_macro = path.span.from_expansion();
        let span = path.span.source_callsite();
        // if let Some(last_segment) = path.segments.last()
        //     && let Some(partial_res) = self.resolver.partial_res_map.get(&last_segment.id)
        //     && let Some(final_def_id) = res_to_def_id(&partial_res.base_res())
        // {
        let mut root_def_id = None;

        // We iterate through the segments of the path to find the root segment that corresponds to an external crate
        // Or if that doesn't exist, we take the first segment as the root segment
        for segment in path.segments.iter() {
            if let Some(res) = self
                .resolver
                .partial_res_map
                .get(&segment.id)
                .map(|r| r.base_res())
            {
                if let Some(def_id) = res.opt_def_id() {
                    // If the usage comes directly from another crate, we stop the processing
                    if !def_id.is_local() {
                        root_def_id = Some(def_id);
                        break;
                    }

                    // If the usage comes from the same crate, we check if it is an external crate import
                    // Cases like `extern crate std as foo; use foo::...` would be caught here,
                    // where the root segment is `foo`, first segment is from the current crate,
                    // but the root_def_id would be from `std`
                    // TODO: Does this actually do what is says it does? 
                    if let rustc_hir::def::Res::Def(rustc_hir::def::DefKind::ExternCrate, _) = res {
                        root_def_id = Some(def_id);
                        break;
                    }
                }
            }
        }

        if root_def_id.is_none() {
            if let Some(first) = path.segments.first() {
                if let Some(res) = self.resolver.partial_res_map.get(&first.id) {
                    root_def_id = res.base_res().opt_def_id();
                }
            }
        }

        // if let Some(first_segment) = path.segments.first()
        //     && let Some(root_res) = self.resolver.partial_res_map.get(&first_segment.id)
        // {
        //     root_def_id = res_to_def_id(&root_res.base_res());
        // }
        let path_text = path
            .segments
            .iter()
            .map(|s| s.ident.to_string())
            .collect::<Vec<_>>()
            .join("::");

        self.results.push(PathResolutionInfo {
            path_text,
            // final_def_id,
            root_segment_def_id: root_def_id,
            // is_macro,
            span,
        });
        // }
        visit::walk_path(self, path);
    }

    fn visit_item(&mut self, item: &'a rustc_ast::Item) {
        if let rustc_ast::ItemKind::Mod(_, ident, _) = &item.kind {
            debug!("Found module: {}", ident.name);
            if ident.name.as_str() == "test" {
                debug!("Ignoring test module");
                self.skipped_spans.push(item.span);
                return;
            }
        }
        visit::walk_item(self, item);
    }
}

// impl<'tcx> Visitor<'tcx> for MyVisitor<'tcx> {
//     type MaybeTyCtxt = TyCtxt<'tcx>;
//     type NestedFilter = nested_filter::All;

//     fn maybe_tcx(&mut self) -> Self::MaybeTyCtxt {
//         self.tcx
//     }

//     fn visit_path(&mut self, path: &hir::Path<'tcx>, _id: hir::HirId) {
//         // println!("Visiting path: {:?}", path);
//         // let segment = path.segments.last();
//         for segment in path.segments.iter() {
//             let def_id = match &segment.res {
//                 def::Res::Def(_, def_id) => def_id,
//                 def::Res::Local(hir_id) => &hir_id.owner.to_def_id(),
//                 def::Res::SelfTyAlias { alias_to, .. } => alias_to,
//                 def::Res::SelfTyParam { trait_ } => trait_,
//                 _ => {
//                     debug!("Not a definition or local");
//                     intravisit::walk_path(self, path);
//                     return;
//                 }
//             };
//             let expn = path.span.ctxt().outer_expn_data();
//             println!("Expr expansion kind: {:?}", expn.kind);
//             // println!("Segment {:#?}", segment);
//             match expn.kind {
//                 ExpnKind::Macro(MacroKind::Derive, _) => {
//                     println!("Ignoring path from derive macro");
//                     return;
//                 }
//                 _ => { /* continue */ }
//             }
//             let def_path_debug = self.tcx.def_path_debug_str(*def_id);
//             // let def_path = self.tcx.def_path_str(*def_id);
//             println!("Path resolved to definition {}", def_path_debug);
//             println!("Path span: {:?}", path.span);
//             let span = path.span.source_callsite();
//             if self.skipped_spans.iter().any(|&s| s.contains(span)) {
//                 debug!("Ignoring path in skipped span");
//                 return;
//             }
//             if def_path_string_with_std(&def_path_debug) {
//                 self.spans.insert(get_readable_span(&self.tcx, span));
//             }
//         }
//         intravisit::walk_path(self, path);
//     }

//     fn visit_item(&mut self, item: &'tcx Item) {
//         if let hir::ItemKind::Mod(ident, _) = item.kind {
//             debug!("Found module: {}", ident.name);
//             if ident.name.as_str() == "test" {
//                 debug!("Ignoring test module");
//                 self.skipped_spans.push(item.span);
//                 return;
//             }
//         }
//         intravisit::walk_item(self, item);
//     }
// }

// fn def_path_string_with_std(def_path: &str) -> bool {
//     if let Some((left, right)) = def_path.split_once(" as ") {
//         let left = left.strip_prefix("<").unwrap_or(left);
//         let right = right.strip_suffix(">").unwrap_or(right);
//         starts_with_std(left) || starts_with_std(right)
//     } else {
//         starts_with_std(def_path)
//     }
// }

// fn starts_with_std(def_path: &str) -> bool {
//     let def_path = def_path.strip_prefix("::").unwrap_or(def_path);
//     def_path.starts_with("std[") || def_path.starts_with("std::")
// }

fn get_readable_span(tcx: &TyCtxt, span: Span) -> ReadableSpan {
    let source_map = tcx.sess.source_map();
    let loc = source_map.lookup_char_pos(span.lo());
    let end_loc = source_map.lookup_char_pos(span.hi());

    let span = ReadableSpan {
        file: loc
            .file
            .name
            .display(FileNameDisplayPreference::Local)
            .to_string(),
        start_line: loc.line,
        start_col: loc.col.0,
        end_line: end_loc.line,
        end_col: end_loc.col.0,
    };
    println!("Found span: {:?}", span);
    span
}

fn dump_spans_as_json(filename: &str, spans: &HashSet<ReadableSpan>) {
    let json = serde_json::to_string_pretty(spans).unwrap();
    std::fs::write(filename, json).expect("Unable to write file");
}

struct MyCompilerCalls {
    compilation: Compilation,
}

impl rustc_driver::Callbacks for MyCompilerCalls {
    fn after_expansion<'tcx>(
        &mut self,
        _compiler: &interface::Compiler,
        tcx: TyCtxt<'tcx>,
    ) -> Compilation {
        if self.compilation == Compilation::Continue {
            return self.compilation;
        }

        let mut skipped_spans: Vec<Span> = Vec::new();
        let mut spans = HashSet::new();

        let results = {
            let resolver_wrapper = tcx.resolver_for_lowering().borrow();
            let (resolver, krate) = &*resolver_wrapper;

            let mut visitor = MyVisitor {
                resolver,
                results: Vec::new(),
                skipped_spans: Vec::new(),
            };

            skipped_spans.extend(visitor.skipped_spans.iter());
            visitor.visit_crate(krate);
            visitor.results
        };

        println!("=== Resolved Paths ===");
        println!(
            "{:<30} | {:<15} | {:<15}",
            "User Path", "Used Via (Crate)", "Defined In (Crate)"
        );
        println!("{:-<30} | {:-<15} | {:-<15}", "", "", "");

        for info in results {
            // The crate where the item *actually* lives (e.g., core)
            // let definition_crate = tcx.crate_name(info.final_def_id.krate);

            // The crate the user *referenced* (e.g., std)
            let usage_crate = if let Some(root_id) = info.root_segment_def_id {
                tcx.crate_name(root_id.krate)
            } else {
                rustc_span::symbol::Symbol::intern("LOCAL")
            };

            if skipped_spans.iter().any(|&s| s.contains(info.span)) {
                debug!("Ignoring path in skipped span: {:?}", info.path_text);
                continue;
            }

            if usage_crate.as_str() == "std" {
                spans.insert(get_readable_span(&tcx, info.span));
            }

            println!(
                "{:<30} | {:<15} | {:<15}",
                info.path_text, usage_crate, "definition_crate"
            );
        }

        println!("Found spans: {:?}", spans);
        dump_spans_as_json(consts::HIR_VISITOR_SPAN_DUMP, &spans);

        self.compilation
    }

    // fn after_analysis<'tcx>(
    //     &mut self,
    //     _compiler: &interface::Compiler,
    //     tcx: TyCtxt<'tcx>,
    // ) -> Compilation {
    //     if self.compilation == Compilation::Continue {
    //         return self.compilation;
    //     }

    //     println!("=== Starting analysis... ===");

    //     let mut visitor = MyVisitor::new(tcx);
    //     let module_items = tcx.hir_crate_items(());
    //     for item_id in module_items.free_items() {
    //         visitor.visit_item(tcx.hir_item(item_id));
    //     }
    //     for item_id in module_items.impl_items() {
    //         visitor.visit_impl_item(tcx.hir_impl_item(item_id));
    //     }
    //     for item_id in module_items.trait_items() {
    //         visitor.visit_trait_item(tcx.hir_trait_item(item_id));
    //     }

    //     println!("Found spans: {:?}", visitor.spans);
    //     dump_spans_as_json(consts::HIR_VISITOR_SPAN_DUMP, &visitor.spans);
    //     self.compilation
    // }
}

pub struct Plugin;

#[derive(Parser, Serialize, Deserialize, Clone, Debug)]
pub struct PluginArgs {
    cargo_args: Vec<String>,
}

impl RustcPlugin for Plugin {
    type Args = PluginArgs;

    fn version(&self) -> Cow<'static, str> {
        env!("CARGO_PKG_VERSION").into()
    }

    fn driver_name(&self) -> Cow<'static, str> {
        "hir-driver".into()
    }

    fn modify_cargo(&self, cargo: &mut Command, args: &Self::Args) {
        cargo.args(&args.cargo_args);
    }

    fn args(&self, _target_dir: &Utf8Path) -> RustcPluginArgs<Self::Args> {
        let args = PluginArgs::parse_from(env::args().skip(1));
        let filter = CrateFilter::AllCrates;
        RustcPluginArgs { args, filter }
    }

    fn run(
        self,
        compiler_args: Vec<String>,
        _plugin_args: Self::Args,
    ) -> rustc_interface::interface::Result<()> {
        let mut action = Compilation::Stop;
        // We hit `build.rs` first if it exists, so continue compilation in that case.
        if compiler_args.iter().any(|arg| arg == "build_script_build") {
            action = Compilation::Continue;
        }

        let mut callbacks = MyCompilerCalls {
            compilation: action,
        };
        rustc_driver::run_compiler(&compiler_args, &mut callbacks);
        Ok(())
    }
}

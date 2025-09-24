extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_session;
extern crate rustc_span;

use rustc_driver::Compilation;
use rustc_hir::{
    self as hir, Item, def,
    intravisit::{self, Visitor},
};
use rustc_interface::interface;
use rustc_middle::hir::nested_filter;
use rustc_middle::ty::TyCtxt;
use rustc_span::{FileNameDisplayPreference, Span};

use rustc_plugin::{CrateFilter, RustcPlugin, RustcPluginArgs, Utf8Path};

use std::env;
use std::process::Command;
use std::{borrow::Cow, collections::HashSet};

use clap::Parser;
use log::debug;
use serde::{Deserialize, Serialize};

use crate::ReadableSpan;
use crate::consts;

struct MyVisitor<'tcx> {
    tcx: TyCtxt<'tcx>,
    spans: HashSet<ReadableSpan>,
    skipped_spans: Vec<Span>,
}

impl<'tcx> MyVisitor<'tcx> {
    fn new(tcx: TyCtxt<'tcx>) -> Self {
        MyVisitor {
            tcx,
            spans: HashSet::new(),
            skipped_spans: Vec::new(),
        }
    }
}

impl<'tcx> Visitor<'tcx> for MyVisitor<'tcx> {
    type MaybeTyCtxt = TyCtxt<'tcx>;
    type NestedFilter = nested_filter::All;

    fn maybe_tcx(&mut self) -> Self::MaybeTyCtxt {
        self.tcx
    }

    fn visit_path(&mut self, path: &hir::Path<'tcx>, _id: hir::HirId) {
        let segment = path.segments.last();
        if let Some(segment) = segment {
            let def_id = match &segment.res {
                def::Res::Def(_, def_id) => def_id,
                def::Res::Local(hir_id) => &hir_id.owner.to_def_id(),
                def::Res::SelfTyAlias { alias_to, .. } => alias_to,
                def::Res::SelfTyParam { trait_ } => trait_,
                _ => {
                    debug!("Not a definition or local");
                    intravisit::walk_path(self, path);
                    return;
                }
            };
            let def_path = self.tcx.def_path_debug_str(*def_id);
            debug!("Path resolved to definition: {}", def_path);
            let span = path.span.source_callsite();
            if self.skipped_spans.iter().any(|&s| s.contains(span)) {
                debug!("Ignoring path in skipped span");
                return;
            }
            if def_path_string_with_std(&def_path) {
                self.spans.insert(get_readable_span(&self.tcx, span));
            }
        }
        intravisit::walk_path(self, path);
    }

    fn visit_item(&mut self, item: &'tcx Item) {
        if let hir::ItemKind::Mod(ident, _) = item.kind {
            debug!("Found module: {}", ident.name);
            if ident.name.as_str() == "test" {
                debug!("Ignoring test module");
                self.skipped_spans.push(item.span);
                return;
            }
        }
        intravisit::walk_item(self, item);
    }
}

fn def_path_string_with_std(def_path: &str) -> bool {
    if let Some((left, right)) = def_path.split_once(" as ") {
        let left = left.strip_prefix("<").unwrap_or(left);
        let right = right.strip_suffix(">").unwrap_or(right);
        starts_with_std(left) || starts_with_std(right)
    } else {
        starts_with_std(def_path)
    }
}

fn starts_with_std(def_path: &str) -> bool {
    let def_path = def_path.strip_prefix("::").unwrap_or(def_path);
    def_path.starts_with("std[") || def_path.starts_with("std::")
}

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
    fn after_analysis<'tcx>(
        &mut self,
        _compiler: &interface::Compiler,
        tcx: TyCtxt<'tcx>,
    ) -> Compilation {
        if self.compilation == Compilation::Continue {
            return self.compilation;
        }

        debug!("Starting analysis...");

        let mut visitor = MyVisitor::new(tcx);
        let module_items = tcx.hir_crate_items(());
        for item_id in module_items.free_items() {
            visitor.visit_item(tcx.hir_item(item_id));
        }
        for item_id in module_items.impl_items() {
            visitor.visit_impl_item(tcx.hir_impl_item(item_id));
        }
        for item_id in module_items.trait_items() {
            visitor.visit_trait_item(tcx.hir_trait_item(item_id));
        }

        println!("Found spans: {:?}", visitor.spans);
        dump_spans_as_json(consts::HIR_VISITOR_SPAN_DUMP, &visitor.spans);
        self.compilation
    }
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

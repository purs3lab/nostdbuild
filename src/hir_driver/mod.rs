extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_session;
extern crate rustc_span;

use rustc_driver::Compilation;
use rustc_hir::{
    self as hir, Expr, Item, PathSegment, def,
    intravisit::{self, Visitor},
};
use rustc_interface::interface;
use rustc_middle::hir::nested_filter;
use rustc_middle::ty::TyCtxt;
use rustc_span::{FileNameDisplayPreference, Span};

use rustc_plugin::{CrateFilter, RustcPlugin, RustcPluginArgs, Utf8Path};

use std::borrow::Cow;
use std::env;
use std::process::Command;

use clap::Parser;
use log::debug;
use serde::{Deserialize, Serialize};

struct MyVisitor<'tcx> {
    tcx: TyCtxt<'tcx>,
    spans: Vec<ReadableSpan>,
    skipped_spans: Vec<Span>,
}

#[derive(Debug, Serialize)]
struct ReadableSpan {
    file: String,
    start_line: usize,
    start_col: usize,
    end_line: usize,
    end_col: usize,
}

impl<'tcx> MyVisitor<'tcx> {
    fn new(tcx: TyCtxt<'tcx>) -> Self {
        MyVisitor {
            tcx,
            spans: Vec::new(),
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

    fn visit_expr(&mut self, ex: &'tcx Expr<'tcx>) {
        let expr_span = ex.span;
        if self.skipped_spans.iter().any(|&s| s.contains(expr_span)) {
            debug!("Ignoring expression in skipped span");
            return;
        }

        if let hir::ExprKind::Path(path) = &ex.kind
            && match_variants_inner(&self.tcx, &hir::TyKind::Path(*path), true)
        {
            // If this expression in a macro expansion, get the callsite span
            self.spans
                .push(get_readable_span(&self.tcx, expr_span.source_callsite()));
        }

        intravisit::walk_expr(self, ex);
    }

    fn visit_item(&mut self, item: &'tcx Item) {
        match item.kind {
            hir::ItemKind::Mod(ident, _) => {
                debug!("Found module: {}", ident.name);
                if ident.name.as_str() == "test" {
                    debug!("Ignoring test module");
                    self.skipped_spans.push(item.span);
                    return;
                }
            }
            hir::ItemKind::Struct(ident, _, variants) => {
                debug!("Found struct: {}", ident.name);
                match_variants(&self.tcx, &mut self.spans, &variants);
            }
            hir::ItemKind::Enum(ident, _, def) => {
                debug!("Found enum: {}", ident.name);
                for variant in def.variants {
                    debug!(" Variant: {}", variant.ident.name);
                    match_variants(&self.tcx, &mut self.spans, &variant.data);
                }
            }

            _ => {}
        }

        intravisit::walk_item(self, item);
    }
}

fn match_variants(tcx: &TyCtxt, spans: &mut Vec<ReadableSpan>, variants: &hir::VariantData) {
    match variants {
        hir::VariantData::Struct { fields, .. } | hir::VariantData::Tuple(fields, _, _) => {
            for field in *fields {
                debug!("  Field: {}", field.ident.name);
                if match_variants_inner(tcx, &field.ty.kind, false) {
                    spans.push(get_readable_span(tcx, field.span));
                }
            }
        }
        _ => {}
    }
}

fn match_variants_inner(tcx: &TyCtxt, tykind: &hir::TyKind, direct: bool) -> bool {
    let mut segment = None;
    if let hir::TyKind::Path(hir::QPath::Resolved(_, path)) = tykind {
        segment = path.segments.last();
    } else if let hir::TyKind::Path(hir::QPath::TypeRelative(ty, _)) = tykind
        && let hir::TyKind::Path(hir::QPath::Resolved(_, path)) = &ty.kind
    {
        segment = path.segments.last();
    }

    if let Some(PathSegment {
        res: def::Res::Def(_, def_id),
        ..
    }) = segment
    {
        let def_path = tcx.def_path_str(*def_id);
        if !direct {
            debug!("    Type resolved to definition: {}", def_path);
        } else {
            debug!("Type resolved to definition: {}", def_path);
        }
        let krate = tcx.crate_name(def_id.krate);
        debug!("    In crate: {}", krate);
        if krate.as_str() == "std" {
            return true;
        }
    }
    false
}

fn get_readable_span(tcx: &TyCtxt, span: Span) -> ReadableSpan {
    let source_map = tcx.sess.source_map();
    let loc = source_map.lookup_char_pos(span.lo());
    let end_loc = source_map.lookup_char_pos(span.hi());

    ReadableSpan {
        file: loc
            .file
            .name
            .display(FileNameDisplayPreference::Local)
            .to_string(),
        start_line: loc.line,
        start_col: loc.col.0,
        end_line: end_loc.line,
        end_col: end_loc.col.0,
    }
}

fn dump_spans_as_json(filename: &str, spans: &[ReadableSpan]) {
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
        for item_id in tcx.hir_crate_items(()).free_items() {
            let item = tcx.hir_item(item_id);
            visitor.visit_item(item);
        }

        println!("Found spans: {:?}", visitor.spans);
        dump_spans_as_json(
            "/evaldisk/sourag/results/hir_visitor_span_dump.json",
            &visitor.spans,
        );
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

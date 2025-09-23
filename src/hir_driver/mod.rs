extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_session;
extern crate rustc_span;

use rustc_driver::Compilation;
use rustc_hir::{
    self as hir, Expr, GenericParam, Generics, Impl, Item, PathSegment, TraitImplHeader, def,
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

    fn visit_expr(&mut self, ex: &'tcx Expr<'tcx>) {
        let expr_span = ex.span;
        if self.skipped_spans.iter().any(|&s| s.contains(expr_span)) {
            debug!("Ignoring expression in skipped span");
            return;
        }

        // TODO: For all the items where generics are possible, handle those generics
        // TODO: Fix match inner variants to handle Local and SelfTyAlias better
        // TODO: Handle fn args and return types
        // TODO: Handle generic parameters with default values (for all kinds of items)
        //       Similar to the handling for Struct.
        //       Should we handle where clauses?
        // TODO: Handle generic types in functions (check for trait bounds as well)
        //       Need to handle trait objects, impl traits

        if let hir::ExprKind::Path(path) = &ex.kind
            && match_variants_inner(&self.tcx, &hir::TyKind::Path(*path), true)
        {
            // If this expression in a macro expansion, get the callsite span
            self.spans
                .insert(get_readable_span(&self.tcx, expr_span.source_callsite()));
        }

        if let hir::ExprKind::MethodCall(segment, _, _, _) = &ex.kind {
            println!("Found method call: {}", segment.ident.name);

            if let Some(method_def_id) = self
                .tcx
                .typeck(ex.hir_id.owner.def_id)
                .type_dependent_def_id(ex.hir_id)
            {
                let method_path = self.tcx.def_path_str(method_def_id);
                if does_def_path_string_has_std(&method_path) {
                    self.spans
                        .insert(get_readable_span(&self.tcx, expr_span.source_callsite()));
                }
            }
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
            hir::ItemKind::Struct(ident, Generics { params, .. }, variants) => {
                debug!("Found struct: {}", ident.name);
                match_variants(&self.tcx, &mut self.spans, &variants);
                handle_generic_param(&self.tcx, &mut self.spans, params);
            }
            hir::ItemKind::Enum(ident, _, def) => {
                debug!("Found enum: {}", ident.name);
                for variant in def.variants {
                    debug!(" Variant: {}", variant.ident.name);
                    match_variants(&self.tcx, &mut self.spans, &variant.data);
                }
            }
            hir::ItemKind::Impl(Impl {
                of_trait, self_ty, ..
            }) => {
                println!(
                    "Found impl with self type: {:?} abd of_trait: {:?}",
                    self_ty, of_trait
                );
                if let Some(TraitImplHeader { trait_ref, .. }) = of_trait {
                    // Pls clean this up later
                    if match_variants_inner(
                        &self.tcx,
                        &hir::TyKind::Path(hir::QPath::Resolved(None, trait_ref.path)),
                        false,
                    ) {
                        self.spans.insert(get_readable_span(&self.tcx, item.span));
                    }
                }
                if match_variants_inner(&self.tcx, &self_ty.kind, false) {
                    self.spans.insert(get_readable_span(&self.tcx, item.span));
                }
            }

            _ => {}
        }

        intravisit::walk_item(self, item);
    }
}

fn handle_generic_param(tcx: &TyCtxt, spans: &mut HashSet<ReadableSpan>, params: &[GenericParam]) {
    for param in params {
        match param.kind {
            hir::GenericParamKind::Type { default, .. } => {
                if let Some(ty) = default
                    && match_variants_inner(tcx, &ty.kind, false)
                {
                    println!("Found generic type parameter with default: {:?}", param);
                    spans.insert(get_readable_span(tcx, param.span));
                }
            }
            _ => {
                unimplemented!("Other generic param kind handling not implemented yet");
            }
        }
    }
}

fn does_def_path_string_has_std(def_path: &str) -> bool {
    println!("Checking def path: {}", def_path);
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
    def_path.starts_with("std::")
}

fn match_variants(tcx: &TyCtxt, spans: &mut HashSet<ReadableSpan>, variants: &hir::VariantData) {
    match variants {
        hir::VariantData::Struct { fields, .. } | hir::VariantData::Tuple(fields, _, _) => {
            for field in *fields {
                debug!("  Field: {}", field.ident.name);
                if match_variants_inner(tcx, &field.ty.kind, false) {
                    spans.insert(get_readable_span(tcx, field.span));
                }
            }
        }
        _ => {}
    }
}

fn match_variants_inner(tcx: &TyCtxt, tykind: &hir::TyKind, direct: bool) -> bool {
    if direct {
        println!("Matching path directly from expression");
    }
    let mut segment = None;
    if let hir::TyKind::Path(hir::QPath::Resolved(_, path)) = tykind {
        segment = path.segments.last();
    } else if let hir::TyKind::Path(hir::QPath::TypeRelative(ty, _)) = tykind
        && let hir::TyKind::Path(hir::QPath::Resolved(_, path)) = &ty.kind
    {
        segment = path.segments.last();
    }

    if let Some(PathSegment { res, .. }) = segment {
        let def_id = match res {
            def::Res::Def(_, def_id) => def_id,
            def::Res::Local(hir_id) => &hir_id.owner.to_def_id(),
            def::Res::SelfTyAlias { alias_to, .. } => alias_to,
            _ => {
                debug!("    Not a definition or local");
                return false;
            }
        };

        let def_path = tcx.def_path_str(*def_id);
        if !direct {
            debug!("    Type resolved to definition: {}", def_path);
        } else {
            debug!("Type resolved to definition: {}", def_path);
        }
        if does_def_path_string_has_std(&def_path) {
            return true;
        }
    }
    false
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

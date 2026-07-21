extern crate rustc_ast;
extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_resolve;
extern crate rustc_session;
extern crate rustc_span;

use rustc_ast::token::{Delimiter, TokenKind};
use rustc_ast::tokenstream::TokenTree;
use rustc_ast::visit::{self, Visitor as AstVisitor};
use rustc_driver::Compilation;
use rustc_interface::interface;
use rustc_middle::ty::{ResolverAstLowering, TyCtxt};
use rustc_span::hygiene::ExpnKind;
use rustc_span::source_map::SourceMap;
use rustc_span::{FileNameDisplayPreference, Span, Symbol};

use std::collections::HashMap;

use rustc_plugin::{CrateFilter, RustcPlugin, RustcPluginArgs, Utf8Path};

use std::borrow::Cow;
use std::env;
use std::process::Command;

use clap::Parser;
use log::debug;
use serde::{Deserialize, Serialize};

use crate::consts;
use crate::types::*;

struct PathResolver<'r, 'tcx> {
    resolver: &'r ResolverAstLowering,
    tcx: TyCtxt<'tcx>,
    records: Vec<PathRecord>,
    current_context: PathContext,
    current_module_path: Vec<String>,
    macro_module_imports: Vec<(String, String)>, // filename, module name
    /// Map from macro name to full `#[cfg(…)]` attribute strings extracted from the macro body.
    macro_cfg_map: HashMap<Symbol, Vec<String>>,
}

/// Walks `tokens` and returns the full source text of every `#[cfg(…)]`
/// attribute found at any nesting depth, using the source map to reconstruct
/// the original text.  Recurses into delimited groups so attributes inside
/// `not(…)` / `all(…)` arms are also captured.
fn collect_cfg_attrs_from_tokens(
    tokens: &rustc_ast::tokenstream::TokenStream,
    source_map: &SourceMap,
) -> Vec<String> {
    let mut result = Vec::new();
    let trees: Vec<TokenTree> = tokens.iter().cloned().collect();
    let mut i = 0;
    while i < trees.len() {
        match &trees[i] {
            TokenTree::Token(tok, _) if tok.kind == TokenKind::Pound => {
                if let Some(TokenTree::Delimited(
                    delim_span,
                    _,
                    Delimiter::Bracket,
                    bracket_inner,
                )) = trees.get(i + 1)
                {
                    let inner: Vec<TokenTree> = bracket_inner.iter().cloned().collect();
                    // First token inside `[…]` must be the ident `cfg`
                    let is_cfg = inner.first().is_some_and(|t| {
                        matches!(t, TokenTree::Token(id, _)
                            if matches!(id.kind, TokenKind::Ident(s, _) if s.as_str() == "cfg"))
                    });
                    if is_cfg {
                        // Span from `#` to the closing `]`
                        let full_span = tok.span.to(delim_span.entire());
                        if let Ok(snippet) = source_map.span_to_snippet(full_span) {
                            result.push(snippet);
                        }
                        i += 2;
                        continue;
                    }
                }
            }
            TokenTree::Delimited(_, _, _, inner) => {
                result.extend(collect_cfg_attrs_from_tokens(inner, source_map));
            }
            _ => {}
        }
        i += 1;
    }
    result
}

impl<'r, 'a, 'tcx> AstVisitor<'a> for PathResolver<'r, 'tcx> {
    fn visit_item(&mut self, item: &'a rustc_ast::Item) {
        let old_context = self.current_context;

        // 1. Track module hierarchy
        let is_mod = matches!(item.kind, rustc_ast::ItemKind::Mod(..));
        if is_mod {
            let (ident, kind) = match &item.kind {
                rustc_ast::ItemKind::Mod(_, ident, kind) => (ident, kind),
                _ => unreachable!(),
            };
            self.current_module_path.push(ident.name.to_string());

            let span = item.span;
            if span.from_expansion() {
                let record = match kind {
                    rustc_ast::ModKind::Loaded(_, rustc_ast::Inline::No { .. }, _) => {
                        debug!("Tracking module from macro expansion: {}", ident.name);
                        true
                    }
                    rustc_ast::ModKind::Loaded(_, rustc_ast::Inline::Yes, _) => {
                        debug!(
                            "Skipping inline module from macro expansion: {}",
                            ident.name
                        );
                        false
                    }
                    rustc_ast::ModKind::Unloaded => {
                        unreachable!(
                            "Tracking unloaded module from macro expansion: {}",
                            ident.name
                        );
                    }
                };
                if record {
                    let root_callsite = span
                        .macro_backtrace()
                        .last()
                        .map(|bt| bt.call_site)
                        .unwrap_or(span);
                    let source_file = self.tcx.sess.source_map().span_to_filename(root_callsite);
                    self.macro_module_imports.push((
                        source_file.prefer_local().to_string(),
                        ident.name.to_string(),
                    ));
                }
            }
        }

        // 2. Handle context and manual extraction
        match &item.kind {
            rustc_ast::ItemKind::Use(..) => {
                self.current_context = PathContext::ImportDeclaration;
            }
            rustc_ast::ItemKind::ExternCrate(orig_name, extern_ident) => {
                if item.span.is_dummy() {
                    debug!(
                        "Skipping dummy extern crate declaration: {}",
                        extern_ident.name
                    );
                    return;
                }
                self.current_context = PathContext::ImportDeclaration;

                let alias_name = extern_ident.to_string();
                let target_crate = orig_name
                    .map(|s| s.to_string())
                    .unwrap_or_else(|| alias_name.clone());

                let defining_module = Some(self.current_module_path.join("::"));
                let readable_span =
                    get_readable_span(&self.tcx, item.span.source_callsite(), &target_crate);

                self.records.push(PathRecord {
                    path_text: alias_name,
                    definition_crate: target_crate.to_string(),
                    local_route: None,
                    defining_module,
                    context: PathContext::ImportDeclaration,
                    span: readable_span,
                    macro_body_cfgs: vec![],
                    is_extern_crate: true,
                    // Set by the driver's facade-gateway pass, not here.
                    gateway_anchor: None,
                });
            }
            _ => {
                self.current_context = PathContext::Other;
            }
        }

        // 3. Walk the item (this will trigger visit_path for inner things like `Use`)
        visit::walk_item(self, item);

        // 4. Clean up
        if is_mod {
            self.current_module_path.pop();
        }
        self.current_context = old_context;
    }

    fn visit_block(&mut self, b: &'a rustc_ast::Block) {
        let old_ctx = self.current_context;
        self.current_context = PathContext::Expression;
        visit::walk_block(self, b);
        self.current_context = old_ctx;
    }

    fn visit_ty(&mut self, t: &'a rustc_ast::Ty) {
        let old_ctx = self.current_context;
        self.current_context = PathContext::Type;
        visit::walk_ty(self, t);
        self.current_context = old_ctx;
    }

    fn visit_path(&mut self, path: &'a rustc_ast::Path) -> Self::Result {
        // If the path is from a macro expansion, trace back to the original call
        // site. Also look up any #[cfg(…)] guards from that macro's body.
        let (effective_span, macro_body_cfgs) = if path.span.from_expansion() {
            let last_expn = path.span.macro_backtrace().last();
            let span = last_expn
                .as_ref()
                .map(|bt| bt.call_site)
                .unwrap_or(path.span);
            let cfgs = last_expn
                .and_then(|expn| {
                    if let ExpnKind::Macro(_, name) = expn.kind {
                        self.macro_cfg_map.get(&name).cloned()
                    } else {
                        None
                    }
                })
                .unwrap_or_default();
            (span, cfgs)
        } else {
            (path.span, vec![])
        };

        let mut deepest_res_def_id = None;

        for segment in path.segments.iter().rev() {
            if let Some(res) = self.resolver.partial_res_map.get(&segment.id)
                && let Some(def_id) = res.base_res().opt_def_id()
            {
                deepest_res_def_id = Some(def_id);
                break;
            }
        }

        if let Some(final_def_id) = deepest_res_def_id {
            let mut root_def_id = None;
            let mut local_route_segments = Vec::new();
            let mut local_route = None;

            for segment in &path.segments {
                let seg_name = segment.ident.to_string();

                if let Some(res) = self.resolver.partial_res_map.get(&segment.id)
                    && let Some(def_id) = res.base_res().opt_def_id()
                    && !def_id.is_local()
                {
                    root_def_id = Some(def_id);
                    if !local_route_segments.is_empty() {
                        local_route = Some(local_route_segments.join("::"));
                    }
                    break;
                }

                local_route_segments.push(seg_name);
            }

            if root_def_id.is_none()
                && let Some(first) = path.segments.first()
                && let Some(res) = self.resolver.partial_res_map.get(&first.id)
            {
                root_def_id = res.base_res().opt_def_id();
            }

            let path_text = path
                .segments
                .iter()
                .map(|s| s.ident.to_string())
                .collect::<Vec<_>>()
                .join("::");

            let definition_crate = self.tcx.crate_name(final_def_id.krate).to_string();
            let gateway_crate = if let Some(root_id) = root_def_id {
                self.tcx.crate_name(root_id.krate).to_string()
            } else {
                "LOCAL".to_string()
            };

            let readable_span = get_readable_span(&self.tcx, effective_span, &gateway_crate);

            let defining_module = if self.current_context == PathContext::ImportDeclaration {
                Some(self.current_module_path.join("::"))
            } else {
                None
            };

            self.records.push(PathRecord {
                path_text,
                definition_crate,
                local_route,
                defining_module,
                context: self.current_context,
                span: readable_span,
                macro_body_cfgs,
                is_extern_crate: false,
                // Set by the driver's facade-gateway pass, not here.
                gateway_anchor: None,
            });
        }

        visit::walk_path(self, path)
    }
}

fn get_readable_span(tcx: &TyCtxt, span: Span, usage_crate: &str) -> ReadableSpan {
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
        usage_crate: Some(usage_crate.to_string()),
    }
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

        let (records, macro_imports) = {
            let resolver_wrapper = tcx.resolver_for_lowering().borrow();
            let (resolver, krate) = &*resolver_wrapper;

            // Pre-scan all macro_rules! definitions to collect #[cfg(…)] attribute
            // strings from their bodies, keyed by macro name.
            let source_map = tcx.sess.source_map();
            let mut macro_cfg_map: HashMap<Symbol, Vec<String>> = HashMap::new();
            for item in krate.items.iter() {
                if let rustc_ast::ItemKind::MacroDef(ident, mac_def) = &item.kind {
                    let cfgs = collect_cfg_attrs_from_tokens(&mac_def.body.tokens, source_map);
                    if !cfgs.is_empty() {
                        macro_cfg_map.entry(ident.name).or_default().extend(cfgs);
                    }
                }
            }

            let mut visitor = PathResolver {
                resolver,
                tcx,
                records: Vec::new(),
                current_context: PathContext::Other,
                current_module_path: vec!["crate".to_string()],
                macro_module_imports: Vec::new(),
                macro_cfg_map,
            };

            visitor.visit_crate(krate);
            (visitor.records, visitor.macro_module_imports)
        };

        let output_data = FeatureRunOutput {
            records,
            macro_module_imports: macro_imports,
            out_dir: env::var("OUT_DIR").ok(),
        };

        let filename = env::var(consts::PLUGIN_OUTPUT_ENV).unwrap_or_else(|_| {
            panic!(
                "Expected environment variable {} to be set",
                consts::PLUGIN_OUTPUT_ENV
            )
        });

        if let Ok(file) = std::fs::File::create(&filename) {
            serde_json::to_writer(file, &output_data).unwrap();
        }

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

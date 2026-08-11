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
use rustc_hir::def_id::{DefId, LOCAL_CRATE};
use rustc_hir::intravisit::{self, Visitor as HirVisitor};
use rustc_interface::interface;
use rustc_middle::ty::{ResolverAstLowering, TyCtxt, TypeckResults};
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

/// Recursively collect every `macro_rules!` body's `#[cfg(…)]` attributes,
/// keyed by macro name, descending into module items. Runs at
/// `after_expansion`, so file-based modules are already `Loaded` inline in the
/// AST and their macros are reached by walking `ModKind::Loaded` children.
///
/// Names are not qualified by module path — two same-named macros in different
/// modules merge their cfgs, matching how `visit_path` looks a macro up by the
/// bare `ExpnKind::Macro` name from the expansion backtrace.
fn collect_macro_cfgs<T: std::ops::Deref<Target = rustc_ast::Item>>(
    items: &[T],
    source_map: &SourceMap,
    out: &mut HashMap<Symbol, Vec<String>>,
) {
    for item in items {
        match &item.kind {
            rustc_ast::ItemKind::MacroDef(ident, mac_def) => {
                let cfgs = collect_cfg_attrs_from_tokens(&mac_def.body.tokens, source_map);
                if !cfgs.is_empty() {
                    out.entry(ident.name).or_default().extend(cfgs);
                }
            }
            rustc_ast::ItemKind::Mod(_, _, rustc_ast::ModKind::Loaded(sub_items, _, _)) => {
                collect_macro_cfgs(sub_items, source_map, out);
            }
            _ => {}
        }
    }
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
                    expansion_crate: expansion_def_crate(self.tcx, item.span),
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
        let (effective_span, macro_body_cfgs) = call_site_span(path.span, &self.macro_cfg_map);

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

            let definition_crate = reported_crate_name(self.tcx, final_def_id);
            let gateway_crate = if let Some(root_id) = root_def_id {
                reported_crate_name(self.tcx, root_id)
            } else {
                consts::LOCAL_CRATE_SENTINEL.to_string()
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
                expansion_crate: expansion_def_crate(self.tcx, path.span),
                is_extern_crate: false,
                // Set by the driver's facade-gateway pass, not here.
                gateway_anchor: None,
            });
        }

        visit::walk_path(self, path)
    }
}

/// Where a span should be *reported*, plus the `#[cfg(…)]` guards of the macro
/// body it came out of.
///
/// A span inside a macro expansion points at code no source file contains, so it
/// finds no ancestor in the ModNode tree, is classified as unguarded, and becomes
/// a false positive. Reporting the outermost call site instead is what keeps the
/// record inside the tree. Shared by the AST path walk and the HIR method walk so
/// the two cannot drift on this.
/// The crate that defines the macro this span came out of, if any.
///
/// The outermost expansion is the one the crate's own source invoked, so its
/// `macro_def_id` names the crate that put these tokens here. A local macro
/// reports `None`: the crate's own macros are its own code.
fn expansion_def_crate(tcx: TyCtxt<'_>, span: Span) -> Option<String> {
    if !span.from_expansion() {
        return None;
    }
    let def_id = span.macro_backtrace().last()?.macro_def_id?;
    if def_id.krate == LOCAL_CRATE {
        return None;
    }
    Some(tcx.crate_name(def_id.krate).to_string())
}

fn call_site_span(
    span: Span,
    macro_cfg_map: &HashMap<Symbol, Vec<String>>,
) -> (Span, Vec<String>) {
    if !span.from_expansion() {
        return (span, vec![]);
    }
    let last_expn = span.macro_backtrace().last();
    let call_site = last_expn.as_ref().map(|bt| bt.call_site).unwrap_or(span);
    let cfgs = last_expn
        .and_then(|expn| {
            if let ExpnKind::Macro(_, name) = expn.kind {
                macro_cfg_map.get(&name).cloned()
            } else {
                None
            }
        })
        .unwrap_or_default();
    (call_site, cfgs)
}

/// Records the crate each **type-dependent callee** resolves into: method calls,
/// and the trait calls behind overloaded operators.
///
/// The AST pass cannot see these. `x.log2()` is an `ExprKind::MethodCall` whose
/// segment has no entry in `partial_res_map` — method resolution is part of type
/// checking, and the answer only exists as `type_dependent_def_id`. So every
/// dot-syntax call is invisible to `visit_path`: `f32::log2` and `f32::round`,
/// which live in `library/std/src/f32.rs` and have no `core` counterpart, read as
/// no std usage at all. afe4404 0.2.4 is the case this was written for — the tool
/// emitted a manifest for it, and its own `src/clock/mod.rs:35` cannot build
/// bare-metal.
///
/// Local resolutions are recorded too, exactly as `visit_path` records a local
/// path. They are not noise — they are the **witness** that a span is not std.
/// zeno 0.3.2 is the case: `lambda.sqrt()` resolves to std's inherent `f32::sqrt`
/// with `std` on, and to zeno's *own* `F32Ext` trait — a local impl forwarding to
/// `libm` — with it off. Drop the local half and the libm run leaves no record at
/// all, so the span looks std-in-the-only-run-that-has-it, the prober blames
/// whichever gate contains it, and zeno loses the `eval` feature its author had
/// on by default.
struct MethodResolver<'a, 'tcx> {
    tcx: TyCtxt<'tcx>,
    typeck: &'tcx TypeckResults<'tcx>,
    records: Vec<PathRecord>,
    macro_cfg_map: &'a HashMap<Symbol, Vec<String>>,
}

impl MethodResolver<'_, '_> {
    fn record(&mut self, site: Span, def_id: DefId) {
        let (effective_span, macro_body_cfgs) = call_site_span(site, self.macro_cfg_map);
        let krate = reported_crate_name(self.tcx, def_id);

        // `Owner::method`, where the owner is the receiver type's name for an
        // inherent method and the trait's name for a trait one — `HashMap::insert`,
        // `Write::write_all`, `f32::log2`.
        //
        // The owner name is what makes the record gateable. A method call is
        // std because its *receiver type* is, and the type is named by an
        // import; `resolve_import_to_use_gateways` joins a use to its import on
        // that bound name, via `use_name`, which reads the first segment. So a
        // `HashMap::insert` record inherits the gate of the
        // `#[cfg(not(target_os = "none"))] use std::collections::HashMap` that
        // brought `HashMap` in, exactly as a bare `HashMap::new()` path does.
        // Spelling the owner as a full path, or hiding it behind `<…>`, severs
        // that join and reports a properly gated call as unguarded std.
        //
        // `f32::log2` is the case no *gate* can reach: the owner is a primitive,
        // nothing binds `f32`, so there is no import whose cfg the record could
        // inherit. That is a statement about gating, not about std-ness — this
        // comment used to go on to claim there is "no configuration in which the
        // call is not std", and that is wrong. The shim case two paragraphs above
        // is exactly such a configuration: with std linked, `x.log2()` binds
        // std's *inherent* `f32::log2` (inherent beats trait) and is recorded
        // here as std; with std off and `micromath::F32Ext` in scope, the same
        // expression binds `F32Ext::log2` and is recorded as `micromath`. Both
        // records are correct. What settles such a span is therefore a std-off
        // covering *run*, not a gate and not a probe — an ungated span is
        // short-circuited by `initial_ungated_results` without compiling.
        // Confirmed on xmrs 0.9.9, whose eight `f32::{powf,log2,round,…}` spans
        // resolve to `micromath` in the run `discover_build_enablers` compiles.
        let parent = self.tcx.parent(def_id);
        let owner = match self.tcx.def_kind(parent) {
            rustc_hir::def::DefKind::Impl { .. } => {
                let self_ty = self.tcx.type_of(parent).instantiate_identity();
                match self_ty.ty_adt_def() {
                    Some(adt) => self.tcx.item_name(adt.did()).to_string(),
                    // Primitives, references, slices — no item name to bind.
                    None => self_ty.to_string(),
                }
            }
            // Trait methods: the trait is the name an import would bind.
            _ => self.tcx.item_name(parent).to_string(),
        };
        let path_text = format!("{}::{}", owner, self.tcx.item_name(def_id));

        let span = get_readable_span(&self.tcx, effective_span, &krate);
        self.records.push(PathRecord {
            path_text,
            definition_crate: krate,
            local_route: None,
            defining_module: None,
            context: PathContext::Expression,
            span,
            macro_body_cfgs,
            expansion_crate: expansion_def_crate(self.tcx, site),
            is_extern_crate: false,
            // Set by the driver's facade-gateway pass, not here.
            gateway_anchor: None,
        });
    }
}

impl<'tcx> HirVisitor<'tcx> for MethodResolver<'_, 'tcx> {
    fn visit_expr(&mut self, expr: &'tcx rustc_hir::Expr<'tcx>) {
        // Every expression whose callee only type checking knows. Operators are
        // the same mechanism as a method call: an overloaded `+`, `[]`, `*` or
        // `+=` is a trait call rustc records in the same table, and one written
        // on primitives is built in and has no entry at all — so this adds
        // records exactly where a trait impl was selected, and nothing where the
        // operation is a machine instruction.
        //
        // Reported at the operator itself where there is one, so the span points
        // at the thing that resolved rather than at the whole expression.
        let site = match expr.kind {
            rustc_hir::ExprKind::MethodCall(seg, ..) => Some(seg.ident.span),
            rustc_hir::ExprKind::Binary(op, ..) => Some(op.span),
            rustc_hir::ExprKind::AssignOp(op, ..) => Some(op.span),
            rustc_hir::ExprKind::Unary(..) | rustc_hir::ExprKind::Index(..) => Some(expr.span),
            _ => None,
        };

        if let Some(site) = site
            && let Some(def_id) = self.typeck.type_dependent_def_id(expr.hir_id)
        {
            self.record(site, def_id);
        }
        intravisit::walk_expr(self, expr);
    }
}

/// Walks every body in the crate and collects its method-call resolutions.
///
/// Bodies are visited one owner at a time, reading the *root* owner's typeck
/// results: a closure's method calls are recorded in its enclosing function's
/// tables, so asking for the closure's own would find nothing. The walk itself
/// does not descend into nested bodies — `hir_body_owners` already yields each
/// closure separately, and descending as well would record every call twice.
fn collect_method_records<'tcx>(
    tcx: TyCtxt<'tcx>,
    macro_cfg_map: &HashMap<Symbol, Vec<String>>,
) -> Vec<PathRecord> {
    let mut records = Vec::new();

    for owner in tcx.hir_body_owners() {
        // Analysis may have failed for this body (the pass runs even when it
        // did), in which case there are no results to read.
        if !tcx.has_typeck_results(owner) {
            continue;
        }
        let root = tcx.typeck_root_def_id(owner.to_def_id());
        let Some(root) = root.as_local() else {
            continue;
        };

        let mut visitor = MethodResolver {
            tcx,
            typeck: tcx.typeck(root),
            records: Vec::new(),
            macro_cfg_map,
        };
        visitor.visit_body(tcx.hir_body_owned_by(owner));
        records.extend(visitor.records);
    }

    records
}

/// The crate name to report for a resolution.
///
/// `definition_crate` and `usage_crate` are read downstream as *identities*, not
/// as labels: `usage_crate == "std"` is what fails a crate. But a package is free
/// to name its library anything, including a sysroot crate's name — stdworld
/// 0.1.1 declares
///
/// ```toml
/// [lib]
/// name = "std"
/// ```
///
/// and then `tcx.crate_name(LOCAL_CRATE)` answers `"std"` for every item the
/// crate defines itself, down to its generic parameters (`W`, `K`, `T` all
/// arrived as std usage). All 47 of stdworld's own names read as real std, and
/// no feature set can remove a crate's own definitions, so the verdict could
/// never be anything but "unguarded std".
///
/// A local `DefId` is by construction not the sysroot crate that shares its name,
/// so report it as `LOCAL` — the sentinel the gateway side already uses for
/// "resolved inside this crate", and which `is_local_reexport` and the
/// cross-crate projection in `analyze_crate` both already recognise.
///
/// Deliberately narrow: a local crate with a non-colliding name keeps reporting
/// its own name, exactly as before. Mapping *every* local resolution to `LOCAL`
/// is the cleaner invariant but a corpus-wide behaviour change — a `crate::…`
/// record currently carries the local crate's name, which is `!= "LOCAL"`, so
/// `is_local_reexport` returns false for it today and would start returning true.
/// That is not this fix.
fn reported_crate_name(tcx: TyCtxt<'_>, def_id: DefId) -> String {
    let name = tcx.crate_name(def_id.krate);
    if def_id.krate == LOCAL_CRATE && consts::SYSROOT_CRATE_NAMES.contains(&name.as_str()) {
        return consts::LOCAL_CRATE_SENTINEL.to_string();
    }
    name.to_string()
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
    /// A `build_script_build` unit. The script has to be compiled and run for
    /// the crate to build at all, and nothing it contains is the crate's own
    /// std usage, so both callbacks stand aside and let it through.
    build_script: bool,
    /// The AST pass's records, held for `after_analysis` to extend with the
    /// method calls only type checking can resolve.
    ast_records: Vec<PathRecord>,
    macro_imports: Vec<(String, String)>,
    macro_cfg_map: HashMap<Symbol, Vec<String>>,
}

impl rustc_driver::Callbacks for MyCompilerCalls {
    fn after_expansion<'tcx>(
        &mut self,
        _compiler: &interface::Compiler,
        tcx: TyCtxt<'tcx>,
    ) -> Compilation {
        if self.build_script {
            return Compilation::Continue;
        }

        let (records, macro_imports) = {
            let resolver_wrapper = tcx.resolver_for_lowering().borrow();
            let (resolver, krate) = &*resolver_wrapper;

            // Pre-scan all macro_rules! definitions to collect #[cfg(…)] attribute
            // strings from their bodies, keyed by macro name. Recurses through
            // modules: a `macro_rules!` is far more often defined inside `mod foo`
            // (`src/foo.rs`) than at the crate root, and iterating only
            // `krate.items` left every such macro's body cfg unrecorded — so an
            // `if_std!`/`trace!`-style body gate never reached its expansion
            // records (stak-vm's `mod vm` trace!, vls-core's `mod util::log_utils`
            // catch_panic!).
            let source_map = tcx.sess.source_map();
            let mut macro_cfg_map: HashMap<Symbol, Vec<String>> = HashMap::new();
            collect_macro_cfgs(&krate.items, source_map, &mut macro_cfg_map);

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
            // The cfg map is taken back rather than rebuilt: `after_analysis`
            // needs it to give a method call inside a macro body the same gates
            // a path there gets, and the AST it was collected from is gone by
            // then (HIR lowering steals the resolver).
            self.macro_cfg_map = visitor.macro_cfg_map;
            (visitor.records, visitor.macro_module_imports)
        };

        // Written here as well as after analysis so an ICE in type checking
        // leaves the records this pass already has, rather than nothing. The
        // output is then taken apart again rather than cloned — feature-heavy
        // crates carry enough records that a spare copy is worth avoiding.
        let output_data = FeatureRunOutput {
            records,
            macro_module_imports: macro_imports,
            out_dir: env::var("OUT_DIR").ok(),
        };
        write_output(&output_data);
        self.ast_records = output_data.records;
        self.macro_imports = output_data.macro_module_imports;

        Compilation::Continue
    }

    /// The type-checked pass. Everything the AST could not resolve — method
    /// calls — is added here, and this is where the run stops.
    ///
    /// Reaching this point at all is the other half of the fix: stopping after
    /// expansion accepted any crate whose *names* resolved, so a call to a
    /// method that does not exist on a bare-metal target (`f32::log2`, an
    /// `E0599`) left the run looking successful, `LAST_GOOD_TARGET` pinned to
    /// bare metal, and the host fallback that would have exposed the std usage
    /// never ran. A covering set that now fails to type check is handled the way
    /// a failing set always was: CEGAR forbids the assignment and re-partitions.
    ///
    /// `Stop` still lands before codegen, so no metadata is emitted — exactly as
    /// before, which is why `--lib` is still what keeps bin targets out.
    fn after_analysis<'tcx>(
        &mut self,
        _compiler: &interface::Compiler,
        tcx: TyCtxt<'tcx>,
    ) -> Compilation {
        if self.build_script {
            return Compilation::Continue;
        }

        let mut records = std::mem::take(&mut self.ast_records);
        records.extend(collect_method_records(tcx, &self.macro_cfg_map));

        write_output(&FeatureRunOutput {
            records,
            macro_module_imports: std::mem::take(&mut self.macro_imports),
            out_dir: env::var("OUT_DIR").ok(),
        });

        Compilation::Stop
    }
}

fn write_output(output_data: &FeatureRunOutput) {
    let filename = env::var(consts::PLUGIN_OUTPUT_ENV).unwrap_or_else(|_| {
        panic!(
            "Expected environment variable {} to be set",
            consts::PLUGIN_OUTPUT_ENV
        )
    });

    if let Ok(file) = std::fs::File::create(&filename) {
        serde_json::to_writer(file, &output_data).unwrap();
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
        let mut callbacks = MyCompilerCalls {
            build_script: compiler_args.iter().any(|arg| arg == "build_script_build"),
            ast_records: Vec::new(),
            macro_imports: Vec::new(),
            macro_cfg_map: HashMap::new(),
        };
        rustc_driver::run_compiler(&compiler_args, &mut callbacks);
        Ok(())
    }
}

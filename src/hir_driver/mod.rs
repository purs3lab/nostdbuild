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
use rustc_middle::ty::{
    self, GenericArgsRef, ResolverAstLowering, Ty, TyCtxt, TypeVisitableExt, TypeckResults,
};
use rustc_span::hygiene::ExpnKind;
use rustc_span::source_map::SourceMap;
use rustc_span::{Span, Symbol};

use std::collections::HashMap;
use std::collections::HashSet;

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
    resolver: &'r ResolverAstLowering<'tcx>,
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
                        source_file.prefer_local_unconditionally().to_string(),
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
                    // An `extern crate` names a crate, not an item in one.
                    definition_span: None,
                    is_float_primitive_method: false,
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
                definition_span: definition_span_of(&self.tcx, final_def_id),
                // A `use`/path record names an item, never a receiver type —
                // there is no `self_ty` here to classify.
                is_float_primitive_method: false,
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

fn call_site_span(span: Span, macro_cfg_map: &HashMap<Symbol, Vec<String>>) -> (Span, Vec<String>) {
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
    /// The impls the compiler selected for this crate's trait obligations —
    /// see [`ImplRecord`] and `record_obligations`.
    impls: Vec<ImplRecord>,
    /// Every impl one already-walked obligation needs, its own where-clauses
    /// included, so a bound that appears at a hundred call sites costs one
    /// selection. Memoising the *whole* subtree and not just the impl selected
    /// at the top is what keeps the answer independent of which call site is
    /// visited first. The records are still emitted per call site: the span is
    /// what decides whether the no_std build reaches the call, so collapsing
    /// them would throw the test away.
    selected: &'a mut HashMap<ty::TraitRef<'tcx>, Vec<SelectedImpl>>,
}

/// One resolved obligation, cached across the call sites that share it.
#[derive(Clone)]
struct SelectedImpl {
    trait_name: String,
    self_ty: String,
    definition_crate: String,
    impl_span: ReadableSpan,
    via_macro: Option<String>,
}

/// How far to follow a selected impl's own where-clauses.
///
/// One level is not enough, and unit-sphere 0.4.0 is why: `.norm_squared()` on a
/// `Vector3<f64>` needs `f64: SimdComplexField`, which simba discharges with an
/// **ungated** blanket `impl<T: ComplexField> SimdComplexField for T`. The impl
/// that actually carries the `#[cfg]` is one where-clause further down
/// (`f64: ComplexField`). Bounded because the chain is a graph and this runs on
/// every call site in every pass.
const MAX_OBLIGATION_DEPTH: u32 = 6;

impl<'tcx> MethodResolver<'_, 'tcx> {
    /// Record the impl the compiler selected for each of this call's trait
    /// obligations, following each selected impl's own where-clauses.
    ///
    /// This is the half of a feature requirement that no amount of source
    /// analysis recovers (KI-27). `PathRecord` reports what the crate *names*,
    /// and multiexp 0.4.0 names `zeroize` — the method, which zeroize gates
    /// behind nothing. What its build needs is `#[cfg(feature = "alloc")]
    /// impl<Z> Zeroize for Vec<Z>`, an item with no identifier that multiexp's
    /// source never mentions. The type checker is the only thing in the run that
    /// knows the call depends on it, so it is asked here.
    ///
    /// Nothing is *decided* here: this reports which impl was selected in the
    /// configuration this pass compiled. Which gate the emitted configuration
    /// has to satisfy is `driver::impl_availability_requirement`'s question, and
    /// it reads every impl of the same obligation rather than the one that won
    /// here — a std-on pass selects the std-gated arm, and requiring *that*
    /// would be exactly backwards.
    fn record_obligations(&mut self, hir_id: rustc_hir::HirId, def_id: DefId, site: Span) {
        let args = self.typeck.node_args(hir_id);
        let (effective_span, _) = call_site_span(site, self.macro_cfg_map);
        let krate = self.tcx.crate_name(LOCAL_CRATE).to_string();
        let span = get_readable_span(&self.tcx, effective_span, &krate);
        let mut out = Vec::new();
        let mut on_path = HashSet::new();
        self.walk_obligations(def_id, args, 0, &mut on_path, &mut out);

        for sel in out {
            self.impls.push(ImplRecord {
                span: span.clone(),
                trait_name: sel.trait_name,
                self_ty: sel.self_ty,
                definition_crate: sel.definition_crate,
                impl_span: sel.impl_span,
                via_macro: sel.via_macro,
            });
        }
    }

    /// Every impl needed to discharge `def_id`'s where-clauses at `args`,
    /// appended to `out`. Returns whether the walk ran to completion.
    ///
    /// `on_path` is the chain of obligations currently being resolved, not a
    /// been-here set: a bound reached twice down two different branches has to
    /// be walked twice, or the memo written for the first branch would be
    /// missing whatever the second one deduped. It exists only so a cyclic
    /// bound terminates.
    ///
    /// The return value is what keeps the memo honest. A subtree cut short —
    /// by the depth cap or by a cycle — is an answer about *this* path, not
    /// about the obligation, so caching it would hand one call site's truncation
    /// to every other one and make the crate's records depend on visit order.
    fn walk_obligations(
        &mut self,
        def_id: DefId,
        args: GenericArgsRef<'tcx>,
        depth: u32,
        on_path: &mut HashSet<ty::TraitRef<'tcx>>,
        out: &mut Vec<SelectedImpl>,
    ) -> bool {
        if depth > MAX_OBLIGATION_DEPTH {
            return false;
        }
        let mut complete = true;
        let tcx = self.tcx;
        for (clause, _) in tcx.clauses_of(def_id).instantiate(tcx, args).into_iter() {
            let Some(trait_clause) = clause.as_trait_clause() else {
                continue;
            };
            let trait_ref = trait_clause.skip_binder().trait_ref;
            // Only a fully concrete obligation has an answer. Inside a generic
            // function the bound is still `T: Zeroize`, and there is no impl to
            // select — the requirement lands on whoever calls it with a concrete
            // type, which is the crate this pass is analysing.
            if trait_ref.has_param() || trait_ref.has_infer() || trait_ref.has_escaping_bound_vars()
            {
                continue;
            }
            if let Some(hit) = self.selected.get(&trait_ref) {
                out.extend(hit.iter().cloned());
                continue;
            }
            if !on_path.insert(trait_ref) {
                complete = false;
                continue;
            }

            let mut subtree = Vec::new();
            let mut subtree_complete = true;
            let input = ty::TypingEnv::fully_monomorphized().as_query_input(trait_ref);
            if let Ok(rustc_middle::traits::ImplSource::UserDefined(data)) =
                tcx.codegen_select_candidate(input)
            {
                if let Some(sel) = self.describe_impl(trait_ref, data.impl_def_id) {
                    subtree.push(sel);
                }
                // The selected impl's own where-clauses are obligations of this
                // call too, and they are where the gate usually is.
                subtree_complete = self.walk_obligations(
                    data.impl_def_id,
                    data.args,
                    depth + 1,
                    on_path,
                    &mut subtree,
                );
            }

            on_path.remove(&trait_ref);
            if subtree_complete {
                self.selected.insert(trait_ref, subtree.clone());
            } else {
                complete = false;
            }
            out.extend(subtree);
        }
        complete
    }

    /// The record for one selected impl, or `None` when it constrains nothing a
    /// dependency's feature set could change.
    fn describe_impl(
        &self,
        trait_ref: ty::TraitRef<'tcx>,
        impl_did: DefId,
    ) -> Option<SelectedImpl> {
        let tcx = self.tcx;
        // An impl in this crate or in the sysroot is not a dependency's to gate.
        // `core`'s impls in particular are the overwhelming majority of what a
        // walk like this turns up, and none of them answers to a cargo feature.
        if impl_did.krate == LOCAL_CRATE {
            return None;
        }
        let krate = tcx.crate_name(impl_did.krate).to_string();
        if consts::SYSROOT_CRATE_NAMES.contains(&krate.as_str()) {
            return None;
        }
        let self_ty = ty_head(tcx, trait_ref.self_ty())?;
        let def_span = tcx.def_span(impl_did);
        let via_macro = match def_span.ctxt().outer_expn_data().kind {
            ExpnKind::Macro(_, name) => Some(name.to_string()),
            _ => None,
        };
        Some(SelectedImpl {
            trait_name: tcx.item_name(trait_ref.def_id).to_string(),
            self_ty,
            definition_crate: krate.clone(),
            // `source_callsite`, not the definition span: an impl written inside
            // a `macro_rules!` body reports a span in that body, and the `#[cfg]`
            // deciding whether it exists sits on the *invocation* (simba's
            // `impl_complex!`). The callsite is the span the crate's own module
            // tree has a gate for.
            impl_span: get_readable_span(&tcx, def_span.source_callsite(), &krate),
            via_macro,
        })
    }
}

/// The head identifier of a type, as the module tree spells it — `Vec` for
/// `Vec<Vec<u8>>`, `f64` for `f64`. References are looked through, because an
/// impl is written for the type and not for a borrow of it.
///
/// `None` for a type with no name to match on (a closure, a function pointer, a
/// tuple). Skipping is the safe answer: the requirement is simply not recorded,
/// which is where every one of them was before.
fn ty_head<'tcx>(tcx: TyCtxt<'tcx>, ty: Ty<'tcx>) -> Option<String> {
    match ty.kind() {
        ty::Ref(_, inner, _) => ty_head(tcx, *inner),
        ty::Adt(def, _) => Some(tcx.item_name(def.did()).to_string()),
        ty::Bool | ty::Char | ty::Int(_) | ty::Uint(_) | ty::Float(_) | ty::Str => {
            Some(ty.to_string())
        }
        _ => None,
    }
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
        // Set alongside `owner` for the inherent-impl arm only: an impl's
        // `self_ty` is the receiver rustc actually bound, not a name read back
        // off `owner`'s rendered text (R34-3 — see `PathRecord::is_float_primitive_method`).
        let mut is_float_primitive_method = false;
        let owner = match self.tcx.def_kind(parent) {
            rustc_hir::def::DefKind::Impl { .. } => {
                let self_ty = self
                    .tcx
                    .type_of(parent)
                    .instantiate_identity()
                    .skip_norm_wip();
                is_float_primitive_method = matches!(self_ty.kind(), ty::Float(_));
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
            definition_span: definition_span_of(&self.tcx, def_id),
            is_float_primitive_method,
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
            self.record_obligations(expr.hir_id, def_id, site);
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
/// Give every `use` record the span of the item it actually imports (R34-6).
///
/// The AST pass cannot answer this. `visit_path` reads `partial_res_map`, which
/// carries a resolution per *segment*, and for a `use` the final segment is not
/// in it — import resolutions live on `PerOwnerResolverData::import_res`
/// instead. So the deepest segment it finds for `use num_traits::float::Float`
/// is the module `float`, and `def_span` answers with `pub mod float;` — an
/// ungated line, from which no requirement follows. That is not a near miss: it
/// is the wrong item, and the gate that matters (`#[cfg(any(feature = "std",
/// feature = "libm"))]` above `pub trait Float`) is never seen.
///
/// HIR is where the question is answerable. `ItemKind::Use` carries a `UsePath`
/// whose `res` holds the resolution per namespace, already following the import
/// to what it names. Running here also sidesteps the reason the AST pass cannot
/// simply ask: `def_span` on a local `DefId` during `after_expansion` steals a
/// resolver that pass is still holding.
///
/// Overwrites rather than fills, because the AST pass's answer for these records
/// is positively wrong and not merely absent.
fn fill_import_definition_spans(tcx: TyCtxt<'_>, records: &mut [PathRecord]) {
    // `(file, line, imported name)` → where that name is defined. The file and
    // line are the `use` statement's own, which is what the AST record's span
    // points at; the name disambiguates the leaves of a braced import, which
    // lower to one HIR item each and therefore share a line.
    let mut by_site: HashMap<(String, usize, String), ReadableSpan> = HashMap::new();

    for item_id in tcx.hir_free_items() {
        let item = tcx.hir_item(item_id);
        let rustc_hir::ItemKind::Use(use_path, _) = item.kind else {
            continue;
        };
        let Some(last) = use_path.segments.last() else {
            continue;
        };
        let name = last.ident.to_string();
        let loc = tcx.sess.source_map().lookup_char_pos(use_path.span.lo());
        let file = loc.file.name.prefer_local_unconditionally().to_string();

        for res in use_path.res.present_items() {
            let Some(def_id) = res.opt_def_id() else {
                continue;
            };
            // Foreign only — the requirement is about a *dependency's* features,
            // and a local item has no dependency edge to constrain.
            if def_id.is_local() {
                continue;
            }
            let Some(span) = definition_span_of(&tcx, def_id) else {
                continue;
            };
            by_site.insert((file.clone(), loc.line, name.clone()), span);
            break;
        }
    }

    if by_site.is_empty() {
        return;
    }

    for record in records.iter_mut() {
        if record.context != PathContext::ImportDeclaration {
            continue;
        }
        let leaf = record
            .path_text
            .rsplit("::")
            .next()
            .unwrap_or(&record.path_text)
            .to_string();
        if let Some(span) = by_site.get(&(record.span.file.clone(), record.span.start_line, leaf)) {
            record.definition_span = Some(span.clone());
        }
    }
}

fn collect_method_records<'tcx>(
    tcx: TyCtxt<'tcx>,
    macro_cfg_map: &HashMap<Symbol, Vec<String>>,
) -> (Vec<PathRecord>, Vec<ImplRecord>) {
    let mut records = Vec::new();
    let mut impls = Vec::new();
    // One cache for the whole crate, not one per body: the same bound turns up
    // in every function that touches the type.
    let mut selected: HashMap<ty::TraitRef<'tcx>, Vec<SelectedImpl>> = HashMap::new();

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
            impls: Vec::new(),
            selected: &mut selected,
        };
        visitor.visit_body(tcx.hir_body_owned_by(owner));
        records.extend(visitor.records);
        impls.extend(visitor.impls);
    }

    // One entry per (call site, obligation): the same call in a generic function
    // instantiated twice resolves to the same impl, and the tree lookup the
    // driver does per record is not free. Deduped through a set rather than by
    // sorting, so a crate with a lot of calls does not pay a comparison sort
    // over the whole list.
    let mut unique: HashSet<ImplRecord> = HashSet::with_capacity(impls.len());
    impls.retain(|record| unique.insert(record.clone()));
    // Sorted for a stable plugin output. On field references, never on a
    // formatted record: that is a `Debug` render per comparison.
    impls.sort_by(|a, b| {
        (
            &a.definition_crate,
            &a.trait_name,
            &a.self_ty,
            &a.span.file,
            a.span.start_line,
            a.span.start_col,
        )
            .cmp(&(
                &b.definition_crate,
                &b.trait_name,
                &b.self_ty,
                &b.span.file,
                b.span.start_line,
                b.span.start_col,
            ))
    });

    (records, impls)
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
        file: loc.file.name.prefer_local_unconditionally().to_string(),
        start_line: loc.line,
        start_col: loc.col.0,
        end_line: end_loc.line,
        end_col: end_loc.col.0,
        usage_crate: Some(usage_crate.to_string()),
    }
}

/// Where `def_id`'s item is written, as a span in the defining crate's own
/// source — `PathRecord::definition_span`.
///
/// `tcx.def_span` answers for a foreign `DefId` as readily as a local one: the
/// definition's source file rides in the defining crate's metadata, which is how
/// rustc renders `found an item that was configured out` against a path under
/// `registry/src/…`. The same call already backs `ImplRecord::impl_span`.
///
/// The `usage_crate` recorded is the defining crate, because that is whose tree
/// the span will be looked up in — `driver::span_in_dep_tree` maps it onto the
/// on-disk copy of that crate.
///
/// **Foreign definitions only, and that is a hard requirement, not a filter.**
/// The AST pass calls this while it holds the `resolver_for_lowering` steal
/// guards, and `def_span` on a *local* `DefId` runs a query that lowers the
/// crate — which tries to steal what is already borrowed and aborts rustc with
/// `stealing value which is locked`. That kills the whole analysis pass, not
/// just this record. A foreign `DefId` is answered from the defining crate's
/// metadata and touches no local query. Cross-crate items are the only ones this
/// is for, so the guard costs nothing.
///
/// `None` for a dummy span, which is what the compiler reports for items it
/// synthesised rather than read from source; there is no `#[cfg]` above those to
/// find.
fn definition_span_of(tcx: &TyCtxt<'_>, def_id: DefId) -> Option<ReadableSpan> {
    if def_id.is_local() {
        return None;
    }
    let span = tcx.def_span(def_id);
    if span.is_dummy() {
        return None;
    }
    let krate = reported_crate_name(*tcx, def_id);
    Some(get_readable_span(tcx, span, &krate))
}

struct MyCompilerCalls {
    /// A build-script unit, per [`is_build_script_unit`]. The script has to be
    /// compiled *and run* for the crate to build at all, and nothing it contains
    /// is the crate's own std usage, so both callbacks stand aside and let it
    /// through to codegen.
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
            // `resolver_for_lowering` used to hand back one `Steal` over the
            // `(resolver, krate)` pair; it now hands back a `Steal` for each, so
            // the two are borrowed separately. Both guards have to outlive the
            // visit — HIR lowering steals them afterwards — hence the two `let`s
            // rather than borrowing inline.
            let resolver_steal = tcx.resolver_for_lowering().0.borrow();
            let krate_steal = tcx.resolver_for_lowering().1.borrow();
            let (resolver, krate) = (&*resolver_steal, &*krate_steal);

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
            // The AST pass resolves no obligations; they arrive with the
            // type-checked pass below.
            impls: Vec::new(),
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
        fill_import_definition_spans(tcx, &mut records);
        let (method_records, impls) = collect_method_records(tcx, &self.macro_cfg_map);
        records.extend(method_records);

        write_output(&FeatureRunOutput {
            records,
            macro_module_imports: std::mem::take(&mut self.macro_imports),
            out_dir: env::var("OUT_DIR").ok(),
            impls,
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

/// A plugin pass is an *analysis* compile, so the crate's own lint levels must not
/// decide its no_std verdict.
///
/// A probe negates a feature gate, which deletes code, which makes other code dead.
/// In a crate that writes `#![deny(warnings)]` — or `deny(unused_imports)`, or
/// `deny(dead_code)` — that is a hard error, so every probe fails and every std span
/// comes back unproven over a diagnostic that says nothing about std.
/// `agnostic-lite-0.5.5` is the case: 74 probe compiles, all of them
/// `error: struct `JoinError` is never constructed`, 114 spans unproven, nothing
/// emitted. `--cap-lints` caps lints only, so a real `E0463` still fails the probe.
///
/// The cap belongs here and not in `RUSTFLAGS`: `RUSTFLAGS=--cap-lints=allow` on the
/// `cargo hir` invocation breaks cargo's target-information probe under the plugin's
/// rustc wrapper (`output of --print=file-names missing when learning about
/// target-specific information from rustc` — 78 occurrences on `bitwrap_extra-2.0.6`
/// with the flag, 0 without, on the first plugin pass for every bare-metal target).
/// It does not degrade the analysis, it deletes it. Setting `RUSTFLAGS` process-wide
/// measures the same. Here, cargo's probe is not involved.
///
/// The verification build in `compiler.rs` deliberately does *not* cap: a
/// configuration that trips the crate's lints really does fail to build, and that
/// failure is the crate's, filed under `DENY_LINT_FALLOUT`.
///
/// Left alone when cargo has already passed a cap. Cargo caps registry dependencies
/// itself, and this filter runs over `CrateFilter::AllCrates`; honouring an existing
/// `--cap-lints` means we never *raise* a dependency's cap, and the only crate this
/// reaches is the workspace crate cargo left uncapped.
fn cap_lints_for_analysis(mut compiler_args: Vec<String>) -> Vec<String> {
    let already_capped = compiler_args
        .iter()
        .any(|arg| arg == "--cap-lints" || arg.starts_with("--cap-lints="));

    if !already_capped {
        compiler_args.push("--cap-lints=warn".to_string());
    }

    compiler_args
}

/// The value cargo passed for `flag`, accepting both `--flag value` and
/// `--flag=value`.
fn flag_value<'a>(compiler_args: &'a [String], flag: &str) -> Option<&'a str> {
    let mut args = compiler_args.iter();
    while let Some(arg) = args.next() {
        if let Some(rest) = arg.strip_prefix(flag) {
            return match rest.strip_prefix('=') {
                Some(inline) => Some(inline),
                None if rest.is_empty() => args.next().map(String::as_str),
                // A longer flag that merely starts with this one.
                None => continue,
            };
        }
    }
    None
}

/// Is this rustc invocation the build script of the crate under analysis?
///
/// Cargo names a build-script unit `build_script_<stem of the manifest's `build`
/// key>`, so the name is only `build_script_build` for the default `build.rs`.
/// `mavlink 0.13.1` declares `build = "build/main.rs"` and gets
/// `build_script_main`; the old equality test missed it, both callbacks ran, and
/// `after_analysis`'s `Stop` landed before codegen, so no `build_script_main`
/// executable was ever written. Cargo then reported the script as *never
/// executed* — `No such file or directory` on a path inside an `out/` that does
/// not exist — with no compile error to explain it, and every pass that decides
/// something by compiling the crate got "no" for free (KI-29).
///
/// The prefix alone would be wrong. `RUSTC_WORKSPACE_WRAPPER` is what puts this
/// driver in front of rustc, and cargo applies that to workspace members only —
/// `CrateFilter::AllCrates` relaxes the driver's `CARGO_PRIMARY_PACKAGE` test but
/// does not widen the wrapper — so the only units reaching here are the root
/// crate and its build script. A root crate whose *own* lib is named
/// `build_script_…` would therefore be waved through, `write_output` would never
/// run, and the crate would read as having no std usage at all: a silent false
/// no_std verdict. The index holds three (`build-script-cfg`,
/// `build_script_file_gen`, `build-script-utils`; `build_script` itself lacks the
/// trailing underscore and never matched).
///
/// `--crate-type bin` is what rules them out. The pass runs under `cargo check`,
/// where every analysed target is `--crate-type lib --emit=dep-info,metadata` and
/// only a unit cargo must really produce a binary for is
/// `--crate-type bin --emit=dep-info,link`. Measured on mavlink: its build script
/// is `--crate-name build_script_main … --crate-type bin`, its lib
/// `--crate-name mavlink … --crate-type lib`.
///
/// **Boundary.** A *bin-only* crate named `build_script_…` is still skipped:
/// `driver.rs` withholds `--lib` when the package has no lib target, so its bin
/// does arrive as `--crate-type bin`. No such crate exists in the index today —
/// all three candidates above are libraries. Testing `--emit` for `link` instead
/// would close it, but that also changes what happens to a proc-macro root, which
/// is not this fix's business.
fn is_build_script_unit(compiler_args: &[String]) -> bool {
    flag_value(compiler_args, "--crate-name")
        .is_some_and(|name| name.starts_with("build_script_"))
        && flag_value(compiler_args, "--crate-type").is_some_and(|kind| kind == "bin")
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
            build_script: is_build_script_unit(&compiler_args),
            ast_records: Vec::new(),
            macro_imports: Vec::new(),
            macro_cfg_map: HashMap::new(),
        };
        let compiler_args = cap_lints_for_analysis(compiler_args);
        rustc_driver::run_compiler(&compiler_args, &mut callbacks);
        Ok(())
    }
}

#[cfg(test)]
mod build_script_unit_tests {
    use super::is_build_script_unit;

    fn args(raw: &[&str]) -> Vec<String> {
        raw.iter().map(|s| s.to_string()).collect()
    }

    /// The two shapes measured off `cargo check -vv` on `mavlink 0.13.1`.
    #[test]
    fn custom_build_path_is_a_build_script() {
        assert!(is_build_script_unit(&args(&[
            "rustc",
            "--crate-name",
            "build_script_main",
            "build/main.rs",
            "--crate-type",
            "bin",
            "--emit=dep-info,link",
        ])));
    }

    #[test]
    fn default_build_rs_still_matches() {
        assert!(is_build_script_unit(&args(&[
            "rustc",
            "--crate-name",
            "build_script_build",
            "build.rs",
            "--crate-type",
            "bin",
        ])));
    }

    #[test]
    fn the_analysed_lib_is_not_a_build_script() {
        assert!(!is_build_script_unit(&args(&[
            "rustc",
            "--crate-name",
            "mavlink",
            "src/lib.rs",
            "--crate-type",
            "lib",
            "--emit=dep-info,metadata",
        ])));
    }

    /// The false positive a bare prefix test would introduce: three crates in the
    /// index are libraries whose own name starts with `build_script_`, and waving
    /// one through costs it every std record it has.
    #[test]
    fn a_lib_named_build_script_is_still_analysed() {
        for name in ["build_script_cfg", "build_script_file_gen", "build_script_utils"] {
            assert!(
                !is_build_script_unit(&args(&[
                    "rustc",
                    "--crate-name",
                    name,
                    "src/lib.rs",
                    "--crate-type",
                    "lib",
                ])),
                "{name} was skipped as a build script"
            );
        }
    }

    /// `build_script` proper has no trailing underscore and never matched.
    #[test]
    fn the_crate_named_build_script_does_not_match() {
        assert!(!is_build_script_unit(&args(&[
            "rustc",
            "--crate-name",
            "build_script",
            "src/lib.rs",
            "--crate-type",
            "bin",
        ])));
    }

    #[test]
    fn inline_flag_values_are_read() {
        assert!(is_build_script_unit(&args(&[
            "rustc",
            "--crate-name=build_script_main",
            "--crate-type=bin",
        ])));
    }

    /// The old test was `any(|arg| arg == "build_script_build")`, which any
    /// argv element could satisfy — a `--cfg` value, say.
    #[test]
    fn only_the_crate_name_operand_counts() {
        assert!(!is_build_script_unit(&args(&[
            "rustc",
            "--crate-name",
            "somecrate",
            "--cfg",
            "feature=\"build_script_build\"",
            "--crate-type",
            "lib",
        ])));
    }
}

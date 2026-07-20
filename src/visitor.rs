use itertools::Itertools;
use log::debug;
use proc_macro2::{Delimiter, Group, Punct, Spacing, Span, TokenStream, TokenTree};
// use quote::ToTokens;
use syn::{
    Attribute, ExprBlock, Item, ItemExternCrate, Meta, Stmt, spanned::Spanned, visit::Visit,
};

use cargo_metadata::{MetadataCommand, TargetKind};
use std::{
    fmt::Debug,
    path::{Path, PathBuf},
};

use std::collections::HashSet;
use z3::ast::Bool;

use crate::types::*;
use crate::{Attributes, driver, parser};

#[derive(Default, Clone, Debug)]
pub struct ParsedAttr {
    pub constants: Vec<String>,
    /// Did we find a typo? Specifically, did we find "feature"
    /// in a typoed form
    pub typoed_keyword: bool,
    pub features: Vec<String>,
    pub filepath: Option<String>,
    pub logic: Vec<parser::Logic>,
}

#[derive(Default, Clone, Debug)]
pub struct ItemExternCrates {
    pub itemexterncrates: Vec<ItemExternCrate>,
}

#[derive(Default, Clone, Debug)]
pub struct ItemExternCratesAll {
    pub itemexterncrates: Vec<ItemExternCrate>,
}

pub trait GetItemExternCrate {
    fn get_item_extern_crate(&mut self) -> Option<&mut Vec<ItemExternCrate>> {
        None
    }
    fn get_spans(&mut self) -> Option<&mut Vec<(Span, Option<String>)>> {
        None
    }
    fn set_current_file(&mut self, _file: String) {}
}

impl GetItemExternCrate for Attributes {
    fn get_spans(&mut self) -> Option<&mut Vec<(Span, Option<String>)>> {
        Some(&mut self.spans)
    }
    fn set_current_file(&mut self, file: String) {
        self.current_file = file;
    }
}

impl GetItemExternCrate for ItemExternCrates {
    fn get_item_extern_crate(&mut self) -> Option<&mut Vec<ItemExternCrate>> {
        Some(&mut self.itemexterncrates)
    }
}
impl GetItemExternCrate for ItemExternCratesAll {
    fn get_item_extern_crate(&mut self) -> Option<&mut Vec<ItemExternCrate>> {
        Some(&mut self.itemexterncrates)
    }
}

impl<'a> Visit<'a> for ItemExternCrates {
    fn visit_item_extern_crate(&mut self, i: &ItemExternCrate) {
        // We will save all the extern crates that have an
        // attribute associated with them.
        if !i.attrs.is_empty() {
            self.itemexterncrates.push(i.clone());
        }
    }

    fn visit_expr_block(&mut self, i: &'a ExprBlock) {
        visit_expr_block_common(self, i);
    }
}

impl<'a> Visit<'a> for ItemExternCratesAll {
    fn visit_item_extern_crate(&mut self, i: &ItemExternCrate) {
        self.itemexterncrates.push(i.clone());
    }

    fn visit_expr_block(&mut self, i: &'a syn::ExprBlock) {
        visit_expr_block_common(self, i);
    }
}

/// Visit ExprBlocks of the form
/// ```ignore
/// #[cfg(feature = "use-locks")]
/// {
///     self.source.lock.unlock();
///
///     #[cfg(feature = "std")]
///     {
///         extern crate std;
///         if std::thread::panicking() {
///             self.source.state.set(State::Poisoned);
///         }
///     }
/// }
/// ```
/// and recurse until it finds an extern crate std.
/// Once this is found, all the attributes in its path in the
/// ast will be added to the `item_extern_crate`
/// # Arguments
/// * `typ` - The type which we update with the new extern crate std
/// * `expr_block` - The expression block to visit
fn visit_expr_block_common<'a, T: Visit<'a> + GetItemExternCrate>(
    typ: &mut T,
    expr_block: &'a ExprBlock,
) {
    let old_len = typ.get_item_extern_crate().unwrap_or(&mut Vec::new()).len();
    syn::visit::visit_expr_block(typ, expr_block);
    let get_item_extern_crate = typ.get_item_extern_crate().unwrap();
    let changed = get_item_extern_crate.len() > old_len;
    if !expr_block.attrs.is_empty() {
        let stmts = &expr_block.block.stmts;
        let attrs = expr_block.attrs.clone();
        for stmt in stmts {
            match stmt {
                Stmt::Item(Item::ExternCrate(item)) => {
                    debug!("Found extern crate: {}", item.ident);
                    get_item_extern_crate.retain(|i| i != item);
                    let new = ItemExternCrate {
                        attrs: attrs.clone(),
                        ..item.clone()
                    };
                    get_item_extern_crate.push(new);
                }
                _ if changed => {
                    let extern_item = get_item_extern_crate.last().unwrap();
                    let new = ItemExternCrate {
                        attrs: attrs.clone(),
                        ..extern_item.clone()
                    };
                    get_item_extern_crate.push(new);
                }
                _ => {
                    debug!(
                        "Found unexpected statement in extern crate block: {:?}",
                        stmt
                    );
                }
            }
        }
    }
}

impl<'a> Visit<'a> for Attributes {
    fn visit_attribute(&mut self, i: &Attribute) {
        if let Some(ident) = i.path().get_ident() {
            if ident == "cfg" || ident == "cfg_attr" {
                match i.meta.clone() {
                    Meta::List(meta_list) => {
                        let tokens = meta_list.tokens.clone();
                        let negated: Attribute = syn::parse_quote!(
                        #[cfg(not(#tokens))]
                        );
                        if self.compile_error_attrs.iter().any(|a| a == &negated) {
                            debug!(
                                "Attribute {:?} is already negated in compile_error_attrs, skipping",
                                i
                            );
                            return;
                        }
                    }
                    _ => {
                        debug!("Not meta list type for cfg/cfg_attr: {:?}", i.meta);
                    }
                }
                // Only add the attributes which we are not negating.
                self.attributes.push(i.clone());
            }
            if ident == "no_std" {
                match i.style {
                    syn::AttrStyle::Outer => {
                        self.wrong_unconditional_setup = true;
                        debug!("Error: no_std attribute should be inner attribute");
                    }
                    _ => {
                        self.unconditional_no_std = true;
                    }
                }
            }
        }
    }

    fn visit_item_macro(&mut self, i: &syn::ItemMacro) {
        if i.mac.path.is_ident("compile_error") {
            let attrs = i.attrs.clone();
            if attrs.is_empty() {
                debug!("No attributes found for compile_error macro");
                return;
            }
            // Remove the attribute from the list which are
            // compiler_error attributes.
            let attr = attrs.iter().find(|a| a.path().is_ident("cfg")).unwrap();
            self.attributes.retain(|a| a != attr);
            // Negate the attrs[0] and add it to the attributes
            match attr.meta.clone() {
                Meta::List(meta_list) => {
                    let path = meta_list.path.get_ident();
                    if path.is_some() && path.unwrap() == "cfg" {
                        let tokens = meta_list.tokens.clone();
                        let negated: Attribute = syn::parse_quote!(
                        #[cfg(not(#tokens))]
                        );
                        self.compile_error_attrs.push(negated.clone());
                        self.attributes.push(negated);
                    }
                }
                _ => {
                    debug!("Unexpected meta type for compiler_error: {:?}", attr.meta);
                }
            }
        }
    }

    fn visit_item_mod(&mut self, i: &'a syn::ItemMod) {
        if i.ident != "test" {
            debug!("Visiting module: {}", i.ident);
            syn::visit::visit_item_mod(self, i);
        }
    }

    fn visit_variant(&mut self, i: &'a syn::Variant) {
        let attrs: &Vec<Attribute> = &i.attrs;
        let span: Span = i.span();
        check_attr_save_span(self, attrs, span);
        syn::visit::visit_variant(self, i);
    }

    fn visit_field(&mut self, i: &'a syn::Field) {
        let attrs: &Vec<Attribute> = &i.attrs;
        let span: Span = i.span();
        check_attr_save_span(self, attrs, span);
        syn::visit::visit_field(self, i);
    }

    fn visit_impl_item_fn(&mut self, i: &'a syn::ImplItemFn) {
        let attrs: &Vec<Attribute> = &i.attrs;
        let span: Span = i.span();
        check_attr_save_span(self, attrs, span);
        syn::visit::visit_impl_item_fn(self, i);
    }

    // All visitors that collect Span
    fn visit_item(&mut self, i: &'a Item) {
        let attrs: &Vec<Attribute>;
        let span: Span;
        match i {
            Item::Fn(func) => {
                attrs = &func.attrs;
                span = func.span();
            }
            Item::Struct(struc) => {
                attrs = &struc.attrs;
                span = struc.span();
            }
            Item::Enum(enm) => {
                attrs = &enm.attrs;
                span = enm.span();
            }
            Item::Const(konst) => {
                attrs = &konst.attrs;
                span = konst.span();
            }
            Item::Static(stat) => {
                attrs = &stat.attrs;
                span = stat.span();
            }
            Item::Type(ty) => {
                attrs = &ty.attrs;
                span = ty.span();
            }
            Item::Union(un) => {
                attrs = &un.attrs;
                span = un.span();
            }
            Item::Trait(trt) => {
                attrs = &trt.attrs;
                span = trt.span();
            }
            Item::Impl(imp) => {
                attrs = &imp.attrs;
                span = imp.span();
            }
            Item::Mod(m) => {
                attrs = &m.attrs;
                span = m.span();
                if attrs.iter().any(|a| a.path().is_ident("cfg")) {
                    debug!("Found cfg attribute for module: {}", m.ident);
                    // Get the first `cfg` attribute
                    let attr = attrs.iter().find(|a| a.path().is_ident("cfg")).unwrap();
                    self.mods.push((m.ident.to_string(), attr.clone()));
                }
            }
            Item::Use(u) => {
                attrs = &u.attrs;
                span = u.span();
            }
            _ => {
                syn::visit::visit_item(self, i);
                return;
            }
        }

        if let Some(cfg_attr) = attrs.iter().find(|a| a.path().is_ident("cfg")) {
            let attr_span =
                driver::proc_macro_span_to_readable(&span, Some(self.current_file.clone()));
            if self.hir_spans.iter().any(|span| attr_span.contains(span)) {
                match cfg_attr.meta.clone() {
                    Meta::List(meta_list) => {
                        let tokens = meta_list.tokens.clone();
                        let negated: Attribute = syn::parse_quote!(
                        #[cfg(not(#tokens))]
                        );
                        debug!(
                            "Adding negated cfg/cfg_attr span: {:?} for file: {}",
                            attr_span, self.current_file
                        );
                        self.compile_error_attrs.push(negated);
                    }
                    _ => {
                        debug!("Not meta list type for cfg/cfg_attr: {:?}", cfg_attr.meta);
                    }
                }
                self.attributes.retain(|a| a != cfg_attr);
            }
        }

        check_attr_save_span(self, attrs, span);
        syn::visit::visit_item(self, i);
    }
    // TODO: Add visit_field and visit_variant if required
}

fn check_attr_save_span(attributes: &mut Attributes, attr: &[Attribute], span: Span) {
    if attr.iter().any(|a| a.path().is_ident("cfg")) {
        attributes.spans.push((span, None));
    }
}

// ---------------------------------------------------------------------------
// Output tree types
// ---------------------------------------------------------------------------

/// A resolved source file for a module, with the condition under which
/// that path applies (from cfg_attr path = "...")
#[derive(Debug, Clone)]
pub struct ModSource<'a> {
    pub path: PathBuf,
    /// None = unconditional
    pub condition: Option<Bool<'a>>,
}

/// A single cfg-gated item (fn, struct, impl, etc.) found directly in a module.
/// We store only the item's OWN condition — not ANDed with ancestors.
/// The solver applies the module's entry_condition separately at solve time.
#[derive(Debug, Clone)]
pub struct LocalItem<'a> {
    pub own_condition: Option<Bool<'a>>,
    pub span: ReadableSpan,
    /// Identifier name of the item, if applicable. Used by
    /// `collect_named_items_with_conditions` to build feature→items maps.
    pub name: Option<String>,
    /// Gated by a `#[cfg(...)]` naming no feature (see
    /// `FileVisitor::is_externally_gated`). Kept separate from `own_condition`
    /// so it can never reach the solver — there is no feature variable to
    /// constrain, only the knowledge that a guard exists.
    pub externally_gated: bool,
}

impl LocalItem<'_> {
    pub fn span_matches(&self, target: &ReadableSpan) -> bool {
        // target is "inside" self if it starts at or after self's start
        // and ends at or before self's end.
        let after_start =
            (target.start_line, target.start_col) >= (self.span.start_line, self.span.start_col);
        let before_end =
            (target.end_line, target.end_col) <= (self.span.end_line, self.span.end_col);
        after_start && before_end
    }
}

/// The fully built tree node for one module.
#[derive(Debug, Clone)]
pub struct ModNode<'a> {
    pub name: String,
    /// The source file for this module.
    pub source_file: PathBuf,
    /// Directory containing this module's source file
    pub source_dir: PathBuf,
    /// The AND of all ancestor entry conditions — gates this whole subtree.
    /// None means unconditionally reachable.
    pub entry_condition: Option<Bool<'a>>,
    /// cfg bools on items directly in this module (not inside child mods)
    pub local_items: Vec<LocalItem<'a>>,
    /// All child modules, both inline and file-based, fully resolved
    pub children: Vec<ModNode<'a>>,
    /// Am I an inline mod?
    pub is_inline: bool,
    /// This module (or an ancestor) is gated by a `#[cfg(...)]` naming no
    /// feature — `#[cfg(all(target_arch = "x86_64", target_os = "linux"))] mod
    /// std_items` and the like. Inherited down the subtree: everything under an
    /// externally gated module is itself externally gated.
    pub externally_gated: bool,
}

impl<'a> ModNode<'a> {
    pub fn find_node_for_file_mut(&mut self, file: &Path) -> Option<&mut ModNode<'a>> {
        if self.source_file == file {
            return Some(self);
        }
        for child in &mut self.children {
            if let Some(found) = child.find_node_for_file_mut(file) {
                return Some(found);
            }
        }
        None
    }
}

/// An `include!(concat!(env!("OUT_DIR"), "<tail>"))` encountered during the syn
/// pass. syn can't resolve `OUT_DIR`, so we defer it: the driver resolves the
/// real path from a plugin run's `OUT_DIR`, parses the generated file, and
/// attaches it to the tree gated by `condition` (the effective cfg at the
/// include site).
#[derive(Clone)]
pub struct PendingInclude<'a> {
    /// Literal suffix appended to `OUT_DIR`, e.g. `/protos/types.rs`.
    pub tail: String,
    /// Effective cfg condition at the include site (full ancestor chain).
    pub condition: Option<Bool<'a>>,
}

// ---------------------------------------------------------------------------
// Internal visitor — one instance per file
// ---------------------------------------------------------------------------

struct FileVisitor<'a> {
    ctx: &'a z3::Context,
    current_file: PathBuf,
    source_dir: PathBuf,
    /// Stack of effective conditions as we descend into inline mods.
    /// Bottom of stack = inherited condition from parent file.
    /// Top of stack = condition at current nesting level.
    condition_stack: Vec<Option<Bool<'a>>>,
    /// Mirrors condition_stack: whether the enclosing module at each level is
    /// gated by a cfg naming no feature. Kept as a separate stack rather than
    /// folded into condition_stack because it must never become a Z3 term.
    externally_gated_stack: Vec<bool>,
    /// Mirrors condition_stack — each frame collects the ModNodes for
    /// children at that nesting level.
    children_stack: Vec<Vec<ModNode<'a>>>,
    /// Items collected at each nesting level.
    items_stack: Vec<Vec<LocalItem<'a>>>,
    hard_constraints: Vec<Bool<'a>>,
    /// Deferred `include!(concat!(env!("OUT_DIR"), …))` sites found in this file.
    pending_includes: Vec<PendingInclude<'a>>,
    /// The condition under which this crate becomes no_std, extracted from
    /// `#![cfg_attr(<cond>, no_std)]`. Stored globally rather than as a
    /// regular covering item so the driver can run each covering set twice
    /// (once with the condition, once with its negation).
    pub no_std_condition: Option<Bool<'a>>,
    /// If the current module has a path override (from #[path]),
    /// this is the directory to search for its children.
    current_search_dir: PathBuf,
    /// Whether the current file is mod-rs style. This is either the root
    /// files or files with name mod.rs. We need to track this
    /// because path overrides above uses different rules based on whether
    /// this is a mod-rs file or not.
    is_mod_rs: bool,
}

impl<'a> FileVisitor<'a> {
    fn new(
        ctx: &'a z3::Context,
        path: &Path,
        source_dir: PathBuf,
        current_search_dir: PathBuf,
        inherited: Option<Bool<'a>>,
        is_mod_rs: bool,
        inherited_externally_gated: bool,
    ) -> Self {
        Self {
            ctx,
            current_file: path.to_path_buf(),
            source_dir,
            current_search_dir,
            is_mod_rs,
            condition_stack: vec![inherited],
            externally_gated_stack: vec![inherited_externally_gated],
            children_stack: vec![vec![]],
            items_stack: vec![vec![]],
            hard_constraints: vec![],
            pending_includes: vec![],
            no_std_condition: None,
        }
    }

    fn should_skip(&self, attrs: &[syn::Attribute]) -> bool {
        should_skip(attrs)
    }

    fn get_span(&self, item: &Span) -> ReadableSpan {
        driver::proc_macro_span_to_readable(item, Some(self.current_file.display().to_string()))
    }

    fn current_condition(&self) -> Option<Bool<'a>> {
        self.condition_stack.last().cloned().flatten()
    }

    fn current_externally_gated(&self) -> bool {
        self.externally_gated_stack.last().copied().unwrap_or(false)
    }

    fn and_conditions(
        ctx: &'a z3::Context,
        a: Option<Bool<'a>>,
        b: Option<Bool<'a>>,
    ) -> Option<Bool<'a>> {
        match (a, b) {
            (Some(a), Some(b)) => Some(Bool::and(ctx, &[&a, &b])),
            (Some(a), None) => Some(a),
            (None, Some(b)) => Some(b),
            (None, None) => None,
        }
    }

    /// Parse the own cfg gate from a set of attributes, ignoring cfg_attr.
    /// TODO: Does this parse multiple if there are multiple cfg attrs? We could support that if needed.
    fn parse_cfg_gate(&self, attrs: &[syn::Attribute]) -> Option<Bool<'a>> {
        attrs
            .iter()
            .find(|a| a.path().is_ident("cfg"))
            .and_then(|attr| {
                let (b, _) = parser::parse_main_attributes_direct(attr, self.ctx);
                b
            })
    }

    /// Is this item gated by a `#[cfg(...)]` that names no feature?
    ///
    /// `parse_token_stream` only turns `feature = "..."` into a Z3 Bool; every
    /// other predicate atom (`target_arch`, `target_os`, `test`, a build-script
    /// `--cfg` like `has_std`) is recorded as a bare string in
    /// `ParsedAttr::constants` and its literal value dropped. So such a cfg
    /// yields `None` — indistinguishable from no `#[cfg]` at all, which is how
    /// target-gated std usage came to look unguarded.
    ///
    /// A `None` equation together with a non-empty `constants` is exactly that
    /// case: a real predicate was present and produced no feature. Logic
    /// keywords (`all`/`any`/`not`) go to `ParsedAttr::logic`, never to
    /// `constants`, so a non-empty `constants` always means a genuine atom.
    ///
    /// Mixed predicates need no handling here: `all(target_os = "linux",
    /// feature = "std")` already parses to just `std`, which is the correct
    /// existential projection — the non-feature atom is free, and disabling the
    /// feature guards the item on every target.
    ///
    /// Polarity does not matter either. `not(all(target_arch = ..., target_os =
    /// ...))` erases exactly like its positive form, so an item in a `cfg_if`
    /// else-arm is caught the same way as one in the if-arm.
    fn is_externally_gated(&self, attrs: &[syn::Attribute]) -> bool {
        attrs
            .iter()
            .filter(|a| a.path().is_ident("cfg"))
            .any(|attr| {
                let (equation, parsed) = parser::parse_main_attributes_direct(attr, self.ctx);
                equation.is_none() && !parsed.constants.is_empty()
            })
    }

    /// Parse all cfg_attr path overrides from a set of attributes.
    fn parse_cfg_attr_sources(
        &self,
        attrs: &[syn::Attribute],
        source_dir: &Path,
    ) -> Vec<ModSource<'a>> {
        attrs
            .iter()
            .filter(|a| a.path().is_ident("cfg_attr"))
            .filter_map(|attr| {
                let (b, parsed) = parser::parse_main_attributes_direct(attr, self.ctx);
                parsed.filepath.map(|fp| ModSource {
                    path: source_dir.join(&fp),
                    condition: b,
                })
            })
            .collect()
    }

    fn does_cfg_attr_override_path(&self, attrs: &[syn::Attribute]) -> bool {
        attrs.iter().any(|a| {
            a.path().is_ident("cfg_attr")
                && parser::parse_main_attributes_direct(a, self.ctx)
                    .1
                    .filepath
                    .is_some()
        })
    }

    /// Standard mod file resolution: foo.rs or foo/mod.rs
    pub fn default_mod_source(source_dir: &Path, name: &str) -> ModSource<'static> {
        let rs = source_dir.join(format!("{}.rs", name));
        let mod_rs = source_dir.join(name).join("mod.rs");
        let path = if rs.exists() { rs } else { mod_rs };
        ModSource {
            path,
            condition: None,
        }
    }

    /// Push a new nesting level (entering an inline mod).
    fn push_level(&mut self, condition: Option<Bool<'a>>, externally_gated: bool) {
        self.condition_stack.push(condition);
        self.externally_gated_stack.push(externally_gated);
        self.children_stack.push(vec![]);
        self.items_stack.push(vec![]);
    }

    /// Pop the current nesting level and return (items, children).
    fn pop_level(&mut self) -> (Vec<LocalItem<'a>>, Vec<ModNode<'a>>) {
        self.condition_stack.pop();
        self.externally_gated_stack.pop();
        let children = self.children_stack.pop().unwrap();
        let items = self.items_stack.pop().unwrap();
        (items, children)
    }

    /// Add a child node at the current nesting level.
    fn push_child(&mut self, node: ModNode<'a>) {
        self.children_stack.last_mut().unwrap().push(node);
    }

    /// Add an item at the current nesting level.
    fn push_item(&mut self, item: LocalItem<'a>) {
        self.items_stack.last_mut().unwrap().push(item);
    }

    /// Record a cfg-gated item as a `LocalItem` at the current nesting level.
    ///
    /// This is the shared body behind nearly every `visit_*` override: it skips
    /// test-gated items (returning `false` so the caller can avoid recursing) and
    /// otherwise pushes a `LocalItem` carrying the item's own `#[cfg(...)]`
    /// condition (or `None`), its span and optional name. Recording the gate for
    /// *every* syn position that can hold a std path is what lets
    /// `ancestors_for_span` recognise gated std usage — a missing position causes
    /// the usage to look ungated and be misclassified as hard std usage.
    fn record_item(
        &mut self,
        kind: &str,
        name: Option<String>,
        attrs: &[syn::Attribute],
        span: Span,
    ) -> bool {
        if self.should_skip(attrs) {
            debug!(
                "Skipping {} {} due to test attribute",
                kind,
                name.as_deref().unwrap_or("")
            );
            return false;
        }
        let own_condition = self.parse_cfg_gate(attrs);
        let externally_gated = self.is_externally_gated(attrs);
        let span = self.get_span(&span);
        self.push_item(LocalItem {
            own_condition,
            span,
            name,
            externally_gated,
        });
        true
    }

    /// Parse a macro invocation's token stream for `#[cfg(...)]`-gated regions.
    ///
    /// syn cannot see inside a macro invocation, so a `#[cfg(feature = "std")]`
    /// passed as a macro *argument* (e.g. valuable's `value!` / `collection!`
    /// list entries, where the macro captures `$(#[$attrs:meta])*`) leaves the
    /// std paths it gates looking unconditional — the whole invocation is
    /// recorded as a single ungated item. We split the stream into top-level
    /// comma-separated segments; any segment whose leading attributes include a
    /// `cfg` gate is recorded as a `LocalItem` spanning that segment, so
    /// `ancestors_for_span` recognises the gate by span containment. We recurse
    /// into nested delimited groups so gates nested inside argument groups are
    /// also captured.
    fn record_macro_cfg_segments(&mut self, tokens: &TokenStream) {
        let trees: Vec<TokenTree> = tokens.clone().into_iter().collect();
        // Entries in list-style macros are comma-separated, but commas also
        // appear inside generic argument lists (`HashSet<T, H>`). Angle brackets
        // are bare `<`/`>` puncts (not token groups), so track their depth and
        // only split on commas at depth 0. `>>` lexes as two joint `>` puncts,
        // so decrementing per `>` keeps nested generics balanced.
        let mut segments: Vec<&[TokenTree]> = Vec::new();
        let mut seg_start = 0usize;
        let mut angle: i32 = 0;
        for (i, tt) in trees.iter().enumerate() {
            if let TokenTree::Punct(p) = tt {
                match p.as_char() {
                    '<' => angle += 1,
                    '>' => angle = (angle - 1).max(0),
                    ',' if angle == 0 => {
                        segments.push(&trees[seg_start..i]);
                        seg_start = i + 1;
                    }
                    _ => {}
                }
            }
        }
        if seg_start < trees.len() {
            segments.push(&trees[seg_start..]);
        }

        // A bare positional `cfg(...)` sibling arg (not the `#[cfg(...)]` attr
        // form) gates the invocation's generated output — e.g. among crate's
        // `impl_specific_ref_and_mut!(::std::path::Path, cfg(feature="std"), ..)`,
        // where the macro re-emits it as `#[$attr]` on the impls that use the type
        // arg. We can't tell from the callsite *which* generated span it gates
        // (that lives in the macro definition), so we PROPOSE it as a candidate
        // over the whole invocation span; the probe validates by negating it and
        // checking the std usage actually vanishes (a wrong guess just stays
        // StillStd — never a false clear). Multiple positional cfgs are ANDed.
        let mut positional: Option<Bool<'a>> = None;
        for seg in &segments {
            if let Some(c) = self.positional_cfg_condition(seg) {
                positional = Self::and_conditions(self.ctx, positional, Some(c));
            }
        }
        if let Some(cond) = positional {
            if !trees.is_empty() {
                let span = self.token_slice_span(&trees);
                self.push_item(LocalItem {
                    own_condition: Some(cond),
                    span,
                    name: None,
                    externally_gated: false,
                });
            }
        }

        for seg in &segments {
            self.process_macro_segment(seg);
        }
    }

    /// Recognise a bare positional `cfg(...)` macro argument (as opposed to the
    /// `#[cfg(...)]` attribute form handled by `leading_cfg_condition`) and return
    /// its Z3 condition. See `record_macro_cfg_segments` for how it is used: this
    /// is a probe-validated *candidate* gate, not a precise determination.
    fn positional_cfg_condition(&self, seg: &[TokenTree]) -> Option<Bool<'a>> {
        match seg {
            [TokenTree::Ident(id), TokenTree::Group(g)]
                if id == "cfg" && g.delimiter() == Delimiter::Parenthesis =>
            {
                // Synthesise `#[cfg(...)]` from the bare `cfg(...)` tokens and
                // reuse the attribute cfg parser.
                let mut bracket_inner = TokenStream::new();
                bracket_inner.extend([
                    TokenTree::Ident(id.clone()),
                    TokenTree::Group(g.clone()),
                ]);
                let bracket = Group::new(Delimiter::Bracket, bracket_inner);
                let mut ts = TokenStream::new();
                ts.extend([
                    TokenTree::Punct(Punct::new('#', Spacing::Alone)),
                    TokenTree::Group(bracket),
                ]);
                parse_attr_stream_cfg(&ts, self.ctx)
            }
            _ => None,
        }
    }

    fn process_macro_segment(&mut self, seg: &[TokenTree]) {
        if seg.is_empty() {
            return;
        }
        if let Some(cond) = self.leading_cfg_condition(seg) {
            let span = self.token_slice_span(seg);
            self.push_item(LocalItem {
                own_condition: Some(cond),
                span,
                name: None,
                externally_gated: false,
            });
        }
        for tt in seg {
            if let TokenTree::Group(g) = tt {
                self.record_macro_cfg_segments(&g.stream());
            }
        }
    }

    /// Collect the `cfg` gates from a segment's leading attributes (`#[...]`),
    /// skipping non-cfg attributes like doc comments, and AND them together.
    fn leading_cfg_condition(&self, seg: &[TokenTree]) -> Option<Bool<'a>> {
        let mut result: Option<Bool<'a>> = None;
        let mut idx = 0;
        while let (Some(TokenTree::Punct(hash)), Some(TokenTree::Group(group))) =
            (seg.get(idx), seg.get(idx + 1))
        {
            if hash.as_char() != '#' || group.delimiter() != Delimiter::Bracket {
                break;
            }
            let mut ts = TokenStream::new();
            ts.extend([
                TokenTree::Punct(hash.clone()),
                TokenTree::Group(group.clone()),
            ]);
            if let Some(cfg_bool) = parse_attr_stream_cfg(&ts, self.ctx) {
                result = Self::and_conditions(self.ctx, result, Some(cfg_bool));
            }
            idx += 2;
        }
        result
    }

    /// Parse a `cfg_if::cfg_if! { if #[cfg(P1)] {..} else if #[cfg(P2)] {..} else {..} }`
    /// invocation and gate each arm body.
    ///
    /// `record_macro_cfg_segments` only recognises a `cfg` as a *leading*
    /// attribute on a comma-separated segment; in `cfg_if!` the `#[cfg(..)]` sits
    /// after `if` and gates a following brace group, so no segment gets a gate and
    /// the whole body looks unconditional. Here we walk the arm grammar and push a
    /// `LocalItem` spanning each arm's `{ .. }` block. `cfg_if` is first-match-wins,
    /// so arm *i* is gated by `Pi ∧ ¬P1 ∧ … ∧ ¬P(i-1)` and the trailing `else` by
    /// the conjunction of all negated predicates. Nested `cfg_if!` inside an arm is
    /// handled by recursing into each block.
    fn record_cfg_if(&mut self, tokens: &TokenStream) {
        let trees: Vec<TokenTree> = tokens.clone().into_iter().collect();
        let mut idx = 0usize;
        // AND of the negations of all predicates seen so far (drives later arms
        // and the final `else`).
        let mut prior_negations: Option<Bool<'a>> = None;
        // Once any earlier arm's predicate names no feature, the `else` arm's
        // condition is that predicate's negation — equally off the feature axis.
        // This is what makes the rule polarity-agnostic for cfg_if.
        let mut prior_externally_gated = false;
        while idx < trees.len() {
            match &trees[idx] {
                // `if` / `else if` <#[cfg(P)]> { block }
                TokenTree::Ident(kw) if kw == "if" => {
                    // Expect `#` `[ .. ]` forming the cfg attribute, then a brace group.
                    let pred = match (trees.get(idx + 1), trees.get(idx + 2)) {
                        (Some(TokenTree::Punct(hash)), Some(TokenTree::Group(attr)))
                            if hash.as_char() == '#'
                                && attr.delimiter() == Delimiter::Bracket =>
                        {
                            let mut ts = TokenStream::new();
                            ts.extend([
                                TokenTree::Punct(hash.clone()),
                                TokenTree::Group(attr.clone()),
                            ]);
                            parse_attr_stream_cfg_ext(&ts, self.ctx)
                        }
                        _ => (None, false),
                    };
                    let (pred, pred_externally_gated) = pred;
                    // The arm body is the next brace group after the attribute.
                    let body_pos = idx + 3;
                    if let Some(TokenTree::Group(body)) = trees.get(body_pos) {
                        if body.delimiter() == Delimiter::Brace {
                            // Gate the arm by its OWN predicate only. The arm is a subset of `pred`
                            // (active ⇒ pred), so `pred` is the tightest gate the probe can negate to
                            // remove it. Do NOT fold in the accumulated `¬earlier-arm` negations: an
                            // earlier arm like `all(target_arch="wasm32", feature="web")` parses to just
                            // `web` (target predicates are dropped, features only), so `¬earlier` becomes
                            // the lossy `¬web` instead of the true `¬(wasm32 ∧ web)`. Folding that in makes
                            // the gate `std ∧ ¬web`, whose negation `¬std ∨ web` lets the probe satisfy the
                            // clear by toggling `web` while leaving std ON — which doesn't deactivate this
                            // arm on the host, so the std usage never vanishes on rebuild.
                            self.record_cfg_if_arm(body, pred.clone(), pred_externally_gated);
                            // Accumulate ¬P for subsequent arms / else.
                            prior_externally_gated |= pred_externally_gated;
                            if let Some(p) = pred {
                                let neg = p.not();
                                prior_negations = Self::and_conditions(
                                    self.ctx,
                                    prior_negations,
                                    Some(neg),
                                );
                            }
                            idx = body_pos + 1;
                            continue;
                        }
                    }
                    idx += 1;
                }
                // trailing `else { block }`
                TokenTree::Ident(kw) if kw == "else" => {
                    if let Some(TokenTree::Group(body)) = trees.get(idx + 1) {
                        if body.delimiter() == Delimiter::Brace {
                            let arm_cond = prior_negations.clone();
                            self.record_cfg_if_arm(body, arm_cond, prior_externally_gated);
                            idx += 2;
                            continue;
                        }
                    }
                    idx += 1;
                }
                _ => idx += 1,
            }
        }
    }

    /// Record one `cfg_if!` arm: push a `LocalItem` spanning its `{ .. }` body with
    /// the arm's effective condition, then recurse for nested cfg-macros inside.
    fn record_cfg_if_arm(
        &mut self,
        body: &proc_macro2::Group,
        cond: Option<Bool<'a>>,
        externally_gated: bool,
    ) {
        let start = body.span_open().start();
        let end = body.span_close().end();
        let span = ReadableSpan {
            file: self.current_file.display().to_string(),
            start_line: start.line,
            start_col: start.column,
            end_line: end.line,
            end_col: end.column,
            usage_crate: None,
        };
        self.push_item(LocalItem {
            own_condition: cond,
            span,
            name: None,
            externally_gated,
        });
        // Nested `cfg_if!` inside this arm, plus any macro-argument cfg segments.
        self.scan_nested_cfg_if(&body.stream());
    }

    /// Walk a token stream looking for nested `cfg_if! { .. }` invocations and
    /// gate their arms too. Also falls back to `record_macro_cfg_segments` so
    /// macro-argument `#[cfg]`s inside an arm are still captured.
    fn scan_nested_cfg_if(&mut self, tokens: &TokenStream) {
        let trees: Vec<TokenTree> = tokens.clone().into_iter().collect();
        let mut i = 0usize;
        while i < trees.len() {
            // `cfg_if` `!` `{ .. }`  — the last path segment before `!` is enough.
            if let TokenTree::Ident(id) = &trees[i] {
                if id == "cfg_if" {
                    if let (Some(TokenTree::Punct(bang)), Some(TokenTree::Group(g))) =
                        (trees.get(i + 1), trees.get(i + 2))
                    {
                        if bang.as_char() == '!' {
                            self.record_cfg_if(&g.stream());
                            i += 3;
                            continue;
                        }
                    }
                }
            }
            i += 1;
        }
    }

    /// Register `mod X;` declarations emitted from inside a macro invocation.
    ///
    /// A `macro_rules!` like agnostic_lite's `cfg_time! { mod after; ... }`
    /// takes the `mod after;` as a passthrough `$item` argument and re-emits it
    /// wrapped in `#[cfg(..)]`. syn hands us the invocation tokens opaquely, so
    /// those modules are never registered and their files never walked — leaving
    /// every std usage inside them ungated. We scan the tokens for the file-based
    /// `mod IDENT ;` shape and push a placeholder child `ModNode` exactly like the
    /// file-based arm of `visit_item_mod`; `resolve_child` then walks the file and
    /// folds in any inner `#![cfg]`.
    ///
    /// This is the SYN counterpart to the plugin's `macro_module_imports`: the
    /// plugin catches modules *generated* inside a macro body (their spans are
    /// `from_expansion()`), but a passthrough `$item` mod is user-written, so its
    /// span is NOT `from_expansion()` and the plugin skips it — only the literal
    /// tokens seen here can catch that shape.
    ///
    /// The module is gated by the current inherited condition only — we do not
    /// try to recover the `#[cfg]` the macro's *definition* wraps items in. That
    /// is sound for a std *detector* (inherit-only can only under-gate, never
    /// wrongly clear a real usage) and sufficient in practice: the std usage is
    /// gated either by the parent `mod`'s feature (submodules) or by inner
    /// `#[cfg]`s once the file is walked. Inline `mod X { .. }` is intentionally
    /// not matched (`;` only) so a brace body never trips the scan.
    /// NOTE: The above ignored case should be looked into if we find a crate that
    /// actually exercises it.
    fn scan_macro_mod_decls(&mut self, tokens: &TokenStream) {
        let trees: Vec<TokenTree> = tokens.clone().into_iter().collect();
        let mut i = 0usize;
        while i < trees.len() {
            if let TokenTree::Ident(kw) = &trees[i] {
                if kw == "mod" {
                    if let (Some(TokenTree::Ident(name)), Some(TokenTree::Punct(semi))) =
                        (trees.get(i + 1), trees.get(i + 2))
                    {
                        if semi.as_char() == ';' {
                            let name = name.to_string();
                            let src =
                                Self::default_mod_source(&self.current_search_dir, &name);
                            let externally_gated = self.current_externally_gated();
                            self.push_child(ModNode {
                                name,
                                source_file: src.path.clone(),
                                source_dir: src
                                    .path
                                    .parent()
                                    .unwrap_or(Path::new("."))
                                    .to_path_buf(),
                                entry_condition: self.current_condition(),
                                local_items: vec![],
                                children: vec![],
                                is_inline: false,
                                externally_gated,
                            });
                            i += 3;
                            continue;
                        }
                    }
                }
            }
            // Recurse into nested delimited groups so `mod`s wrapped in inner
            // groups are still found.
            if let TokenTree::Group(g) = &trees[i] {
                self.scan_macro_mod_decls(&g.stream());
            }
            i += 1;
        }
    }

    /// Build a `ReadableSpan` covering a slice of tokens (first token start to
    /// last token end).
    fn token_slice_span(&self, seg: &[TokenTree]) -> ReadableSpan {
        let start = seg.first().unwrap().span().start();
        let end = seg.last().unwrap().span().end();
        ReadableSpan {
            file: self.current_file.display().to_string(),
            start_line: start.line,
            start_col: start.column,
            end_line: end.line,
            end_col: end.column,
            usage_crate: None,
        }
    }

    /// Handle `include!("path")` by parsing the included file and visiting its
    /// contents inline (same condition/items/children stacks, no new ModNode).
    fn visit_include_macro(&mut self, tokens: &proc_macro2::TokenStream, own: Option<Bool<'a>>) {
        // Gate for everything the include contributes: the condition in effect at
        // the include site, ANDed with any `#[cfg(...)]` on the `include!` item
        // itself (e.g. bigdecimal's `#[cfg(feature = "std")] include!("./with_std.rs");`).
        let effective = Self::and_conditions(self.ctx, self.current_condition(), own);

        let lit = match syn::parse2::<syn::LitStr>(tokens.clone()) {
            Ok(l) => l,
            Err(_) => {
                // Not a string literal. Recognise the build-script codegen idiom
                // `include!(concat!(env!("OUT_DIR"), "<tail>"))` and defer it: the
                // driver resolves OUT_DIR and parses the generated file, gated by
                // the effective condition here (e.g. a `#[cfg(...)]` mod above it).
                if let Some(tail) = parse_out_dir_include(tokens) {
                    debug!("Deferring OUT_DIR include! with tail {}", tail);
                    self.pending_includes.push(PendingInclude {
                        tail,
                        condition: effective,
                    });
                } else {
                    debug!("include! with non-literal path, skipping");
                }
                return;
            }
        };
        let included_path = self
            .current_file
            .parent()
            .unwrap_or(Path::new("."))
            .join(lit.value());
        if !included_path.exists() {
            debug!(
                "include! path not found (possibly OUT_DIR): {}",
                included_path.display()
            );
            return;
        }

        // Register the included file as a gated child node instead of visiting it
        // inline into the current node. Visiting inline left its items reachable
        // only in theory: `find_ancestors_for_span` scans a node's `local_items`
        // only when that node's `source_file` matches the target span's file, so
        // items pulled in from another file could never be found — gated or not.
        // As a child node the file is matched directly and `effective` (including
        // a `#[cfg]` on the include! itself) applies to everything in it.
        // `resolve_child` walks it and folds in any file-inner `#![cfg(...)]`.
        // This mirrors how OUT_DIR includes are already attached as gated nodes.
        //
        // NOTE: `include!` is textual, so the items really belong to the including
        // module; modelling it as a child is an approximation already shared with
        // the OUT_DIR path. One consequence: a `mod foo;` inside an included file
        // resolves relative to the included file here, whereas rustc resolves it
        // relative to the includer.
        //
        // `included_path` is kept exactly as built (e.g. `src/./with_std.rs`) —
        // rustc reports the span file with the same `./`, and the ancestor lookup
        // is a suffix match on the node's source_file.
        let name = included_path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("included")
            .to_string();
        let source_dir = included_path
            .parent()
            .unwrap_or(Path::new("."))
            .to_path_buf();
        debug!(
            "include! {} registered as gated child node (gated={})",
            included_path.display(),
            effective.is_some()
        );
        let externally_gated = self.current_externally_gated();
        self.push_child(ModNode {
            name,
            source_file: included_path,
            source_dir,
            entry_condition: effective,
            local_items: vec![],
            children: vec![],
            is_inline: false,
            externally_gated,
        });
    }

    /// Consume the visitor and return the top-level (items, children).
    fn finish(
        mut self,
    ) -> (
        Vec<LocalItem<'a>>,
        Vec<ModNode<'a>>,
        Vec<Bool<'a>>,
        Option<Bool<'a>>,
        Vec<PendingInclude<'a>>,
    ) {
        assert_eq!(self.condition_stack.len(), 1);
        (
            self.items_stack.pop().unwrap(),
            self.children_stack.pop().unwrap(),
            self.hard_constraints,
            self.no_std_condition,
            self.pending_includes,
        )
    }
}

/// Recognise `concat!(env!("OUT_DIR"), "<lit>", …)` (the include! argument for
/// build-script codegen) and return the concatenated literal tail, e.g.
/// `/protos/types.rs`. Returns `None` for any other token shape.
fn parse_out_dir_include(tokens: &TokenStream) -> Option<String> {
    let trees: Vec<TokenTree> = tokens.clone().into_iter().collect();
    let group = match trees.as_slice() {
        [
            TokenTree::Ident(id),
            TokenTree::Punct(bang),
            TokenTree::Group(g),
        ] if id.to_string() == "concat"
            && bang.as_char() == '!'
            && g.delimiter() == Delimiter::Parenthesis =>
        {
            g
        }
        _ => return None,
    };
    let inner: Vec<TokenTree> = group.stream().into_iter().collect();
    // First argument must be `env!("OUT_DIR")`.
    let env_ok = matches!(inner.first(), Some(TokenTree::Ident(id)) if id.to_string() == "env")
        && matches!(inner.get(1), Some(TokenTree::Punct(p)) if p.as_char() == '!')
        && matches!(inner.get(2), Some(TokenTree::Group(g)) if {
            let e: Vec<TokenTree> = g.stream().into_iter().collect();
            e.len() == 1 && matches!(&e[0], TokenTree::Literal(l) if litstr_value(l).as_deref() == Some("OUT_DIR"))
        });
    if !env_ok {
        return None;
    }
    // Remaining args: `, "lit"` repeated — concatenate their values.
    let mut tail = String::new();
    let mut idx = 3;
    while idx < inner.len() {
        match &inner[idx] {
            TokenTree::Punct(p) if p.as_char() == ',' => idx += 1,
            TokenTree::Literal(l) => {
                tail.push_str(&litstr_value(l)?);
                idx += 1;
            }
            _ => return None,
        }
    }
    Some(tail)
}

/// Parse a string-literal token to its unescaped value. Returns None if the
/// token is not a string literal.
fn litstr_value(lit: &proc_macro2::Literal) -> Option<String> {
    syn::parse_str::<syn::LitStr>(&lit.to_string())
        .ok()
        .map(|l| l.value())
}

/// Walk a use tree and collect the leaf name(s) that are imported.
/// `use foo::Bar` → ["Bar"]
/// `use foo::{Bar, Baz}` → ["Bar", "Baz"]
/// `use foo::Bar as B` → ["B"]
/// `use foo::*` → ["*"]
fn extract_use_leaf_names(tree: &syn::UseTree) -> Vec<String> {
    match tree {
        syn::UseTree::Name(n) => vec![n.ident.to_string()],
        syn::UseTree::Rename(r) => vec![r.rename.to_string()],
        syn::UseTree::Glob(_) => vec!["*".to_string()],
        syn::UseTree::Group(g) => g.items.iter().flat_map(extract_use_leaf_names).collect(),
        syn::UseTree::Path(p) => extract_use_leaf_names(&p.tree),
    }
}

impl<'a> Visit<'_> for FileVisitor<'a> {
    fn visit_item_mod(&mut self, i: &'_ syn::ItemMod) {
        if self.should_skip(&i.attrs) {
            debug!("Skipping module {} due to test attribute", i.ident);
            return;
        }

        let path_attr = i.attrs.iter().find(|a| a.path().is_ident("path"));
        let path = path_attr.and_then(|attr| match &attr.meta {
            Meta::NameValue(nv) => {
                if let syn::Expr::Lit(syn::ExprLit {
                    lit: syn::Lit::Str(lit_str),
                    ..
                }) = &nv.value
                {
                    Some(PathBuf::from(lit_str.value()))
                } else {
                    debug!(
                        "Unexpected non-string literal in path attribute: {:?}",
                        nv.value
                    );
                    None
                }
            }
            _ => {
                debug!("Unexpected meta type for path attribute: {:?}", attr.meta);
                None
            }
        });

        let name = i.ident.to_string();
        let own_gate = self.parse_cfg_gate(&i.attrs);
        // Inherit: a module inside an externally gated one is externally gated
        // too, however its own `#[cfg]` reads.
        let externally_gated =
            self.is_externally_gated(&i.attrs) || self.current_externally_gated();

        // Effective entry condition = inherited (from stack) AND own gate
        let effective = Self::and_conditions(self.ctx, self.current_condition(), own_gate);

        match &i.content {
            Some(_) => {
                // Inline mod — push a level, recurse, pop and assemble
                self.push_level(effective.clone(), externally_gated);

                let old_search_dir = self.current_search_dir.clone();
                if let Some(path) = path {
                    // Special case if the source is a mod-rs style file
                    if !self.is_mod_rs {
                        // Let this be hard error if the path is malformed
                        self.current_search_dir = old_search_dir.parent().unwrap().to_path_buf();
                    }
                    self.current_search_dir = self.current_search_dir.join(path);
                    debug!(
                        "Old search dir: {}, new search dir from path attribute: {}",
                        old_search_dir.display(),
                        self.current_search_dir.display()
                    );
                } else {
                    self.current_search_dir = self.current_search_dir.join(&name);
                }

                syn::visit::visit_item_mod(self, i);

                self.current_search_dir = old_search_dir;

                let (items, children) = self.pop_level();

                let node = ModNode {
                    name,
                    source_file: self.current_file.clone(),
                    source_dir: self.source_dir.clone(),
                    entry_condition: effective,
                    local_items: items,
                    children,
                    is_inline: true,
                    externally_gated,
                };
                self.push_child(node);
            }
            None => {
                // File-based mod — resolve sources, build a placeholder child.
                // Actual file contents will be filled in by ModCollector recursion.
                let cfg_attr_sources = self.parse_cfg_attr_sources(&i.attrs, &self.source_dir);

                let sources: Vec<ModSource<'a>> = if !cfg_attr_sources.is_empty() {
                    cfg_attr_sources
                } else if let Some(path) = path {
                    vec![ModSource {
                        path: self.current_search_dir.join(path),
                        condition: None,
                    }]
                } else {
                    vec![Self::default_mod_source(&self.current_search_dir, &name)]
                };

                // One child node per conditional source path.
                // If there's only one (the common case), it's straightforward.
                // If there are multiple (cfg_attr with different paths), each
                // becomes its own branch — the solver later picks which is reachable.
                for source in sources {
                    let source_condition =
                        Self::and_conditions(self.ctx, effective.clone(), source.condition);

                    self.push_child(ModNode {
                        name: name.clone(),
                        source_file: source.path.clone(),
                        source_dir: source.path.parent().unwrap_or(Path::new(".")).to_path_buf(),
                        entry_condition: source_condition,
                        local_items: vec![],
                        children: vec![],
                        is_inline: false,
                        externally_gated,
                    });
                }
            }
        }
    }

    fn visit_attribute(&mut self, i: &'_ syn::Attribute) {
        if i.path().is_ident("cfg_attr")
            && !self.does_cfg_attr_override_path(std::slice::from_ref(i))
        {
            let (own, parsed) = parser::parse_main_attributes_direct(i, self.ctx);
            if let Some(own) = own {
                if parser::is_no_std(&parsed, true) {
                    self.no_std_condition = Some(own.clone());
                }
                // Only *outer* cfg_attrs (`#[cfg_attr(..)]` on an item) can carry a
                // proc-macro whose generated code lands inside these tokens. Inner
                // ones (`#![cfg_attr(..)]`) are crate/module-level configuration —
                // e.g. `#![cfg_attr(not(feature = "std"), no_std)]` — which generate
                // no code at all, so there is nothing to gate. Recording a gate for
                // them is not merely useless: the item is picked up by
                // `collect_all_items` and pollutes both the covering-set pool and
                // `all_constraints` with a stray `not(std)`, which perturbs the
                // solver. That regressed tarfs, where the lost constraint let
                // `builtin_devices` (whose module uses std unconditionally) be
                // enabled in a no_std build.
                if !matches!(i.style, syn::AttrStyle::Outer) {
                    syn::visit::visit_attribute(self, i);
                    return;
                }
                // A `#[cfg_attr(pred, some_attr(..))]` conditionally applies an
                // *attribute*; it does not gate the item itself, so `parse_cfg_gate`
                // correctly reports the item as unconditional. But when the applied
                // attribute is a proc-macro (a derive or attribute macro), the code
                // it generates has no source location of its own and rustc maps it
                // back to a span INSIDE these tokens — e.g. thiserror's generated
                // `impl std::error::Error` lands on the `Error` token of
                // `derive(Error)`. That generated code only exists when `pred`
                // holds, so gate the attribute's own token range by `pred`; without
                // it the std usage there looks unguarded.
                // See base58_monero (derive) and accesskit (pyo3 `pyclass` attr).
                self.push_item(LocalItem {
                    own_condition: Some(own),
                    span: self.get_span(&i.span()),
                    name: None,
                    externally_gated: false,
                });
            }
        }

        syn::visit::visit_attribute(self, i);
    }

    fn visit_variant(&mut self, i: &'_ syn::Variant) {
        if self.record_item("variant", None, &i.attrs, i.span()) {
            syn::visit::visit_variant(self, i);
        }
    }

    fn visit_field(&mut self, i: &'_ syn::Field) {
        if self.record_item("field", None, &i.attrs, i.span()) {
            syn::visit::visit_field(self, i);
        }
    }

    // A `#[cfg(...)]`-gated field in a struct *literal*, e.g.
    // `Self { section, #[cfg(feature = "std")] backtrace: Backtrace::capture() }`.
    // `visit_field` above only covers struct *definition* fields (`syn::Field`);
    // the expression-position field is a `syn::FieldValue` with its own attrs, so
    // without this the gate is never recorded and the std expression in its value
    // looks unconditional. See array_section, bevy_ecs.
    fn visit_field_value(&mut self, i: &'_ syn::FieldValue) {
        let name = match &i.member {
            syn::Member::Named(id) => Some(id.to_string()),
            syn::Member::Unnamed(_) => None,
        };
        if self.record_item("field value", name, &i.attrs, i.span()) {
            syn::visit::visit_field_value(self, i);
        }
    }

    // Items inside an `impl` block. A `#[cfg(feature = "std")]` gate on e.g.
    // `type Error = std::io::Error;` must be recorded, or the std path in its
    // value/type looks ungated. `record_item` is shared with all other positions.
    fn visit_impl_item_fn(&mut self, i: &'_ syn::ImplItemFn) {
        let name = Some(i.sig.ident.to_string());
        if self.record_item("impl function", name, &i.attrs, i.span()) {
            syn::visit::visit_impl_item_fn(self, i);
        }
    }

    fn visit_impl_item_const(&mut self, i: &'_ syn::ImplItemConst) {
        let name = Some(i.ident.to_string());
        if self.record_item("impl const", name, &i.attrs, i.span()) {
            syn::visit::visit_impl_item_const(self, i);
        }
    }

    fn visit_impl_item_type(&mut self, i: &'_ syn::ImplItemType) {
        let name = Some(i.ident.to_string());
        if self.record_item("impl type", name, &i.attrs, i.span()) {
            syn::visit::visit_impl_item_type(self, i);
        }
    }

    fn visit_impl_item_macro(&mut self, i: &'_ syn::ImplItemMacro) {
        if self.record_item("impl macro", None, &i.attrs, i.span()) {
            syn::visit::visit_impl_item_macro(self, i);
        }
    }

    // Items inside a `trait` definition. Without this, a `#[cfg(feature = "std")]`
    // gate on a provided (default-body) method is never recorded, so std usages
    // inside it (e.g. zerocopy's `FromBytes::read_from_io`) appear ungated and get
    // misclassified as hard/unguarded std usage.
    fn visit_trait_item_fn(&mut self, i: &'_ syn::TraitItemFn) {
        let name = Some(i.sig.ident.to_string());
        if self.record_item("trait function", name, &i.attrs, i.span()) {
            syn::visit::visit_trait_item_fn(self, i);
        }
    }

    fn visit_trait_item_const(&mut self, i: &'_ syn::TraitItemConst) {
        let name = Some(i.ident.to_string());
        if self.record_item("trait const", name, &i.attrs, i.span()) {
            syn::visit::visit_trait_item_const(self, i);
        }
    }

    fn visit_trait_item_type(&mut self, i: &'_ syn::TraitItemType) {
        let name = Some(i.ident.to_string());
        if self.record_item("trait type", name, &i.attrs, i.span()) {
            syn::visit::visit_trait_item_type(self, i);
        }
    }

    fn visit_trait_item_macro(&mut self, i: &'_ syn::TraitItemMacro) {
        if self.record_item("trait macro", None, &i.attrs, i.span()) {
            syn::visit::visit_trait_item_macro(self, i);
        }
    }

    fn visit_item_fn(&mut self, i: &'_ syn::ItemFn) {
        let name = Some(i.sig.ident.to_string());
        if self.record_item("function", name, &i.attrs, i.span()) {
            syn::visit::visit_item_fn(self, i);
        }
    }

    fn visit_item_struct(&mut self, i: &'_ syn::ItemStruct) {
        let name = Some(i.ident.to_string());
        if self.record_item("struct", name, &i.attrs, i.span()) {
            syn::visit::visit_item_struct(self, i);
        }
    }

    fn visit_item_enum(&mut self, i: &'_ syn::ItemEnum) {
        let name = Some(i.ident.to_string());
        if self.record_item("enum", name, &i.attrs, i.span()) {
            syn::visit::visit_item_enum(self, i);
        }
    }

    fn visit_item_impl(&mut self, i: &'_ syn::ItemImpl) {
        if self.record_item("impl block", None, &i.attrs, i.span()) {
            syn::visit::visit_item_impl(self, i);
        }
    }

    fn visit_item_type(&mut self, i: &'_ syn::ItemType) {
        let name = Some(i.ident.to_string());
        if self.record_item("type", name, &i.attrs, i.span()) {
            syn::visit::visit_item_type(self, i);
        }
    }

    fn visit_item_trait(&mut self, i: &'_ syn::ItemTrait) {
        let name = Some(i.ident.to_string());
        if self.record_item("trait", name, &i.attrs, i.span()) {
            syn::visit::visit_item_trait(self, i);
        }
    }

    fn visit_item_trait_alias(&mut self, i: &'_ syn::ItemTraitAlias) {
        let name = Some(i.ident.to_string());
        if self.record_item("trait alias", name, &i.attrs, i.span()) {
            syn::visit::visit_item_trait_alias(self, i);
        }
    }

    fn visit_item_const(&mut self, i: &'_ syn::ItemConst) {
        let name = Some(i.ident.to_string());
        if self.record_item("const", name, &i.attrs, i.span()) {
            syn::visit::visit_item_const(self, i);
        }
    }

    fn visit_item_static(&mut self, i: &'_ syn::ItemStatic) {
        let name = Some(i.ident.to_string());
        if self.record_item("static", name, &i.attrs, i.span()) {
            syn::visit::visit_item_static(self, i);
        }
    }

    fn visit_item_union(&mut self, i: &'_ syn::ItemUnion) {
        let name = Some(i.ident.to_string());
        if self.record_item("union", name, &i.attrs, i.span()) {
            syn::visit::visit_item_union(self, i);
        }
    }

    // `extern "C" { ... }` blocks and their items. Often cfg/target-gated, and
    // foreign signatures can reference gated type aliases; record the gate so std
    // usage inside is not treated as ungated.
    fn visit_item_foreign_mod(&mut self, i: &'_ syn::ItemForeignMod) {
        if self.record_item("foreign mod", None, &i.attrs, i.span()) {
            syn::visit::visit_item_foreign_mod(self, i);
        }
    }

    fn visit_foreign_item_fn(&mut self, i: &'_ syn::ForeignItemFn) {
        let name = Some(i.sig.ident.to_string());
        if self.record_item("foreign function", name, &i.attrs, i.span()) {
            syn::visit::visit_foreign_item_fn(self, i);
        }
    }

    fn visit_foreign_item_static(&mut self, i: &'_ syn::ForeignItemStatic) {
        let name = Some(i.ident.to_string());
        if self.record_item("foreign static", name, &i.attrs, i.span()) {
            syn::visit::visit_foreign_item_static(self, i);
        }
    }

    fn visit_foreign_item_type(&mut self, i: &'_ syn::ForeignItemType) {
        let name = Some(i.ident.to_string());
        if self.record_item("foreign type", name, &i.attrs, i.span()) {
            syn::visit::visit_foreign_item_type(self, i);
        }
    }

    fn visit_foreign_item_macro(&mut self, i: &'_ syn::ForeignItemMacro) {
        if self.record_item("foreign macro", None, &i.attrs, i.span()) {
            syn::visit::visit_foreign_item_macro(self, i);
        }
    }

    // A `#[cfg(...)]`-gated match arm. The arm body is an `Expr`, not a `Stmt`,
    // so `visit_stmt` never sees its gate; without this, std usage inside a gated
    // arm looks ungated.
    fn visit_arm(&mut self, i: &'_ syn::Arm) {
        if self.record_item("match arm", None, &i.attrs, i.body.span()) {
            syn::visit::visit_arm(self, i);
        }
    }

    fn visit_item_use(&mut self, i: &'_ syn::ItemUse) {
        if self.should_skip(&i.attrs) {
            debug!("Skipping use due to test attribute");
            return;
        }

        let own = self.parse_cfg_gate(&i.attrs);
        let externally_gated = self.is_externally_gated(&i.attrs);
        let span = self.get_span(&i.span());
        // Emit one LocalItem per leaf name so collect_named_items_with_conditions
        // can determine which items a feature gates via re-exports.
        let names = extract_use_leaf_names(&i.tree);
        for name in names {
            self.push_item(LocalItem {
                own_condition: own.clone(),
                span: span.clone(),
                name: Some(name),
                externally_gated,
            });
        }
        syn::visit::visit_item_use(self, i);
    }

    fn visit_item_extern_crate(&mut self, i: &'_ syn::ItemExternCrate) {
        if self.should_skip(&i.attrs) {
            debug!("Skipping extern crate {} due to test attribute", i.ident);
            return;
        }

        let externally_gated = self.is_externally_gated(&i.attrs);
        if let Some(own) = self.parse_cfg_gate(&i.attrs) {
            self.push_item(LocalItem {
                own_condition: Some(own),
                span: self.get_span(&i.span()),
                name: None,
                externally_gated,
            });
        } else {
            self.push_item(LocalItem {
                own_condition: None,
                span: self.get_span(&i.span()),
                name: None,
                externally_gated,
            });
        }
        syn::visit::visit_item_extern_crate(self, i);
    }

    fn visit_macro(&mut self, i: &'_ syn::Macro) {
        // `cfg_if! { if #[cfg(..)] {..} else {..} }` gates each arm body; handle it
        // specially since its `#[cfg]` isn't a leading segment attribute.
        if is_cfg_if_path(&i.path) {
            self.record_cfg_if(&i.tokens);
            syn::visit::visit_macro(self, i);
            return;
        }
        // syn treats the macro argument tokens as opaque; extract any
        // `#[cfg(...)]`-gated regions from them so gated std usage passed as a
        // macro argument isn't misclassified as unconditional.
        self.record_macro_cfg_segments(&i.tokens);
        syn::visit::visit_macro(self, i);
    }

    fn visit_item_macro(&mut self, i: &'_ syn::ItemMacro) {
        if self.should_skip(&i.attrs) {
            debug!("Skipping macro due to test attribute");
            return;
        }

        if is_cfg_if_path(&i.mac.path) {
            self.record_cfg_if(&i.mac.tokens);
            syn::visit::visit_item_macro(self, i);
            return;
        }

        if i.mac.path.is_ident("include") {
            // Capture any `#[cfg(...)]` on the include! item — previously the early
            // return discarded it, so the included file's std usage looked ungated.
            let own = self.parse_cfg_gate(&i.attrs);
            self.visit_include_macro(&i.mac.tokens, own);
            return;
        } else if i.mac.path.is_ident("compile_error") {
            let attrs = i.attrs.iter().filter(|a| a.path().is_ident("cfg"));
            for attr in attrs {
                match attr.meta.clone() {
                    Meta::List(meta_list) => {
                        let tokens = meta_list.tokens.clone();
                        let negated: Attribute = syn::parse_quote!(
                        #[cfg(not(#tokens))]
                        );
                        let constraint = parser::parse_main_attributes_direct(&negated, self.ctx).0;
                        if let Some(c) = constraint {
                            self.hard_constraints.push(c);
                        }
                    }
                    _ => {
                        debug!("Unexpected meta type for compiler_error: {:?}", attr.meta);
                    }
                }
            }
        } else if let Some(own) = self.parse_cfg_gate(&i.attrs) {
            let externally_gated = self.is_externally_gated(&i.attrs);
            self.push_item(LocalItem {
                own_condition: Some(own),
                span: self.get_span(&i.span()),
                name: None,
                externally_gated,
            });
        } else {
            let externally_gated = self.is_externally_gated(&i.attrs);
            self.push_item(LocalItem {
                own_condition: None,
                span: self.get_span(&i.span()),
                name: None,
                externally_gated,
            });
        }
        // A custom macro (e.g. cfg_if-style `cfg_time!`) may take `mod X;` as a
        // passthrough `$item` arg that syn can't see as a module; register any it
        // declares so their files are walked. Complements the plugin's
        // `macro_module_imports`, which only catches macro-*generated* mods.
        self.scan_macro_mod_decls(&i.mac.tokens);
        syn::visit::visit_item_macro(self, i);
    }

    fn visit_stmt(&mut self, i: &'_ syn::Stmt) {
        let attrs: &[syn::Attribute] = match i {
            syn::Stmt::Local(l) => &l.attrs,
            syn::Stmt::Macro(m) => &m.attrs,
            syn::Stmt::Item(_) => &[], // handled by their own visit_item_* methods
            syn::Stmt::Expr(expr, _) => match expr {
                syn::Expr::Array(e) => &e.attrs,
                syn::Expr::Assign(e) => &e.attrs,
                syn::Expr::Async(e) => &e.attrs,
                syn::Expr::Await(e) => &e.attrs,
                syn::Expr::Binary(e) => &e.attrs,
                syn::Expr::Block(e) => &e.attrs,
                syn::Expr::Break(e) => &e.attrs,
                syn::Expr::Call(e) => &e.attrs,
                syn::Expr::Cast(e) => &e.attrs,
                syn::Expr::Closure(e) => &e.attrs,
                syn::Expr::Const(e) => &e.attrs,
                syn::Expr::Continue(e) => &e.attrs,
                syn::Expr::Field(e) => &e.attrs,
                syn::Expr::ForLoop(e) => &e.attrs,
                syn::Expr::Group(e) => &e.attrs,
                syn::Expr::If(e) => &e.attrs,
                syn::Expr::Index(e) => &e.attrs,
                syn::Expr::Infer(e) => &e.attrs,
                syn::Expr::Let(e) => &e.attrs,
                syn::Expr::Lit(e) => &e.attrs,
                syn::Expr::Loop(e) => &e.attrs,
                syn::Expr::Macro(e) => &e.attrs,
                syn::Expr::Match(e) => &e.attrs,
                syn::Expr::MethodCall(e) => &e.attrs,
                syn::Expr::Paren(e) => &e.attrs,
                syn::Expr::Path(e) => &e.attrs,
                syn::Expr::Range(e) => &e.attrs,
                syn::Expr::Reference(e) => &e.attrs,
                syn::Expr::Repeat(e) => &e.attrs,
                syn::Expr::Return(e) => &e.attrs,
                syn::Expr::Struct(e) => &e.attrs,
                syn::Expr::Try(e) => &e.attrs,
                syn::Expr::TryBlock(e) => &e.attrs,
                syn::Expr::Tuple(e) => &e.attrs,
                syn::Expr::Unary(e) => &e.attrs,
                syn::Expr::Unsafe(e) => &e.attrs,
                syn::Expr::While(e) => &e.attrs,
                syn::Expr::Yield(e) => &e.attrs,
                _ => &[],
            },
        };

        // Push when the statement carries *either* a feature gate or a
        // non-feature one. Keying only off `parse_cfg_gate` would drop
        // `#[cfg(target_os = "linux")] let f = std::fs::File::open(..);`
        // entirely — no LocalItem, so `ancestors_for_span` sees nothing and the
        // usage reads as unguarded.
        let own = self.parse_cfg_gate(attrs);
        let externally_gated = self.is_externally_gated(attrs);
        if own.is_some() || externally_gated {
            let span = match i {
                syn::Stmt::Local(l) => self.get_span(&l.span()),
                syn::Stmt::Macro(m) => self.get_span(&m.span()),
                syn::Stmt::Expr(expr, _) => self.get_span(&expr.span()),
                syn::Stmt::Item(item) => self.get_span(&item.span()),
            };
            self.push_item(LocalItem {
                own_condition: own,
                span,
                name: None,
                externally_gated,
            });
        }

        syn::visit::visit_stmt(self, i);
    }
}

// ---------------------------------------------------------------------------
// ModCollector — drives recursive file visiting
// ---------------------------------------------------------------------------

/// Combine the `#[cfg(...)]` gates found in a file's *inner* attributes
/// (`#![cfg(...)]` at the top of the file). A module can be gated entirely by
/// such an inner attribute rather than a `#[cfg]` on its `mod foo;`
/// declaration (e.g. `set.rs` starting with `#![cfg(feature = "std")]` while
/// `lib.rs` has a bare `mod set;`). Without folding this into the module's
/// entry condition the file's std usage looks ungated and is misclassified as
/// hard/unconditional std usage.
fn file_inner_cfg_gate<'a>(ctx: &'a z3::Context, attrs: &[syn::Attribute]) -> Option<Bool<'a>> {
    attrs
        .iter()
        .filter(|a| a.path().is_ident("cfg"))
        .filter_map(|attr| parser::parse_main_attributes_direct(attr, ctx).0)
        .reduce(|acc, b| Bool::and(ctx, &[&acc, &b]))
}

pub struct ModCollector<'a> {
    ctx: &'a z3::Context,
    pub hard_constraints: Vec<Bool<'a>>,
    pub no_std_condition: Option<Bool<'a>>,
    /// Deferred `include!(concat!(env!("OUT_DIR"), …))` sites across all files,
    /// for the driver to resolve once a plugin run reveals OUT_DIR.
    pub pending_includes: Vec<PendingInclude<'a>>,
}

impl<'a> ModCollector<'a> {
    pub fn new(ctx: &'a z3::Context) -> Self {
        Self {
            ctx,
            hard_constraints: vec![],
            no_std_condition: None,
            pending_includes: vec![],
        }
    }

    pub fn collect(&mut self, root_path: &Path, name: &str) -> ModNode<'a> {
        self.visit_file(root_path, name, None)
    }

    pub fn visit_file(
        &mut self,
        path: &Path,
        name: &str,
        inherited: Option<Bool<'a>>,
    ) -> ModNode<'a> {
        let content = std::fs::read_to_string(path)
            .unwrap_or_else(|e| panic!("Failed to read {}: {}", path.display(), e));
        let syntax = syn::parse_file(&content)
            .unwrap_or_else(|e| panic!("Failed to parse {}: {}", path.display(), e));

        let source_dir = path.parent().unwrap_or(Path::new(".")).to_path_buf();
        // Fold any file-level inner `#![cfg(...)]` gate into the entry condition
        // so items gated only by the inner attribute are seen as conditional.
        let file_gate = file_inner_cfg_gate(self.ctx, &syntax.attrs);
        let effective = FileVisitor::and_conditions(self.ctx, inherited, file_gate);
        let mut visitor = FileVisitor::new(
            self.ctx,
            path,
            source_dir.clone(),
            source_dir.clone(),
            effective.clone(),
            true, // entrypoint is always mod-rs style
            false,
        );
        visitor.visit_file(&syntax);
        let (local_items, mut children, hard_constraints, no_std_cond, pending_includes) =
            visitor.finish();
        self.hard_constraints.extend(hard_constraints);
        self.pending_includes.extend(pending_includes);
        if no_std_cond.is_some() {
            self.no_std_condition = no_std_cond;
        }

        for child in &mut children {
            Self::resolve_child(self.ctx, child, &mut self.hard_constraints, &mut self.pending_includes);
        }

        ModNode {
            name: name.to_string(),
            source_file: path.to_path_buf(),
            source_dir,
            entry_condition: effective,
            local_items,
            children,
            is_inline: false,
            externally_gated: false,
        }
    }

    fn resolve_child(
        ctx: &'a z3::Context,
        child: &mut ModNode<'a>,
        hard_constraints: &mut Vec<Bool<'a>>,
        pending_includes: &mut Vec<PendingInclude<'a>>,
    ) {
        if child.is_inline {
            for gc in &mut child.children {
                Self::resolve_child(ctx, gc, hard_constraints, pending_includes);
            }
            return;
        }

        // Try to find the file for this child.
        // source_dir was set to the file's parent by the visitor for file-based mods.
        let rs = &child.source_file;
        let file_path = if rs.exists() { Some(rs) } else { None };

        if let Some(path) = file_path {
            debug!("Visiting child module {} at {}", child.name, path.display());
            let content = std::fs::read_to_string(path)
                .unwrap_or_else(|e| panic!("Failed to read {}: {}", path.display(), e));
            let syntax = syn::parse_file(&content)
                .unwrap_or_else(|e| panic!("Failed to parse {}: {}", path.display(), e));

            let source_dir = path.parent().unwrap_or(Path::new(".")).to_path_buf();
            let is_mod_rs = path.file_name().is_some_and(|n| n == "mod.rs");

            let current_search_dir = if is_mod_rs {
                source_dir.clone()
            } else {
                child.source_dir.join(&child.name)
            };

            // Fold any file-level inner `#![cfg(...)]` gate into this child's
            // entry condition. A `mod set;` with no gate whose file starts with
            // `#![cfg(feature = "std")]` is gated entirely by the inner
            // attribute; without this its std usage looks ungated.
            let file_gate = file_inner_cfg_gate(ctx, &syntax.attrs);
            let effective =
                FileVisitor::and_conditions(ctx, child.entry_condition.clone(), file_gate);

            let mut fv = FileVisitor::new(
                ctx,
                path,
                source_dir.clone(),
                current_search_dir,
                effective.clone(),
                is_mod_rs,
                child.externally_gated,
            );
            fv.visit_file(&syntax);
            let (local_items, mut grandchildren, hard_constraints_child, _no_std_cond, pend) =
                fv.finish();
            hard_constraints.extend(hard_constraints_child);
            pending_includes.extend(pend);

            // Recurse into grandchildren
            for gc in &mut grandchildren {
                Self::resolve_child(ctx, gc, hard_constraints, pending_includes);
            }

            child.source_dir = source_dir;
            child.entry_condition = effective;
            child.local_items = local_items;
            child.children = grandchildren;
        }
    }
}

/// Resolve deferred `include!(concat!(env!("OUT_DIR"), …))` sites now that a
/// plugin run has revealed the real `out_dir`. Each generated file is parsed
/// and attached to `root` as a child gated by the include site's condition, so
/// `ancestors_for_span` sees generated std usage as conditional (not hard std).
/// The attached node's `source_file` is the normalized `$OUT_DIR/…` form so it
/// matches the (equally normalized) HIR span paths. Nested OUT_DIR includes in
/// the generated file are resolved transitively via a worklist.
pub fn resolve_pending_includes<'a>(
    ctx: &'a z3::Context,
    root: &mut ModNode<'a>,
    pending: &[PendingInclude<'a>],
    out_dir: &str,
) {
    let out = Path::new(out_dir);
    let mut worklist: Vec<PendingInclude<'a>> = pending.to_vec();
    let mut seen: HashSet<String> = HashSet::new();

    while let Some(inc) = worklist.pop() {
        let real_path = out.join(inc.tail.trim_start_matches('/'));
        let canonical = driver::normalize_generated_path(&real_path.to_string_lossy());
        if !seen.insert(canonical.clone()) {
            continue;
        }
        if !real_path.exists() {
            debug!(
                "OUT_DIR include target not found on disk: {}",
                real_path.display()
            );
            continue;
        }

        let name = real_path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("generated")
            .to_string();

        // Parse the generated file with the include-site condition as its entry
        // condition. A fresh collector gives us the node plus any nested
        // OUT_DIR includes to enqueue.
        let mut sub = ModCollector::new(ctx);
        let mut node = sub.visit_file(&real_path, &name, inc.condition.clone());
        // Normalize the whole parsed subtree, not just the top node: a generated
        // `mod.rs` may declare `pub mod types;` (rust-protobuf does), which
        // ModCollector resolves to a sibling generated file with a raw hash
        // path. All of them must carry the normalized `$OUT_DIR/…` form so
        // generated HIR spans (also normalized) match during ancestor lookup.
        normalize_subtree_source_files(&mut node);
        root.children.push(node);
        worklist.extend(sub.pending_includes);
    }
}

/// Recursively rewrite every `source_file` in a parsed generated subtree to its
/// normalized `$OUT_DIR/…` form (a no-op for non-generated paths).
fn normalize_subtree_source_files(node: &mut ModNode) {
    node.source_file =
        PathBuf::from(driver::normalize_generated_path(&node.source_file.to_string_lossy()));
    for child in &mut node.children {
        normalize_subtree_source_files(child);
    }
}

/// Whether a macro path refers to `cfg_if` — matches both the bare `cfg_if!` and
/// the qualified `cfg_if::cfg_if!` forms by checking the final path segment.
fn is_cfg_if_path(path: &syn::Path) -> bool {
    path.segments
        .last()
        .map_or(false, |s| s.ident == "cfg_if")
}

/// Parse a single outer attribute from a `# [ ... ]` token stream and, if it is
/// a `cfg(...)`, return its Z3 condition. Returns `None` for non-cfg attributes
/// (e.g. doc comments) or on parse failure.
fn parse_attr_stream_cfg<'a>(ts: &TokenStream, ctx: &'a z3::Context) -> Option<Bool<'a>> {
    parse_attr_stream_cfg_ext(ts, ctx).0
}

/// As `parse_attr_stream_cfg`, but also reports whether the predicate is gated
/// by cfgs naming no feature (see `FileVisitor::is_externally_gated`). A
/// `cfg_if!` arm like `if #[cfg(all(target_arch = "riscv32", target_os =
/// "none"))]` yields `None` for the Bool, which alone is indistinguishable from
/// an unconditional arm.
fn parse_attr_stream_cfg_ext<'a>(
    ts: &TokenStream,
    ctx: &'a z3::Context,
) -> (Option<Bool<'a>>, bool) {
    use syn::parse::Parser;
    let Ok(attrs) = syn::Attribute::parse_outer.parse2(ts.clone()) else {
        return (None, false);
    };
    let Some(attr) = attrs.into_iter().next() else {
        return (None, false);
    };
    if !attr.path().is_ident("cfg") {
        return (None, false);
    }
    let (equation, parsed) = parser::parse_main_attributes_direct(&attr, ctx);
    let externally_gated = equation.is_none() && !parsed.constants.is_empty();
    (equation, externally_gated)
}

fn should_skip(attrs: &[syn::Attribute]) -> bool {
    attrs.iter().any(|a| {
        // bare #[test]
        if a.path().is_ident("test") {
            return true;
        }
        // bare #[cfg(test)]
        if a.path().is_ident("cfg") {
            let mut is_test = false;
            let _ = a.parse_nested_meta(|meta| {
                if meta.path.is_ident("test") {
                    is_test = true;
                }
                Ok(())
            });
            return is_test;
        }
        false
    })
}

/// Run `cargo metadata` for `manifest`, working around published tarballs whose
/// `[workspace]` section lists members that crates.io stripped out (e.g. old
/// `libc` referencing `libc-test/`). Such a manifest makes cargo fail outright,
/// so on error we retry once with the member lists emptied. The `[workspace]`
/// table itself is kept — dropping it entirely would let cargo walk upwards and
/// attach the crate to an unrelated parent workspace. The original file is
/// always restored before returning.
fn run_cargo_metadata(manifest: &str) -> cargo_metadata::Metadata {
    let run = || MetadataCommand::new().manifest_path(manifest).no_deps().exec();

    let first_err = match run() {
        Ok(metadata) => return metadata,
        Err(e) => e,
    };

    let Some(patched) = strip_workspace_members(manifest) else {
        panic!("Failed to execute cargo metadata: {first_err:?}");
    };

    debug!("cargo metadata failed for {manifest}, retrying without workspace members");
    let original = std::fs::read_to_string(manifest).expect("Failed to read manifest");
    std::fs::write(manifest, &patched).expect("Failed to write patched manifest");
    let result = run();
    std::fs::write(manifest, &original).expect("Failed to restore manifest");

    result.unwrap_or_else(|e| panic!("Failed to execute cargo metadata: {e:?}"))
}

/// Return `manifest`'s contents with `members`/`default-members`/`exclude`
/// removed from its `[workspace]` table, or `None` if there is nothing to strip.
/// Formatting is not preserved — the caller restores the original file.
fn strip_workspace_members(manifest: &str) -> Option<String> {
    let mut doc: toml::Table = std::fs::read_to_string(manifest).ok()?.parse().ok()?;
    let workspace = doc.get_mut("workspace")?.as_table_mut()?;

    let stripped = ["members", "default-members", "exclude"]
        .iter()
        .any(|k| workspace.remove(*k).is_some());

    stripped.then(|| toml::to_string(&doc).ok()).flatten()
}

/// A `[lib]` target can be reported under any of the library-like kinds
/// depending on the crate's declared `crate-type` (e.g. `rlib`, `cdylib`,
/// `proc-macro`). Match all of them, not just the plain `lib` kind.
fn is_lib_kind(k: &TargetKind) -> bool {
    matches!(
        k,
        TargetKind::Lib
            | TargetKind::RLib
            | TargetKind::DyLib
            | TargetKind::CDyLib
            | TargetKind::StaticLib
            | TargetKind::ProcMacro
    )
}

/// Does this package have a library target?
///
/// Mirrors the `is_lib || (is_bin && !has_lib)` entrypoint rule in
/// [`find_entrypoints`]: when a lib exists it is the only target we analyse, so
/// the HIR pass must be restricted to it too.
pub fn package_has_lib(manifest: &str) -> bool {
    run_cargo_metadata(manifest)
        .workspace_packages()
        .iter()
        .any(|p| p.targets.iter().any(|t| t.kind.iter().any(is_lib_kind)))
}

/// Collect every source file covered by a resolved module tree.
///
/// This is the reachability-driven answer to "which files belong to this
/// crate", as opposed to sweeping the source directory. Inline modules share
/// their parent's file, so they contribute nothing new.
///
/// NOTE: the tree is only complete *after* a covering run. Modules generated
/// inside macro expansions arrive via the plugin's `macro_modules`, and
/// `include!(concat!(env!("OUT_DIR"), …))` files are spliced in by
/// `resolve_pending_includes` once a run reveals OUT_DIR. Calling this before
/// analysis yields only the syn-reachable subset.
pub fn collect_source_files(root: &ModNode<'_>) -> Vec<PathBuf> {
    let mut out = Vec::new();
    let mut seen = HashSet::new();
    collect_source_files_recursive(root, &mut out, &mut seen);
    out
}

fn collect_source_files_recursive(
    node: &ModNode<'_>,
    out: &mut Vec<PathBuf>,
    seen: &mut HashSet<PathBuf>,
) {
    if !node.is_inline && seen.insert(node.source_file.clone()) {
        out.push(node.source_file.clone());
    }
    for child in &node.children {
        collect_source_files_recursive(child, out, seen);
    }
}

pub fn find_entrypoints(manifest: &str, known_modules: &mut Vec<PathBuf>) -> PathBuf {
    let metadata = run_cargo_metadata(manifest);

    for package in metadata.workspace_packages() {
        let targets = &package.targets;
        let has_lib = targets.iter().any(|t| t.kind.iter().any(is_lib_kind));

        for target in targets {
            let is_lib = target.kind.iter().any(is_lib_kind);
            let is_bin = target.kind.iter().any(|k| k == &TargetKind::Bin);

            if is_lib || (is_bin && !has_lib) {
                known_modules.push(PathBuf::from(target.src_path.to_string()));
            }
        }
    }

    PathBuf::from(&manifest).parent().unwrap().to_path_buf()
}

/// Collect (name, full_condition) pairs for every named item in the tree.
/// full_condition = root-to-leaf: inherited entry_conditions AND own_condition.
/// Items with no name or no condition are emitted with `Bool::from_bool(ctx, true)`
/// as their condition (unconditionally present).
/// Used by `process_dep_crate` to build the feature→items map.
pub fn collect_named_items_with_conditions<'a>(
    node: &ModNode<'a>,
    ctx: &'a z3::Context,
) -> Vec<(String, Bool<'a>)> {
    let mut result = vec![];
    collect_named_recursive(node, node.entry_condition.clone(), ctx, &mut result);
    result
}

fn collect_named_recursive<'a>(
    node: &ModNode<'a>,
    inherited: Option<Bool<'a>>,
    ctx: &'a z3::Context,
    out: &mut Vec<(String, Bool<'a>)>,
) {
    let module_gate = match (&inherited, &node.entry_condition) {
        (Some(i), Some(e)) => Some(Bool::and(ctx, &[i, e])),
        (Some(i), None) => Some(i.clone()),
        (None, Some(e)) => Some(e.clone()),
        (None, None) => None,
    };

    for item in &node.local_items {
        let Some(ref name) = item.name else { continue };
        let effective = match (&module_gate, &item.own_condition) {
            (Some(g), Some(c)) => Bool::and(ctx, &[g, c]),
            (Some(g), None) => g.clone(),
            (None, Some(c)) => c.clone(),
            (None, None) => Bool::from_bool(ctx, true),
        };
        out.push((name.clone(), effective));
    }

    for child in &node.children {
        collect_named_recursive(child, module_gate.clone(), ctx, out);
    }
}

/// Flatten the tree into a list of (effective_condition, item) pairs
/// for the solver. effective_condition = entry_condition AND own_condition.
pub fn collect_all_items<'a>(node: &ModNode<'a>, ctx: &'a z3::Context) -> Vec<Bool<'a>> {
    let mut result = vec![];
    collect_items_recursive(node, node.entry_condition.clone(), ctx, &mut result);
    result
}

fn collect_items_recursive<'a>(
    node: &ModNode<'a>,
    inherited: Option<Bool<'a>>,
    ctx: &'a z3::Context,
    out: &mut Vec<Bool<'a>>,
) {
    // Emit the module's entry condition as its own equation
    let module_gate = match (&inherited, &node.entry_condition) {
        (Some(i), Some(e)) => Some(Bool::and(ctx, &[i, e])),
        (Some(i), None) => Some(i.clone()),
        (None, Some(e)) => Some(e.clone()),
        (None, None) => None,
    };

    if let Some(ref gate) = module_gate {
        out.push(gate.clone());
    }

    // Then emit each item's condition ANDed with the module gate
    for item in &node.local_items {
        let Some(own_condition) = &item.own_condition else {
            continue; // unconditional item, no need to emit
        };
        let effective = match &module_gate {
            Some(gate) => Bool::and(ctx, &[gate, own_condition]),
            None => own_condition.clone(),
        };
        out.push(effective);
    }

    for child in &node.children {
        collect_items_recursive(child, module_gate.clone(), ctx, out);
    }
}

/// Collect items only along the path from root to the node matching `target_file`,
/// including that node's entire subtree (since it's new, all of it needs solving).
pub fn collect_items_for_new_file<'a>(
    crate_root: &Path,
    node: &ModNode<'a>,
    target_file: &Path,
    ctx: &'a z3::Context,
    solved_files: &HashSet<PathBuf>,
) -> Vec<Bool<'a>> {
    let target_path = crate_root.join(target_file);
    let mut result = vec![];
    collect_path_to_file(node, &target_path, ctx, solved_files, &mut result);
    result
}

fn collect_path_to_file<'a>(
    node: &ModNode<'a>,
    target_file: &Path,
    ctx: &'a z3::Context,
    solved_files: &HashSet<PathBuf>,
    out: &mut Vec<Bool<'a>>,
) -> bool {
    if node.source_file == target_file {
        // Found the target — collect its entire subtree, it's all new
        collect_items_recursive(node, node.entry_condition.clone(), ctx, out);
        return true;
    }

    for child in &node.children {
        if solved_files.contains(&child.source_file) {
            continue;
        }
        if collect_path_to_file(child, target_file, ctx, solved_files, out) {
            // This ancestor is on the path to target — emit its own gate
            // and its local items so the solver has full context
            if let Some(ref gate) = node.entry_condition {
                out.push(gate.clone());
            }
            for item in &node.local_items {
                let Some(own_condition) = &item.own_condition else {
                    continue;
                };

                let effective = match &node.entry_condition {
                    Some(gate) => Bool::and(ctx, &[gate, own_condition]),
                    None => own_condition.clone(),
                };
                out.push(effective);
            }
            return true;
        }
    }

    false
}

pub fn insert_child_into_tree<'a>(
    crate_root: &Path,
    node: &mut ModNode<'a>,
    target_file: &str,
    new_child: ModNode<'a>,
) -> bool {
    let target_path = crate_root.join(target_file);
    if node.source_file == target_path {
        node.children.push(new_child);
        return true;
    }
    for child in &mut node.children {
        if insert_child_into_tree(crate_root, child, target_file, new_child.clone()) {
            return true;
        }
    }
    false
}

// when inserting the new node, look up the parent's entry condition
pub fn get_entry_condition_for_file<'a>(
    crate_root: &Path,
    root: &ModNode<'a>,
    filename: &str,
) -> Option<Bool<'a>> {
    let target_path = crate_root.join(filename);
    if root.source_file == target_path {
        return root.entry_condition.clone();
    }
    for child in &root.children {
        if let Some(cond) = get_entry_condition_for_file(crate_root, child, filename) {
            return Some(cond);
        }
    }
    None
}

pub fn find_ancestors_for_span<'a>(
    node: &ModNode<'a>,
    target: &ReadableSpan,
    path: &mut Vec<Bool<'a>>,
) -> Option<Vec<Bool<'a>>> {
    // check if target span lives in this node's file
    if node.source_file.to_string_lossy().ends_with(&target.file) {
        // check local items for the span
        let collected: Vec<_> = node
            .local_items
            .iter()
            .filter(|item| item.span_matches(target))
            .sorted_by_key(|item| {
                let lines = item.span.end_line.saturating_sub(item.span.start_line);
                let cols = if item.span.end_line == item.span.start_line {
                    item.span.end_col.saturating_sub(item.span.start_col)
                } else {
                    0
                };
                (lines, cols)
            })
            .collect();

        let best = collected
            .iter()
            .find(|item| item.own_condition.is_some())
            .or_else(|| collected.first())
            .copied();

        if let Some(item) = best {
            // found it: push this item's own condition and return full path
            let mut result = path.clone();
            if let Some(own_condition) = &item.own_condition {
                result.push(own_condition.clone());
            }
            result.reverse(); // deepest first
            if !result.is_empty() {
                return Some(result);
            } else {
                return None;
            }
        }
    }

    for child in &node.children {
        let mut pushed = false;
        if let Some(ref entry) = child.entry_condition {
            path.push(entry.clone());
            pushed = true;
        }

        if let Some(result) = find_ancestors_for_span(child, target, path) {
            return Some(result);
        }
        if pushed {
            path.pop();
        }
    }

    None
}

pub fn ancestors_for_span<'a>(node: &ModNode<'a>, target: &ReadableSpan) -> Option<Vec<Bool<'a>>> {
    find_ancestors_for_span(node, target, &mut vec![])
}

/// Is `target` under a `#[cfg(...)]` that names no feature?
///
/// Mirrors `find_ancestors_for_span`'s traversal — same file matching, same
/// span containment — but answers a question the `Option<Vec<Bool>>` return
/// cannot express. Such a gate produces no Z3 term at all (see
/// `FileVisitor::is_externally_gated`), so to `ancestors_for_span` the span is
/// indistinguishable from an ungated one.
///
/// Module-level and item-level gating are ORed: a `LocalItem` records only its
/// own attributes, while a `ModNode` carries the flag inherited from its
/// declaration, so `#[cfg(target_os = "linux")] mod m { use std::fs::File; }` is
/// caught by the node even though the `use` itself is unannotated.
pub fn externally_gated_for_span(node: &ModNode<'_>, target: &ReadableSpan) -> bool {
    fn walk(node: &ModNode<'_>, target: &ReadableSpan, gated_so_far: bool) -> Option<bool> {
        let gated = gated_so_far || node.externally_gated;

        if node.source_file.to_string_lossy().ends_with(&target.file) {
            // OR across *every* containing item, unlike `find_ancestors_for_span`
            // which picks a single best one. Items nest — a `#[cfg(target_os)]`
            // statement sits inside its enclosing `fn`, and the fn is recorded
            // first — so taking the first match would read the fn's `false` and
            // miss the statement's gate entirely.
            let mut matched = false;
            let mut item_gated = false;
            for item in node.local_items.iter().filter(|i| i.span_matches(target)) {
                matched = true;
                item_gated |= item.externally_gated;
            }
            if matched {
                return Some(gated || item_gated);
            }
        }

        for child in &node.children {
            if let Some(found) = walk(child, target, gated) {
                return Some(found);
            }
        }
        None
    }

    walk(node, target, false).unwrap_or(false)
}

use itertools::Itertools;
use log::debug;
use proc_macro2::Span;
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
    /// Mirrors condition_stack — each frame collects the ModNodes for
    /// children at that nesting level.
    children_stack: Vec<Vec<ModNode<'a>>>,
    /// Items collected at each nesting level.
    items_stack: Vec<Vec<LocalItem<'a>>>,
    hard_constraints: Vec<Bool<'a>>,
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
    ) -> Self {
        Self {
            ctx,
            current_file: path.to_path_buf(),
            source_dir,
            current_search_dir,
            is_mod_rs,
            condition_stack: vec![inherited],
            children_stack: vec![vec![]],
            items_stack: vec![vec![]],
            hard_constraints: vec![],
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
    fn push_level(&mut self, condition: Option<Bool<'a>>) {
        self.condition_stack.push(condition);
        self.children_stack.push(vec![]);
        self.items_stack.push(vec![]);
    }

    /// Pop the current nesting level and return (items, children).
    fn pop_level(&mut self) -> (Vec<LocalItem<'a>>, Vec<ModNode<'a>>) {
        self.condition_stack.pop();
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
        let span = self.get_span(&span);
        self.push_item(LocalItem {
            own_condition,
            span,
            name,
        });
        true
    }

    /// Handle `include!("path")` by parsing the included file and visiting its
    /// contents inline (same condition/items/children stacks, no new ModNode).
    fn visit_include_macro(&mut self, tokens: &proc_macro2::TokenStream) {
        let lit = match syn::parse2::<syn::LitStr>(tokens.clone()) {
            Ok(l) => l,
            Err(_) => {
                debug!("include! with non-literal path, skipping");
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
        let content = match std::fs::read_to_string(&included_path) {
            Ok(c) => c,
            Err(e) => {
                debug!(
                    "Failed to read include! file {}: {}",
                    included_path.display(),
                    e
                );
                return;
            }
        };
        let syntax = match syn::parse_file(&content) {
            Ok(s) => s,
            Err(e) => {
                debug!(
                    "Failed to parse include! file {}: {}",
                    included_path.display(),
                    e
                );
                return;
            }
        };

        let included_dir = included_path
            .parent()
            .unwrap_or(Path::new("."))
            .to_path_buf();

        let old_file = std::mem::replace(&mut self.current_file, included_path);
        let old_source_dir = std::mem::replace(&mut self.source_dir, included_dir.clone());
        let old_search_dir = std::mem::replace(&mut self.current_search_dir, included_dir);
        let old_is_mod_rs = self.is_mod_rs;
        self.is_mod_rs = true;

        syn::visit::visit_file(self, &syntax);

        self.current_file = old_file;
        self.source_dir = old_source_dir;
        self.current_search_dir = old_search_dir;
        self.is_mod_rs = old_is_mod_rs;
    }

    /// Consume the visitor and return the top-level (items, children).
    fn finish(
        mut self,
    ) -> (
        Vec<LocalItem<'a>>,
        Vec<ModNode<'a>>,
        Vec<Bool<'a>>,
        Option<Bool<'a>>,
    ) {
        assert_eq!(self.condition_stack.len(), 1);
        (
            self.items_stack.pop().unwrap(),
            self.children_stack.pop().unwrap(),
            self.hard_constraints,
            self.no_std_condition,
        )
    }
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

        // Effective entry condition = inherited (from stack) AND own gate
        let effective = Self::and_conditions(self.ctx, self.current_condition(), own_gate);

        match &i.content {
            Some(_) => {
                // Inline mod — push a level, recurse, pop and assemble
                self.push_level(effective.clone());

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
            if let Some(own) = own
                && parser::is_no_std(&parsed, true)
            {
                self.no_std_condition = Some(own);
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
        let span = self.get_span(&i.span());
        // Emit one LocalItem per leaf name so collect_named_items_with_conditions
        // can determine which items a feature gates via re-exports.
        let names = extract_use_leaf_names(&i.tree);
        for name in names {
            self.push_item(LocalItem {
                own_condition: own.clone(),
                span: span.clone(),
                name: Some(name),
            });
        }
        syn::visit::visit_item_use(self, i);
    }

    fn visit_item_extern_crate(&mut self, i: &'_ syn::ItemExternCrate) {
        if self.should_skip(&i.attrs) {
            debug!("Skipping extern crate {} due to test attribute", i.ident);
            return;
        }

        if let Some(own) = self.parse_cfg_gate(&i.attrs) {
            self.push_item(LocalItem {
                own_condition: Some(own),
                span: self.get_span(&i.span()),
                name: None,
            });
        } else {
            self.push_item(LocalItem {
                own_condition: None,
                span: self.get_span(&i.span()),
                name: None,
            });
        }
        syn::visit::visit_item_extern_crate(self, i);
    }

    fn visit_item_macro(&mut self, i: &'_ syn::ItemMacro) {
        if self.should_skip(&i.attrs) {
            debug!("Skipping macro due to test attribute");
            return;
        }

        if i.mac.path.is_ident("include") {
            self.visit_include_macro(&i.mac.tokens);
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
            self.push_item(LocalItem {
                own_condition: Some(own),
                span: self.get_span(&i.span()),
                name: None,
            });
        } else {
            self.push_item(LocalItem {
                own_condition: None,
                span: self.get_span(&i.span()),
                name: None,
            });
        }
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

        if let Some(own) = self.parse_cfg_gate(attrs) {
            let span = match i {
                syn::Stmt::Local(l) => self.get_span(&l.span()),
                syn::Stmt::Macro(m) => self.get_span(&m.span()),
                syn::Stmt::Expr(expr, _) => self.get_span(&expr.span()),
                syn::Stmt::Item(item) => self.get_span(&item.span()),
            };
            self.push_item(LocalItem {
                own_condition: Some(own),
                span,
                name: None,
            });
        }

        syn::visit::visit_stmt(self, i);
    }
}

// ---------------------------------------------------------------------------
// ModCollector — drives recursive file visiting
// ---------------------------------------------------------------------------

pub struct ModCollector<'a> {
    ctx: &'a z3::Context,
    pub hard_constraints: Vec<Bool<'a>>,
    pub no_std_condition: Option<Bool<'a>>,
}

impl<'a> ModCollector<'a> {
    pub fn new(ctx: &'a z3::Context) -> Self {
        Self {
            ctx,
            hard_constraints: vec![],
            no_std_condition: None,
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
        let mut visitor = FileVisitor::new(
            self.ctx,
            path,
            source_dir.clone(),
            source_dir.clone(),
            inherited.clone(),
            true, // entrypoint is always mod-rs style
        );
        visitor.visit_file(&syntax);
        let (local_items, mut children, hard_constraints, no_std_cond) = visitor.finish();
        self.hard_constraints.extend(hard_constraints);
        if no_std_cond.is_some() {
            self.no_std_condition = no_std_cond;
        }

        for child in &mut children {
            Self::resolve_child(self.ctx, child, &mut self.hard_constraints);
        }

        ModNode {
            name: name.to_string(),
            source_file: path.to_path_buf(),
            source_dir,
            entry_condition: inherited,
            local_items,
            children,
            is_inline: false,
        }
    }

    fn resolve_child(
        ctx: &'a z3::Context,
        child: &mut ModNode<'a>,
        hard_constraints: &mut Vec<Bool<'a>>,
    ) {
        if child.is_inline {
            for gc in &mut child.children {
                Self::resolve_child(ctx, gc, hard_constraints);
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

            let mut fv = FileVisitor::new(
                ctx,
                path,
                source_dir.clone(),
                current_search_dir,
                child.entry_condition.clone(),
                is_mod_rs,
            );
            fv.visit_file(&syntax);
            let (local_items, mut grandchildren, hard_constraints_child, _no_std_cond) =
                fv.finish();
            hard_constraints.extend(hard_constraints_child);

            // Recurse into grandchildren
            for gc in &mut grandchildren {
                Self::resolve_child(ctx, gc, hard_constraints);
            }

            child.source_dir = source_dir;
            child.local_items = local_items;
            child.children = grandchildren;
        }
    }
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

pub fn find_entrypoints(manifest: &str, known_modules: &mut Vec<PathBuf>) -> PathBuf {
    let metadata = MetadataCommand::new()
        .manifest_path(manifest)
        .no_deps()
        .exec()
        .expect("Failed to execute cargo metadata");

    // A `[lib]` target can be reported under any of the library-like kinds
    // depending on the crate's declared `crate-type` (e.g. `rlib`, `cdylib`,
    // `proc-macro`). Match all of them, not just the plain `lib` kind.
    let is_lib_kind = |k: &TargetKind| {
        matches!(
            k,
            TargetKind::Lib
                | TargetKind::RLib
                | TargetKind::DyLib
                | TargetKind::CDyLib
                | TargetKind::StaticLib
                | TargetKind::ProcMacro
        )
    };

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

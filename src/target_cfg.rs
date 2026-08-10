//! Deciding a `#[cfg]` predicate that names only *target* atoms, against a
//! concrete build target.
//!
//! Policy G erases every non-`feature` cfg atom, and rightly so wherever the
//! question is "which features make this crate no_std": the target is the
//! consumer's choice, not an axis this tool controls, and an erased atom is
//! *unknown* (O-1 is the record of what happens when an unknown is given a truth
//! value under a negation).
//!
//! There is one question where the target is not unknown at all: **was this
//! crate's own `#![no_std]` in effect in the run whose records we are about to
//! believe?** That run compiled for one specific target, and `rustc --print cfg
//! --target <t>` reports that target's complete cfg set — every atom absent from
//! it is false, exactly as rustc decides it. On that axis the predicate is
//! *decidable*, not erased.
//!
//! Only on that axis, and [`is_decidable`] is the boundary. `--print cfg` is
//! complete for what rustc derives from the target and silent about everything
//! else — a build script's `cargo:rustc-cfg=…`, a `--cfg` flag, `test`, `doc`.
//! For an atom from one of those sources, absent does **not** mean false, and
//! this module says "undecided" rather than guessing.
//!
//! This module is deliberately confined to that use. Nothing here reaches the
//! solver: a `CfgPred` never becomes a Z3 term and never weakens or strengthens
//! a feature formula.
//!
//! ```text
//! #![cfg_attr(target_arch = "spirv", no_std)]   // macaw, renderling, saft-sdf
//! #![cfg_attr(target_os   = "cuda",  no_std)]   // cuda_std
//! #![cfg_attr(target_os   = "none",  no_std)]   // xous-ipc, xous-api-names
//! ```
//! On the host none of these holds, so the crate compiled there is a plain
//! **std** crate — its std records describe a configuration that is not the one
//! under test. See `driver::run_rustc_plugin_pass`.

use std::collections::{HashMap, HashSet};
use std::process::Command;
use std::sync::Mutex;

use log::debug;
use syn::punctuated::Punctuated;

use crate::consts;

/// The cfg atoms rustc reports for one target: `unix`, `target_os="none"`, …
pub type CfgSet = HashSet<(String, Option<String>)>;

/// A `#[cfg]` predicate, kept as written.
///
/// Only the four shapes rustc itself defines are represented; anything else
/// makes [`CfgPred::parse`] return `None`, which the callers read as "do not
/// apply this rule" rather than as any kind of default.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CfgPred {
    Atom {
        key: String,
        value: Option<String>,
    },
    Not(Box<CfgPred>),
    All(Vec<CfgPred>),
    Any(Vec<CfgPred>),
}

impl CfgPred {
    /// Parse the predicate of a `#[cfg(...)]` / first argument of a
    /// `#[cfg_attr(...)]`. `None` for any shape not recognised.
    pub fn parse(meta: &syn::Meta) -> Option<CfgPred> {
        match meta {
            syn::Meta::Path(p) => Some(CfgPred::Atom {
                key: p.get_ident()?.to_string(),
                value: None,
            }),
            syn::Meta::NameValue(nv) => {
                let key = nv.path.get_ident()?.to_string();
                let syn::Expr::Lit(syn::ExprLit {
                    lit: syn::Lit::Str(s),
                    ..
                }) = &nv.value
                else {
                    return None;
                };
                Some(CfgPred::Atom {
                    key,
                    value: Some(s.value()),
                })
            }
            syn::Meta::List(list) => {
                let op = list.path.get_ident()?.to_string();
                let inner = list
                    .parse_args_with(Punctuated::<syn::Meta, syn::Token![,]>::parse_terminated)
                    .ok()?;
                let parts = inner
                    .iter()
                    .map(CfgPred::parse)
                    .collect::<Option<Vec<_>>>()?;
                match op.as_str() {
                    "not" if parts.len() == 1 => {
                        Some(CfgPred::Not(Box::new(parts.into_iter().next().unwrap())))
                    }
                    "all" => Some(CfgPred::All(parts)),
                    "any" => Some(CfgPred::Any(parts)),
                    _ => None,
                }
            }
        }
    }

    /// Does any atom in the predicate use this key? The callers use it to refuse
    /// predicates naming `feature`: those are the tool's own axis, they already
    /// produce a solver equation, and nothing here should second-guess it.
    pub fn mentions(&self, key: &str) -> bool {
        self.keys().contains(key)
    }

    /// Every key the predicate names, `not`/`all`/`any` flattened away.
    pub fn keys(&self) -> HashSet<String> {
        match self {
            CfgPred::Atom { key, .. } => HashSet::from([key.clone()]),
            CfgPred::Not(inner) => inner.keys(),
            CfgPred::All(parts) | CfgPred::Any(parts) => {
                parts.iter().flat_map(|p| p.keys()).collect()
            }
        }
    }

    /// Evaluate against a target's cfg set. An atom the set does not contain is
    /// false — that is rustc's rule, and it is why `test` and `doc` come out
    /// false for the builds this tool performs. `all()` is true and `any()` is
    /// false when empty, also as rustc has it.
    pub fn holds(&self, cfgs: &CfgSet) -> bool {
        match self {
            CfgPred::Atom { key, value } => cfgs.contains(&(key.clone(), value.clone())),
            CfgPred::Not(inner) => !inner.holds(cfgs),
            CfgPred::All(parts) => parts.iter().all(|p| p.holds(cfgs)),
            CfgPred::Any(parts) => parts.iter().any(|p| p.holds(cfgs)),
        }
    }
}

impl std::fmt::Display for CfgPred {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CfgPred::Atom { key, value: None } => write!(f, "{key}"),
            CfgPred::Atom {
                key,
                value: Some(v),
            } => write!(f, "{key} = \"{v}\""),
            CfgPred::Not(inner) => write!(f, "not({inner})"),
            CfgPred::All(parts) | CfgPred::Any(parts) => {
                let op = if matches!(self, CfgPred::All(_)) {
                    "all"
                } else {
                    "any"
                };
                let joined = parts
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{op}({joined})")
            }
        }
    }
}

/// `rustc --print cfg` for one target, cached for the process. `None` when rustc
/// refuses the target or cannot be run at all — a fact about this environment,
/// never a reason to end a run, so every caller degrades to "rule not applied".
pub fn cfg_set(target: Option<&str>) -> Option<CfgSet> {
    static CACHE: Mutex<Option<HashMap<String, Option<CfgSet>>>> = Mutex::new(None);

    let key = target.unwrap_or("host").to_string();
    let mut guard = CACHE.lock().unwrap();
    let cache = guard.get_or_insert_with(HashMap::new);
    if let Some(hit) = cache.get(&key) {
        return hit.clone();
    }

    let mut args = vec!["+nightly", "--print", "cfg"];
    if let Some(t) = target {
        args.push("--target");
        args.push(t);
    }
    let computed = Command::new("rustc")
        .args(&args)
        .output()
        .ok()
        .filter(|o| o.status.success())
        .map(|o| parse_cfg_output(&String::from_utf8_lossy(&o.stdout)));
    if computed.is_none() {
        debug!("rustc --print cfg failed for target [{key}]; target predicate left undecided");
    }
    cache.insert(key, computed.clone());
    computed
}

/// One `rustc --print cfg` line per atom: `unix`, `target_os="none"`.
fn parse_cfg_output(stdout: &str) -> CfgSet {
    stdout
        .lines()
        .map(str::trim)
        .filter(|l| !l.is_empty())
        .map(|line| match line.split_once('=') {
            Some((k, v)) => (
                k.trim().to_string(),
                Some(v.trim().trim_matches('"').to_string()),
            ),
            None => (line.to_string(), None),
        })
        .collect()
}

/// Does `pred` hold for `target` (`None` = the host)? `None` when the answer
/// cannot be established — the target's cfg set could not be obtained, or the
/// predicate names an atom rustc does not derive from the target at all (see
/// [`is_decidable`]).
pub fn holds_for(pred: &CfgPred, target: Option<&str>) -> Option<bool> {
    if !is_decidable(pred) {
        return None;
    }
    cfg_set(target).map(|set| pred.holds(&set))
}

/// Is every atom in `pred` one rustc derives from the target?
///
/// `rustc --print cfg` is complete for the cfgs that *come from the target* and
/// silent about every other source: a build script's
/// `cargo:rustc-cfg=rustc_1_6` (str_overlap 0.4.3), a `--cfg docsrs`
/// (impl-more 0.1.9), `--cfg mesalock_sgx` (gbdt 0.1.3), and `test` / `doc` /
/// `doctest` / `fuzzing`. For those, "absent from the set" does not mean false —
/// it means injected by something this evaluation cannot see, and the honest
/// answer is that the predicate is undecided.
///
/// The test is evidence, not a list of names: an atom's key is target-derived
/// exactly when rustc reports it for *some* target in `{host} ∪ TARGET_LIST`.
/// `unix` qualifies (the host reports it) even though a bare-metal target does
/// not; `rustc_1_6` qualifies nowhere.
///
/// Measured over the corpus: of the 789 crate-root `#![cfg_attr(<pred>,
/// no_std)]` attributes whose predicate names no feature, 46 are decidable
/// (the `target_*` families) and the rest are `not(test)` and its variants,
/// which this correctly refuses to decide — and which need no decision, since
/// O-14(a) already treats `not(test)` as unconditionally no_std on the separate
/// grounds that this tool never sets `test`.
pub fn is_decidable(pred: &CfgPred) -> bool {
    let keys = target_derived_keys();
    pred.keys().iter().all(|k| keys.contains(k))
}

/// Every cfg key rustc reports for at least one target it can build here.
fn target_derived_keys() -> HashSet<String> {
    static CACHE: Mutex<Option<HashSet<String>>> = Mutex::new(None);

    let mut guard = CACHE.lock().unwrap();
    if let Some(hit) = guard.as_ref() {
        return hit.clone();
    }
    let mut keys = HashSet::new();
    for target in std::iter::once(None).chain(consts::TARGET_LIST.iter().map(|t| Some(*t))) {
        if let Some(set) = cfg_set(target) {
            keys.extend(set.into_iter().map(|(k, _)| k));
        }
    }
    debug!("{} cfg keys are target-derived", keys.len());
    *guard = Some(keys.clone());
    keys
}

/// The targets in [`consts::TARGET_LIST`] that satisfy `pred` — the ones on
/// which a crate carrying `#![cfg_attr(pred, no_std)]` is actually `#![no_std]`.
///
/// Reported for triage rather than used for a decision: an empty list is the
/// difference between a crate that is no_std only somewhere this tool cannot
/// build (`target_arch = "spirv"`, which rustc has no target for at all) and one
/// whose no_std target is in the sweep and simply failed to compile
/// (`target_os = "cuda"` → `nvptx64-nvidia-cuda`).
pub fn supported_no_std_targets(pred: &CfgPred) -> Vec<&'static str> {
    consts::TARGET_LIST
        .iter()
        .copied()
        .filter(|t| holds_for(pred, Some(t)).unwrap_or(false))
        .collect()
}

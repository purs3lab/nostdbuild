#![feature(rustc_private)]

use anyhow::Context;
use bincode::{Decode, Encode};
use lazy_static::lazy_static;
use proc_macro2::{Span, TokenStream};
use serde::{Deserialize, Serialize};
use std::{fs, path, sync::Mutex};
use syn::Attribute;

pub mod compiler;
pub mod consts;
pub mod db;
pub mod downloader;
pub mod driver;
pub mod parser;
pub mod phases;
pub mod solver;
pub mod target_cfg;
pub mod timing;
pub mod types;
pub mod visitor;

pub mod hir_driver;

use crate::types::*;
use std::collections::{HashMap, HashSet};

/// Stack the recursive-descent passes over a `syn` tree are guaranteed to get.
///
/// `syn` parses and `syn::visit` walks by recursive descent, so depth in a type
/// tree is depth on the stack. Generated files reach depths no hand-written
/// source does — `typenum`'s `src/gen/consts.rs` is 280 KB of
/// `UInt<UInt<UInt<…>>>` aliases nested hundreds deep, and it overflows a
/// 2 MiB stack in a debug build (KI-32). Size is not the predictor: typenum's
/// *larger* `tests/generated.rs` parses fine.
///
/// Without this the depth the tool can absorb is whatever stack the calling
/// thread happens to have — 8 MiB on the main thread, 2 MiB on a libtest
/// thread — and a stack overflow is a process abort, so `catch_unwind` cannot
/// turn it into the "treat the file as empty" miss that
/// [`visitor::parse_file_lenient`] records. Skipping big files instead would be
/// wrong twice over: wrong predictor, and a silently dropped file becomes a
/// false positive downstream.
const SYN_STACK_SIZE: usize = 64 * 1024 * 1024;

/// Grow before dropping below this much headroom. Larger than any stack a
/// caller is likely to supply, so the first `with_syn_stack` on a thread
/// switches to a `SYN_STACK_SIZE` segment and every nested one is a cheap
/// pointer compare inside it.
const SYN_STACK_RED_ZONE: usize = 16 * 1024 * 1024;

/// Run `f` with at least [`SYN_STACK_RED_ZONE`] bytes of stack, switching to a
/// fresh segment on the *same* thread if the current one is short.
///
/// Same-thread matters: `syn::File` holds `proc_macro2::Span`, which is
/// `!Send`, so a parsed tree cannot be handed back from a worker thread. Wrap
/// every `syn::parse_file` and every `Visit::visit_file` over a syn tree; hoist
/// one call as high as convenient (`bin/main.rs` does) so a whole run pays for
/// a single segment.
pub fn with_syn_stack<R>(f: impl FnOnce() -> R) -> R {
    stacker::maybe_grow(SYN_STACK_RED_ZONE, SYN_STACK_SIZE, f)
}

lazy_static! {
    // This is a list of all dependencies for a crate.
    // TODO: Convert this to a variable passed between functions instead of a global variable
    pub static ref DEPENDENCIES: Mutex<Vec<String>> = Mutex::new(Vec::new());
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub enum Dependency {
    Simple(String),
    Detailed {
        version: String,
        package: Option<String>,
        features: Option<Vec<String>>,
        optional: Option<bool>,
        #[serde(rename = "default-features")]
        default_features: Option<bool>,
        git: Option<String>,
    },
    // We use this to match weird patterns.
    Special {
        optional: Option<bool>,
    },
}

#[derive(Debug, Serialize)]
pub enum Status {
    Success,
    Failed,
}

#[derive(Debug, Serialize)]
pub struct Results {
    pub name: String,
    pub version: String,
    pub target: String,
    pub args: Vec<String>,
    pub status: Status,
    pub error: Option<String>,
}

#[derive(Default, Clone, Debug)]
pub struct Attributes {
    attributes: Vec<Attribute>,
    /// This will be a list of attributes associated with
    /// compiler_error macros. Note that the negated attributes present
    /// here will also be present in `attributes` field.
    /// This also does the double duty of storing negated
    /// attributes where the attribute would have included
    /// some direct usage of `std`.
    compile_error_attrs: Vec<Attribute>,
    /// This holds both name and version seperated by `:`
    pub crate_name: String,
    pub unconditional_no_std: bool,
    /// Sometimes, crate authors put `#[no_std]` instead of
    /// `#![no_std]`. This field will help track such cases.
    pub wrong_unconditional_setup: bool,
    /// Stores the filename as well since we can't recover
    /// it later from the Span.
    pub spans: Vec<(Span, Option<String>)>,
    /// We also collect modules whose imports is conditional
    /// on cfg attributes along with the attribute.
    /// ```
    /// #[cfg(feature = "my_mod")]
    /// mod my_mod;
    /// ```
    /// In this case, we don't consider direct usages of `std`
    /// in `my_mod` because it is possible to build the crate
    /// without enabling `my_mod` feature. But we need to ensure
    /// that the `cfg` is negated when solving the equations.
    pub mods: Vec<(String, Attribute)>,
    /// Rust allows including files conditionally using
    /// `cfg_attr` attribute.
    pub files_in_cfg_attrs: Vec<String>,
    /// The spans collected from HIR visitor.
    /// We will use this to determine if any of the attributes
    /// are gating direct usages of `std`.
    pub hir_spans: Vec<ReadableSpan>,
    /// The current file being parsed.
    pub current_file: String,
    /// The `#[cfg(...)]` token streams of the inline `mod`s currently being
    /// walked, outermost first — traversal state, not a result.
    ///
    /// cfg stripping is outside-in, so a `compile_error!` nested in
    /// `#[cfg(A)] mod m { .. }` fires only under `A ∧ <its own cfg>`; without
    /// the enclosing gates the negation pushed to `compile_error_attrs` is the
    /// fragment `¬<own cfg>`, which is strictly stronger than anything the crate
    /// wrote. See `visitor::negated_compile_error_cfg_within`.
    ///
    /// Inline modules only: this walk is flat and file-by-file, so the gate on
    /// an out-of-line `mod m;` (or a file's own `#![cfg]`) is not visible from
    /// inside `m`'s file. `ModCollector`, which does build the module tree,
    /// covers that case for the hard constraint.
    pub(crate) mod_cfg_stack: Vec<TokenStream>,
    /// How many source files `visit` actually read *and* handed to `syn`
    /// successfully.
    ///
    /// Zero means the parse established nothing about this crate: either the
    /// file list was empty (cargo reported no lib/bin target) or every candidate
    /// failed to read or parse (an edition-2015 crate `syn` 2 rejects). The
    /// attribute fields are then empty for want of evidence, not because the
    /// crate carries no attributes — a distinction `check_for_no_std` used to
    /// collapse into "this crate is not no_std".
    pub files_parsed: usize,
}

/// Used to pass huge amount of params between functions
#[derive(Default)]
pub struct DataExchange {
    pub name_with_version: String,
    pub db_data: Vec<DBData>,
    pub crate_info: CrateInfo,
    pub telemetry: Telemetry,
    pub crate_name_rename: TupleVec,
    /// (dep_crate_name_norm, item_name) pairs used by the main crate in a
    /// no_std-compatible context. Populated before the dep processing loop
    /// so finalize_dep_crate can skip removal of features that gate these items.
    pub valid_cross_crate_items: HashSet<(String, String)>,
    /// The impls the compiler selected for the main crate's trait obligations,
    /// restricted to call sites a no_std build reaches. Populated beside
    /// `valid_cross_crate_items` and read by `process_dep_crate`, which turns
    /// the ones naming a dependency into a constraint on that dependency's
    /// feature solve (KI-27, `driver::impl_availability_requirement`).
    ///
    /// Separate from `valid_cross_crate_items` because it answers a different
    /// question. That set says which *named* items the crate uses, and its only
    /// consumer protects a feature already on an edge from removal. This one is
    /// a requirement: multiexp 0.4.0 needs `zeroize/alloc`, which is on no edge
    /// at all, so there is nothing for protection to protect.
    pub impl_records: Vec<ImplRecord>,
    /// The cross-crate items the main crate names, each with the definition site
    /// the compiler resolved it to — the plain-path counterpart of
    /// `impl_records` (R34-6), and derived the same way.
    ///
    /// Read by `process_dep_crate`, which turns the ones naming a dependency
    /// into a constraint on that dependency's feature solve: earcut names
    /// `num_traits::float::Float`, the definition sits under
    /// `#[cfg(any(feature = "std", feature = "libm"))]`, and with `std`
    /// forbidden the solve is left with `libm`.
    ///
    /// Separate from `valid_cross_crate_items`, which is a different question
    /// asked a different way. That set protects a dependency feature from
    /// *removal*, so it errs wide on purpose and is built only from covering
    /// runs. This one *adds* a feature, so it uses the stricter reachability
    /// test — and it needs the fallback to a std-on pass, because a crate
    /// missing one of these items does not compile and therefore has no
    /// covering run to be built from at all.
    pub path_items: Vec<CrossCrateItem>,
    /// The main crate's no_std enable list — used by finalize_dep_crate to
    /// check if a main [features] entry references a protected dep feature.
    pub main_enable: Vec<String>,
    /// (dep_crate_name_norm, feat_name) pairs that must not be removed from
    /// either the dep declaration or the main crate's [features] table.
    /// Accumulated across finalize_dep_crate calls and consumed by
    /// move_unnecessary_dep_feats.
    pub protected_dep_features: HashSet<(String, String)>,
    /// Per-dependency (`<name>:<version>`): the dependency's own features that
    /// **must be off** if it is to be no_std — its `removable` set from
    /// `finalize_dep_crate`, closed over the dependency's `[features]` table so
    /// a feature that merely *reaches* a forbidden one counts too.
    ///
    /// This is the one notion of "this dep feature has to go" every writer
    /// shares. `move_unnecessary_dep_feats` reads it instead of asking whether
    /// the dependency's solve happened to *ask* for the feature: a feature the
    /// solve had no reason to set is a don't-care, and deleting the main crate's
    /// `<dep>/<feat>` on the strength of that is what emptied mtxgroup's
    /// `spin = ["spin/mutex", "spin/spin_mutex"]` (R31-5). Same distinction as
    /// F4/T4(a), applied to the third writer.
    ///
    /// Absent for a dependency whose analysis never ran (a DB-cache hit records
    /// the `disable`-derived fallback that `finalize_dep_crate` uses there).
    pub dep_forbidden_features: HashMap<String, HashSet<String>>,
    /// The polarity the *main* crate's own no_std condition forces on its
    /// declared features: features it entails true, and features it entails
    /// false. Written by `parser::process_crate` on the main path, where the
    /// condition is fully assembled, and read by `bin/main.rs` — every pass
    /// between the solve and the command line has to respect the same statement
    /// the solve did. See `solver::no_std_forced_features` (R31-3).
    pub main_no_std_required: Vec<String>,
    pub main_no_std_forbidden: Vec<String>,
}

impl DataExchange {
    /// The cross-crate items whose definition sits in, or below, this
    /// dependency — the input to `driver::path_availability_requirement` and to
    /// its `db.bin` guard (R34-6).
    ///
    /// A borrow of `path_items`; the filtering by defining crate happens inside
    /// the requirement, which has the edge graph needed to answer "or below".
    pub fn cross_crate_items(&self) -> &[CrossCrateItem] {
        &self.path_items
    }
}

/// We store already resolved features for a crate
/// to be compiled as no_std in a db file.
/// This is the structure of the db file.
#[derive(Debug, Encode, Decode)]
pub struct DBData {
    pub name_with_version: String,
    pub features: DoubleTupleVecString,
}

#[derive(Debug, Clone, Default, Serialize)]
pub struct CrateInfo {
    pub name: String,
    pub version: String,
    /// Dependencies of the crate along with the features that are enabled for them
    /// by the main crate during the dependency inclusion.
    pub deps_and_features: Vec<(CrateInfo, Vec<String>)>,
    pub features: Vec<(String, TupleVec)>,
    pub default_features: bool,
    pub optional: bool,
    pub git: Option<String>,
}

#[derive(Debug, Default, Serialize)]
pub struct AllStats {
    pub name: String,
    pub compilation_res: Vec<Results>,
    pub crate_info: Option<CrateInfo>,
    // Collects all unguarded std usages found by hir analysis
    pub std_usage_matches: Vec<ReadableSpan>,
    // Std spans that are std in every covering run but whose probe never
    // compiled, so they were never shown avoidable — see
    // `Telemetry::unproven_std_spans`.
    pub unproven_std_usage_matches: Vec<ReadableSpan>,
    pub telemetry: Option<Telemetry>,
    pub coverage_comparison: Option<types::CoverageComparison>,
}

impl AllStats {
    pub fn new(name: String) -> Self {
        Self {
            name,
            compilation_res: Vec::new(),
            crate_info: None,
            std_usage_matches: Vec::new(),
            unproven_std_usage_matches: Vec::new(),
            telemetry: None,
            coverage_comparison: None,
        }
    }

    /// Save all the stats to the respective files.
    /// Also restore the original Cargo.toml from the backup.
    /// # Arguments
    /// * `manifest` - If true, restore the original Cargo.toml file.
    pub fn dump(&mut self, manifest: bool) {
        let stats_dir = path::Path::new(consts::RESULTS_PATH)
            .join(self.name.replace("-", "_").replace(":", "-"));

        println!("Dumping stats to directory: {:?}", stats_dir);

        let dir = std::path::Path::new(consts::DOWNLOAD_PATH).join(self.name.replace(':', "-"));
        if manifest {
            let manifest = parser::determine_manifest_file(&self.name, None);
            // Copy the current Cargo.toml to a backup for later use.
            fs::copy(&manifest, dir.join("Cargo.toml.modified"))
                .context("Failed to backup original Cargo.toml")
                .unwrap();
            fs::copy(dir.join("Cargo.toml.bak"), &manifest)
                .context("Failed to restore original Cargo.toml")
                .unwrap();
            fs::remove_file(dir.join("Cargo.toml.bak"))
                .context("Failed to remove backup Cargo.toml")
                .unwrap();
        }

        std::fs::create_dir_all(&stats_dir).unwrap();
        let crate_info_file = stats_dir.join("crate_info.json");
        let compilation_res_file = stats_dir.join("compilation_results.json");
        let std_usage_file = stats_dir.join("std_usages.json");
        let telemetry_file = stats_dir.join("telemetry.json");
        if let Some(crate_info) = &self.crate_info {
            let crate_info_data = serde_json::to_string_pretty(crate_info).unwrap();
            std::fs::write(crate_info_file, crate_info_data).unwrap();
        }
        if let Some(telemetry) = &mut self.telemetry {
            // Filled here rather than at the failure sites: the visitor collects
            // them process-wide (main crate and every dependency), and `dump` is
            // the one point every exit path funnels through.
            telemetry.files_syn_failed = visitor::syn_failed_files();
            telemetry.cargo_metadata_failed = visitor::cargo_metadata_failures();
            let telemetry_data = serde_json::to_string_pretty(telemetry).unwrap();
            std::fs::write(telemetry_file, telemetry_data).unwrap();
        }
        let compilation_res_data = serde_json::to_string_pretty(&self.compilation_res).unwrap();
        let std_usage_data = serde_json::to_string_pretty(&self.std_usage_matches).unwrap();
        std::fs::write(compilation_res_file, compilation_res_data).unwrap();
        std::fs::write(std_usage_file, std_usage_data).unwrap();
        // Written unconditionally, like `std_usages.json`: an empty file is the
        // positive statement that nothing was left unproven, which is what
        // separates a proven clearance from a quiet one.
        let unproven_data = serde_json::to_string_pretty(&self.unproven_std_usage_matches).unwrap();
        std::fs::write(stats_dir.join("unproven_std_usages.json"), unproven_data).unwrap();
        if let Some(cov) = &self.coverage_comparison {
            let cov_data = serde_json::to_string_pretty(cov).unwrap();
            std::fs::write(stats_dir.join("coverage_comparison.json"), cov_data).unwrap();
        }
        // Written here rather than at the end of `main` because every exit path
        // — proc-macro bail, not-no_std bail, dep-not-no_std bail, success —
        // funnels through `dump`. A run that dies early is exactly the one whose
        // time budget is worth knowing.
        let timing = timing::report(&self.name);
        let timing_data = serde_json::to_string_pretty(&timing).unwrap();
        std::fs::write(stats_dir.join("timing.json"), timing_data).unwrap();
    }
}

/// One dependency the initial verification pass found not to support no_std,
/// with enough context to check the verdict without re-running the tool.
///
/// `dep_not_no_std` on its own names nobody: it was set from a `bool` that had
/// already thrown away which dependency produced it, at what depth, and on what
/// evidence. Every crate in that bucket therefore reported a verdict it could
/// not justify.
#[derive(Debug, Serialize)]
pub struct DepNoStdFailure {
    /// `name:version` of the dependency that is not no_std.
    pub dep: String,
    /// `name:version` of the crate that depends on it — the main crate for a
    /// direct dependency.
    pub parent: String,
    /// 0 for a direct dependency of the main crate, 1 for a dependency of one
    /// of those, and so on.
    pub depth: u32,
}

/// A proc macro that injected `std` into a crate whose manifest has no edge to
/// it, so the parking cannot turn its `std` feature off (**KI-22**).
///
/// The parking writes `default-features = false` on an edge of the consumer's
/// own manifest; a macro two levels down is not such an edge. Reported instead
/// of leaving the run to fail on `E0463 can't find crate for std` at a span the
/// crate never wrote. See `driver::report_unreachable_proc_macro_injectors`.
#[derive(Debug, Serialize)]
pub struct UnreachableProcMacro {
    /// The proc-macro crate the records were attributed to.
    pub macro_crate: String,
    /// `name:version` of the crate it injected std into.
    pub consumer: String,
    /// Downloaded crates that declare an edge to it — how it entered the graph.
    /// Best effort: who *could* have brought it in, not who resolved it.
    pub parents: Vec<String>,
    /// How many std records the pass attributed to it.
    pub records: usize,
}

/// A crate below a direct dependency that links std under a feature **no edit
/// to the root manifest can turn off**, because some crate on the way up hands
/// that feature over on a non-optional edge (R34-20's residual).
///
/// `parser::transitive_forbidden_dep_features` translates a leaf's verdict up
/// the edges it was reached through, and a hop it cannot express ends the chain
/// — that silence is `DEP_TREE_TRANSITIVE_STD`'s boundary and is deliberate.
/// The silence is right for *this* run and wrong for the ecosystem: the edge
/// that hard-enables the feature is a line in a published manifest, and the
/// repair is an upstream one. Recorded so those edges can be reported rather
/// than only survived.
///
/// Non-optional is the whole of it. The same shape on an optional edge *is*
/// repairable — the features that activate the dependency are named instead —
/// so nothing here is a case the tool could have handled and did not.
#[derive(Debug, Serialize)]
pub struct UnrepairableStdEdge {
    /// `name:version` of the crate whose feature is forced on.
    pub crate_name: String,
    /// The feature of `crate_name` that nothing above can turn off.
    pub feature: String,
    /// `name:version` of the crate whose edge supplies it — where an upstream
    /// fix would go.
    pub supplier: String,
    /// The key `supplier`'s manifest spells that edge with, which is what a
    /// patch has to edit (it is the package name unless the edge renames it).
    pub dep_key: String,
    /// How the edge supplies it: `"features"` for a name in the edge's own
    /// `features = [...]` list, `"default"` for the dependency's `default`
    /// closure reaching it with `default-features` left on.
    pub via: String,
    /// `name:version` of the crate at the bottom of the chain, the one that
    /// actually links std.
    pub links_std: String,
    /// The feature of `links_std` that links it.
    pub std_feature: String,
    /// `name:version` of the main crate's direct dependency the chain starts
    /// at — the edge the root *can* rewrite, and which this finding says would
    /// not be enough on its own.
    pub direct_dep: String,
}

/// A proc-macro dependency of the main crate, which the no_std walk skips and
/// `driver::park_injecting_proc_macros` then examines for what it injects.
///
/// Carries the resolved manifest path rather than `name:version` so the rule can
/// be driven against a fixture tree instead of only against `DOWNLOAD_PATH`.
#[derive(Debug, Clone)]
pub struct ProcMacroDep {
    /// The package name, as the consumer's dependency edge names it.
    pub package: String,
    /// Path to the proc-macro crate's own `Cargo.toml`.
    pub manifest: String,
}

/// Everything about the crate being processed is stored here.
/// This is specifically useful when we want to keep track of
/// special handling for certain crates.
/// TODO: Some fields are redundant because existance of another field implies it.
/// For example, if `unconditional_no_std` is true, then `no_std`
#[derive(Default, Debug, Serialize)]
pub struct Telemetry {
    /// Name of the crate
    pub name: String,
    /// Version of the crate
    pub version: String,
    /// Whether the crate is no_std or not
    pub no_std: bool,
    /// Is the crate a proc-macro crate
    pub is_proc_macro: bool,
    /// Did the crate have `#[no_std]` instead of `#![no_std]`
    pub wrong_unconditional_setup: bool,
    /// Number of direct dependencies
    pub num_deps: usize,
    /// Total depth traversed in the dependency graph to verify no_std
    pub deps_depth_traversed: u32,
    /// Did one of the dependencies not support no_std
    pub dep_not_no_std: bool,
    /// Which ones, and where — every dependency that produced the verdict
    /// above, not just the first. Empty exactly when `dep_not_no_std` is false.
    ///
    /// The pass no longer stops at the first offender: nothing downstream acts
    /// on `dep_not_no_std`, so stopping only truncated the download/registration
    /// of the *remaining* dependencies, which then reached the emission stage
    /// unanalysed. Verification now runs to the end and reports every violation.
    pub dep_not_no_std_deps: Vec<DepNoStdFailure>,
    /// Dependencies whose sources could not be parsed at all (`files_parsed ==
    /// 0`): no lib/bin target, or every file rejected by `syn`.
    ///
    /// Absence of a `no_std` attribute in a parse that read nothing is not
    /// evidence, so these are *not* counted as `dep_not_no_std` — the hardcoded
    /// `consts::KNOWN_SYN_FAILURES` escape hatch is the same case, handled one
    /// crate at a time.
    pub deps_no_sources_parsed: Vec<String>,
    /// Files the module walk could not read or hand to `syn`, main crate and
    /// dependencies alike (`visitor::syn_failed_files`).
    ///
    /// Such a file used to end the process — 338 of the 344 panicked runs in the
    /// run30 corpus died here (KI-19). It is now treated as contributing no
    /// items, so the crate still gets an analysis; a non-empty list means that
    /// analysis is missing whatever those files contained. Deliberately-invalid
    /// files (serde_json's `features_check/error.rs`) show up here as a matter
    /// of course and are not a defect.
    pub files_syn_failed: Vec<String>,
    /// Manifests `cargo metadata` refused (`visitor::cargo_metadata_failures`).
    ///
    /// Also once fatal: `secp256k1-sys`'s published manifest specifies no
    /// targets, which panicked every dependent. Each caller now degrades — no
    /// entrypoints, no lib target, features read from the file — so a listed
    /// manifest means that crate contributed nothing to the analysis.
    pub cargo_metadata_failed: Vec<String>,
    /// Dependencies that failed to download, as `name:version-requirement`.
    /// They are skipped by the verification pass, so a non-empty list means the
    /// no_std verdict for the tree covers fewer dependencies than it appears to.
    pub deps_download_failed: Vec<String>,
    /// Is the main crate using conditional no_std
    pub main_conditional_no_std: bool,
    /// Does the dependency use conditional no_std
    pub conditional_no_std_deps: Vec<(String, bool)>,
    /// Is the main crate using unconditional no_std
    pub main_unconditional_no_std: bool,
    /// Does the dependency use unconditional no_std
    pub unconditional_no_std_deps: Vec<(String, bool)>,
    /// If the main crate is unconditional no_std, does it have an `extern crate std;` statement
    pub direct_extern_std_usage_main: bool,
    /// List of dependencies having `extern crate std;` statement
    pub direct_extern_std_usage_deps: Vec<String>,
    /// Does the main crate import files conditionally using `cfg` attributes
    pub conditional_file_import_main: bool,
    /// List of files which are conditionally imported using `cfg` attributes
    pub conditional_file_import_deps: Vec<(String, bool)>,
    /// List of files which are conditionally imported using `cfg` attributes
    /// and contain `extern crate std;` statements in them
    pub conditional_files_with_std_main: Vec<String>,
    /// Same as above but for dependencies
    pub conditional_files_with_std_deps: Vec<(String, Vec<String>)>,
    /// Total number of features to enable for no_std build
    pub final_features_length: usize,
    /// Did the main crate not have a feature that is rqeuired for it to compile in no_std mode
    pub new_feats_added_to_main: bool,
    /// What were the new features that we added to the main crate features list
    /// Each entry is an array of features that were added for that particular dependency
    pub new_feats_added_to_main_list: Vec<String>,
    /// Did we have to add any features to the main crate features to enable some feature for a dependency
    /// This is the dependency equivalent of `new_feats_added_to_main` field
    pub custom_features_added: Vec<(String, bool)>,
    /// What were the new features that we added to the main crate features list for dependencies
    /// This is the dependency equivalent of `new_feats_added_to_main_list` field
    pub custom_features_added_list: Vec<(String, Vec<String>)>,
    /// Feature atoms the solve set true that the crate cannot actually be asked
    /// for: named by a `#[cfg(feature = "…")]` in its source, absent from its
    /// `[features]` table and not cargo's implicit feature for an optional
    /// dependency. Dropped before emission (R34-2, R34-14) and recorded here,
    /// because a crate whose only `#![no_std]` switch is such an atom cannot be
    /// made no_std through cargo at all and should be triaged, not silently
    /// emitted without it. Each entry is `(name:version, atoms)`.
    pub undeclared_feature_atoms: Vec<(String, Vec<String>)>,
    /// Did we have to modify the default features that main set for any of its dependencies
    pub default_list_modified: Vec<(String, bool)>,
    /// For each dependency whose edge was modified above, *which* author-declared
    /// values were taken out of its `[dependencies.<dep>] features = [...]` list.
    ///
    /// The bool alone could not answer that, and this is the one deletion the tool
    /// makes to something the author wrote by hand, so it is the one most worth
    /// showing in a diff. Each entry is in `removable` — the dependency's solve
    /// proved it cannot be on and the crate still be no_std — which is what
    /// licenses removing it at all; contrast `features_not_required_but_declared`,
    /// which is merely unjustified and is therefore kept.
    ///
    /// Note this is the dependency *edge*. `unnecessary_features_removed_list` is
    /// the sibling record for the main crate's own `[features]` table, where the
    /// deleted values are `<dep>/<feat>` strings inside a feature's array.
    pub default_list_modified_list: Vec<(String, Vec<String>)>,
    /// Did we change the default-features to false for any dependency
    pub default_true_unset_deps: Vec<(String, bool)>,
    /// Edges below a direct dependency that hand a std-linking feature over
    /// non-optionally, so no root-manifest edit reaches them (R34-20 residual).
    ///
    /// The walk that carries a leaf's verdict up an edge chain stops at such a
    /// hop and claims nothing, which is correct and is also the whole of what
    /// the run can do about it. Each entry names the published manifest line
    /// that would have to change, for an upstream report.
    pub unrepairable_std_edges: Vec<UnrepairableStdEdge>,
    /// Proc-macro defaults parked on the edge, as `<package>/<feature>` (O-9). A
    /// proc-macro's features are the *consumer's* — they select which tokens the
    /// macro injects into this crate — so one of them is turned off like any
    /// other dependency's std, even though the macro crate itself is exempt from
    /// the no_std walk. Every entry here is one the compiler attributed std to
    /// and one whose removal was verified by a build: see
    /// `driver::park_injecting_proc_macros`.
    pub proc_macro_std_parked: Vec<String>,
    /// Proc-macro dependencies a pass caught injecting `std` into this crate
    /// (a std record whose `expansion_crate` is the macro's). A superset of what
    /// was parked — the parking still has to compile.
    pub proc_macro_std_injectors: Vec<String>,
    /// Injectors the parking cannot reach at all, because the manifest owns no
    /// edge to them (KI-22). Not a superset or subset of the field above: those
    /// are edges this crate has, these are macros it reaches only through
    /// another dependency. Accumulates across the main crate and every
    /// dependency analysed after it, so each entry names its own consumer.
    pub proc_macro_std_unreachable_injectors: Vec<UnreachableProcMacro>,
    /// Injectors where no *default of the macro* was the switch: every trial either
    /// broke the macro's own build (needs_std, bebytes_derive) or left the injected
    /// std exactly where it was. The second is not always a dead end — when the
    /// consumer's own `std` feature forwards `dep/std`
    /// (ibc-types-core-client: `std = [… "displaydoc/std" …]`), the edge default is
    /// not what holds it on and the ordinary feature solve is what turns it off.
    /// Either way the edge is left alone, and the fact is reported rather than a
    /// no-op manifest change being claimed as a fix.
    pub proc_macro_std_unparkable: Vec<String>,
    /// Parked, but with the *effect* unconfirmed: the macro built without the
    /// feature and this crate's **default** configuration then did not, so no
    /// record set could be compared. Kept because the default configuration is a
    /// std build and never the one under test — the macro's std-off expansion does
    /// not have to fit it (multiwii_serial_protocol_v2 → packed_struct_codegen).
    /// The covering runs settle it.
    pub proc_macro_std_parked_unverified: Vec<String>,
    /// Features added to repair a violated `compile_error!` after a build that
    /// failed on every target, and kept because the rebuild then succeeded
    /// (`parser::compile_error_repair_features`). Empty when nothing was
    /// violated, when no declared feature set satisfies the constraint, or when
    /// the repaired build failed too — in the last two cases
    /// `compile_error_constraint_unsatisfied` still names the crate.
    pub compile_error_repair_features: Vec<String>,
    /// Features added after a build that failed on every target because the
    /// emitted configuration was one the crate has never been compiled in, and
    /// kept because the rebuild then succeeded (`driver::enablers_for_selection`,
    /// KI-30). Distinct from `build_enabler_features`, which is the *analysis*
    /// -time search over a solved base: this one starts from the set that
    /// actually shipped. Empty when the emitted set was witnessed by a
    /// bare-metal-compiling run, when no candidate makes the crate build, or when
    /// the repaired build failed too.
    pub emitted_set_enabler_features: Vec<String>,
    /// `<dep>/<feat>` pairs added after a build that failed on every target,
    /// because a direct dependency's own configuration left it in a state
    /// nothing had shown compiles (R34-16, `parser::dep_edge_retry_candidates`).
    /// Candidates come from that dependency's own declared features, never the
    /// main crate's, and the pair is kept only because the rebuild then
    /// succeeded — the same "retry, not a smarter solve" shape as
    /// `emitted_set_enabler_features`, one edge down. Empty when every direct
    /// dependency's emitted edge already compiles, or when every retry failed
    /// too.
    pub dep_edge_enabler_features: Vec<(String, String)>,
    /// Main-crate features dropped after a build that failed on every target,
    /// kept off because the rebuild without them then succeeded (R34-23,
    /// `driver::search_removals`). The other three post-failure retries
    /// (`emitted_set_enabler_features`, `dep_edge_enabler_features`, and the
    /// analysis-time `build_enabler_features`) only ever *add* a feature to a
    /// fixed base; this is the first that asks the opposite question — is
    /// something already selected the reason the build fails. Empty when the
    /// full emitted set is minimal already, or when no subset compiles.
    pub selected_feature_removed: Vec<String>,
    /// Did we remove any unnecessary features from main crate features that main enabled for any of its dependencies
    pub unnecessary_features_removed: Vec<(String, bool)>,
    /// Features that were moved for the above case
    pub unnecessary_features_removed_list: Vec<(String, Vec<String>)>,
    /// Per dependency, the features the *author* declared on that dependency's
    /// edge which the analysis cannot justify: the dependency's own solve did not
    /// prove them (forcing them off stays SAT) and the parent reaches no item they
    /// gate. These are **kept** — a pass downstream of the solve only subtracts
    /// what it added, never what the author wrote (F15, and the uom/`bmp390` case
    /// where stripping declared values left uom with no storage type) — so this
    /// list is a report, not an action.
    ///
    /// Distinct from `unnecessary_features_removed_list`, which is the *proven*
    /// must-be-off set and does get removed. This one is "nothing shows it is
    /// needed", which is a weaker claim and the reason it is only reported.
    ///
    /// Read it when proposing upstream changes: each entry is a candidate for
    /// deletion from the crate's own Cargo.toml, to be confirmed by a build rather
    /// than taken on the tool's word — KI-27 is the standing reason the analysis
    /// can miss a requirement that is real (a trait impl nobody names).
    pub features_not_required_but_declared: Vec<(String, Vec<String>)>,
    /// List of optional dependencies that were enabled due to some other feature being enabled
    pub optional_deps_enabled: Vec<String>,
    /// List of optional dependencies that were enabled due to some other feature being enabled
    /// along with the features that enabled them
    pub optional_deps_enabled_features: Vec<(String, Vec<String>)>,
    /// List of optional dependencies that were disabled after it got enabled due to some other feature being enabled.
    /// This does not count optional dependencies that were never enabled
    pub optional_deps_disabled: Vec<String>,
    /// For the above list, the features that caused them to be enabled in the first place, that were then moved to
    /// another list.
    pub optional_deps_disabled_features_moved: Vec<(String, Vec<String>)>,
    /// Features dropped from the emitted set after the first build failed and a
    /// retry without them succeeded. Each exists only to link an optional
    /// dependency that turned out to be unusable for the target (KI-11), which
    /// no dependency-level check can predict — the retry is the evidence.
    pub optional_dep_features_dropped: Vec<String>,
    /// The injected `<dep>/<feat>` pairs dropped from the emitted set after the
    /// first build failed on every target and a retry without them succeeded
    /// (R34-11). Each was parked in `custom_no_std_feature_enabled` because a
    /// dependency's own isolated solve asked for it and no feature of the main
    /// crate reached it; that solve cannot see the rest of the graph, so the pair
    /// can be a legal feature of the dependency and still skew a sibling or select
    /// a target-incompatible path. Only a build says which, and the retry is the
    /// evidence. Empty when nothing was injected, or when the retry failed too —
    /// the emitted set then still carries the injection.
    pub injected_dep_features_dropped: Vec<String>,
    /// Dependencies whose chosen feature assignment makes an optional-dep
    /// enabler mandatory, where that enabler is *not* in the feature list we
    /// emit for the dependency (KI-12).
    ///
    /// The main crate gets these added back in `bin/main.rs` via
    /// `solver::forced_optional_dep_enablers`; `process_dep_crate` never ran
    /// that step, so a dependency with the bucket-11 shape
    /// (`#[cfg(not(feature = "std"))] use hashbrown::…`, `hashbrown` optional
    /// with only its implicit feature) can be emitted without the dependency
    /// its own no_std half imports. Observation only — nothing is added to the
    /// feature list. A non-empty entry here is the repro KI-12 is waiting for;
    /// an entry whose enablers are already implied by the dep's `[features]`
    /// table (rand 0.8's `serde1 = ["serde", …]`) is benign and expected.
    pub dep_missing_optional_dep_enablers: Vec<(String, Vec<String>)>,
    /// Was the crate build successful for any target
    pub build_success: bool,
    /// Number of targets the crate built successfully for
    pub build_success_count: u32,
    /// List of targets the crate built successfully for
    pub build_success_targets: Vec<String>,
    /// List of targets the crate failed to build for
    pub build_fail_targets: Vec<String>,
    /// Did we have to do hir analysis to check for unguarded std usages
    pub hir_analysis_done: bool,
    /// Did we find any unguarded std usages
    pub unguarded_std_usages: bool,
    /// How many std spans were excused because they sit under a cfg naming no
    /// feature (`target_arch`, `target_os`, `test`, build-script `--cfg`, …).
    ///
    /// These are accepted as guarded without being probed — the target is the
    /// consumer's choice, not an axis this tool controls. A crate that clears
    /// only because of these, with an empty final condition, is a materially
    /// weaker result than one that clears on a real feature condition, so the
    /// count is recorded to keep the two separable in the eval.
    pub externally_gated_spans: usize,
    /// How many probe conditions were dropped because a covering run already
    /// showed the span present and non-std with that condition false — i.e. the
    /// prober blamed a feature that only *contains* the code. See
    /// `phases::condition_contradicted_by_runs`; uom 0.36's storage features are
    /// the case it was written for.
    pub conditions_contradicted_by_runs: usize,
    /// How many probe conditions were dropped because a purpose-built run — one
    /// that *satisfies* the span's gate under the hard constraints — compiled
    /// with the span not std. The same veto as above for spans no covering run
    /// ever witnessed; see `phases::gate_satisfied_std_spans`. zeno 0.3.2's
    /// `eval` is the feature it exists to keep.
    pub conditions_refuted_by_gate_run: usize,
    /// Whether the final selection had to be corrected to satisfy the crate's
    /// own `#![cfg_attr(<cond>, no_std)]` — a feature the condition entails was
    /// missing, or one it forbids was still on. Set by
    /// `parser::enforce_no_std_polarity` (R31-3). A crate that never needed the
    /// repair leaves this false, so it counts the bucket rather than the fix.
    pub no_std_polarity_restored: bool,
    /// How many std spans share a source position with records from another
    /// crate *and* resolve to std in every covering run.
    ///
    /// A `#[derive(...)]` attribute span collects the whole expansion under one
    /// position, so a single source location routinely emits std and core
    /// records at once. Such a span is not avoidable — no run exists in which
    /// it is std-free — but it used to be classified `Conditional` purely
    /// because of the co-located records, and `Conditional` never reaches
    /// `all_hard`. This counts the spans that hinge on that distinction.
    pub collided_std_spans: usize,
    /// Covering runs that compiled only on the host *and* only because no
    /// bare-metal attempt ever reached this crate — every one died inside a
    /// dependency. Such a run is not a no_std environment (the deps keep their
    /// own default `std` features), so its std records are ignored by
    /// `phases::classify_spans` and a probe that ends there reports unproven
    /// rather than `StillStd`. Non-zero means part of this crate's evidence was
    /// discounted for that reason.
    ///
    /// A high-water mark over every analysis that shares this `Telemetry` (the
    /// main crate and then each dependency), so a later zero cannot erase it.
    pub std_inconclusive_runs: usize,
    /// The predicate of a crate-root `#![cfg_attr(<pred>, no_std)]` that names no
    /// feature and that rustc can decide — `target_arch = "spirv"`, `target_os =
    /// "cuda"`, `target_os = "none"`. `None` for every crate whose no_std switch
    /// is a feature (nearly all of them) and for one rustc does not derive from
    /// the target at all (`not(test)`, `docsrs`, a build-script cfg — see
    /// `target_cfg::is_decidable`). Measured over the 20789-crate corpus: 789
    /// crate roots carry a non-feature predicate, of which 46 are decidable.
    ///
    /// Such a crate is no_std on some targets and a plain std crate on the rest,
    /// so a run that compiled only on the host may say nothing about its
    /// no_std-ness — see `driver::HOST_NOT_NO_STD`, which is what decides that.
    pub no_std_cfg_predicate: Option<String>,
    /// Which members of `consts::TARGET_LIST` satisfy that predicate, i.e. where
    /// this crate is actually `#![no_std]`.
    ///
    /// **Empty is the interesting value**: the crate is no_std only in an
    /// environment this tool cannot build at all (`target_arch = "spirv"` — rustc
    /// has no spirv target; the codegen backend is out of tree), so no verdict
    /// about its std usage can rest on a compile. Non-empty and still failing is
    /// the ordinary case of a target that exists and did not compile (cuda_std
    /// 0.2.2 → `nvptx64-nvidia-cuda`, which fails on removed language features).
    ///
    /// Empty is never "undecided" — an undecidable predicate leaves
    /// `no_std_cfg_predicate` `None` and never reaches this field, precisely so
    /// the two cannot be confused.
    pub no_std_predicate_targets: Vec<String>,
    /// Set when every `TARGET_LIST` member has failed on the emitted feature
    /// set and a post-failure probe of that *same, unchanged* argv against one
    /// representative OS target (`consts::OS_TARGET_PROBES`) built —
    /// the target that built, e.g. `x86_64-unknown-linux-gnu`. R34-17's shape:
    /// a crate whose platform layer is gated by `target_os` with no arm on any
    /// of our bare-metal-only targets (`sc-0.2.7`'s `#[cfg(target_os =
    /// "linux")] mod platform;`, no other arm) is otherwise indistinguishable
    /// from one that is genuinely not no_std-capable anywhere.
    ///
    /// **This is not a verification and not a verdict.** It says the crate
    /// compiles *given an operating system* under the argv the tool already
    /// emitted — nothing more. It does not confirm the crate is intentionally
    /// OS-only rather than a bare-metal target the author simply never wired
    /// up, and it says nothing about whether the emitted argv was even the
    /// right one — only that swapping `--target` changed the outcome. Treat
    /// it as a hint for `SCOPE` classification, not as std-verification the
    /// way a `TARGET_LIST` pass is.
    ///
    /// The probe's own build/fail records are never kept (`mark_build_records`
    /// / `rewind_build_records` around it) — an OS target is not a member of
    /// `TARGET_LIST`, so counting it there would corrupt `build_success_count`
    /// and the per-target success/fail lists that a `HARD` verdict is read
    /// off. `None` means either every probe target failed too, or the crate
    /// never reached this step (something else in the retry chain already
    /// succeeded).
    pub os_target_probe: Option<String>,
    /// Std records that inherited a `#[cfg]` from the import that bound their
    /// name, summed over the covering runs (see
    /// `driver::resolve_import_to_use_gateways`).
    ///
    /// These spans carry no attribute of their own, so before the join they
    /// reached the prober with no gate at all and were short-circuited to
    /// `StillStd` — reported as unguarded std usage without a single compile.
    /// A non-zero count means the crate re-exports std items through a gated
    /// import.
    pub routed_import_anchors: usize,
    /// How many std spans were dropped because their probe could never compile
    /// (`ProbeDecision::CompileFailed` — broken dep tree or unsatisfiable
    /// feature combo).
    ///
    /// `all_hard` keeps only `StillStd`, so a `CompileFailed` span silently
    /// disappears from `std_usages.json` with no counter — a false *negative*
    /// of the collided-span family: a crate whose only hard std sits behind an
    /// uncompilable probe reads clean. This observation makes the quiet
    /// clearance visible (KI-7 routes far more spans into it).
    pub compile_failed_spans: usize,
    /// The `AlwaysStd` subset of `compile_failed_spans` — the spans that leave
    /// the crate's std-ness genuinely *unknown*.
    ///
    /// A `Conditional` span that fails its probe is excluded: a covering run
    /// already exists in which it produced no std record, so only its *condition*
    /// is unpinned. What is left is std in every run and never shown avoidable.
    ///
    /// These do not go in `std_usages.json`, which asserts *proven* unavoidable
    /// std usage and would gain false positives from the (b) case — a probe that
    /// failed for a reason unrelated to the span (broken dep tree, infeasible
    /// combo). They go in `unproven_std_usages.json` instead, and a non-empty
    /// list stops the crate reading clean. Measured over the 12289-crate corpus
    /// before this split: 166 crates had `compile_failed_spans > 0`, 55 of them
    /// reported no std usage at all, and none of those 55 produced a config that
    /// built — so the separation costs no crate that currently works.
    pub unproven_std_spans: usize,
    /// Why each of those probes came back with nothing — the distinct
    /// `ProbeDecision::CompileFailed` reasons behind `unproven_std_spans`,
    /// deduplicated and in first-seen order.
    ///
    /// The count alone is what made `PROBE_SET_INFEASIBLE` (R31-6, 40 crates)
    /// untriageable: every row read "every std span unproven, run aborts before
    /// emitting" and nothing distinguished *the crate cannot compile with this
    /// gate negated, whatever else is enabled* — which is the crate's problem and
    /// not the tool's — from *the configuration the probe happened to carry does
    /// not build*, which is. The compiler already answered that question in the
    /// build the probe ran; this is where the answer is kept.
    pub unproven_std_span_reasons: Vec<String>,
    /// Of `unproven_std_spans`, how many were excused as a host-only-run
    /// don't-care (R34-3, `driver::host_only_downgrade_is_safe`) rather than
    /// left blocking. Excused spans stay `CompileFailed` — `all_hard` and
    /// `final_condition` never see them differently — only the fatal
    /// `unproven` exit in `bin/main.rs` no longer counts them, so a
    /// configuration is emitted instead of `[]`. Kept as a count, not silence:
    /// the whole reason for the fatal exit this widens was that a quiet
    /// clearance should not pass for a proven one.
    pub host_only_excused_spans: usize,
    /// Features `driver::discover_build_enablers` proved the crate cannot build
    /// for any bare-metal target without (bevy_input's `libm`).
    ///
    /// Non-empty only for a crate where no covering run compiled off the host, so
    /// it doubles as the marker for "this run went down the T2 recovery path".
    /// They are pinned true for the probes and folded into the emitted config.
    pub build_enabler_features: Vec<String>,
    /// Maximum length of constraint string while solving features
    pub max_contraint_length: Vec<(String, usize)>,
    /// Maximum depth of constraint string while solving features
    pub max_constrait_depth: Vec<(String, usize)>,
    // All wall-clock accounting now lives in `timing.json` (see `crate::timing`).
    // The `*_time_ms` scalars that used to sit here measured three isolated calls
    // and nothing else — in particular `hir_driver_time_ms` was never assigned at
    // all, and none of them survived a dependency analysis, whose `Telemetry` is
    // thrown away by the caller.
    /// Did we do a recursive requirement check for dependencies at the end
    pub recursive_requirement_check_done: bool,
    /// Did the recursive requirement check fail
    pub recursive_requirement_check_failed: bool,
    /// If the above is true, which dependency caused it to fail
    pub recursive_requirement_check_failed_dep: Option<String>,
    /// Human-readable diagnostic for every misconfigured dependency/feature
    /// pair found during the recursive requirement check (not just the
    /// first one) — e.g. a dependency requiring a feature its parent never
    /// enables, or a parent forcing on a feature the dependency does not
    /// need and does not protect via its own item usage.
    pub recursive_requirement_check_violations: Vec<String>,
    /// Per dependency, the features its own solve put in `enable` that nothing
    /// justifies: the solve did not prove them (forcing them off stays SAT) and
    /// the parent reaches no item they gate. These are Z3 don't-cares, and the
    /// recursive check no longer reports them as no_std requirements. Recorded
    /// because the same set is what a manifest-writing pass would have to stop
    /// emitting — `custom_features_added_list` is where they land today.
    pub unjustified_enable_features: Vec<(String, Vec<String>)>,
    /// Per crate, the features a `#[cfg]` condition would have forced on that the
    /// crate's own no_std condition forbids (directly, or through its `[features]`
    /// table). The condition is dropped rather than asserted: it claimed the crate
    /// is no_std only when it enables std, which cannot be what the author meant
    /// and is not a configuration worth emitting. `bitcoin 0.32`'s
    /// `all(secp-recovery, base64, rand-std)` forcing `rand-std = ["std", ...]` is
    /// the shape.
    pub self_contradictory_cfg_equations: Vec<(String, Vec<String>)>,
    /// Per crate, the features that came out of the solve as *entailed true* only
    /// because a `#[cfg]` condition was asserted — the crate's own no_std condition
    /// does not require them.
    ///
    /// This is the split `entailed_true` cannot make on its own, and it is the list
    /// to read when proposing a dependency-edge change upstream: everything here is
    /// a feature the emitted manifest asks for that no statement about no_std
    /// justifies. num-complex 0.4.6 — `#![no_std]` unconditionally — reports `libm`
    /// here, from `#[cfg(any(feature = "std", feature = "libm"))]` asserted under
    /// `not(std)`.
    ///
    /// Not evidence the feature is unwanted: KI-27 (a trait impl nobody names) is the
    /// standing case where a genuinely required feature has nothing to justify it in
    /// this analysis. Confirm with a build before dropping one.
    pub features_forced_by_cfg_assertion: Vec<(String, Vec<String>)>,
    /// List of unknown keywords found in attributes
    pub unknown_idents_in_attrs: bool,
    /// List of unknown keywords found in attributes for dependencies
    pub unknown_idents_in_attrs_deps: Vec<(String, bool)>,
    /// When the implicit conditions + hard constraints are considered together with a seed, we are getting Unsat. This means the code guarded by this
    /// condition is dead code.
    pub unsatisfied_features: Vec<(String, Vec<String>)>,
    /// Crates where the solved feature set did not satisfy an excluded compile_error constraint.
    /// An excluded constraint is one whose features have no overlap with the no_std condition features,
    /// so it was not added to the solver's filtered list.
    pub compile_error_constraint_unsatisfied: Vec<String>,
    /// Main crate whose hard constraints alone are unsatisfiable (parent-imposed requirements are
    /// internally contradictory). Stores the stringified hard constraint.
    pub hard_unsat_main: Option<String>,
    /// Dependencies whose hard constraints alone are unsatisfiable: (crate:version, condition).
    pub hard_unsat_deps: Vec<(String, String)>,
    /// Main crate whose hard constraints together with the no_std equation are unsatisfiable
    /// (the crate has no viable no_std configuration under the constraints). Stores the condition.
    pub hard_with_main_unsat_main: Option<String>,
    /// Dependencies whose hard constraints + no_std equation are unsatisfiable: (crate:version, condition).
    pub hard_with_main_unsat_deps: Vec<(String, String)>,
}

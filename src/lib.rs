#![feature(rustc_private)]

use anyhow::Context;
use bincode::{Decode, Encode};
use lazy_static::lazy_static;
use proc_macro2::Span;
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
pub mod timing;
pub mod types;
pub mod visitor;

pub mod hir_driver;

use crate::types::*;
use std::collections::HashSet;

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
    /// The main crate's no_std enable list — used by finalize_dep_crate to
    /// check if a main [features] entry references a protected dep feature.
    pub main_enable: Vec<String>,
    /// (dep_crate_name_norm, feat_name) pairs that must not be removed from
    /// either the dep declaration or the main crate's [features] table.
    /// Accumulated across finalize_dep_crate calls and consumed by
    /// move_unnecessary_dep_feats.
    pub protected_dep_features: HashSet<(String, String)>,
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
        let unproven_data =
            serde_json::to_string_pretty(&self.unproven_std_usage_matches).unwrap();
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
    /// If the main crate is unconditional no_std, does it have a dependency which has `extern crate std;` statement
    pub indirect_extern_std_usage: bool,
    /// If the above is true, what is the depth of the dependency which has `extern crate std;` statement
    pub indirect_extern_std_usage_depth: u32,
    /// If the above is true, what is the name of the dependency which has `extern crate std;` statement.
    /// This will be None if the crate is using conditional no_std or is unconditional no_std without any extern crate std usage
    pub indirect_extern_std_usage_crate: Option<String>,
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
    /// Did we have to modify the default features that main set for any of its dependencies
    pub default_list_modified: Vec<(String, bool)>,
    /// Did we change the default-features to false for any dependency
    pub default_true_unset_deps: Vec<(String, bool)>,
    /// Did we remove any unnecessary features from main crate features that main enabled for any of its dependencies
    pub unnecessary_features_removed: Vec<(String, bool)>,
    /// Features that were moved for the above case
    pub unnecessary_features_removed_list: Vec<(String, Vec<String>)>,
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

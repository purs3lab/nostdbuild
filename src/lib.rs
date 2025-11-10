#![feature(rustc_private)]

use anyhow::Context;
use bincode::{Decode, Encode};
use lazy_static::lazy_static;
use serde::{Deserialize, Serialize};
use std::{fs, sync::Mutex};

pub mod compiler;
pub mod consts;
pub mod db;
pub mod downloader;
pub mod hir;
pub mod parser;
pub mod solver;

pub mod hir_driver;

lazy_static! {
    // This is a list of all dependencies for a crate.
    // TODO: Convert this to a variable passed between functions instead of a global variable
    pub static ref DEPENDENCIES: Mutex<Vec<String>> = Mutex::new(Vec::new());
}

/// A vector of (String, String) tuples.
pub type TupleVec = Vec<(String, String)>;

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

/// We store already resolved features for a crate
/// to be compiled as no_std in a db file.
/// This is the structure of the db file.
#[derive(Debug, Encode, Decode)]
pub struct DBData {
    pub name_with_version: String,
    pub features: (Vec<String>, Vec<String>),
}

#[derive(Debug, Default, Serialize)]
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

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub struct ReadableSpan {
    file: String,
    start_line: usize,
    start_col: usize,
    end_line: usize,
    end_col: usize,
}

impl ReadableSpan {
    pub fn contains(&self, other: &ReadableSpan) -> bool {
        if self.file != other.file {
            return false;
        }
        if self.start_line > other.start_line || self.end_line < other.end_line {
            return false;
        }
        if self.start_line == other.start_line && self.start_col > other.start_col {
            return false;
        }
        if self.end_line == other.end_line && self.end_col < other.end_col {
            return false;
        }
        true
    }
}

#[derive(Debug, Default, Serialize)]
pub struct AllStats<'a> {
    pub name: String,
    pub compilation_res: Vec<Results>,
    pub crate_info: Option<&'a CrateInfo>,
    // Collects all unguarded std usages found by hir analysis
    pub std_usage_matches: Vec<ReadableSpan>,
    pub telemetry: Option<Telemetry>,
}

impl<'a> AllStats<'a> {
    pub fn new(name: String) -> Self {
        Self {
            name,
            compilation_res: Vec::new(),
            crate_info: None,
            std_usage_matches: Vec::new(),
            telemetry: None,
        }
    }

    /// Save all the stats to the respective files.
    /// Also restore the original Cargo.toml from the backup.
    pub fn dump(&mut self) {
        let stats_dir = format!(
            "{}/{}",
            consts::RESULTS_PATH,
            self.name.replace("-", "_").replace(":", "_")
        );

        let dir = std::path::Path::new(consts::DOWNLOAD_PATH).join(self.name.replace(':', "-"));
        let manifest = parser::determine_manifest_file(&self.name);
        fs::copy(dir.join("Cargo.toml.bak"), &manifest)
            .context("Failed to restore original Cargo.toml")
            .unwrap();
        fs::remove_file(dir.join("Cargo.toml.bak"))
            .context("Failed to remove backup Cargo.toml")
            .unwrap();

        std::fs::create_dir_all(&stats_dir).unwrap();
        let crate_info_file = format!("{}/crate_info.json", stats_dir);
        let compilation_res_file = format!("{}/compilation_results.json", stats_dir);
        let std_usage_file = format!("{}/std_usages.json", stats_dir);
        let telemetry_file = format!("{}/telemetry.json", stats_dir);
        if let Some(crate_info) = &self.crate_info {
            let crate_info_data = serde_json::to_string_pretty(crate_info).unwrap();
            std::fs::write(crate_info_file, crate_info_data).unwrap();
        }
        if let Some(telemetry) = &self.telemetry {
            let telemetry_data = serde_json::to_string_pretty(telemetry).unwrap();
            std::fs::write(telemetry_file, telemetry_data).unwrap();
        }
        let compilation_res_data = serde_json::to_string_pretty(&self.compilation_res).unwrap();
        let std_usage_data = serde_json::to_string_pretty(&self.std_usage_matches).unwrap();
        std::fs::write(compilation_res_file, compilation_res_data).unwrap();
        std::fs::write(std_usage_file, std_usage_data).unwrap();
    }
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
    /// Maximum length of constraint string while solving features
    pub max_contraint_length: Vec<(String, usize)>,
    /// Maximum depth of constraint string while solving features
    pub max_constrait_depth: Vec<(String, usize)>,
    /// Time taken for hir driver analysis in milliseconds
    pub hir_driver_time_ms: u128,
    /// Time taken for constraint solving in milliseconds
    /// This includes the time taken to filter out possible equations
    pub constraint_solving_time_ms: Vec<(String, u128)>,
    /// Time taken for initial recrusive visit to verify dependencies are no_std in milliseconds
    pub initial_dep_verification_time_ms: u128,
    /// Did we do a recursive requirement check for dependencies at the end
    pub recursive_requirement_check_done: bool,
    /// Time taken for recursive requirement check in milliseconds
    pub recursive_requirement_check_time_ms: u128,
    /// Did the recursive requirement check fail
    pub recursive_requirement_check_failed: bool,
    /// If the above is true, which dependency caused it to fail
    pub recursive_requirement_check_failed_dep: Option<String>,
}

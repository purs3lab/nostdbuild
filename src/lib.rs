#![feature(rustc_private)]

use bincode::{Decode, Encode};
use lazy_static::lazy_static;
use serde::{Deserialize, Serialize};
use std::sync::Mutex;

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
    pub deps_and_features: Vec<(CrateInfo, Vec<String>)>,
    pub features: Vec<(String, Vec<(String, String)>)>,
    pub default_features: bool,
    pub optional: bool,
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
    // pub telemetry: Telemetry,
}

impl<'a> AllStats<'a> {
    pub fn new(name: String) -> Self {
        Self {
            name,
            compilation_res: Vec::new(),
            crate_info: None,
            std_usage_matches: Vec::new(),
            // telemetry: Telemetry::default(),
        }
    }

    /// Save all the stats to the respective files.
    pub fn shutdown(&mut self) {
        let stats_dir = format!(
            "{}/{}",
            consts::RESULTS_PATH,
            self.name.replace("-", "_").replace(":", "_")
        );
        std::fs::create_dir_all(&stats_dir).unwrap();
        let crate_info_file = format!("{}/crate_info.json", stats_dir);
        let compilation_res_file = format!("{}/compilation_results.json", stats_dir);
        let std_usage_file = format!("{}/std_usages.json", stats_dir);
        if let Some(crate_info) = &self.crate_info {
            let crate_info_data = serde_json::to_string_pretty(crate_info).unwrap();
            std::fs::write(crate_info_file, crate_info_data).unwrap();
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
    /// Number of direct dependencies
    pub num_deps: usize,
    /// Total depth traversed in the dependency graph to verify no_std
    pub deps_depth_traversed: usize,
    /// Is the main crate not no_std
    pub crate_not_no_std: bool,
    /// Did one of the dependencies not support no_std
    pub dep_not_no_std: bool,
    /// Is the main crate using conditional no_std
    pub conditional_no_std: bool,
    /// Is the main crate using unconditional no_std
    pub unconditional_no_std: bool,
    /// If the main crate is unconditional no_std, does it have an `extern crate std;` statement
    pub direct_extern_std_usage: bool,
    /// If the main crate is unconditional no_std, does it have a dependency which has `extern crate std;` statement
    pub indirect_extern_std_usage: bool,
    /// If the above is true, what is the depth of the dependency which has `extern crate std;` statement
    pub indirect_extern_std_usage_depth: usize,
    /// Does the main crate import files conditionally using `cfg` attributes
    pub conditional_file_import: bool,
    /// Total number of features to enable for no_std build
    pub final_features_length: usize,
    /// Did we have to modify the default features of the main crate
    pub default_list_modified: bool,
    /// Did we remove any unnecessary features from main crate features
    pub unnecessary_features_removed: bool,
    /// Did we have to add any features to the main crate features
    pub custom_features_added: bool,
    /// Was the crate build successful for any target
    pub build_success: bool,
    /// Number of targets the crate built successfully for
    pub build_success_count: usize,
    /// List of targets the crate built successfully for
    pub build_success_targets: Vec<String>,
    /// List of targets the crate failed to build for
    pub build_fail_targets: Vec<String>,
    /// Did we have to do hir analysis to check for unguarded std usages
    pub hir_analysis_done: bool,
    /// Did we find any unguarded std usages
    pub unguarded_std_usages: bool,
    /// Maximum length of constraint string while solving features
    pub max_contraint_length: usize,
    /// Time taken for hir driver analysis in milliseconds
    pub hir_driver_time_ms: u128,
    /// Time taken for constraint solving in milliseconds
    pub constraint_solving_time_ms: u128,
    /// Time taken for initial recrusive visit to verify dependencies are no_std in milliseconds
    pub initial_dep_verification_time_ms: u128,
}

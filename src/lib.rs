use bincode::{Decode, Encode};
use lazy_static::lazy_static;
use serde::Deserialize;
use std::sync::Mutex;

pub mod consts;
pub mod db;
pub mod downloader;
pub mod parser;
pub mod solver;

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
}

/// We store already resolved features for a crate
/// to be compiled as no_std in a db file.
/// This is the structure of the db file.
#[derive(Debug, Encode, Decode)]
pub struct DBData {
    name_with_version: String,
    features: Vec<String>,
}

#[derive(Debug, Default)]
pub struct CrateInfo {
    pub name: String,
    pub version: String,
    pub deps_and_features: Vec<(CrateInfo, Vec<String>)>,
    pub features: Vec<(String, Vec<(String, String)>)>,
    pub default_features: bool,
    pub optional: bool,
}

use lazy_static::lazy_static;
use serde::Deserialize;
use std::sync::Mutex;

pub mod consts;
pub mod downloader;
pub mod parser;
pub mod solver;

lazy_static! {
    // This is a list of all dependencies for a crate.
    // Convert this to a variable passed between functions instead of a global variable
    pub static ref DEPENDENCIES: Mutex<Vec<String>> = Mutex::new(Vec::new());
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub enum Dependency {
    Simple(String),
    Complex {
        version: String,
        package: Option<String>,
        features: Option<Vec<String>>,
        #[serde(rename = "default-features")]
        default_features: Option<bool>,
    },
}

#[derive(Debug, Default)]
pub struct CrateInfo {
    pub name: String,
    pub version: String,
    pub deps_and_features: Vec<(CrateInfo, Vec<String>)>,
    pub features: Vec<String>,
    pub default_features: bool,
}

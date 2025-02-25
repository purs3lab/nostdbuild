use lazy_static::lazy_static;
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

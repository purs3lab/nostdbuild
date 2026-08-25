#![feature(rustc_private)]

extern crate rustc_plugin;

use std::process::ExitCode;

use nostd::hir_driver;

/// `cli_main` used to call `exit()` itself; since rustc_plugin 0.15 it *returns*
/// the child cargo's code instead. Returning it from `main` is what keeps a
/// failed `cargo hir` looking like a failure — dropping it makes every pass exit
/// 0, and `driver::run_rustc_plugin_pass` reads a failed probe as a clean run.
fn main() -> ExitCode {
    rustc_plugin::cli_main(hir_driver::Plugin)
}

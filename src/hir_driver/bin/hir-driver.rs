#![feature(rustc_private)]

extern crate rustc_plugin;

use std::process::ExitCode;

use nostd::hir_driver;

/// See `cargo-hir.rs`: since rustc_plugin 0.15 `driver_main` returns the rustc
/// exit code rather than calling `exit()`, so `main` has to propagate it.
fn main() -> ExitCode {
    rustc_plugin::driver_main(hir_driver::Plugin)
}

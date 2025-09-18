#![feature(rustc_private)]

extern crate rustc_plugin;

use nostd::hir_driver;

fn main() {
    rustc_plugin::cli_main(hir_driver::Plugin);
}

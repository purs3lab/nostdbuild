#![feature(rustc_private)]

extern crate rustc_plugin;

use nostd::hir_driver;

fn main() {
    rustc_plugin::driver_main(hir_driver::Plugin);
}

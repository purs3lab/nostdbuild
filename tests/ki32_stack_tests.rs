#![feature(rustc_private)]

//! KI-32: a generated dependency file must not abort the process through
//! `syn`'s recursion.
//!
//! `typenum`'s `src/gen/consts.rs` is 280 KB of `UInt<UInt<UInt<…>>>` aliases
//! nested hundreds deep. `syn` is recursive descent, so it overflows the 2 MiB
//! stack libtest gives each test thread — and a stack overflow is a process
//! abort, so this test failing takes every other test in the binary with it
//! rather than reporting.
use std::path::Path;

/// A file that used to abort the test binary parses, on the 2 MiB stack
/// libtest runs this on.
#[test]
fn a_deeply_nested_generated_file_parses_on_a_test_thread() {
    let path = Path::new(
        "/evaldisk/sourag/downloads/pasetors-0.7.4_deps/typenum-1.20.1/src/gen/consts.rs",
    );
    if !path.exists() {
        eprintln!("skipping: {} is not on disk", path.display());
        return;
    }
    let content = std::fs::read_to_string(path).unwrap();
    let file = nostd::with_syn_stack(|| syn::parse_file(&content)).expect("syn should accept it");
    assert!(
        file.items.len() > 3000,
        "expected the generated alias pile, got {} items",
        file.items.len()
    );
}

/// The same depth on the visit side: `syn::visit` recurses too, so parsing
/// with headroom and then walking without it would abort just the same.
#[test]
fn the_same_file_can_be_walked_on_a_test_thread() {
    let path = Path::new(
        "/evaldisk/sourag/downloads/pasetors-0.7.4_deps/typenum-1.20.1/src/gen/consts.rs",
    );
    if !path.exists() {
        eprintln!("skipping: {} is not on disk", path.display());
        return;
    }
    let content = std::fs::read_to_string(path).unwrap();
    nostd::with_syn_stack(|| {
        let file = syn::parse_file(&content).expect("syn should accept it");
        let mut counter = TypeCounter(0);
        syn::visit::Visit::visit_file(&mut counter, &file);
        assert!(counter.0 > 0, "the walk should have reached the types");
    });
}

struct TypeCounter(usize);

impl<'ast> syn::visit::Visit<'ast> for TypeCounter {
    fn visit_type(&mut self, t: &'ast syn::Type) {
        self.0 += 1;
        syn::visit::visit_type(self, t);
    }
}

/// The call the KI names: the whole-crate walk reaches the generated file
/// through `get_all_rs_files`, so it aborted where a single parse did.
#[test]
fn the_crate_walk_that_reaches_that_file_survives() {
    let dir = Path::new("/evaldisk/sourag/downloads/pasetors-0.7.4_deps/typenum-1.20.1");
    if !dir.exists() {
        eprintln!("skipping: {} is not on disk", dir.display());
        return;
    }
    let found = nostd::parser::parse_item_extern_crates("typenum:1.20.1", Some("pasetors:0.7.4"));
    // The assertion that matters is that we got here at all; typenum has no
    // `extern crate` items, so an empty list is the right answer.
    assert!(
        found.itemexterncrates.is_empty(),
        "typenum declares no extern crates, got {:?}",
        found.itemexterncrates
    );
}

/// The measurement caveat KI-32 forced on R34-20's step 1: called from a test
/// thread, the dep walk overflowed on four of vru-noise's five direct deps, so
/// those numbers had to be taken with `RUST_MIN_STACK` raised. It no longer
/// needs raising.
#[test]
fn the_dep_walk_that_needed_rust_min_stack_no_longer_does() {
    let deps = [
        "generic-array:0.14.9",
        "digest:0.10.7",
        "hkdf:0.12.4",
        "hmac:0.12.1",
        "aead:0.5.2",
    ];
    let root = Path::new("/evaldisk/sourag/downloads/vru-noise-1.7.1_deps");
    if !root.exists() {
        eprintln!("skipping: {} is not on disk", root.display());
        return;
    }
    let cfg = z3::Config::new();
    let ctx = z3::Context::new(&cfg);
    for dep in deps {
        if !root.join(dep.replace(':', "-")).exists() {
            eprintln!("skipping {dep}: not on disk");
            continue;
        }
        let mut telemetry = nostd::Telemetry::default();
        // The assertion is that the call returns at all: every one of these
        // reaches typenum's generated file through the transitive walk.
        let _ = nostd::parser::transitive_forbidden_dep_features(
            dep,
            "vru-noise:1.7.1",
            &ctx,
            &mut telemetry,
        );
    }
}

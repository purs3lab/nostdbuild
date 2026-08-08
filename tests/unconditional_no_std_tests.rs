#![feature(rustc_private)]

//! O-3 — a crate that is `#![no_std]` unconditionally gets a no_std condition
//! from its gated crate-root `extern crate std`.
//!
//! `ModCollector::no_std_condition` was only ever set from
//! `#![cfg_attr(<cond>, no_std)]`. A crate that is `#![no_std]` outright has no
//! such attribute, so the condition stayed `None` — and both consumers in
//! `find_feature_combs_for_all_code` are guarded on it: the baseline no_std run
//! (`if let Some(ref cond) = no_std_cond`) never fires, and `covering_set_modes`
//! returns a single arbitrary model instead of a std / no_std pair. Every
//! covering run could then have `std` on, which makes `std_in_every_run`
//! trivially true and lands every std span `AlwaysStd`.
//!
//! orchard 0.11.0 failed exactly that way — one covering run, `[unstable-frost,
//! std, test-dependencies, proptest, circuit]`, four `AlwaysStd` spans in
//! `src/keys.rs` — while `cargo check --no-default-features --lib --target
//! aarch64-unknown-none` compiled it clean.
//!
//! Such a crate does state when it links std: under `#![no_std]` no `std::`
//! path resolves without a crate-root `extern crate std`, so the no_std
//! condition is the negation of that declaration's `#[cfg]`. `parser::
//! process_crate` already derives the same equation the same way for the
//! feature-selection solve (`attrs.unconditional_no_std` → `get_item_extern_std`
//! → `eq.not()`); these tests are the coverage phase learning it too.

use std::path::{Path, PathBuf};

use nostd::visitor::ModCollector;
use z3::SatResult;
use z3::ast::Bool;

fn fixture(name: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/unconditional_no_std")
        .join(name)
}

/// The real entry point: `find_feature_combs_for_all_code` reads
/// `collector.no_std_condition` off exactly this walk. Returns the condition's
/// rendering, or `None`.
fn no_std_condition<'a>(ctx: &'a z3::Context, fixture_name: &str) -> Option<Bool<'a>> {
    let mut collector = ModCollector::new(ctx);
    collector.collect(&fixture(fixture_name), "lib");
    collector.no_std_condition
}

/// Is `cond` equivalent to `¬<feature>`? Compared semantically rather than by
/// rendering: the condition arrives as `(not (or std))` or `(not std)`
/// depending on how the parser grouped the atoms, and neither spelling is the
/// point.
fn is_negation_of(ctx: &z3::Context, cond: &Bool<'_>, feature: &str) -> bool {
    let feat = Bool::new_const(ctx, feature);
    let solver = z3::Solver::new(ctx);
    solver.assert(&cond.iff(&feat.not()).not());
    solver.check() == SatResult::Unsat
}

// ---------------------------------------------------------------------------
// The crates that were failing
// ---------------------------------------------------------------------------

/// orchard / sapling-crypto: `#![no_std]` plus `#[cfg(feature = "std")] extern
/// crate std;`. The condition is `¬std`, which is what gives the driver its
/// baseline no_std run and its std / no_std covering split.
#[test]
fn gated_root_extern_std_becomes_the_no_std_condition() {
    let ctx = z3::Context::new(&z3::Config::new());
    let cond = no_std_condition(&ctx, "orchard_shape.rs")
        .expect("an unconditional #![no_std] crate with a gated `extern crate std` has a condition");
    assert!(
        is_negation_of(&ctx, &cond, "std"),
        "expected the negation of `feature = \"std\"`, got {cond}"
    );
}

/// lyon_tessellation: the same, with `test` mixed into an `any(...)`. Erasing an
/// atom out of `any` assumes it false, which is true of every build the tool
/// asks cargo for, so the gate is `feature = "std"` and the negation is sound
/// (bucket 3C's argument, 346f239).
#[test]
fn any_erased_atom_still_yields_the_condition() {
    let ctx = z3::Context::new(&z3::Config::new());
    let cond = no_std_condition(&ctx, "lyon_shape.rs")
        .expect("`any(test, feature = \"std\")` is negatable once `test` erases");
    assert!(
        is_negation_of(&ctx, &cond, "std"),
        "expected the negation of `feature = \"std\"`, got {cond}"
    );
}

// ---------------------------------------------------------------------------
// Controls — each one is a shape the inference must NOT fire on
// ---------------------------------------------------------------------------

/// The author's own `#![cfg_attr(<cond>, no_std)]` wins. Here it names
/// `no-std` while the `extern crate std` gate names `std`, so a condition of
/// `¬std` would prove the cfg_attr was overwritten.
#[test]
fn an_explicit_cfg_attr_condition_is_not_overwritten() {
    let ctx = z3::Context::new(&z3::Config::new());
    let cond = no_std_condition(&ctx, "cfg_attr_shape.rs").expect("the cfg_attr states one");
    assert!(
        is_negation_of(&ctx, &cond, "no-std"),
        "expected the cfg_attr's own `not(feature = \"no-std\")`, got {cond}"
    );
}

/// `all(target_os = "linux", feature = "std")`: erasing the target atom out of
/// an `all` assumes it *true*, so negating what is left would forbid `std` on
/// targets where the gate is false anyway. Nothing is claimed.
#[test]
fn all_erased_atom_yields_no_condition() {
    let ctx = z3::Context::new(&z3::Config::new());
    assert!(
        no_std_condition(&ctx, "all_erased_shape.rs").is_none(),
        "an `all(...)` with an erased atom cannot be negated soundly"
    );
}

/// No `extern crate std` anywhere: nothing links std, so there is no condition.
#[test]
fn no_extern_std_yields_no_condition() {
    let ctx = z3::Context::new(&z3::Config::new());
    assert!(no_std_condition(&ctx, "no_extern_std_shape.rs").is_none());
}

/// tinywasm's shape: the declaration is inside a module, so it binds `std` there
/// and says nothing about the crate root.
#[test]
fn extern_std_below_the_crate_root_yields_no_condition() {
    let ctx = z3::Context::new(&z3::Config::new());
    assert!(
        no_std_condition(&ctx, "nested_extern_std_shape.rs").is_none(),
        "an `extern crate` binds the name in its own module only"
    );
}

/// An ungated declaration means std is linked whatever the features do; the
/// gated one next to it is not the whole story, so the inference is vetoed.
#[test]
fn an_ungated_extern_std_vetoes_the_inference() {
    let ctx = z3::Context::new(&z3::Config::new());
    assert!(
        no_std_condition(&ctx, "ungated_extern_std_shape.rs").is_none(),
        "std is linked unconditionally; negating the gated declaration proves nothing"
    );
}

/// elfloader: `#[cfg(test)] extern crate std`. `should_skip` drops the item
/// before it is recorded, so no gate is seen — and rightly, since the crate is
/// no_std in every configuration cargo builds here.
#[test]
fn a_test_only_extern_std_yields_no_condition() {
    let ctx = z3::Context::new(&z3::Config::new());
    assert!(no_std_condition(&ctx, "cfg_test_shape.rs").is_none());
}

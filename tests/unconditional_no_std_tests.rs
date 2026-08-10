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

/// Is `cond` true under every assignment? The O-14 condition, for a crate that
/// links std in no configuration at all.
fn is_tautology(ctx: &z3::Context, cond: &Bool<'_>) -> bool {
    let solver = z3::Solver::new(ctx);
    solver.assert(&cond.not());
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

/// O-14: no `extern crate std` anywhere, so nothing links std in *any*
/// configuration and the condition is `true`. It used to be `None`, which left
/// the driver with no baseline run — and a crate whose only feature-gated items
/// are `#[cfg(feature = "std")]` then has `(or std)` as its one covering seed,
/// std on in every run, and every std span `AlwaysStd`.
#[test]
fn no_extern_std_yields_an_unconditional_condition() {
    let ctx = z3::Context::new(&z3::Config::new());
    let cond = no_std_condition(&ctx, "no_extern_std_shape.rs")
        .expect("a crate that never links std is no_std unconditionally");
    assert!(
        is_tautology(&ctx, &cond),
        "expected an unconditionally true condition, got {cond}"
    );
}

/// The declaration is inside an *inline* `mod` block, which is that module's own
/// binding rather than the file's. Only a file's top level counts.
#[test]
fn extern_std_inside_an_inline_module_yields_no_condition() {
    let ctx = z3::Context::new(&z3::Config::new());
    assert!(
        no_std_condition(&ctx, "nested_extern_std_shape.rs").is_none(),
        "an inline `mod`'s `extern crate` binds the name in that module only"
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
/// before it is recorded, so no gate is seen — and no *declaration* is seen
/// either, which is right: the crate is no_std in every configuration cargo
/// builds here, so O-14's `true` is the condition.
#[test]
fn a_test_only_extern_std_is_no_std_unconditionally() {
    let ctx = z3::Context::new(&z3::Config::new());
    let cond = no_std_condition(&ctx, "cfg_test_shape.rs")
        .expect("a declaration cargo never compiles does not link std");
    assert!(is_tautology(&ctx, &cond), "expected `true`, got {cond}");
}

// ---------------------------------------------------------------------------
// The declaration below the crate root — nate-common 0.1.10, tinywasm 0.8.0
// ---------------------------------------------------------------------------

/// nate-common's `src/details.rs`, tinywasm's `src/std.rs`: the crate root has no
/// `extern crate std` at all and the whole std facade is a module of its own,
/// reached unconditionally. That module's declaration links std for the crate
/// exactly as a root one would, so it is the crate's statement of when it does.
///
/// Without this, nate-common got no no_std condition, no baseline no_std run
/// (its log has no `Baseline no_std run:` line at all) and 23 `AlwaysStd` spans.
#[test]
fn a_facade_module_supplies_the_condition() {
    let ctx = z3::Context::new(&z3::Config::new());
    let cond = no_std_condition(&ctx, "submodule_facade/lib.rs")
        .expect("an unconditionally reached module's `extern crate std` is the crate's");
    assert!(
        is_negation_of(&ctx, &cond, "std"),
        "expected the negation of `feature = \"std\"`, got {cond}"
    );
}

/// Control: the module holding the facade is itself gated, so the declaration's
/// real condition carries the module's gate too. Folding that into the OR is
/// what O-1 warns about — an erased atom inside it flips meaning under the
/// negation — and leaving it out only leaves the OR too narrow, which keeps std
/// linked in a run meant to be std-off. Nothing is claimed.
#[test]
fn a_gated_facade_module_yields_no_condition() {
    let ctx = z3::Context::new(&z3::Config::new());
    assert!(
        no_std_condition(&ctx, "gated_submodule_facade/lib.rs").is_none(),
        "a conditionally compiled module does not speak for the crate"
    );
}

/// Control: an ungated declaration two levels down vetoes the root's gate, just
/// as an ungated one in the root does — std is linked whatever the features do.
/// Doubles as the proof that the fold recurses past the first level.
#[test]
fn an_ungated_extern_std_in_a_grandchild_vetoes_the_inference() {
    let ctx = z3::Context::new(&z3::Config::new());
    assert!(
        no_std_condition(&ctx, "deep_ungated_facade/lib.rs").is_none(),
        "std is linked unconditionally in a module the tree always reaches"
    );
}

// ---------------------------------------------------------------------------
// O-14 — the crate that links std in no configuration at all
//
// Two halves, both measured on the 46 crates that reach a verdict with no
// no_std condition in the log. `#![cfg_attr(not(test), no_std)]` is a bare
// `#![no_std]` for everything this tool builds, and a crate with no `extern
// crate std` anywhere has a no_std condition of `true`.
// ---------------------------------------------------------------------------

/// splay-safe-rs 0.8.3 / blas-array2 0.3.0. The payoff half: the attribute is
/// recognised as unconditional, so the *existing* O-3 negation fires and the
/// crate gets `¬std`. Before this its only covering seed was `(or std)`.
#[test]
fn a_not_test_cfg_attr_reaches_the_extern_std_negation() {
    let ctx = z3::Context::new(&z3::Config::new());
    let cond = no_std_condition(&ctx, "not_test_cfg_attr_shape.rs")
        .expect("`not(test)` is an unconditional `#![no_std]`");
    assert!(
        is_negation_of(&ctx, &cond, "std"),
        "expected the negation of `feature = \"std\"`, got {cond}"
    );
}

/// ckc-rs 0.1.15: the same attribute with nothing linking std — both halves.
#[test]
fn a_not_test_cfg_attr_with_no_extern_std_is_unconditional() {
    let ctx = z3::Context::new(&z3::Config::new());
    let cond = no_std_condition(&ctx, "not_test_no_extern_shape.rs")
        .expect("`not(test)` and no `extern crate std` is no_std unconditionally");
    assert!(is_tautology(&ctx, &cond), "expected `true`, got {cond}");
}

/// Control: `not(test)` inherits every veto the bare attribute has. It is the
/// same `unconditional_no_std` flag, not a second inference.
#[test]
fn a_not_test_cfg_attr_still_respects_an_ungated_extern_std() {
    let ctx = z3::Context::new(&z3::Config::new());
    assert!(
        no_std_condition(&ctx, "not_test_ungated_extern_shape.rs").is_none(),
        "std is linked whatever the features do"
    );
}

/// Control: a target cfg erases exactly as `test` does and must NOT be read as
/// unconditional — on `TARGET_LIST` those atoms are true or unknown, never
/// known-false. This is the line between O-14 and the 7 crates it excludes
/// (macaw, renderling, saft-sdf, rukako-shader, cuda-std, xous-ipc,
/// xous-api-names).
#[test]
fn a_target_cfg_attr_claims_nothing() {
    let ctx = z3::Context::new(&z3::Config::new());
    assert!(
        no_std_condition(&ctx, "target_cfg_attr_shape.rs").is_none(),
        "`target_arch = \"spirv\"` is not known-false the way `test` is"
    );
}

/// Control for the `any_extern_std` veto. `std_extern_gate` is only recorded at
/// a file's top level, so an inline `mod`'s declaration is invisible to it —
/// and without a separate veto the crate would read as "nothing links std" and
/// get `true`, which is wrong: it links std whenever that gate holds. Pairs
/// with `extern_std_inside_an_inline_module_yields_no_condition` above, which
/// asserts the same fixture from the other direction.
#[test]
fn an_inline_module_extern_std_vetoes_the_unconditional_inference() {
    let ctx = z3::Context::new(&z3::Config::new());
    assert!(
        no_std_condition(&ctx, "nested_extern_std_shape.rs").is_none(),
        "a declaration the gate fold skips is still a declaration"
    );
}

/// The same veto across the boundary `StdExternFacts::merge` deliberately stops
/// at: a *gated* module's facts are dropped wholesale, so `any_extern_std` has
/// to be ORed in separately by `resolve_child`. Without that, this crate — which
/// links std whenever `extras` and `std` are both on — would claim `true`.
#[test]
fn a_gated_module_extern_std_vetoes_the_unconditional_inference() {
    let ctx = z3::Context::new(&z3::Config::new());
    assert!(
        no_std_condition(&ctx, "gated_submodule_facade/lib.rs").is_none(),
        "a conditionally compiled module's `extern crate std` still links std"
    );
}

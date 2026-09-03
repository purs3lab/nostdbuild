#![feature(rustc_private)]

//! KI-3 — `driver::compile_error_infeasible_backend_constraints`.
//!
//! bulletproofs-bls-4.0.0's `compile_error!` picks between two optional-
//! dependency backends (`rust` → bls12_381_plus, `blst` → blstrs_plus), and
//! nothing in the feature solve knew that only one of them supports no_std.
//! Z3 satisfied the disjunction with `blst`, the dependency walk severed
//! blstrs_plus as not-no_std (KI-2), and the crate died on `E0433 … unlinked
//! crate blstrs_plus` instead of building through the working backend.
//!
//! This is KI-3's option 3 (see the function's own doc comment for why that
//! one and not 1 or 2): a syntactic, no-compile check of just the optional
//! dependencies a `compile_error!` disjunction names, run before the solve
//! rather than as a retry after a failed build.

use std::path::{Path, PathBuf};

use nostd::driver;

fn fixture(name: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/compile_error_backend_capability")
        .join(name)
        .join("Cargo.toml")
}

fn forbids<'a>(ctx: &'a z3::Context, name: &str) -> Vec<z3::ast::Bool<'a>> {
    let manifest = fixture(name);
    let manifest = manifest.display().to_string();
    let toml = driver::read_manifest_toml(&manifest);
    driver::compile_error_infeasible_backend_constraints(ctx, &manifest, &toml)
}

/// blstrs_plus (behind `blst`) has no `no_std` attribute at its crate root;
/// bls12_381_plus (behind `rust`) does. The one candidate that fails the
/// check must come back forbidden.
#[test]
fn the_backend_without_no_std_is_forbidden() {
    let ctx = z3::Context::new(&z3::Config::new());
    let forbid = forbids(&ctx, "backend_pick");
    assert_eq!(
        forbid.len(),
        1,
        "expected exactly one forbidden backend, got {:?}",
        forbid
    );

    let solver = z3::Solver::new(&ctx);
    for f in &forbid {
        solver.assert(f);
    }
    solver.assert(&z3::ast::Bool::new_const(&ctx, "blst"));
    assert_eq!(
        solver.check(),
        z3::SatResult::Unsat,
        "the forbidden constraint must rule out `blst`, the backend with no no_std attribute"
    );
}

/// `rust` — the working backend — must not be touched by this check at all.
#[test]
fn the_working_backend_is_left_alone() {
    let ctx = z3::Context::new(&z3::Config::new());
    let forbid = forbids(&ctx, "backend_pick");

    let solver = z3::Solver::new(&ctx);
    for f in &forbid {
        solver.assert(f);
    }
    solver.assert(&z3::ast::Bool::new_const(&ctx, "rust"));
    assert_eq!(
        solver.check(),
        z3::SatResult::Sat,
        "`rust` must stay reachable — it is the backend the fix exists to steer the solve onto"
    );
}

/// Negative control: when every backend a `compile_error!` names can build
/// no_std, nothing is forbidden. A version of this check that always fires
/// would silently narrow every crate with this shape, not just the broken
/// ones.
#[test]
fn nothing_is_forbidden_when_every_named_backend_supports_no_std() {
    let ctx = z3::Context::new(&z3::Config::new());
    let forbid = forbids(&ctx, "both_fine");
    assert!(
        forbid.is_empty(),
        "both backends support no_std; expected no forbidden features, got {:?}",
        forbid
    );
}

/// A crate with no `compile_error!` at all contributes nothing — this check
/// only ever touches a feature already named in one of the crate's own
/// disjunctions, per the function's documented scope.
#[test]
fn a_crate_without_a_compile_error_is_untouched() {
    let ctx = z3::Context::new(&z3::Config::new());
    let forbid = forbids(&ctx, "both_fine_deps/rust_backend-1.0.0");
    assert!(
        forbid.is_empty(),
        "no compile_error! here at all, got {:?}",
        forbid
    );
}

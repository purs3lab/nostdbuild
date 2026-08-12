#![feature(rustc_private)]

//! O-12(e) — a dependency's `compile_error!` reaching its dependent's solve.
//!
//! O-2 established that a crate's own `compile_error!` has to be in the solve
//! that picks its covering runs: a run that ignores it dies on the macro and
//! contributes no records. The same failure arrives from one level down and
//! nothing carried it. peniko 0.3.1 declares none of its own, solved its
//! baseline to `[]`, and died on
//!
//! ```text
//! error: color requires either the `std` or `libm` feature
//! error: kurbo requires either the `std` or `libm` feature
//! ```
//!
//! so it had no std-off run at all — while its *emitted* config is
//! `--features libm` and builds on 14 targets. What was lost was evidence.
//! 87 crates in the corpus have a baseline killed this way.
//!
//! The interesting part is the translation: a constraint over the dependency's
//! feature names says nothing until each atom is re-expressed in the consumer's.
//! `paired/libm` is reachable here through `libm` and through `math`, and the
//! two atoms `std`/`libm` are *not* the consumer's same-named features — which
//! is why this is a substitution and not an assertion of the dependency's
//! formula.

use std::path::{Path, PathBuf};

use nostd::driver;

fn fixture(name: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/dep_compile_error")
        .join(name)
        .join("Cargo.toml")
}

fn constraints_for<'a>(ctx: &'a z3::Context, name: &str) -> Vec<z3::ast::Bool<'a>> {
    let manifest = fixture(name);
    let manifest = manifest.display().to_string();
    let toml = driver::read_manifest_toml(&manifest);
    driver::dependency_compile_error_constraints(ctx, &manifest, &toml)
}

/// Is `assumption ∧ <every constraint>` satisfiable?
fn satisfiable(ctx: &z3::Context, constraints: &[z3::ast::Bool], on: &[&str], off: &[&str]) -> bool {
    let solver = z3::Solver::new(ctx);
    for c in constraints {
        solver.assert(c);
    }
    for f in on {
        solver.assert(&z3::ast::Bool::new_const(ctx, *f));
    }
    for f in off {
        solver.assert(&z3::ast::Bool::new_const(ctx, *f).not());
    }
    solver.check() == z3::SatResult::Sat
}

/// The peniko case. The consumer declares no `compile_error!` of its own, so
/// without this every constraint it is solved against is silent about the
/// dependency that will fail the build.
#[test]
fn a_dependencys_compile_error_becomes_a_constraint_on_the_consumer() {
    let ctx = z3::Context::new(&z3::Config::new());
    let constraints = constraints_for(&ctx, "consumer");
    assert!(
        !constraints.is_empty(),
        "expected the dependency's constraint to be carried up"
    );
}

/// The point of the whole thing: with std off, the solve is forced to pick the
/// feature that makes the dependency compile. peniko's baseline goes from `[]`
/// to `[libm]` exactly here.
#[test]
fn with_std_off_the_constraint_forces_the_other_disjunct() {
    let ctx = z3::Context::new(&z3::Config::new());
    let constraints = constraints_for(&ctx, "consumer");
    assert!(
        !satisfiable(&ctx, &constraints, &[], &["std", "default", "libm", "math"]),
        "no_std with neither disjunct must be refused — that is the run that died"
    );
    assert!(
        satisfiable(&ctx, &constraints, &["libm"], &["std", "default"]),
        "no_std with libm on must be allowed"
    );
}

/// `math = ["libm"]` reaches `paired/libm` transitively, and the translation has
/// to see it — otherwise the solve is forced onto one specific feature when the
/// crate offers two ways there.
///
/// Asserted on the constraint's own text as well as on satisfiability: with the
/// translation switched off there are no constraints at all, and *every*
/// satisfiability assertion passes vacuously.
#[test]
fn a_transitively_forwarding_feature_also_satisfies_it() {
    let ctx = z3::Context::new(&z3::Config::new());
    let constraints = constraints_for(&ctx, "consumer");
    let rendered: Vec<String> = constraints.iter().map(|c| format!("{:?}", c)).collect();
    assert!(
        rendered.iter().any(|c| c.contains("math")),
        "expected `math` among the features that reach paired/libm, got {:?}",
        rendered
    );
    assert!(
        satisfiable(&ctx, &constraints, &["math"], &["std", "default", "libm"]),
        "math forwards to libm forwards to paired/libm"
    );
}

/// The atoms are the *dependency's* features and must not be read as the
/// consumer's same-named ones. Nothing here may constrain `quiet`'s `std`, which
/// shares its name with a feature of the consumer and of `paired`.
#[test]
fn a_dependency_without_a_compile_error_contributes_nothing() {
    let ctx = z3::Context::new(&z3::Config::new());
    let constraints = constraints_for(&ctx, "consumer");
    let rendered: Vec<String> = constraints.iter().map(|c| format!("{:?}", c)).collect();
    assert!(
        rendered.iter().all(|c| !c.contains("quiet")),
        "quiet declares no compile_error!, got {:?}",
        rendered
    );
}

/// An optional dependency's constraint holds only in the configurations that
/// link it. Asserting it unconditionally would impose its requirement on builds
/// that never compile it.
#[test]
fn an_optional_dependencys_constraint_is_not_imposed() {
    let ctx = z3::Context::new(&z3::Config::new());
    let constraints = constraints_for(&ctx, "consumer");
    // `optional-dep` demands `std`, which the consumer cannot have in a no_std
    // run. If it were imposed, no_std would be unsatisfiable outright.
    assert!(
        satisfiable(&ctx, &constraints, &["libm"], &["std", "default"]),
        "the optional dependency's `std` demand must not reach the solve"
    );
}

/// O-12(b)'s shape, and the dangerous one. Nothing in `unreachable`'s
/// `[features]` reaches `paired/libm` or `paired/std`, so the translation has no
/// value for those atoms. Reading them as `false` makes the constraint
/// unsatisfiable, and an unsat `all_hard` costs the crate every covering run —
/// no baseline, no solved sets, every span `AlwaysStd`. Saying nothing is the
/// only safe answer.
#[test]
fn an_unreachable_dependency_feature_drops_the_constraint() {
    let ctx = z3::Context::new(&z3::Config::new());
    let constraints = constraints_for(&ctx, "unreachable");
    assert!(
        constraints.is_empty(),
        "expected no constraint, got {:?}",
        constraints
    );
}

/// The negative control for the test above: an unsatisfiable set would have been
/// produced had the atoms been read as false, so pin that the fixture's solve is
/// still open in both directions.
#[test]
fn an_unreachable_dependency_leaves_no_std_satisfiable() {
    let ctx = z3::Context::new(&z3::Config::new());
    let constraints = constraints_for(&ctx, "unreachable");
    assert!(
        satisfiable(&ctx, &constraints, &[], &["std", "default"]),
        "a std-off run must still be solvable"
    );
}

/// When the edge itself names the feature (`features = ["libm"]`), cargo turns
/// it on whatever the consumer's own features do. The constraint is satisfied by
/// the manifest as written, so nothing is added — a constraint that simplifies
/// to `true` is not a constraint.
#[test]
fn a_feature_the_edge_supplies_needs_no_constraint() {
    let ctx = z3::Context::new(&z3::Config::new());
    let constraints = constraints_for(&ctx, "pinned");
    assert!(
        constraints.is_empty(),
        "the edge already enables libm, got {:?}",
        constraints
    );
}

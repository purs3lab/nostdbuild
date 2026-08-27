#![feature(rustc_private)]

//! KI-27 — a feature required by a *trait impl* nobody names.
//!
//! The item-usage check (`feature_to_items` × `valid_cross_crate_items`) answers
//! one question: does this crate name an item that a feature gates? multiexp
//! 0.4.0 does not. It writes `groupings.zeroize()`, the method it names is
//! ungated in zeroize, and what its build needs is
//!
//! ```ignore
//! #[cfg(feature = "alloc")]
//! impl<Z> Zeroize for Vec<Z> { … }
//! ```
//!
//! — an item with no identifier, that multiexp's source never mentions. The cfg
//! is already in zeroize's module tree; what was missing is a key to address it
//! by, and a statement of *which* impl the call needed. The compiler supplies
//! the second (`ImplRecord`, from the plugin's obligation walk) and
//! `LocalItem::impl_trait` the first.
//!
//! What is tested here is the step between them: turning "this call resolved to
//! that impl" into a constraint on the dependency's feature solve. The end-to-end
//! half is `main_tests::test_multiexp`.

use std::path::{Path, PathBuf};

use nostd::driver::{self, impl_availability_requirement, reachable_impl_records};
use nostd::types::{CoveringRun, FeatureRunOutput, ImplRecord, ReadableSpan};
use nostd::visitor::{ModCollector, ModNode, collect_trait_impl_gates};
use z3::ast::Bool;

fn consumer_manifest() -> String {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/trait_impl_requirement/consumer/Cargo.toml")
        .to_string_lossy()
        .to_string()
}

fn output(impls: &[ImplRecord]) -> FeatureRunOutput {
    FeatureRunOutput {
        records: Vec::new(),
        macro_module_imports: Vec::new(),
        out_dir: None,
        impls: impls.to_vec(),
    }
}

/// One covering run: the features it was compiled with, and what it saw.
fn run(features: &[&str], impls: &[ImplRecord]) -> CoveringRun {
    CoveringRun {
        features: features.iter().map(|f| f.to_string()).collect(),
        output: output(impls),
        std_inconclusive: false,
    }
}

fn dep_dir(name: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/trait_impl_requirement/consumer_deps")
        .join(name)
}

fn dep_tree<'a>(ctx: &'a z3::Context, name: &str) -> ModNode<'a> {
    let entry = dep_dir(name).join("src/lib.rs");
    let mut collector = ModCollector::new(ctx);
    collector.collect(&entry, "lib")
}

/// A span on the line where `needle` first appears in the dependency's source,
/// the way `ImplRecord::impl_span` points at the impl the compiler selected.
fn span_of_impl(dep: &str, needle: &str) -> ReadableSpan {
    let file = dep_dir(dep).join("src/lib.rs");
    let text = std::fs::read_to_string(&file).unwrap();
    let (idx, line) = text
        .lines()
        .enumerate()
        .find(|(_, l)| l.contains(needle))
        .unwrap_or_else(|| panic!("fixture has no line containing {needle:?}"));
    ReadableSpan {
        // Deliberately *not* the fixture path: the compiler reports the registry
        // checkout it built, and `span_in_dep_tree` re-roots it. Writing the
        // fixture path here would skip the mapping this depends on.
        file: format!("/some/registry/src/index.crates.io-abcd/{dep}/src/lib.rs"),
        start_line: idx + 1,
        start_col: 0,
        end_line: idx + 1,
        end_col: line.len(),
        usage_crate: None,
    }
}

fn record(dep: &str, krate: &str, trait_name: &str, self_ty: &str, needle: &str) -> ImplRecord {
    ImplRecord {
        span: ReadableSpan {
            file: "src/lib.rs".to_string(),
            start_line: 4,
            start_col: 4,
            end_line: 4,
            end_col: 16,
            usage_crate: Some("consumer".to_string()),
        },
        trait_name: trait_name.to_string(),
        self_ty: self_ty.to_string(),
        definition_crate: krate.to_string(),
        impl_span: span_of_impl(dep, needle),
        via_macro: None,
    }
}

fn requirement<'a>(
    ctx: &'a z3::Context,
    tree: &ModNode<'a>,
    dep: &str,
    records: &[ImplRecord],
    hard: Option<&Bool<'a>>,
) -> Option<Bool<'a>> {
    impl_availability_requirement(
        ctx,
        tree,
        &dep_dir(dep),
        dep.rsplit_once('-').unwrap().0,
        records,
        hard,
    )
}

fn satisfiable(ctx: &z3::Context, req: &Bool, on: &[&str], off: &[&str]) -> bool {
    let solver = z3::Solver::new(ctx);
    solver.assert(req);
    for f in on {
        solver.assert(&Bool::new_const(ctx, *f));
    }
    for f in off {
        solver.assert(&Bool::new_const(ctx, *f).not());
    }
    solver.check() == z3::SatResult::Sat
}

/// The multiexp case. The call resolved to a `#[cfg]`-gated impl, so the feature
/// set the dependency is built with has to keep one alive.
#[test]
fn a_gated_impl_a_call_resolved_to_becomes_a_requirement() {
    let ctx = z3::Context::new(&z3::Config::new());
    let tree = dep_tree(&ctx, "gated-1.0.0");
    let recs = vec![record(
        "gated-1.0.0",
        "gated",
        "Zeroize",
        "Vec",
        r#"#[cfg(feature = "alloc")]"#,
    )];
    let req = requirement(&ctx, &tree, "gated-1.0.0", &recs, None)
        .expect("a gated impl the crate's call needs is a requirement");

    assert!(
        !satisfiable(&ctx, &req, &[], &["alloc", "portable"]),
        "with neither arm on there is no impl and the build cannot succeed"
    );
    assert!(satisfiable(&ctx, &req, &["alloc"], &["portable"]));
}

/// The requirement is the **disjunction** over the arms, not the gate of the arm
/// the compiler happened to pick. The record above came from a pass with `alloc`
/// on — a std-on pass is where these records come from, since a crate missing
/// the impl does not type check — and asserting that arm's gate would rule out
/// every other way of satisfying the same obligation. simba is the case in the
/// large: its `ComplexField for f64` has a `std` arm and a `libm` arm, and a
/// std-on pass selects the first.
#[test]
fn the_requirement_admits_an_arm_the_compiler_did_not_select() {
    let ctx = z3::Context::new(&z3::Config::new());
    let tree = dep_tree(&ctx, "gated-1.0.0");
    let recs = vec![record(
        "gated-1.0.0",
        "gated",
        "Zeroize",
        "Vec",
        r#"#[cfg(feature = "alloc")]"#,
    )];
    let req = requirement(&ctx, &tree, "gated-1.0.0", &recs, None).unwrap();

    assert!(
        satisfiable(&ctx, &req, &["portable"], &["alloc"]),
        "the second arm satisfies the same obligation and must stay reachable"
    );
}

/// Recording the arm the compiler did *not* select gives the same answer: the
/// requirement is a property of the obligation, not of the pass that observed it.
#[test]
fn either_arm_yields_the_same_requirement() {
    let ctx = z3::Context::new(&z3::Config::new());
    let tree = dep_tree(&ctx, "gated-1.0.0");
    let from_alloc = requirement(
        &ctx,
        &tree,
        "gated-1.0.0",
        &[record(
            "gated-1.0.0",
            "gated",
            "Zeroize",
            "Vec",
            r#"#[cfg(feature = "alloc")]"#,
        )],
        None,
    )
    .unwrap();
    let from_portable = requirement(
        &ctx,
        &tree,
        "gated-1.0.0",
        &[record(
            "gated-1.0.0",
            "gated",
            "Zeroize",
            "Vec",
            r#"#[cfg(all(feature = "portable""#,
        )],
        None,
    )
    .unwrap();

    for (on, off) in [
        (vec!["alloc"], vec!["portable"]),
        (vec!["portable"], vec!["alloc"]),
        (vec![], vec!["alloc", "portable"]),
    ] {
        assert_eq!(
            satisfiable(&ctx, &from_alloc, &on, &off),
            satisfiable(&ctx, &from_portable, &on, &off),
            "arms disagree for on={on:?} off={off:?}"
        );
    }
}

/// An ungated blanket impl of the same trait must not answer for a concrete self
/// type. zeroize ships exactly this pair — `#[cfg(feature = "alloc")] impl<Z>
/// Zeroize for Vec<Z>` beside an ungated `impl<Z: DefaultIsZeroes> Zeroize for
/// Z` — and coherence guarantees at most one of them applies to any given type.
/// Reading the blanket as "an ungated impl exists, so nothing is required" is
/// what would drop multiexp's `alloc` on the floor.
#[test]
fn an_ungated_blanket_impl_does_not_answer_for_a_concrete_type() {
    let ctx = z3::Context::new(&z3::Config::new());
    let tree = dep_tree(&ctx, "gated-1.0.0");
    let keys: Vec<(String, String)> = collect_trait_impl_gates(&tree, &ctx)
        .into_iter()
        .map(|(key, _)| key)
        .collect();
    assert!(
        keys.contains(&("Zeroize".to_string(), "_".to_string())),
        "the blanket impl is keyed, and keyed as blanket: {keys:?}"
    );

    let recs = vec![record(
        "gated-1.0.0",
        "gated",
        "Zeroize",
        "Vec",
        r#"#[cfg(feature = "alloc")]"#,
    )];
    assert!(
        requirement(&ctx, &tree, "gated-1.0.0", &recs, None).is_some(),
        "the blanket cannot apply to Vec, so it does not make the Vec impl free"
    );
}

/// A call that resolved to an impl with no `#[cfg]` requires nothing. Asserting
/// something here would turn a feature the solve was free to choose into a fixed
/// one, for no gain.
#[test]
fn an_unconditional_impl_requires_nothing() {
    let ctx = z3::Context::new(&z3::Config::new());
    let tree = dep_tree(&ctx, "gated-1.0.0");
    let recs = vec![record(
        "gated-1.0.0",
        "gated",
        "Zeroize",
        "Slice",
        "impl Zeroize for Slice",
    )];
    assert!(requirement(&ctx, &tree, "gated-1.0.0", &recs, None).is_none());
}

/// The requirement is read off the impl the compiler *selected*, so a feature
/// gating some other self type is never proposed. `simd` gates `Zeroize for
/// u128`, which does nothing for a `Vec` obligation — and zeroize's real
/// `x86`/`aarch64` impls are exactly this shape, with their target cfgs erased
/// to a bare feature by policy G.
#[test]
fn a_feature_gating_a_different_self_type_is_not_proposed() {
    let ctx = z3::Context::new(&z3::Config::new());
    let tree = dep_tree(&ctx, "gated-1.0.0");
    let recs = vec![record(
        "gated-1.0.0",
        "gated",
        "Zeroize",
        "Vec",
        r#"#[cfg(feature = "alloc")]"#,
    )];
    let req = requirement(&ctx, &tree, "gated-1.0.0", &recs, None).unwrap();
    assert!(
        !satisfiable(&ctx, &req, &["simd"], &["alloc", "portable"]),
        "simd gates an impl for u128 and cannot answer a Vec obligation"
    );
}

/// A requirement no feature set can meet under the dependency's own constraints
/// is left out rather than asserted. An unsatisfiable conjunct costs the crate
/// every covering run — no baseline, no solved sets, every span `AlwaysStd` —
/// which is the failure `dependency_compile_error_constraints` learned to avoid
/// with its `unreachable_atom` check.
#[test]
fn an_unmeetable_requirement_is_left_out() {
    let ctx = z3::Context::new(&z3::Config::new());
    let tree = dep_tree(&ctx, "gated-1.0.0");
    let recs = vec![record(
        "gated-1.0.0",
        "gated",
        "Zeroize",
        "u128",
        r#"#[cfg(feature = "simd")]"#,
    )];
    assert!(
        requirement(&ctx, &tree, "gated-1.0.0", &recs, None).is_some(),
        "on its own the simd impl is a real requirement"
    );

    let no_simd = Bool::new_const(&ctx, "simd").not();
    assert!(
        requirement(&ctx, &tree, "gated-1.0.0", &recs, Some(&no_simd)).is_none(),
        "with simd forbidden there is no feature set to ask for"
    );
}

/// A record names the crate that defines the impl, and only that crate's tree
/// answers for it. `quiet` declares the same trait and the same self type behind
/// its own `alloc`; a requirement on `gated` must not be met from it, and a
/// requirement on `gated` must not turn into one on `quiet`.
#[test]
fn a_record_only_constrains_the_crate_that_defines_the_impl() {
    let ctx = z3::Context::new(&z3::Config::new());
    let gated = dep_tree(&ctx, "gated-1.0.0");
    let recs = vec![record(
        "gated-1.0.0",
        "gated",
        "Zeroize",
        "Vec",
        r#"#[cfg(feature = "alloc")]"#,
    )];
    let quiet = dep_tree(&ctx, "quiet-1.0.0");
    assert!(
        requirement(&ctx, &quiet, "quiet-1.0.0", &recs, None).is_none(),
        "a record naming `gated` says nothing about `quiet`"
    );
    assert!(requirement(&ctx, &gated, "gated-1.0.0", &recs, None).is_some());
}

/// Inherent `impl` blocks carry no key: there is no trait to require, and one
/// showing up among the alternatives would let an unrelated feature satisfy the
/// disjunction.
#[test]
fn inherent_impls_are_not_keyed() {
    let ctx = z3::Context::new(&z3::Config::new());
    let tree = dep_tree(&ctx, "gated-1.0.0");
    let gates = collect_trait_impl_gates(&tree, &ctx);
    assert!(
        gates
            .iter()
            .all(|((trait_name, _), _)| trait_name == "Zeroize"),
        "only the trait impls are keyed: {:?}",
        gates.iter().map(|(k, _)| k).collect::<Vec<_>>()
    );
}

/// A call site the no_std build never reaches demands nothing of a dependency.
///
/// Deliberately not the test `compute_valid_cross_crate_items` uses. That one
/// asks whether the gate holds in a configuration where the no_std condition
/// does *not* — the opposite question — which is defensible where its answer
/// only protects a dependency feature from removal and erring wide costs
/// nothing. Here the answer *adds* a feature, and one added for a call the
/// emitted configuration never compiles is a dependency the crate did not need.
///
/// This is the gate on the call. Whether the *pass* that saw the obligation was
/// compiling a no_std configuration is the other half, and no span can answer
/// it — see `a_pass_that_was_not_no_std_does_not_supply_requirements`.
#[test]
fn only_a_call_site_the_emitted_config_compiles_is_a_requirement() {
    let ctx = z3::Context::new(&z3::Config::new());
    let consumer = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/trait_impl_requirement/consumer/src/lib.rs");
    let mut collector = ModCollector::new(&ctx);
    let root = collector.collect(&consumer, "lib");
    let text = std::fs::read_to_string(&consumer).unwrap();

    let at = |needle: &str| -> ImplRecord {
        let line = text.lines().position(|l| l.contains(needle)).unwrap() + 1;
        let mut rec = record(
            "gated-1.0.0",
            "gated",
            "Zeroize",
            "Vec",
            r#"#[cfg(feature = "alloc")]"#,
        );
        rec.span = ReadableSpan {
            file: consumer.to_string_lossy().to_string(),
            start_line: line,
            start_col: 4,
            end_line: line,
            end_col: 16,
            usage_crate: Some("consumer".to_string()),
        };
        rec
    };
    // Both calls are `v.zeroize()`; the second is the one inside the
    // `#[cfg(feature = "std")]` function.
    let ungated = at("v.zeroize();");
    let mut std_only = ungated.clone();
    let std_line = text
        .lines()
        .enumerate()
        .filter(|(_, l)| l.contains("v.zeroize();"))
        .nth(1)
        .unwrap()
        .0
        + 1;
    std_only.span.start_line = std_line;
    std_only.span.end_line = std_line;

    // The crate is no_std exactly when `std` is off — the shape of nearly every
    // condition the probe derives.
    let hard = Bool::new_const(&ctx, "std").not();

    assert_eq!(
        reachable_impl_records(&root, std::slice::from_ref(&ungated), Some(&hard), &ctx).len(),
        1,
        "an ungated call is in the emitted build and its impl is required"
    );
    assert_eq!(
        reachable_impl_records(&root, std::slice::from_ref(&std_only), Some(&hard), &ctx).len(),
        0,
        "a call the emitted config does not compile requires nothing"
    );

    // With no no_std condition there is nothing to be incompatible with, so both
    // count — the crate constrains no configuration.
    assert_eq!(
        reachable_impl_records(&root, &[ungated, std_only], None, &ctx).len(),
        2
    );
}

/// Which pass saw the obligation decides whether it counts, and no span test can
/// stand in for it.
///
/// uom 0.38.0 is the case. Its `system! { … }` at `src/si/mod.rs:10` generates
/// the entire SI module, and a pass with `std` on resolves an `f32: MulAdd`
/// inside that expansion — an obligation the no_std configuration never raises,
/// because the code carrying it is not generated. The invocation is ungated, so
/// every span test calls the call site reachable, and the requirement that
/// follows (`std ∨ libm` in num-traits, `std` forbidden) put `num-traits/libm`
/// on a build that compiles clean without it.
#[test]
fn a_pass_that_was_not_no_std_does_not_supply_requirements() {
    let ctx = z3::Context::new(&z3::Config::new());
    let manifest = consumer_manifest();
    let rec = record(
        "gated-1.0.0",
        "gated",
        "Zeroize",
        "Vec",
        r#"#[cfg(feature = "alloc")]"#,
    );
    // The crate is no_std exactly when `std` is off.
    let hard = Bool::new_const(&ctx, "std").not();

    let std_on = run(&["std"], &[rec.clone()]);
    let std_off = run(&[], &[]);

    // A no_std pass exists, so the std-on pass's obligations are not evidence
    // about the build being produced — even though it is the only pass with any.
    assert!(
        driver::impls_from_no_std_passes(
            &ctx,
            &manifest,
            Some(&hard),
            &[std_off, std_on.clone()],
            None
        )
        .is_empty(),
        "the std-on pass compiled code the no_std configuration does not"
    );
}

/// The fallback, and the reason it is not a hedge: a crate missing an impl does
/// not type check, so it has no no_std pass at all. multiexp 0.4.0 has zero
/// covering runs and a `default = ["std"]` pass, and that std-on pass is the only
/// compilation in the entire run that ever resolved `Vec<Vec<u8>>: Zeroize`.
#[test]
fn with_no_no_std_pass_at_all_the_std_pass_is_the_evidence() {
    let ctx = z3::Context::new(&z3::Config::new());
    let manifest = consumer_manifest();
    let rec = record(
        "gated-1.0.0",
        "gated",
        "Zeroize",
        "Vec",
        r#"#[cfg(feature = "alloc")]"#,
    );
    let hard = Bool::new_const(&ctx, "std").not();

    // No covering runs; the default pass has `default = ["std"]`, so it is not a
    // no_std pass either. That is multiexp exactly.
    let default_pass = output(&[rec]);
    assert_eq!(
        driver::impls_from_no_std_passes(&ctx, &manifest, Some(&hard), &[], Some(&default_pass))
            .len(),
        1
    );
}

/// The fallback keys on whether a no_std pass *exists*, never on whether one
/// produced records. uom's produce none, and reading that as "no evidence, fall
/// back" would put its `libm` straight back.
#[test]
fn a_silent_no_std_pass_still_blocks_the_fallback() {
    let ctx = z3::Context::new(&z3::Config::new());
    let manifest = consumer_manifest();
    let rec = record(
        "gated-1.0.0",
        "gated",
        "Zeroize",
        "Vec",
        r#"#[cfg(feature = "alloc")]"#,
    );
    let hard = Bool::new_const(&ctx, "std").not();

    let silent_no_std = run(&[], &[]);
    let default_pass = output(&[rec]);
    assert!(
        driver::impls_from_no_std_passes(
            &ctx,
            &manifest,
            Some(&hard),
            &[silent_no_std],
            Some(&default_pass)
        )
        .is_empty(),
        "a no_std pass that saw no obligation is still a no_std pass"
    );
}

/// A crate with no no_std condition constrains no configuration, so every pass
/// counts. `#![no_std]` written unconditionally is the common shape.
#[test]
fn without_a_no_std_condition_every_pass_counts() {
    let ctx = z3::Context::new(&z3::Config::new());
    let manifest = consumer_manifest();
    let rec = record(
        "gated-1.0.0",
        "gated",
        "Zeroize",
        "Vec",
        r#"#[cfg(feature = "alloc")]"#,
    );
    let std_on = run(&["std"], &[rec.clone()]);
    assert_eq!(
        driver::impls_from_no_std_passes(&ctx, &manifest, None, &[std_on], None).len(),
        1
    );
}

// ---------------------------------------------------------------------------
// KI-27 part 2 — the two-hop, macro-generated case (unit-sphere 0.4.0).
//
// unit-sphere writes `(b - a).norm_squared()` on a `Vector3<f64>`. nalgebra
// defines that method ungated, and the impl the obligation actually needs —
// `impl ComplexField for f64` — is in **simba**, which unit-sphere neither
// names nor depends on, and which generates the impl from `impl_complex!` so
// that no `impl` item exists in the source at all. Both halves have to work:
// the impl has to be findable by the macro that made it, and the requirement
// has to be carried up the edge chain into a feature the direct dependency
// declares.
//
// `deep` is simba in miniature, `middle` is nalgebra, `opaque` is the same edge
// with nothing forwarded.
// ---------------------------------------------------------------------------

/// `record`, for an impl that only exists after a macro expands. `via_macro` is
/// what rustc reports for one, and `impl_span` is the **invocation** — the impl
/// itself has no span in the source to report.
fn macro_record(
    dep: &str,
    krate: &str,
    trait_name: &str,
    self_ty: &str,
    needle: &str,
) -> ImplRecord {
    let mut rec = record(dep, krate, trait_name, self_ty, needle);
    rec.via_macro = Some("impl_complex".to_string());
    rec
}

/// The std arm of `deep`'s `impl_complex!` — the one a std-on pass selects, and
/// a std-on pass is where these records come from.
const DEEP_STD_ARM: &str = r#"#[cfg(all(feature = "std", not(feature = "libm_force")))]"#;
/// The arm a no_std build can have.
const DEEP_LIBM_ARM: &str =
    r#"#[cfg(all(not(feature = "std"), not(feature = "libm_force"), feature = "libm"))]"#;

fn deep_record() -> ImplRecord {
    macro_record("deep-1.0.0", "deep", "ComplexField", "f64", DEEP_STD_ARM)
}

/// An impl a macro generated has no `impl` item in the tree and no
/// `(trait, self-type)` key to enumerate its arms by. The key that survives
/// expansion is the macro's own name: rustc reports it, the tree records it on
/// the invocation, and the invocation is where the `#[cfg]` sits.
#[test]
fn a_macro_generated_impl_is_found_by_the_macro_that_made_it() {
    let ctx = z3::Context::new(&z3::Config::new());
    let tree = dep_tree(&ctx, "deep-1.0.0");
    // Sanity: the two generated impls are not in the tree at all. The only
    // keyed `ComplexField for f64` is the hand-written `libm_force` one, so a
    // key-based lookup would answer this obligation with the wrong feature.
    let keyed = collect_trait_impl_gates(&tree, &ctx)
        .into_iter()
        .filter(|((trait_name, self_ty), _)| trait_name == "ComplexField" && self_ty == "f64")
        .count();
    assert_eq!(
        keyed, 1,
        "only the hand-written impl exists before expansion"
    );

    let req = requirement(&ctx, &tree, "deep-1.0.0", &[deep_record()], None)
        .expect("the generated impl exists only under a cfg, so it is a requirement");

    assert!(
        !satisfiable(&ctx, &req, &[], &["std", "libm", "libm_force"]),
        "with neither invocation compiled there is no impl"
    );
    assert!(satisfiable(&ctx, &req, &["libm"], &["std", "libm_force"]));
}

/// The alternation is over the macro's **invocations**. A std-on pass selects
/// the `std` arm; asserting that arm's gate would demand the one thing the run
/// exists to remove, and the `libm` arm is the whole point of looking.
#[test]
fn the_alternation_is_over_the_macros_invocations() {
    let ctx = z3::Context::new(&z3::Config::new());
    let tree = dep_tree(&ctx, "deep-1.0.0");
    let no_std = Bool::new_const(&ctx, "std").not();
    let req = requirement(&ctx, &tree, "deep-1.0.0", &[deep_record()], Some(&no_std)).unwrap();

    assert!(
        satisfiable(&ctx, &req, &["libm"], &["std", "libm_force"]),
        "the arm the compiler did not select has to stay reachable"
    );
    // With `std` off, the only invocation left is the `libm` one — which is the
    // inference the whole entry is about.
    let solver = z3::Solver::new(&ctx);
    solver.assert(&req);
    solver.assert(&no_std);
    solver.assert(&Bool::new_const(&ctx, "libm").not());
    assert_eq!(
        solver.check(),
        z3::SatResult::Unsat,
        "no `std`, no `libm` — and `libm_force` is not one of the macro's arms"
    );
}

/// Recording the arm the compiler did *not* select gives the same answer, the
/// same way it does for a written-out impl: the requirement is a property of the
/// obligation, not of the pass that observed it.
#[test]
fn either_macro_invocation_yields_the_same_requirement() {
    let ctx = z3::Context::new(&z3::Config::new());
    let tree = dep_tree(&ctx, "deep-1.0.0");
    let from_std = requirement(&ctx, &tree, "deep-1.0.0", &[deep_record()], None).unwrap();
    let from_libm = requirement(
        &ctx,
        &tree,
        "deep-1.0.0",
        &[macro_record(
            "deep-1.0.0",
            "deep",
            "ComplexField",
            "f64",
            DEEP_LIBM_ARM,
        )],
        None,
    )
    .unwrap();

    for (on, off) in [
        (vec!["std"], vec!["libm", "libm_force"]),
        (vec!["libm"], vec!["std", "libm_force"]),
        (vec!["libm_force"], vec!["std", "libm"]),
        (vec![], vec!["std", "libm", "libm_force"]),
    ] {
        assert_eq!(
            satisfiable(&ctx, &from_std, &on, &off),
            satisfiable(&ctx, &from_libm, &on, &off),
            "invocations disagree for on={on:?} off={off:?}"
        );
    }
}

/// A hand-written impl of the same trait for the same type is **not** an arm of
/// the macro. simba writes `#[cfg(feature = "libm_force")] impl ComplexField for
/// f32` beside its two `impl_complex!` invocations; the inference being made
/// here is "the same macro generates the same impls", and it does not extend to
/// a block someone wrote out. Folding it in would let the solve answer the
/// requirement with `libm_force` — a configuration nothing in the run compiled.
#[test]
fn a_hand_written_impl_is_not_an_arm_of_the_macro() {
    let ctx = z3::Context::new(&z3::Config::new());
    let tree = dep_tree(&ctx, "deep-1.0.0");
    let req = requirement(&ctx, &tree, "deep-1.0.0", &[deep_record()], None).unwrap();
    assert!(
        !satisfiable(&ctx, &req, &["libm_force"], &["std", "libm"]),
        "the libm_force impl is hand-written, not generated by impl_complex!"
    );
}

/// Every macro invocation is a `LocalItem`, so the macro reading has to be
/// guarded on the compiler actually saying the impl came from one. Without
/// `via_macro` an impl whose span merely fell inside an invocation would be
/// answered by that macro's alternation, which is a different question.
#[test]
fn without_via_macro_a_span_inside_an_invocation_is_not_read() {
    let ctx = z3::Context::new(&z3::Config::new());
    let tree = dep_tree(&ctx, "deep-1.0.0");
    let mut rec = deep_record();
    rec.via_macro = None;
    assert!(requirement(&ctx, &tree, "deep-1.0.0", &[rec], None).is_none());
}

/// An ungated invocation generates its impls in every configuration, so a call
/// resolving into one requires nothing — the macro counterpart of
/// `an_unconditional_impl_requires_nothing`.
#[test]
fn an_ungated_macro_invocation_requires_nothing() {
    let ctx = z3::Context::new(&z3::Config::new());
    let tree = dep_tree(&ctx, "deep-1.0.0");
    let mut rec = macro_record(
        "deep-1.0.0",
        "deep",
        "Marker",
        "f64",
        "impl_marker!(f32, f64);",
    );
    rec.via_macro = Some("impl_marker".to_string());
    assert!(requirement(&ctx, &tree, "deep-1.0.0", &[rec], None).is_none());
}

/// The two-hop case itself. The record names `deep`, the dependency being
/// solved is `middle`, and the requirement only means something to `middle` once
/// each of `deep`'s features is re-expressed as the `middle` feature that
/// forwards it — `libm = ["deep/libm"]`, which is nalgebra's
/// `libm = ["simba/libm"]`.
#[test]
fn an_impl_two_hops_down_constrains_the_direct_dependency() {
    let ctx = z3::Context::new(&z3::Config::new());
    let middle = dep_tree(&ctx, "middle-1.0.0");
    let req = requirement(&ctx, &middle, "middle-1.0.0", &[deep_record()], None)
        .expect("simba's impl is nalgebra's problem to forward");

    // With `middle`'s own std route off — which is what the emitted
    // `--no-default-features` configuration does — only its `libm` remains.
    let solver = z3::Solver::new(&ctx);
    solver.assert(&req);
    for off in ["std", "default", "libm"] {
        solver.assert(&Bool::new_const(&ctx, off).not());
    }
    assert_eq!(
        solver.check(),
        z3::SatResult::Unsat,
        "nothing forwards the impl once std and libm are both off"
    );
    assert!(satisfiable(
        &ctx,
        &req,
        &["libm"],
        &["std", "default", "libm-force"]
    ));
}

/// The atoms translated are the *lower* crate's, and they are translated into
/// the upper crate's own names. `deep`'s `libm_force` is `middle`'s `libm-force`
/// — the same feature under two spellings — and reading `deep`'s spelling as a
/// `middle` feature would constrain a feature that does not exist.
#[test]
fn each_hop_translates_into_the_upper_crates_own_feature_names() {
    let ctx = z3::Context::new(&z3::Config::new());
    let middle = dep_tree(&ctx, "middle-1.0.0");
    let req = requirement(&ctx, &middle, "middle-1.0.0", &[deep_record()], None).unwrap();
    let printed = format!("{req:?}");
    assert!(
        printed.contains("libm-force"),
        "middle spells it `libm-force`: {printed}"
    );
    assert!(
        !printed.contains("libm_force"),
        "deep's spelling must not survive the hop: {printed}"
    );
}

/// A hop with no feature that forwards the requirement substitutes `false` for
/// that atom and keeps only what is left — the `unreachable_atom` discipline
/// `dependency_compile_error_constraints` established. `opaque` forwards `std`
/// and nothing else, so the `libm` arm is gone and the `std` arm is all there
/// is; under a no_std condition that is nothing, and nothing is what gets
/// asserted. An unsatisfiable conjunct here would cost the crate every covering
/// run.
#[test]
fn a_hop_that_forwards_nothing_drops_the_requirement_rather_than_failing() {
    let ctx = z3::Context::new(&z3::Config::new());
    let opaque = dep_tree(&ctx, "opaque-1.0.0");
    let req = requirement(&ctx, &opaque, "opaque-1.0.0", &[deep_record()], None)
        .expect("the std arm is still reachable through this edge");
    assert!(
        !satisfiable(&ctx, &req, &["libm"], &["std", "default"]),
        "opaque has no libm feature, so that arm cannot be asked for"
    );

    let no_std = z3::ast::Bool::and(
        &ctx,
        &[
            &Bool::new_const(&ctx, "std").not(),
            &Bool::new_const(&ctx, "default").not(),
        ],
    );
    assert!(
        requirement(
            &ctx,
            &opaque,
            "opaque-1.0.0",
            &[deep_record()],
            Some(&no_std)
        )
        .is_none(),
        "with the only forwardable arm forbidden the requirement is left out, not asserted"
    );
}

/// A record naming a crate that is not below this dependency says nothing about
/// it. `gated` does not depend on `deep`, and the edge that does reach it gets
/// its own `process_dep_crate` call.
#[test]
fn a_crate_not_below_this_dependency_is_not_constrained() {
    let ctx = z3::Context::new(&z3::Config::new());
    let gated = dep_tree(&ctx, "gated-1.0.0");
    assert!(requirement(&ctx, &gated, "gated-1.0.0", &[deep_record()], None).is_none());
}

/// The `db.bin` guard has to see the two-hop case too. The DB is keyed by the
/// dependency alone, and "nalgebra needs `libm`" is a fact about *unit-sphere* —
/// a cache hit would silently skip the constraint, which is the shape
/// [[db-cache-invalidates-verification]] is about. Nothing in the strings
/// "simba" and "nalgebra" says they are related, so the guard walks the same
/// edge graph the requirement does.
#[test]
fn the_db_guard_sees_a_crate_below_the_dependency() {
    use nostd::driver::dep_carries_impl_requirements;

    let rec = [deep_record()];
    assert!(
        dep_carries_impl_requirements(&dep_dir("middle-1.0.0"), "middle", &rec),
        "middle forwards deep's features, so its solve is the one that has to run"
    );
    assert!(
        !dep_carries_impl_requirements(&dep_dir("gated-1.0.0"), "gated", &rec),
        "gated does not reach deep, so the DB is still sound for it"
    );
    assert!(
        dep_carries_impl_requirements(
            &dep_dir("gated-1.0.0"),
            "gated",
            &[record(
                "gated-1.0.0",
                "gated",
                "Zeroize",
                "Vec",
                r#"#[cfg(feature = "alloc")]"#,
            )],
        ),
        "the one-hop case is the guard's original job"
    );
    assert!(
        !dep_carries_impl_requirements(&dep_dir("middle-1.0.0"), "middle", &[]),
        "a crate with no records at all keeps the DB"
    );
}

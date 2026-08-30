#![feature(rustc_private)]

//! R34-6 — a feature required by an *item the crate names outright*.
//!
//! KI-27's sibling, and it needs its own machinery for one reason: the impl side
//! never fires here. earcut 0.4.4 writes
//!
//! ```ignore
//! use num_traits::float::Float;
//! ```
//!
//! and `Float` exists only under `#[cfg(any(feature = "std", feature = "libm"))]`.
//! With the edge's defaults off the *import* fails, so no call site is ever type
//! checked, no obligation is ever raised, and `impl_records` is empty. What the
//! compiler did record is that in a covering run the path resolved — to
//! `float.rs:932`. That span, read in num-traits' own module tree, is the
//! requirement `std ∨ libm`, and since the dependency must stay no_std the solve
//! is left with `libm`.
//!
//! What is tested here is that step: turning "this path resolved to that
//! definition" into a constraint on the dependency's feature solve. The two
//! lookups it rests on are both load-bearing and are tested separately — the
//! **span** says which definition the compiler actually chose, and the **name**
//! finds that definition's alternatives.

use std::path::{Path, PathBuf};

use nostd::driver::{dep_carries_path_requirements, path_availability_requirement};
use nostd::types::{CrossCrateItem, ReadableSpan};
use nostd::visitor::{ModCollector, ModNode};
use z3::ast::Bool;

fn dep_dir(name: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/path_item_requirement/consumer_deps")
        .join(name)
}

fn dep_tree<'a>(ctx: &'a z3::Context, name: &str) -> ModNode<'a> {
    let entry = dep_dir(name).join("src/lib.rs");
    let mut collector = ModCollector::new(ctx);
    collector.collect(&entry, "lib")
}

/// A span on the line where `needle` first appears in the dependency's source,
/// the way `PathRecord::definition_span` points at the definition the compiler
/// resolved to.
fn span_of_def(dep: &str, needle: &str) -> ReadableSpan {
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

fn item(dep_crate: &str, item: &str, dep: &str, needle: &str) -> CrossCrateItem {
    CrossCrateItem {
        dep: dep_crate.to_string(),
        item: item.to_string(),
        // The consumer's `use` line. Only `reachable_path_items` reads it, and
        // these tests call the requirement directly, past that filter.
        use_span: ReadableSpan {
            file: "src/lib.rs".to_string(),
            start_line: 5,
            start_col: 4,
            end_line: 5,
            end_col: 30,
            usage_crate: Some("consumer".to_string()),
        },
        def_span: span_of_def(dep, needle),
    }
}

fn requirement<'a>(
    ctx: &'a z3::Context,
    tree: &ModNode<'a>,
    dep: &str,
    items: &[CrossCrateItem],
    hard: Option<&Bool<'a>>,
) -> Option<Bool<'a>> {
    path_availability_requirement(
        ctx,
        tree,
        &dep_dir(dep),
        dep.rsplit_once('-').unwrap().0,
        items,
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

/// The earcut case. The import resolved to a `#[cfg]`-gated definition, so the
/// feature set the dependency is built with has to keep one arm alive.
#[test]
fn a_gated_item_the_crate_names_becomes_a_requirement() {
    let ctx = z3::Context::new(&z3::Config::new());
    let tree = dep_tree(&ctx, "mathdep-1.0.0");
    let items = vec![item("mathdep", "Float", "mathdep-1.0.0", "pub trait Float")];
    let req = requirement(&ctx, &tree, "mathdep-1.0.0", &items, None)
        .expect("a gated item the crate imports is a requirement");

    assert!(
        !satisfiable(&ctx, &req, &[], &["std", "libm"]),
        "with neither arm on the item does not exist and the import cannot resolve"
    );
    assert!(satisfiable(&ctx, &req, &["libm"], &["std"]));
}

/// **The test this landing exists for.** The requirement is the disjunction over
/// the arms, not the gate of the arm the compiler happened to resolve to. The
/// record can only come from a pass with `std` on — a pass with both off does
/// not compile — so reading the resolved arm's gate literally would demand
/// `std`, the one thing the run exists to remove. That is also precisely the
/// difference between deriving this and scraping it out of a rustc diagnostic.
#[test]
fn the_requirement_admits_the_arm_the_compiler_did_not_select() {
    let ctx = z3::Context::new(&z3::Config::new());
    let tree = dep_tree(&ctx, "mathdep-1.0.0");
    let items = vec![item("mathdep", "Float", "mathdep-1.0.0", "pub trait Float")];
    let req = requirement(&ctx, &tree, "mathdep-1.0.0", &items, None).unwrap();

    assert!(
        satisfiable(&ctx, &req, &["libm"], &["std"]),
        "`libm` defines the same item and must stay reachable with `std` off"
    );
}

/// A path that resolved to an item with no `#[cfg]` requires nothing. Asserting
/// something here would turn a feature the solve was free to choose into a fixed
/// one, for no gain.
#[test]
fn an_unconditional_item_requires_nothing() {
    let ctx = z3::Context::new(&z3::Config::new());
    let tree = dep_tree(&ctx, "mathdep-1.0.0");
    let items = vec![item(
        "mathdep",
        "NumCast",
        "mathdep-1.0.0",
        "pub trait NumCast",
    )];
    assert!(requirement(&ctx, &tree, "mathdep-1.0.0", &items, None).is_none());
}

/// Two complementary arms of one name: one of them is there in every
/// configuration, so the disjunction simplifies to `true` and nothing is asked
/// for. This is the item-level counterpart of an unconditional impl, and it is
/// why the alternatives are enumerated by name rather than read off the one span.
#[test]
fn complementary_arms_of_one_name_require_nothing() {
    let ctx = z3::Context::new(&z3::Config::new());
    let tree = dep_tree(&ctx, "mathdep-1.0.0");
    let items = vec![item(
        "mathdep",
        "now",
        "mathdep-1.0.0",
        r#"#[cfg(feature = "std")]"#,
    )];
    assert!(
        requirement(&ctx, &tree, "mathdep-1.0.0", &items, None).is_none(),
        "`not(std)` supplies the same name, so the build always has it"
    );
}

/// A single arm with no alternative yields exactly that feature.
#[test]
fn a_single_gated_arm_yields_that_feature() {
    let ctx = z3::Context::new(&z3::Config::new());
    let tree = dep_tree(&ctx, "mathdep-1.0.0");
    let items = vec![item(
        "mathdep",
        "Serializer",
        "mathdep-1.0.0",
        "pub struct Serializer",
    )];
    let req = requirement(&ctx, &tree, "mathdep-1.0.0", &items, None).unwrap();

    assert!(satisfiable(&ctx, &req, &["serde"], &[]));
    assert!(!satisfiable(&ctx, &req, &[], &["serde"]));
}

/// A requirement no feature set can meet while keeping the dependency no_std is
/// dropped rather than asserted — the `unreachable_atom` discipline. An
/// unsatisfiable conjunct costs the crate every covering run, which is far worse
/// than the failure it was trying to prevent.
#[test]
fn a_requirement_the_no_std_condition_forbids_is_dropped() {
    let ctx = z3::Context::new(&z3::Config::new());
    let tree = dep_tree(&ctx, "mathdep-1.0.0");
    let items = vec![item(
        "mathdep",
        "Serializer",
        "mathdep-1.0.0",
        "pub struct Serializer",
    )];
    let hard = Bool::new_const(&ctx, "serde").not();
    assert!(
        requirement(&ctx, &tree, "mathdep-1.0.0", &items, Some(&hard)).is_none(),
        "no feature set gives `Serializer` and keeps the crate no_std"
    );
}

/// The definition is not in this crate's tree — a re-export, a macro-generated
/// item, a file the pass did not walk. There is no gate here to read, and
/// inventing one from the item's *name* is the guess this design exists to
/// avoid: `Float` is a name many crates use.
#[test]
fn a_definition_outside_the_tree_requires_nothing() {
    let ctx = z3::Context::new(&z3::Config::new());
    let tree = dep_tree(&ctx, "mathdep-1.0.0");
    let mut stray = item("mathdep", "Float", "mathdep-1.0.0", "pub trait Float");
    stray.def_span.file = "/some/registry/src/index.crates.io-abcd/mathdep/src/nowhere.rs".into();
    assert!(requirement(&ctx, &tree, "mathdep-1.0.0", &[stray], None).is_none());
}

/// Two hops. The consumer reaches `mathdep` only through `wrapper`, so a
/// requirement derived in mathdep's namespace says nothing about wrapper's until
/// each atom is re-expressed across the edge. wrapper declares
/// `libm = ["mathdep/libm"]`, so mathdep's `libm` becomes wrapper's `libm` and
/// the constraint lands where the solve can act on it.
#[test]
fn an_item_defined_below_the_edge_is_carried_up() {
    let ctx = z3::Context::new(&z3::Config::new());
    let tree = dep_tree(&ctx, "wrapper-1.0.0");
    let items = vec![item("mathdep", "Float", "mathdep-1.0.0", "pub trait Float")];
    let req = requirement(&ctx, &tree, "wrapper-1.0.0", &items, None)
        .expect("mathdep's requirement has to arrive as wrapper's");

    // `default` is a third enabler and has to come off too: wrapper's
    // `default = ["std"]` reaches `mathdep/std` just as `std` does, and
    // `local_features_enabling_dep_feature` is right to report it. Listing only
    // the two named features would leave the requirement satisfiable and prove
    // nothing.
    assert!(
        !satisfiable(&ctx, &req, &[], &["std", "libm", "default"]),
        "no route to `mathdep/std` or `mathdep/libm` means the item is not there"
    );
    assert!(
        satisfiable(&ctx, &req, &["libm"], &["std", "default"]),
        "wrapper's `libm` forwards `mathdep/libm`, which defines the item"
    );
}

/// The `db.bin` guard. `db.bin` answers "what does this dependency need to be
/// no_std", keyed by the dependency alone — parent-independent, and sound for
/// that question. A path requirement is not: `mathdep/libm` is needed because
/// *this* consumer names `Float`. Without the guard the constraint is derived
/// correctly and then skipped for every dependency the cache already holds.
#[test]
fn the_db_guard_sees_a_requirement_on_the_dependency_itself() {
    let items = vec![item("mathdep", "Float", "mathdep-1.0.0", "pub trait Float")];
    assert!(dep_carries_path_requirements(
        &dep_dir("mathdep-1.0.0"),
        "mathdep",
        &items
    ));
}

/// And through it, which is why the guard is not a name comparison: the item
/// names `mathdep`, the dependency is `wrapper`, and nothing about the two
/// strings says they are related.
#[test]
fn the_db_guard_sees_a_requirement_through_the_edge() {
    let items = vec![item("mathdep", "Float", "mathdep-1.0.0", "pub trait Float")];
    assert!(dep_carries_path_requirements(
        &dep_dir("wrapper-1.0.0"),
        "wrapper",
        &items
    ));
}

/// A dependency nothing is required of keeps its cache entry. The guard costs a
/// re-analysis, so it must not fire on every dependency in the tree.
#[test]
fn the_db_guard_leaves_an_unrelated_dependency_alone() {
    let items = vec![item("mathdep", "Float", "mathdep-1.0.0", "pub trait Float")];
    assert!(!dep_carries_path_requirements(
        &dep_dir("mathdep-1.0.0"),
        "unrelated",
        &items
    ));
}

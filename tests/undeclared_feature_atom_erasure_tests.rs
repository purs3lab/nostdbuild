#![feature(rustc_private)]

//! R34-16 residue (item 16): `parser::process_crate` builds its equation
//! through `parse_main_attributes` and `parse_attributes`, and both used to
//! call `parse_meta_for_cfg_attr(…, None)` — never erasing a `feature = "X"`
//! Cargo cannot enable for this crate, even though `parse_meta_for_cfg_attr`
//! has carried a `known_features` parameter that does exactly this since
//! `compile_error_constraint` started using it, and `driver`'s
//! `ModCollector::with_known_features` has used the same mechanism for the
//! module tree all along.
//!
//! `kwap-common 0.7.0` is the fixture: no `[features]` table at all, and a
//! crate root of `#![cfg_attr(all(not(test), feature = "no_std"), no_std)]`.
//! Before this fix, `feature = "no_std"` became a live Z3 `Bool` with nothing
//! in the manifest that could ever set it, so the model picked it arbitrarily
//! — a quiet wrong answer, since `retain_selectable_features` (R34-2/R34-14)
//! only strips such an atom from the *emitted* list afterwards, not from the
//! equation the solve reasoned from. These tests pin the erasure at the point
//! the equation is *built*: the atom must never become a `Bool`, in either of
//! the two functions this item names.

use std::collections::HashSet;

use nostd::Attributes;
use nostd::parser::{parse_attributes, parse_main_attributes};
use syn::visit::Visit;

fn attrs_of(src: &str) -> Attributes {
    let file: syn::File = syn::parse_str(src).expect("fixture source parses");
    let mut attrs = Attributes::default();
    attrs.visit_file(&file);
    attrs
}

fn set(names: &[&str]) -> HashSet<String> {
    names.iter().map(|s| s.to_string()).collect()
}

/// kwap-common 0.7.0's actual crate root, verbatim.
const KWAP_COMMON_ROOT: &str = r#"
    #![cfg_attr(all(not(test), feature = "no_std"), no_std)]
"#;

#[test]
fn no_known_features_keeps_the_atom_live() {
    // Baseline: `None` means every `feature = "…"` stays a real Bool, exactly
    // as before this fix — callers that have not been given a features set
    // (or that are deliberately parsing a dependency's own attributes) must
    // see no behaviour change.
    let ctx = z3::Context::new(&z3::Config::new());
    let (no_std, equation, parsed) = parse_main_attributes(&attrs_of(KWAP_COMMON_ROOT), &ctx, None);

    assert!(no_std, "the cfg_attr's target is literally `no_std`");
    assert!(
        parsed.features.contains(&"no_std".to_string()),
        "with known_features=None the feature atom is still modelled, not erased"
    );
    assert!(
        equation.is_some(),
        "the feature atom is asserted as a live Bool, so the condition carries one"
    );
}

#[test]
fn an_undeclared_feature_in_the_no_std_condition_is_erased_not_modelled() {
    // kwap-common declares no features at all, so the empty set is what
    // `process_crate` would compute for it via `solver::selectable_features`.
    let known = set(&[]);
    let ctx = z3::Context::new(&z3::Config::new());
    let (no_std, equation, parsed) =
        parse_main_attributes(&attrs_of(KWAP_COMMON_ROOT), &ctx, Some(&known));

    // is_no_std reads the cfg_attr's *target* ident, which is independent of
    // feature erasure — this must not regress just because the condition's
    // atom happens to share the string "no_std".
    assert!(
        no_std,
        "erasing the feature atom must not blind is_no_std to the attribute's own target"
    );
    assert!(
        !parsed.features.contains(&"no_std".to_string()),
        "an undeclared feature must never reach `parsed.features` — it cannot be \
         selected, so it must not be offered as something the solve could pick"
    );
    assert!(
        parsed.constants.contains(&"no_std".to_string()),
        "erasure records the atom as a constant, the same treatment bucket-G \
         already gives a target_os atom, so it is still visible as evidence"
    );
    assert!(
        equation.is_none(),
        "the atom was the condition's only real operand once `not(test)` (already \
         erased as a non-feature predicate) drops out — with nothing left to assert, \
         `process_crate` must see no equation rather than a trivially-satisfiable one"
    );
}

#[test]
fn a_declared_feature_survives_erasure_in_the_same_condition() {
    // A mixed condition — one erased atom beside one real one — must still
    // solve over the real atom. Reduced from the same shape, with an actual
    // `[features]` table this time.
    let src = r#"
        #![cfg_attr(any(feature = "alloc", feature = "ghost"), no_std)]
    "#;
    let known = set(&["alloc"]);
    let ctx = z3::Context::new(&z3::Config::new());
    let (no_std, equation, parsed) = parse_main_attributes(&attrs_of(src), &ctx, Some(&known));

    assert!(no_std);
    assert_eq!(
        parsed.features,
        vec!["alloc".to_string()],
        "the declared atom is modelled; the undeclared sibling in the same `any(…)` is not"
    );
    assert!(
        equation.is_some(),
        "the surviving declared atom still gives the condition something to assert"
    );
}

#[test]
fn parse_attributes_erases_an_undeclared_atom_in_an_item_level_cfg() {
    // `parse_attributes` walks `#[cfg(...)]` on items elsewhere in the source
    // (not the crate-root `cfg_attr`) and its result feeds `filter_equations`,
    // which matches by feature-atom membership. An erased atom must not
    // survive into the atom list `filter_equations` reads, or a constraint
    // whose only real content is a dead atom would still be offered to the
    // solver as though it named something the main condition could match.
    //
    // Two declared atoms plus one undeclared one, all under `all(…)`, so the
    // erasure is visible without also tripping the unrelated "a single-feature
    // cfg is not interesting" skip a few lines below in `parse_attributes` —
    // that heuristic is pre-existing, orthogonal, and already flagged by its
    // own `// TODO: Should this check be removed?`; this test is not about it.
    let src = r#"
        #[cfg(all(feature = "declared_one", feature = "declared_two", feature = "ghost"))]
        pub fn f() {}
    "#;
    let known = set(&["declared_one", "declared_two"]);
    let ctx = z3::Context::new(&z3::Config::new());
    let equations = parse_attributes(&attrs_of(src), &ctx, Some(&known));

    assert_eq!(equations.len(), 1);
    let (equation, feats) = &equations[0];
    assert_eq!(
        feats,
        &vec!["declared_one".to_string(), "declared_two".to_string()],
        "`ghost` must not appear among the atoms this cfg names, or a constraint \
         whose only undeclared-vs-declared distinction was erased would still look \
         like it names something `filter_equations` can match against"
    );
    assert!(equation.is_some());
}

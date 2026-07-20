#![feature(rustc_private)]

//! Tests for `compute_valid_cross_crate_items`.
//!
//! The input is a deduplicated `HashSet<CrossCrateRef>`, so the same
//! `(dep, item)` can still appear under several spans with different cfg gates,
//! and iteration order is not stable between runs. An item qualifies as soon as
//! *one* of its spans is no_std-accessible, which is what lets the loop skip
//! spans for an already-accepted item — these tests pin that the skip cannot
//! change the answer whichever span is visited first.

use std::collections::HashSet;
use std::path::PathBuf;

use nostd::driver::compute_valid_cross_crate_items;
use nostd::types::{CrossCrateRef, ReadableSpan};
use nostd::visitor::{LocalItem, ModNode};
use z3::ast::Bool;

fn span(line: usize) -> ReadableSpan {
    ReadableSpan {
        file: "src/lib.rs".to_string(),
        start_line: line,
        start_col: 0,
        end_line: line,
        end_col: 40,
        usage_crate: None,
    }
}

fn cross_ref(line: usize) -> CrossCrateRef {
    CrossCrateRef {
        dep: "serde".to_string(),
        item: "Deserialize".to_string(),
        span: span(line),
    }
}

/// A crate root holding two items: one at line 10 gated on `feature_x`, one at
/// line 20 with no gate at all.
fn gated_and_ungated_root<'a>(ctx: &'a z3::Context) -> ModNode<'a> {
    ModNode {
        name: "crate".to_string(),
        source_file: PathBuf::from("src/lib.rs"),
        source_dir: PathBuf::from("src"),
        entry_condition: None,
        local_items: vec![
            LocalItem {
                own_condition: Some(Bool::new_const(ctx, "feature_x")),
                span: span(10),
                name: Some("gated".to_string()),
            },
            LocalItem {
                own_condition: None,
                span: span(20),
                name: Some("ungated".to_string()),
            },
        ],
        children: Vec::new(),
        is_inline: false,
    }
}

#[test]
fn one_accessible_span_qualifies_the_item() {
    let ctx = z3::Context::new(&z3::Config::new());
    let root = gated_and_ungated_root(&ctx);
    // `hard` forces feature_x on, so the line-10 span is UNSAT (blocked) while
    // the ungated line-20 span stays reachable.
    let hard = Bool::new_const(&ctx, "feature_x");

    // Repeat: the set's iteration order is randomly seeded per instance, so a
    // short-circuit that depended on visiting the accessible span first would
    // fail here intermittently rather than never.
    for _ in 0..50 {
        let records: HashSet<CrossCrateRef> = [cross_ref(10), cross_ref(20)].into_iter().collect();
        let result = compute_valid_cross_crate_items(&root, &records, Some(&hard), &ctx);
        assert!(
            result.contains(&("serde".to_string(), "Deserialize".to_string())),
            "item with one accessible span must qualify regardless of visit order"
        );
    }
}

#[test]
fn item_with_every_span_blocked_is_excluded() {
    let ctx = z3::Context::new(&z3::Config::new());
    let root = gated_and_ungated_root(&ctx);
    let hard = Bool::new_const(&ctx, "feature_x");

    // Only the gated span — nothing accessible, so nothing to accept.
    let records: HashSet<CrossCrateRef> = [cross_ref(10)].into_iter().collect();
    let result = compute_valid_cross_crate_items(&root, &records, Some(&hard), &ctx);

    assert!(result.is_empty(), "got {result:?}");
}

#[test]
fn no_hard_constraints_admits_everything() {
    let ctx = z3::Context::new(&z3::Config::new());
    let root = gated_and_ungated_root(&ctx);

    let records: HashSet<CrossCrateRef> = [cross_ref(10), cross_ref(20)].into_iter().collect();
    let result = compute_valid_cross_crate_items(&root, &records, None, &ctx);

    assert_eq!(
        result,
        [("serde".to_string(), "Deserialize".to_string())]
            .into_iter()
            .collect()
    );
}

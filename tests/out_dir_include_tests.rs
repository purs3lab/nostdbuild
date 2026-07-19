#![feature(rustc_private)]

//! Regression test for build-script-generated code pulled in via
//! `include!(concat!(env!("OUT_DIR"), …))` (e.g. akd_core's protobuf output).
//! syn can't resolve OUT_DIR, so the driver defers the include, then splices the
//! generated file into the tree gated by the include site's `#[cfg(...)]` once a
//! plugin run reveals OUT_DIR. Verified without running the plugin by simulating
//! a generated file on disk and calling `resolve_pending_includes` directly.

use std::path::{Path, PathBuf};

use nostd::types::ReadableSpan;
use nostd::visitor::{ModCollector, ancestors_for_span, resolve_pending_includes};

fn fixture_lib() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/out_dir_include/lib.rs")
}

fn span_of(content: &str, needle: &str, file: &str) -> ReadableSpan {
    let idx = content.find(needle).expect("needle not found");
    let before = &content[..idx];
    let line = before.matches('\n').count() + 1;
    let col = idx - before.rfind('\n').map(|p| p + 1).unwrap_or(0);
    ReadableSpan {
        file: file.to_string(),
        start_line: line,
        start_col: col,
        end_line: line,
        end_col: col + needle.len(),
        usage_crate: Some("std".to_string()),
    }
}

#[test]
fn generated_include_is_gated_by_cfg() {
    let ctx = z3::Context::new(&z3::Config::new());

    // 1. Collect the tree — the non-literal include! becomes a pending include.
    let mut collector = ModCollector::new(&ctx);
    let mut root = collector.collect(&fixture_lib(), "out_dir_include");
    assert_eq!(
        collector.pending_includes.len(),
        1,
        "the OUT_DIR include! should be deferred as one pending include"
    );
    assert_eq!(collector.pending_includes[0].tail, "/protos/mod.rs");
    assert!(
        collector.pending_includes[0].condition.is_some(),
        "pending include must carry the enclosing #[cfg(...)] condition"
    );

    // 2. Simulate the build-script output (rust-protobuf shape): a fake OUT_DIR
    //    shaped like a real cargo build dir so normalization kicks in, with a
    //    generated mod.rs that declares `pub mod types;` and a sibling types.rs
    //    holding the std usage — the file the include! never names directly.
    let out_dir = std::env::temp_dir().join(format!(
        "nostd_out_dir_test_{}/build/out_dir_include-0123456789abcdef/out",
        std::process::id()
    ));
    let protos = out_dir.join("protos");
    std::fs::create_dir_all(&protos).unwrap();
    std::fs::write(protos.join("mod.rs"), "pub mod types;\n").unwrap();
    let gen_src = "pub fn make() -> std::string::String { std::string::String::new() }\n";
    std::fs::write(protos.join("types.rs"), gen_src).unwrap();

    // 3. Resolve the deferred include against the revealed OUT_DIR.
    resolve_pending_includes(
        &ctx,
        &mut root,
        &collector.pending_includes,
        out_dir.to_str().unwrap(),
    );

    // 4. A std span inside the *transitively-resolved* generated types.rs
    //    (addressed by its normalized $OUT_DIR path, as the HIR pass would after
    //    normalization) must resolve to the include-site gate — proving the
    //    whole parsed subtree, not just the top include, is normalized/gated.
    let normalized = "$OUT_DIR/out_dir_include/protos/types.rs";
    let target = span_of(gen_src, "std::string::String", normalized);
    let anc = ancestors_for_span(&root, &target);
    println!("ancestors: {:?}", anc);

    // cleanup
    let _ = std::fs::remove_dir_all(
        std::env::temp_dir().join(format!("nostd_out_dir_test_{}", std::process::id())),
    );

    assert!(
        anc.is_some(),
        "generated std usage should be gated by the include-site #[cfg(...)], got None"
    );
}

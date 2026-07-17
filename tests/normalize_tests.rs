#![feature(rustc_private)]

//! Tests for `normalize_generated_path` — canonicalising cargo build-script
//! (`OUT_DIR`) paths so a generated span has one stable, hash-free identity
//! across feature sets.

use nostd::driver::normalize_generated_path;

#[test]
fn rewrites_out_dir_paths_and_drops_hash() {
    let a = "/x/target/plugin-nightly/debug/build/akd_core-3f7210c5d5766928/out/protos/types.rs";
    let b = "/x/target/plugin-nightly/debug/build/akd_core-6bfed3d45043f090/out/protos/types.rs";
    // Different per-feature-set hashes must normalise to the same identity.
    assert_eq!(normalize_generated_path(a), "$OUT_DIR/akd_core/protos/types.rs");
    assert_eq!(normalize_generated_path(a), normalize_generated_path(b));
}

#[test]
fn keeps_pkg_so_deps_do_not_collide() {
    let main = "/x/build/akd_core-3f7210c5d5766928/out/protos/types.rs";
    let dep = "/x/build/other_dep-3f7210c5d5766928/out/protos/types.rs";
    assert_ne!(normalize_generated_path(main), normalize_generated_path(dep));
}

#[test]
fn leaves_ordinary_and_malformed_paths_untouched() {
    assert_eq!(normalize_generated_path("src/print.rs"), "src/print.rs");
    // hash wrong length → not a generated path
    assert_eq!(
        normalize_generated_path("/x/build/foo-1234/out/gen.rs"),
        "/x/build/foo-1234/out/gen.rs"
    );
}

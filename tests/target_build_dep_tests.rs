#![feature(rustc_private)]

//! R34-19: the emitted manifest severed a platform-gated build dependency.
//!
//! `[target.<cfg>]` holds the platform-specific halves of three tables and only
//! the dev half is unwanted. `remove_table_from_toml` filtered them with an
//! allowlist of `dependencies` alone, which differs from "drop dev-dependencies"
//! in exactly one case — and that case is `[target.'cfg(unix)'.build-dependencies]`.
//!
//! `mp3lame-sys-0.1.8` is the measured row. It declares
//! `[target.'cfg(unix)'.build-dependencies] autotools = "0.2.6"` and names the
//! crate once, at `build.rs:7`, as `autotools::Config::new(LAME_DIR)`. With the
//! entry gone the build script does not compile:
//!
//! ```text
//! error[E0433]: failed to resolve: use of unresolved module or unlinked crate `autotools`
//!  --> build.rs:7:22
//! ```
//!
//! on all 26 bare-metal targets **and** on `x86_64-unknown-linux-gnu`, which is
//! what makes it the corpus's cheapest regression case: detecting it needs no
//! no_std reasoning, only a build. (The row still does not reach 26/26 after this
//! — behind the restored edge sits R34-17, libc having no `target_os = "none"`
//! definitions. The host build is the criterion.)
//!
//! **A second row has the same defect under a different error code.**
//! `libsodium-sys-0.2.7` declares
//! `[target.'cfg(not(target_env = "msvc"))'.build-dependencies] cc = "1.0"` and writes
//! `extern crate cc;` at `build.rs:2`. With the entry severed, `cc` resolves to rustc's
//! *own* `cc` in the sysroot rather than failing to resolve, so the crate reports
//! `E0658 rustc_private` instead of `E0433` and no error-text grouping ever put the two
//! rows together. Both were measured going `rc=101` → `rc=0` on
//! `x86_64-unknown-linux-gnu` across this change.
//!
//! The third test guards the coupling: `remove_features_of_deps` harvested dep
//! names from `build-dependencies` too, which was harmless only while that table
//! was also being deleted. Keeping the table and stripping its `dep:` reference
//! leaves an optional dependency enabled by no feature, and cargo refuses the
//! manifest outright — the T3 failure `manifest_emission_tests` exists to prevent.

use std::fs;
use std::path::PathBuf;

use nostd::parser::{remove_features_of_deps, remove_table_from_toml};

/// `remove_table_from_toml` writes the edited manifest back to `filename`, so the
/// fixture needs a real path. Scoped to the test's own directory.
struct Fixture {
    dir: PathBuf,
    path: String,
}

impl Fixture {
    fn new(slug: &str, manifest: &str) -> Self {
        let dir = std::env::temp_dir().join(format!("nostd-target-build-dep-{slug}"));
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).expect("failed to create fixture dir");
        let path = dir.join("Cargo.toml");
        fs::write(&path, manifest).expect("failed to write fixture manifest");
        Self {
            path: path.to_string_lossy().into_owned(),
            dir,
        }
    }

    fn toml(&self) -> toml::Value {
        toml::from_str(&fs::read_to_string(&self.path).expect("fixture reread"))
            .expect("fixture is not valid TOML")
    }
}

impl Drop for Fixture {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.dir);
    }
}

/// mp3lame-sys's shape: two platform-gated build deps and a platform-gated dev
/// dep. The build deps stay, the dev dep goes.
#[test]
fn target_gated_build_dependencies_survive_the_scrub() {
    let fixture = Fixture::new(
        "mp3lame",
        r#"[package]
name = "mp3lame-sys"
version = "0.1.8"

[dependencies.libc]
version = "0.2"
default-features = false

[target.'cfg(unix)'.build-dependencies.autotools]
version = "0.2.6"

[target.'cfg(windows)'.build-dependencies.cc]
version = "1.0.77"

[target.'cfg(unix)'.dev-dependencies.tempfile]
version = "3"

[target.'cfg(unix)'.dependencies.nix]
version = "0.27"
"#,
    );

    let mut toml = fixture.toml();
    remove_table_from_toml("target", &mut toml, &fixture.path).expect("scrub failed");

    let after = fixture.toml();
    let target = after
        .get("target")
        .and_then(toml::Value::as_table)
        .expect("target table was removed wholesale");

    let unix = target
        .get("cfg(unix)")
        .and_then(toml::Value::as_table)
        .expect("cfg(unix) entry lost");
    assert!(
        unix.get("build-dependencies")
            .and_then(toml::Value::as_table)
            .is_some_and(|t| t.contains_key("autotools")),
        "the build script's own dependency was severed: {unix:?}"
    );
    assert!(
        unix.get("dependencies")
            .and_then(toml::Value::as_table)
            .is_some_and(|t| t.contains_key("nix")),
        "a target-gated normal dependency was severed: {unix:?}"
    );
    assert!(
        !unix.contains_key("dev-dependencies"),
        "dev-dependencies must still go: {unix:?}"
    );

    let windows = target
        .get("cfg(windows)")
        .and_then(toml::Value::as_table)
        .expect("cfg(windows) entry lost");
    assert!(
        windows
            .get("build-dependencies")
            .and_then(toml::Value::as_table)
            .is_some_and(|t| t.contains_key("cc")),
        "cc was severed: {windows:?}"
    );
}

/// libsodium-sys's shape: an `extern crate` in the build script naming a crate that
/// also exists in the sysroot, so severing the edge silently changes *which* crate
/// the name resolves to. Same defect, different diagnostic.
#[test]
fn a_build_dep_that_shadows_a_sysroot_crate_survives_too() {
    let fixture = Fixture::new(
        "libsodium",
        r#"[package]
name = "libsodium-sys"
version = "0.2.7"

[dependencies.libc]
version = "0.2"
default-features = false

[target.'cfg(not(target_env = "msvc"))'.build-dependencies.cc]
version = "1.0"

[target.'cfg(target_env = "msvc")'.build-dependencies.libc]
version = "0.2"
default-features = false
"#,
    );

    let mut toml = fixture.toml();
    remove_table_from_toml("target", &mut toml, &fixture.path).expect("scrub failed");

    let after = fixture.toml();
    let target = after
        .get("target")
        .and_then(toml::Value::as_table)
        .expect("target table was removed wholesale");
    assert!(
        target
            .values()
            .filter_map(toml::Value::as_table)
            .filter_map(|t| t.get("build-dependencies"))
            .filter_map(toml::Value::as_table)
            .any(|t| t.contains_key("cc")),
        "`cc` was severed and would resolve to the sysroot crate instead: {target:?}"
    );
}

/// The coupling. A kept table whose `dep:` reference is stripped is a manifest
/// cargo refuses; a dropped table whose reference survives is the same. The two
/// functions must name the same sub-tables.
#[test]
fn a_kept_build_dep_keeps_the_feature_that_enables_it() {
    let fixture = Fixture::new(
        "optional-build-dep",
        r#"[package]
name = "fixture"
version = "0.0.0"

[features]
vendored = ["dep:vcpkg", "vcpkg/prebuilt"]
testing = ["dep:tempfile"]

[target.'cfg(windows)'.build-dependencies.vcpkg]
version = "0.2"
optional = true

[target.'cfg(unix)'.dev-dependencies.tempfile]
version = "3"
optional = true
"#,
    );

    let mut toml = fixture.toml();
    remove_features_of_deps("target", &mut toml, &fixture.path, &[]).expect("feature scrub failed");
    remove_table_from_toml("target", &mut toml, &fixture.path).expect("scrub failed");

    let after = fixture.toml();
    let features = after
        .get("features")
        .and_then(toml::Value::as_table)
        .expect("features table lost");

    let vendored: Vec<&str> = features["vendored"]
        .as_array()
        .expect("vendored is not an array")
        .iter()
        .filter_map(toml::Value::as_str)
        .collect();
    assert!(
        vendored.contains(&"dep:vcpkg"),
        "the kept build dep lost the only feature that enables it: {vendored:?}"
    );
    assert!(
        vendored.contains(&"vcpkg/prebuilt"),
        "a feature reference to a kept build dep was stripped: {vendored:?}"
    );

    let testing: Vec<&str> = features["testing"]
        .as_array()
        .expect("testing is not an array")
        .iter()
        .filter_map(toml::Value::as_str)
        .collect();
    assert!(
        testing.is_empty(),
        "the dev dep is gone, so its reference must be too: {testing:?}"
    );
}

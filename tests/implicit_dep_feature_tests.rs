#![feature(rustc_private)]

//! Regression test for Cargo's implicit per-optional-dependency features.
//!
//! `rand_core` declares `[dependencies.getrandom] optional = true` and never
//! mentions `dep:getrandom`, so Cargo synthesises a `getrandom` feature that
//! appears nowhere in `[features]`. Its source has
//! `#[cfg(all(feature = "getrandom", not(feature = "std")))]`, which makes
//! `getrandom` a solver variable that is satisfiable alongside `not(std)` — so
//! the model switches it on and it lands in the enable list.
//!
//! `minimize` could not take it back out: `features_for_optional_deps` only
//! enumerated declared `[features]` entries, so the feature was never recognised
//! as one whose sole purpose is pulling in an optional dependency. The feature
//! survived into `custom_no_std_feature_enabled`, dragging `getrandom` into
//! bare-metal builds where it has no backend (`compile_error!("target is not
//! supported")` on all 26 targets in `consts::TARGET_LIST`).

use std::collections::HashSet;
use std::fs;
use std::path::PathBuf;

use nostd::CrateInfo;
use nostd::consts;
use nostd::parser::{features_for_optional_deps, minimize};
use nostd::types::TupleVec;

/// A `[dependencies]` entry — only `name` and `optional` are read here.
fn dep(name: &str) -> (CrateInfo, Vec<String>) {
    (
        CrateInfo {
            name: name.to_string(),
            version: "0.0.0".to_string(),
            optional: true,
            ..Default::default()
        },
        Vec::new(),
    )
}

/// rand_core 0.6.4's feature graph: `getrandom` and `serde` are optional deps,
/// `std = ["alloc", "getrandom", "getrandom/std"]`. The bare `getrandom` value
/// is what keeps Cargo's implicit feature alive — no `dep:` prefix anywhere.
fn rand_core_like() -> CrateInfo {
    CrateInfo {
        name: "fixture".to_string(),
        version: "0.0.0".to_string(),
        deps_and_features: vec![dep("getrandom"), dep("serde")],
        features: vec![
            ("alloc".to_string(), vec![]),
            (
                "serde1".to_string(),
                vec![("serde".to_string(), "serde".to_string())],
            ),
            (
                "std".to_string(),
                vec![
                    ("alloc".to_string(), "alloc".to_string()),
                    ("getrandom".to_string(), "getrandom".to_string()),
                    ("getrandom".to_string(), "std".to_string()),
                ],
            ),
        ],
        ..Default::default()
    }
}

/// `minimize` reads the crate's manifest off disk through
/// `determine_manifest_file`, so a fixture needs a real file at the path that
/// derives from its name-with-version. Each test uses its own directory so the
/// parallel test threads cannot collide.
struct Fixture {
    name_with_version: String,
    dir: PathBuf,
}

impl Fixture {
    fn new(slug: &str) -> Self {
        Self::with_manifest(
            slug,
            "[package]\nname = \"fixture\"\nversion = \"0.0.0\"\n\n\
             [dependencies.getrandom]\nversion = \"0.2\"\noptional = true\n\n\
             [dependencies.serde]\nversion = \"1\"\noptional = true\n\n\
             [features]\nalloc = []\nserde1 = [\"serde\"]\n\
             std = [\"alloc\", \"getrandom\", \"getrandom/std\"]\n",
        )
    }

    fn with_manifest(slug: &str, manifest: &str) -> Self {
        let name_with_version = format!("{slug}:0.0.0");
        let dir = PathBuf::from(consts::DOWNLOAD_PATH).join(format!("{slug}-0.0.0"));
        fs::create_dir_all(&dir).expect("failed to create fixture crate dir");
        fs::write(dir.join("Cargo.toml"), manifest).expect("failed to write fixture manifest");
        Self {
            name_with_version,
            dir,
        }
    }

    fn run_minimize(
        &self,
        crate_info: &CrateInfo,
        enable: &mut Vec<String>,
        non_minimalizable: &HashSet<String>,
    ) -> TupleVec {
        self.run_minimize_with_default(crate_info, enable, non_minimalizable, true)
    }

    /// Returns the surviving `(dep, feature)` pairs — what `should_skip_dep` reads
    /// afterwards to decide whether an optional dep is still live.
    fn run_minimize_with_default(
        &self,
        crate_info: &CrateInfo,
        enable: &mut Vec<String>,
        non_minimalizable: &HashSet<String>,
        disable_default: bool,
    ) -> TupleVec {
        self.run_minimize_with_pins(
            crate_info,
            enable,
            non_minimalizable,
            disable_default,
            Some(&HashSet::new()),
        )
    }

    /// `pins` is `minimize`'s `deps_to_keep`: `Some` is an armed pass, `None` a pass
    /// that cannot know yet (`process_crate` for the main crate).
    fn run_minimize_with_pins(
        &self,
        crate_info: &CrateInfo,
        enable: &mut Vec<String>,
        non_minimalizable: &HashSet<String>,
        disable_default: bool,
        pins: Option<&HashSet<String>>,
    ) -> TupleVec {
        let mut optional_dep_feats: TupleVec = features_for_optional_deps(crate_info);
        minimize(
            crate_info,
            &mut optional_dep_feats,
            enable,
            non_minimalizable,
            disable_default,
            &self.name_with_version,
            None,
            None,
            "test",
            pins,
        );
        optional_dep_feats
    }

    fn manifest_text(&self) -> String {
        fs::read_to_string(self.dir.join("Cargo.toml")).expect("fixture manifest")
    }

    /// The values of one `[features]` entry as the manifest holds them after the
    /// pass — reading the raw text instead would match `dep:D` in the orphan
    /// `custom_default_features` list and call a strip a keep.
    fn feature_values(&self, feat: &str) -> Vec<String> {
        let toml: toml::Value = self.manifest_text().parse().expect("fixture manifest");
        toml.get("features")
            .and_then(|f| f.get(feat))
            .and_then(|v| v.as_array())
            .map(|arr| {
                arr.iter()
                    .filter_map(|v| v.as_str().map(str::to_string))
                    .collect()
            })
            .unwrap_or_default()
    }
}

impl Drop for Fixture {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.dir);
    }
}

#[test]
fn implicit_optional_dep_feature_is_reported() {
    let pairs = features_for_optional_deps(&rand_core_like());

    // The implicit feature Cargo synthesises for the optional dep.
    assert!(
        pairs.contains(&("getrandom".to_string(), "getrandom".to_string())),
        "implicit `getrandom` feature must be reported as a dep enabler, got {pairs:?}"
    );
    // Control: the declared enablers are still found, so the new pairs are an
    // addition rather than a replacement.
    assert!(
        pairs.contains(&("getrandom".to_string(), "std".to_string())),
        "`std` must still be reported as enabling `getrandom`, got {pairs:?}"
    );
    assert!(
        pairs.contains(&("serde".to_string(), "serde1".to_string())),
        "`serde1` must still be reported as enabling `serde`, got {pairs:?}"
    );
}

#[test]
fn implicit_optional_dep_feature_is_minimized_away() {
    let crate_info = rand_core_like();
    let fixture = Fixture::new("implicit-dep-feat-drop");

    let mut enable = vec!["getrandom".to_string()];
    fixture.run_minimize(&crate_info, &mut enable, &HashSet::new());

    assert!(
        enable.is_empty(),
        "`getrandom` exists only to pull in the optional dep and must be dropped, got {enable:?}"
    );
}

#[test]
fn declared_feature_with_real_content_survives() {
    let crate_info = rand_core_like();
    let fixture = Fixture::new("implicit-dep-feat-keep");

    // Mutual control for the test above: `alloc` is a declared feature that
    // gates real code, so minimize must leave it alone. Without this, a
    // minimize that emptied the enable list unconditionally would pass.
    let mut enable = vec!["alloc".to_string(), "getrandom".to_string()];
    fixture.run_minimize(&crate_info, &mut enable, &HashSet::new());

    assert_eq!(
        enable,
        vec!["alloc".to_string()],
        "only the implicit dep feature may be dropped"
    );
}

/// watchface 0.4.0's feature graph: `default = ["std"]`, `std = ["chrono"]`, with
/// `chrono` an optional dep referenced bare (no `dep:`), so Cargo's implicit
/// `chrono` feature is alive.
fn watchface_like() -> CrateInfo {
    CrateInfo {
        name: "fixture".to_string(),
        version: "0.0.0".to_string(),
        deps_and_features: vec![dep("chrono")],
        features: vec![
            (
                "default".to_string(),
                vec![("std".to_string(), "std".to_string())],
            ),
            (
                "std".to_string(),
                vec![("chrono".to_string(), "chrono".to_string())],
            ),
        ],
        ..Default::default()
    }
}

const WATCHFACE_MANIFEST: &str = "[package]\nname = \"fixture\"\nversion = \"0.0.0\"\n\n\
     [dependencies.chrono]\nversion = \"0.4\"\noptional = true\n\n\
     [features]\ndefault = [\"std\"]\nstd = [\"chrono\"]\n";

#[test]
fn implicit_pair_dies_with_its_declared_enabler() {
    let crate_info = watchface_like();
    let fixture = Fixture::with_manifest("implicit-dep-feat-severed", WATCHFACE_MANIFEST);

    // Nothing is explicitly enabled; `default` is live, so minimize strips the
    // `chrono` entry out of `std` and moves it to custom-disabled. The declared
    // pairs ("chrono","default") and ("chrono","std") are invalidated with it.
    let mut enable: Vec<String> = Vec::new();
    let surviving =
        fixture.run_minimize_with_default(&crate_info, &mut enable, &HashSet::new(), false);

    // The implicit pair must go too. Left behind, `should_skip_dep` rebuilds the
    // link by walking the in-memory `default -> std -> chrono` chain — the manifest
    // entry it re-reads is the one minimize just removed — and treats the severed
    // dep as live. That is what dragged chrono into watchface's no_std build with
    // only `alloc`, switching on `#[cfg(feature = "chrono")]` code that needs
    // `chrono/clock` and failing with `E0412 cannot find type Local`.
    assert!(
        surviving.is_empty(),
        "every pair for the severed dep must be invalidated, got {surviving:?}"
    );
}

#[test]
fn implicit_pair_survives_when_the_feature_is_really_enabled() {
    let crate_info = watchface_like();
    let fixture = Fixture::with_manifest("implicit-dep-feat-live", WATCHFACE_MANIFEST);

    // Mutual control for the test above. Here the implicit feature is on the command
    // line and pinned, so the dep genuinely is part of the build even though the
    // `std -> chrono` entry gets stripped. Evicting the pair would make
    // `should_skip_dep` skip a dependency that is actually being compiled.
    let non_minimalizable: HashSet<String> = ["chrono".to_string()].into_iter().collect();
    let mut enable = vec!["chrono".to_string()];
    let surviving =
        fixture.run_minimize_with_default(&crate_info, &mut enable, &non_minimalizable, false);

    assert_eq!(
        enable,
        vec!["chrono".to_string()],
        "a non-minimalizable implicit feature must not be dropped"
    );
    assert!(
        surviving.contains(&("chrono".to_string(), "chrono".to_string())),
        "the implicit pair must survive while the feature is enabled, got {surviving:?}"
    );
}

#[test]
fn required_implicit_feature_is_kept() {
    let crate_info = rand_core_like();
    let fixture = Fixture::new("implicit-dep-feat-required");

    // When a parent crate's hard constraints require the dep feature, it lands in
    // `non_minimalizable_features` and must survive — dropping it there would
    // break the parent instead of the target.
    let non_minimalizable: HashSet<String> = ["getrandom".to_string()].into_iter().collect();
    let mut enable = vec!["getrandom".to_string()];
    fixture.run_minimize(&crate_info, &mut enable, &non_minimalizable);

    assert_eq!(
        enable,
        vec!["getrandom".to_string()],
        "a non-minimalizable implicit feature must not be dropped"
    );
}

// ---------------------------------------------------------------------------
// Bucket 3c: `feat = ["<optdep>/<subfeat>"]` implies the implicit `<optdep>`
// feature. `optional_dep_feature_edges` must emit `(feat, dep)` for a strong
// reference to an optional dep, and nothing for weak / required / `dep:` refs.
// ---------------------------------------------------------------------------

use nostd::downloader::{optional_dep_feature_edges, read_local_features};
use nostd::solver::{feature_implication_constraints, optional_dep_implication_constraints};

fn parse_manifest(manifest: &str) -> toml::Value {
    manifest.parse().expect("failed to parse fixture manifest")
}

/// inout 0.1.4: `std = ["block-padding/std"]`, `block-padding` optional.
const INOUT_MANIFEST: &str = "[package]\nname = \"inout\"\nversion = \"0.1.4\"\n\n\
     [features]\nstd = [\"block-padding/std\"]\n\n\
     [dependencies.block-padding]\nversion = \"0.3\"\noptional = true\n\n\
     [dependencies.generic-array]\nversion = \"0.14\"\n";

#[test]
fn strong_ref_to_optional_dep_yields_edge() {
    let toml = parse_manifest(INOUT_MANIFEST);
    let edges = optional_dep_feature_edges(&toml);
    assert_eq!(
        edges,
        vec![("std".to_string(), "block-padding".to_string())],
        "std => block-padding must be emitted for `std = [\"block-padding/std\"]`, got {edges:?}"
    );
}

#[test]
fn weak_ref_yields_no_edge() {
    // `block-padding?/std` enables the sub-feature only if block-padding is
    // already on, so it does NOT imply block-padding. Adding the edge would be
    // unsound.
    let manifest = INOUT_MANIFEST.replace("block-padding/std", "block-padding?/std");
    let toml = parse_manifest(&manifest);
    let edges = optional_dep_feature_edges(&toml);
    assert!(
        edges.is_empty(),
        "a weak `dep?/feat` reference must not produce an implication edge, got {edges:?}"
    );
}

#[test]
fn ref_to_required_dep_yields_no_edge() {
    // generic-array is not optional, so there is no implicit `generic-array`
    // feature to imply.
    let manifest = INOUT_MANIFEST.replace("block-padding/std", "generic-array/std");
    let toml = parse_manifest(&manifest);
    let edges = optional_dep_feature_edges(&toml);
    assert!(
        edges.is_empty(),
        "a `dep/feat` reference to a required dep must not produce an edge, got {edges:?}"
    );
}

#[test]
fn dep_and_plain_refs_yield_no_optional_edge() {
    // `dep:` names a dependency (handled elsewhere) and a plain `alloc` link is a
    // feature-to-feature edge handled by `feature_implication_constraints`.
    // Neither is a `dep/feat` optional reference.
    let manifest = "[package]\nname = \"x\"\nversion = \"0.0.0\"\n\n\
         [features]\nstd = [\"dep:block-padding\", \"alloc\"]\nalloc = []\n\n\
         [dependencies.block-padding]\nversion = \"0.3\"\noptional = true\n";
    let toml = parse_manifest(manifest);
    let edges = optional_dep_feature_edges(&toml);
    assert!(
        edges.is_empty(),
        "`dep:` and plain feature links must not be treated as optional-dep edges, got {edges:?}"
    );
}

#[test]
fn target_specific_optional_dep_is_recognised() {
    let manifest = "[package]\nname = \"x\"\nversion = \"0.0.0\"\n\n\
         [features]\nstd = [\"foo/std\"]\n\n\
         [target.'cfg(unix)'.dependencies.foo]\nversion = \"1\"\noptional = true\n";
    let toml = parse_manifest(manifest);
    let edges = optional_dep_feature_edges(&toml);
    assert_eq!(
        edges,
        vec![("std".to_string(), "foo".to_string())],
        "an optional dep under [target.*.dependencies] must be recognised, got {edges:?}"
    );
}

#[test]
fn constraint_forbids_feat_on_dep_off() {
    // The whole point: with the edge asserted, `std & !block-padding` is unsat, so
    // the probe can never pick the model that Cargo would re-unify (bucket 3c).
    let ctx = z3::Context::new(&z3::Config::new());
    let toml = parse_manifest(INOUT_MANIFEST);
    let edges = optional_dep_feature_edges(&toml);
    let constraints = optional_dep_implication_constraints(&ctx, &edges);
    assert_eq!(constraints.len(), 1, "expected exactly one edge constraint");

    let solver = z3::Solver::new(&ctx);
    for c in &constraints {
        solver.assert(c);
    }
    let std_var = z3::ast::Bool::new_const(&ctx, "std");
    let bp_var = z3::ast::Bool::new_const(&ctx, "block-padding");
    solver.assert(&std_var);
    solver.assert(&bp_var.not());
    assert_eq!(
        solver.check(),
        z3::SatResult::Unsat,
        "std=true, block-padding=false must be forbidden by the edge"
    );

    // Control: the existing plain-link function alone does NOT forbid it — proving
    // the new path is what closes the gap, not pre-existing behaviour.
    let feat_map = read_local_features(&toml);
    let plain = feature_implication_constraints(&ctx, &feat_map);
    let solver2 = z3::Solver::new(&ctx);
    for c in &plain {
        solver2.assert(c);
    }
    solver2.assert(&std_var);
    solver2.assert(&bp_var.not());
    assert_eq!(
        solver2.check(),
        z3::SatResult::Sat,
        "plain-link constraints alone must NOT forbid std&!block-padding (the 3c gap)"
    );
}

// ---------------------------------------------------------------------------
// KI-14: the unarmed `minimize` inside `process_crate`
// ---------------------------------------------------------------------------

/// bevy_input-0.16.0's shape: `smol_str` is an optional dep AND an explicitly
/// declared feature carrying a second value, so the feature stays enabled after
/// `dep:smol_str` is deleted out of it and `#[cfg(feature = "smol_str")] use
/// smol_str::SmolStr` keeps compiling against a crate cargo never links (E0432).
fn bevy_input_like() -> CrateInfo {
    CrateInfo {
        name: "fixture".to_string(),
        version: "0.0.0".to_string(),
        deps_and_features: vec![dep("smol_str")],
        features: vec![(
            "smol_str".to_string(),
            vec![
                ("smol_str".to_string(), "dep:".to_string()),
                ("bevy_reflect".to_string(), "smol_str".to_string()),
            ],
        )],
        ..Default::default()
    }
}

const BEVY_INPUT_MANIFEST: &str = "[package]\nname = \"fixture\"\nversion = \"0.0.0\"\n\n\
     [dependencies.smol_str]\nversion = \"0.2\"\noptional = true\n\n\
     [features]\nsmol_str = [\"dep:smol_str\", \"bevy_reflect/smol_str\"]\n";

/// `process_crate` runs for the main crate too, and it runs before `bin/main.rs`
/// can compute the pin set — so it used to hand `minimize` an empty one, which
/// reads as "nothing is pinned" and unlinks the dep before the armed pass ever
/// sees it. `None` must leave the entry alone instead.
#[test]
fn unknown_pin_set_defers_the_unlink() {
    let crate_info = bevy_input_like();
    let fixture = Fixture::with_manifest("ki14-pins-unknown", BEVY_INPUT_MANIFEST);
    let mut enable = vec!["smol_str".to_string()];

    fixture.run_minimize_with_pins(&crate_info, &mut enable, &HashSet::new(), true, None);

    assert!(
        fixture
            .feature_values("smol_str")
            .contains(&"dep:smol_str".to_string()),
        "with the pin set unknown the dep entry must survive for the armed pass, got:\n{}",
        fixture.manifest_text()
    );
    assert!(
        !fixture
            .manifest_text()
            .contains(consts::CUSTOM_FEATURES_DISABLED),
        "nothing may be orphaned into the custom-disabled list by an unarmed pass, got:\n{}",
        fixture.manifest_text()
    );
}

/// Control: an armed pass that was told nothing is pinned still strips, so the
/// test above is about the `None`/empty distinction and not about `minimize`
/// having stopped doing surgical removal.
#[test]
fn armed_pin_set_still_strips() {
    let crate_info = bevy_input_like();
    let fixture = Fixture::with_manifest("ki14-pins-armed", BEVY_INPUT_MANIFEST);
    let mut enable = vec!["smol_str".to_string()];

    fixture.run_minimize_with_pins(
        &crate_info,
        &mut enable,
        &HashSet::new(),
        true,
        Some(&HashSet::new()),
    );

    assert!(
        !fixture
            .feature_values("smol_str")
            .contains(&"dep:smol_str".to_string()),
        "an armed pass with an empty pin set must still unlink, got:\n{}",
        fixture.manifest_text()
    );
    assert!(
        fixture
            .feature_values(consts::CUSTOM_FEATURES_DISABLED)
            .contains(&"dep:smol_str".to_string()),
        "the unlinked entry must land in the custom-disabled list, got:\n{}",
        fixture.manifest_text()
    );
}

/// And when the armed pass is told the dep is pinned, the entry stays.
#[test]
fn armed_pin_set_keeps_a_pinned_dep() {
    let crate_info = bevy_input_like();
    let fixture = Fixture::with_manifest("ki14-pins-kept", BEVY_INPUT_MANIFEST);
    let mut enable = vec!["smol_str".to_string()];
    let pins: HashSet<String> = HashSet::from(["smol_str".to_string()]);

    fixture.run_minimize_with_pins(&crate_info, &mut enable, &HashSet::new(), true, Some(&pins));

    assert!(
        fixture
            .feature_values("smol_str")
            .contains(&"dep:smol_str".to_string()),
        "a pinned dep must keep its entry, got:\n{}",
        fixture.manifest_text()
    );
}

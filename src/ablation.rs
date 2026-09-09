//! Process-wide flags for the ablation study (see `ablation_plan.md`).
//!
//! Six of the nine mechanisms the study measures are threaded 4-5 calls deep
//! into `driver.rs`/`phases.rs`, too deep to pass as a parameter without
//! touching every intermediate signature. Following the precedent already in
//! this codebase for that shape of problem (`CRATE_REACHED_BARE_METAL` and
//! `EXPLICIT_TARGET` in `driver.rs`), the flags are set once from the CLI at
//! process startup and read via a process-wide global from wherever a
//! mechanism's call site lives.

use std::sync::OnceLock;

/// One field per mechanism in the ablation study's table of nine. `true`
/// means that mechanism is disabled for this run. See `ablation_plan.md` §1
/// for what each one names and §3 for its disabled-semantics.
#[derive(Debug, Default, Clone, Copy)]
pub struct AblationFlags {
    /// §3.1 — ignore the HIR-verified hard constraint std-finding produces,
    /// so it doesn't seed the solve.
    pub no_std_finding: bool,
    /// §3.2 — stop the covering-set search after the seed (default-features)
    /// run instead of iterating `solve_with_negation` to full coverage.
    pub no_combo_search: bool,
    /// §3.3 — skip per-dependency no_std feature analysis; deps are left at
    /// their own default features.
    pub no_dep_analysis: bool,
    /// §3.4 — don't assert `compile_error!`-derived constraints into the
    /// solver.
    pub no_compile_error_constraints: bool,
    /// §3.5 — don't run the cfg-gate / gateway resolution passes.
    pub no_gateway_resolution: bool,
    /// §3.6 — don't check whether the main crate's usage of a dependency
    /// requires bypassing the DB cache for that dependency.
    pub no_cross_crate_propagation: bool,
    /// §3.7 — don't read or write the shared no_std result DB (`db.bin`).
    pub no_db: bool,
    /// §3.8 — don't use the in-process `cargo hir` cache (L1).
    pub no_local_cache: bool,
    /// §3.9 — don't use the cross-process `cargo hir` cache (L2).
    pub no_global_cache: bool,
}

impl AblationFlags {
    /// Names of the flags set on this instance, in table order — used to
    /// populate `Telemetry::ablation_flags` so a dumped result self-describes
    /// which arm produced it.
    pub fn active_names(&self) -> Vec<String> {
        let candidates: [(bool, &str); 9] = [
            (self.no_std_finding, "no_std_finding"),
            (self.no_combo_search, "no_combo_search"),
            (self.no_dep_analysis, "no_dep_analysis"),
            (
                self.no_compile_error_constraints,
                "no_compile_error_constraints",
            ),
            (self.no_gateway_resolution, "no_gateway_resolution"),
            (
                self.no_cross_crate_propagation,
                "no_cross_crate_propagation",
            ),
            (self.no_db, "no_db"),
            (self.no_local_cache, "no_local_cache"),
            (self.no_global_cache, "no_global_cache"),
        ];
        candidates
            .into_iter()
            .filter(|(active, _)| *active)
            .map(|(_, name)| name.to_string())
            .collect()
    }
}

static ABLATION_FLAGS: OnceLock<AblationFlags> = OnceLock::new();

/// Set once at startup from the parsed CLI flags, before any analysis runs.
/// A second call is a no-op (the flags are process-wide and never change
/// after `main` reads the CLI once).
pub fn set_flags(flags: AblationFlags) {
    let _ = ABLATION_FLAGS.set(flags);
}

/// The active ablation flags for this process. Defaults to all-off (the
/// unmodified tool's behavior) when `set_flags` was never called, e.g. in
/// tests that exercise `driver`/`phases` directly without going through
/// `main`.
pub fn flags() -> AblationFlags {
    ABLATION_FLAGS.get().copied().unwrap_or_default()
}

#[cfg(test)]
mod tests {
    use super::*;

    // Exercises `AblationFlags::active_names()` directly on a value that
    // never goes through `set_flags`/`flags()` — the global `OnceLock` can
    // only be set once per process, so a test that raced other tests to set
    // it would be order-dependent on however `cargo test` schedules this
    // binary's tests.
    #[test]
    fn active_names_lists_only_set_flags_in_table_order() {
        assert!(AblationFlags::default().active_names().is_empty());

        let flags = AblationFlags {
            no_dep_analysis: true,
            no_global_cache: true,
            ..Default::default()
        };
        assert_eq!(
            flags.active_names(),
            vec!["no_dep_analysis", "no_global_cache"]
        );

        assert_eq!(
            AblationFlags {
                no_std_finding: true,
                no_combo_search: true,
                no_dep_analysis: true,
                no_compile_error_constraints: true,
                no_gateway_resolution: true,
                no_cross_crate_propagation: true,
                no_db: true,
                no_local_cache: true,
                no_global_cache: true,
            }
            .active_names(),
            vec![
                "no_std_finding",
                "no_combo_search",
                "no_dep_analysis",
                "no_compile_error_constraints",
                "no_gateway_resolution",
                "no_cross_crate_propagation",
                "no_db",
                "no_local_cache",
                "no_global_cache",
            ]
        );
    }
}

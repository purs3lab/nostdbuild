# Ablation study plan

Goal: measure how much each of 9 mechanisms in the no_std detection pipeline
contributes to the final result, by running the tool with each one turned off
(individually, and in a few combinations) over a fixed corpus and diffing the
output against the unmodified tool.

This doc is written for whoever implements the toggles — it names exact
functions, call sites, and what "disabled" should degrade to for each
mechanism, plus how the runs should be organized so the comparison is valid.
Read the whole doc before writing code: sections 2 and 6 (dependency
ordering, cache) affect how *all nine* toggles must be implemented, not just
one.

## 1. The nine mechanisms

| # | Name | Core function(s) | File |
|---|------|-------------------|------|
| 1 | Std finding | `analyze_crate`, `probe_usages`, `probe_candidates` | [driver.rs](src/driver.rs), [phases.rs](src/phases.rs) |
| 2 | All-combinations (covering-set) search | `find_feature_combs_for_all_code`, `solve_with_negation` | [driver.rs:2965](src/driver.rs#L2965), [phases.rs:14](src/phases.rs#L14) |
| 3 | Dep analysis | `process_dep_crate_wrapper` | [main.rs:104](src/bin/main.rs#L104) |
| 4 | compile_error! constraint modeling | `dependency_compile_error_constraints`, `compile_error_infeasible_backend_constraints` | [driver.rs:2391](src/driver.rs#L2391), [driver.rs:2541](src/driver.rs#L2541) |
| 5 | cfg-gate / gateway resolution | `resolve_local_facade_gateways`, `resolve_import_to_use_gateways` | [driver.rs:455](src/driver.rs#L455), [driver.rs:662](src/driver.rs#L662) |
| 6 | Cross-crate impl/path requirement propagation | `dep_carries_impl_requirements`, `dep_carries_path_requirements`, `transitive_impl_requirements`, `transitive_path_requirements` | [driver.rs:4222-4736](src/driver.rs#L4222) |
| 7 | DB result cache (`--no-db`) | `db::read_db_file`, `db::get_from_db_data`, `db::write_db_file` | [db.rs](src/db.rs), call sites in [main.rs](src/bin/main.rs) |
| 8 | In-process `cargo hir` cache (`--no-local-cache`) | `CARGO_HIR_CACHE` (L1) inside `run_cargo_hir_cached` | [driver.rs:1401](src/driver.rs#L1401), [driver.rs:1748](src/driver.rs#L1748) |
| 9 | Cross-process `cargo hir` cache (`--no-global-cache`) | `persistent_cache_dir`/`persistent_cache_paths`/`read_persistent_cache_entry`/`write_persistent_cache_entry` (L2) inside `run_cargo_hir_cached` | [driver.rs:1518-1721](src/driver.rs#L1518), [driver.rs:1748](src/driver.rs#L1748) |

## 2. These are not 9 independent stages — read this before implementing

Mechanisms 4, 5, and 6 are sub-mechanisms that run *inside* 1, 2, and 3, not
parallel stages you can freely cross with them:

- **#5 (gateway resolution)** runs as a correction pass over the output of
  **#1 (std finding)**, inside `analyze_crate` (`resolve_import_to_use_gateways`
  at [driver.rs:5710](src/driver.rs#L5710)) and inside `run_default_features_pass`
  (`resolve_local_facade_gateways` at [driver.rs:959](src/driver.rs#L959)). It
  has nothing to resolve if std-finding didn't run.
- **#4 (compile_error modeling)** is called *inside* **#2's** own function
  (`find_feature_combs_for_all_code`, [driver.rs:3059](src/driver.rs#L3059))
  as one of several constraint sources feeding the Z3 solve. Disabling #2
  wholesale makes #4's toggle moot for that code path (but #4 has two other
  call sites — see §3.4 — that survive independently).
- **#6 (cross-crate impl/path propagation)** only has an effect through
  **#3 (dep analysis)** — it's consulted once per dependency in
  `process_dep_crate_wrapper` ([main.rs:136-163](src/bin/main.rs#L136)) to
  decide whether to trust a cached DB answer for that dependency. With #3
  off, #6 has nothing to run inside.
- **#1 (std finding)** is the foundation all the others read from (the HIR
  plugin's `FeatureRunOutput`, the `ModNode` tree). It cannot be "turned off"
  the way the other five can without the tool producing no verdict at all.
- **#7 (DB cache)** is the odd one out in the other direction: it's not
  nested inside any of 1-6, it's a shortcut that *replaces* a dependency's
  entire #1+#2 analysis with a cached answer when one exists
  ([main.rs:161](src/bin/main.rs#L161)). #6 exists specifically to decide
  when that shortcut isn't safe to take (see §3.6/§3.7) — so #7 and #6
  interact, but #7 doesn't sit "inside" #1-#3 the way #4/#5/#6 do.
- **#8 and #9 (the `cargo hir` compile caches)** are different again: they
  sit *underneath* #1, #2, and #3 alike, not inside any single one of them.
  Every `cargo hir` invocation the tool ever makes — whether it's #1
  classifying the main crate, #2 trying another feature combination, or #3
  analyzing a dependency — goes through the one function,
  `run_cargo_hir_cached` ([driver.rs:1748](src/driver.rs#L1748)), and that
  function is where both cache layers live. Unlike #7, #8/#9 are not
  expected to change *results* at all: `CargoHirCacheKey` is documented in
  the code as "a pure function of" the compile inputs
  (manifest hash, sorted features, target, `has_lib`) — see
  [driver.rs:1744](src/driver.rs#L1744) — so a cache hit should be
  indistinguishable from a fresh compile. Their ablation is mainly a
  *speed* measurement, with a same-key-different-value diff being a bug
  report about the cache key, not a design tradeoff like #6/#7's. See §3.8
  and §3.9.

Because of this, **do not build a full 2^9 grid**. Build single-toggle arms
first (§7), and only combine a sub-mechanism's toggle with its parent
mechanism's toggle when the combination is actually distinct from the parent
alone (e.g. "#3 off" and "#3 off + #6 off" are the same run — skip the
second).

## 3. Per-mechanism detail

### 3.1 Std finding

What it does: runs the `cargo-hir` rustc plugin pass, builds the `ModNode`
module tree, and classifies every code span as std-reachable or not under a
given feature/cfg condition. Everything downstream (the Z3 combination
search, dep analysis, gateway resolution) consumes its output
(`FeatureRunOutput`: path records, impl records).

**Why it can't be toggled like the others:** there's no fallback code path
in this tool that produces a no_std verdict without it — turning it off
doesn't produce a *worse* verdict, it produces *no* verdict.

**Proposed "disabled" semantics:** don't touch `analyze_crate`. Instead, add
a separate, much cheaper verdict path that trusts the crate's own declared
attribute instead of verifying it:

- The declared-intent bits already exist independent of the HIR plugin:
  `Telemetry::main_unconditional_no_std` and
  `Telemetry::main_conditional_no_std`, set in
  [parser.rs:358](src/parser.rs#L358) and
  [parser.rs:378](src/parser.rs#L378) straight from the crate's
  `#![no_std]` / `#![cfg_attr(cond, no_std)]` attribute — no compilation,
  no HIR.
- The "std finding disabled" arm's verdict should be: no_std ⟺ the crate
  declares one of those attributes, full stop, with no verification that the
  declared condition is achievable or that the code actually compiles
  no_std under it.
- This is a real, meaningful baseline to compare against, not a stub: it's
  the naive answer a scraper that only reads crate metadata would give, and
  quantifies how much the HIR-verified pipeline (§3.1's normal path) buys
  over trusting the crate's self-report.

Implementation: in [main.rs](src/bin/main.rs), gate the call into
`analyze_crate_wrapper`/`analyze_crate` behind the ablation flag; when the
flag says std-finding is off, skip straight to a verdict built from
`main_unconditional_no_std` / `main_conditional_no_std` (treat a declared
conditional attribute as satisfied without solving anything). Confirm this
exact semantics with whoever's driving the study before writing it — it's
a design choice, not something read off the code.

### 3.2 All-combinations (covering-set) search

**Corrected from the original draft of this section** (kept implemented as
`--no-combo-search`; see §7): the description below was wrong about which
functions this mechanism actually calls, discovered while wiring the toggle.
`run_default_features_pass` ([driver.rs:862](src/driver.rs#L862)) is *not*
called from `find_feature_combs_for_all_code` at all — it's invoked
separately, later, in `analyze_crate`, purely to compute a diagnostic
`coverage_comparison`; its output never feeds the solve. And
`solve_with_negation` ([phases.rs:14](src/phases.rs#L14)) belongs to the
*probing* subsystem (mechanism #1 — `probe_one_target`/`gate_satisfied_std_spans`
in [phases.rs](src/phases.rs), consuming the hard constraints this mechanism
produces), not to this mechanism's own search loop.

What it actually does: `find_feature_combs_for_all_code`
([driver.rs:2997](src/driver.rs#L2997)) runs a CEGAR loop
([driver.rs:3230-3366](src/driver.rs#L3230)) that calls
`solver::get_solved_sets` ([driver.rs:3247](src/driver.rs#L3247)) to partition
*all* of the crate's classified code spans into covering sets in one pass,
compiles each set, and only re-iterates — re-partitioning around whichever
sets failed to compile, via a `forbidden` constraint — when a compile failure
leaves spans uncovered. So even the first iteration already finds multiple
feature combinations; the loop's job is failure-driven retry, not "discover
one more combination per pass." (A conditional-no_std crate also gets a fixed
"baseline no_std run" before the loop, and a crate with zero classified spans
gets a one-off "empty items fallback" — both untouched by this toggle, see
below.)

**Implemented "disabled" semantics:** cap the CEGAR loop to its first
iteration. The loop still runs whatever covering sets `get_solved_sets` finds
on that first pass (still possibly several feature combinations — this is not
reducible to "one default-features run" given the actual code shape), but a
compile failure no longer triggers re-partitioning/retry. This is the
narrowest cut that isolates "no failure-driven CEGAR retries" without
touching the initial multi-set solve, which is architecturally inseparable
from the rest of the function.

Where it's added: [driver.rs:3366](src/driver.rs#L3366), the loop's existing
`if !made_progress { break; }` now also breaks when
`ablation::flags().no_combo_search` is set.

**What to measure:** how many spans that were only covered by a *retried*
(post-failure) run in the full pipeline go unclassified (or get
force-assumed std) in this arm — a narrower question than the original draft
posed, since the first-pass multi-set solve is unaffected. `classify_spans` /
`get_conditional_spans` in [phases.rs](src/phases.rs) are where those spans
are identified in the normal run — diff their output between arms.

**Second correction, found while validating the ablation arms against real
crates rather than just reading the diff.** The CEGAR loop above is not the
only failure-driven retry over the main crate's own feature selection —
[main.rs](src/bin/main.rs) runs two more, *after* final verification has
already failed on every target: the KI-30 build-enabler search
(`driver::discover_build_enablers`) and its mirror, the R34-23 feature-removal
search (`driver::search_removals`). Both are the same shape as the CEGAR
retry this toggle caps — try something else because the emitted set failed —
just running later, outside `find_feature_combs_for_all_code` entirely. Left
unguarded, `--no-combo-search` measured "no CEGAR retries" while these two
kept retrying anyway. Both are now gated the same way, `!ablation::flags().no_combo_search`
added to their own `if no_std && !one_succeeded` guards.

### 3.3 Dep analysis

What it does: `process_dep_crate_wrapper` ([main.rs:104](src/bin/main.rs#L104))
is called once per direct dependency (call sites at
[main.rs:969](src/bin/main.rs#L969) and
[main.rs:1017](src/bin/main.rs#L1017), inside a loop over the dep list). It
runs the *same* std-finding + combination-search pipeline recursively on
each dependency, decides what features that dependency needs to be no_std,
and either serves that from the DB cache or computes it fresh.

Note: this is distinct from the `--no-recursive` CLI flag already in
[main.rs:34](src/bin/main.rs#L34) — that flag only skips one *final sanity
check* (`parser::recursive_dep_requirement_check`, gated at
[main.rs:1796](src/bin/main.rs#L1796)) that re-verifies the whole dep tree
once the top-level answer is assembled. It does not skip per-dependency
analysis itself. Don't reuse `--no-recursive` for this ablation arm; it
measures a different thing.

**Proposed "disabled" semantics:** treat every dependency as-is (its default
feature set, no no_std-specific feature selection), i.e. don't call
`process_dep_crate_wrapper` at all for the ablation arm — leave
`main_features`/`deps_args`/`disable_default` untouched by dependency
analysis and let the final build attempt whatever the main crate's own
features imply for its deps. Expect a large increase in failed builds; that
delta *is* the measurement (how much of the tool's success rate depends on
actively steering dependency features vs. just inheriting the main crate's
choices).

**Second correction, found the same way as §3.2's.** `process_dep_crate_wrapper`
is not the only place a dependency's feature set gets steered — [main.rs](src/bin/main.rs)
runs two reactive retries, after final verification has failed on every
target, that read a dependency's own declared features directly and add one
to the edge: the R34-16 dep-edge retry (`parser::dep_edge_retry_candidates`)
and the KI-34 transitive-package promotion (`parser::implicated_transitive_package`
+ `parser::add_synthetic_dependency`, for a dependency-of-a-dependency rustc's
own diagnostic names). Neither reads anything `process_dep_crate_wrapper`
produced — both are independent, always-available mechanisms — so
`--no-dep-analysis` measured "no proactive dependency steering" while these
two kept steering dependency features reactively regardless. Both are now
gated the same way, `!ablation::flags().no_dep_analysis` added to their own
`if no_std && !one_succeeded` guards.

### 3.4 compile_error! constraint modeling

**Corrected from the original draft of this section** (kept implemented as
`--no-compile-error-constraints`; see §7): the "three call sites" list below
was missing a fourth, real source, found while wiring the toggle — this
crate's own `compile_error!`, parsed not by a driver.rs function but directly
by the visitor during the module-tree walk. It fits the opening sentence's
stated scope ("a dependency's (or the crate's own)") even though the
call-site list never named it. Also corrected: `dependency_feature_requirement`
does not exist solely for this mechanism — most of its output is unrelated
feature-table semantics that must survive the toggle, not be zeroed with it.

What it does: reads a dependency's (or the crate's own) `compile_error!`
macro invocations that are conditioned on features (e.g. "you must enable
`std` or `libm`") and turns them into Z3 constraints, so the solver never
picks a feature combination that would hit that macro and fail to compile.
Four call sites, three distinct sources:

- **The crate's own, general case** — parsed directly during the module-tree
  walk ([visitor.rs:2594-2648](src/visitor.rs#L2594)) into
  `ModCollector::hard_constraints`, which becomes `compile_error_constraints`
  at [driver.rs:3083](src/driver.rs#L3083) and seeds `all_hard` at
  [driver.rs:3108](src/driver.rs#L3108) — the "seed veto" that keeps the
  covering-set search from proposing a configuration this crate's own
  `compile_error!` forbids. Runs for the main crate and every dependency
  alike (`find_feature_combs_for_all_code` runs recursively for both).
- `dependency_compile_error_constraints` — a *dependency's* `compile_error!`
  constrains the crate being analyzed. Called from inside
  `find_feature_combs_for_all_code` ([driver.rs:3099-3100](src/driver.rs#L3099))
  and from `dependency_feature_requirement`
  ([driver.rs:2734](src/driver.rs#L2734), used on the dependency-analysis
  side) — but that function's `parts` also accumulates
  `solver::feature_implication_constraints` and
  `solver::optional_dep_implication_constraints`, which are the crate's own
  `[features]`-table semantics, not `compile_error!` modeling at all; only
  the `dependency_compile_error_constraints` slice of `parts` belongs to this
  mechanism, and the other two must not be disturbed by this toggle.
- `compile_error_infeasible_backend_constraints` — a crate's *own*
  `compile_error!` used to pick between backends (e.g. a crypto crate
  choosing among optional-dep backends) forbids a backend feature if that
  backend has no no_std attribute at its root. Called at
  [driver.rs:6168](src/driver.rs#L6168), inside the function that computes
  `final_condition` for `analyze_crate`.

**Implemented "disabled" semantics:** at all four sites, the returned/parsed
constraints are excluded from `all_hard` / `parts` / `final_condition` rather
than asserted into the solver — for `dependency_feature_requirement`
specifically, only the `dependency_compile_error_constraints` slice is
dropped, the feature-implication and optional-dep-implication constraints
stay.

**What to measure:** count of runs where the tool now picks a feature
combination that fails to build specifically on the `compile_error!` line
(grep the build failure stderr for the macro's message text) — those are
false "success" verdicts this mechanism was preventing.

**Third correction, found empirically: a fifth site, reactive rather than
proactive, that the "four call sites" framing above never accounted for
because it is architecturally a different mechanism reading the same data.**
[main.rs](src/bin/main.rs) has its own post-build repair for a violated
`compile_error!` — `parser::violated_compile_error_constraints` (the check)
and `parser::compile_error_repair_features` (the KI-11-shaped repair: only
after a build that failed on every target, applied by a retry, kept only if
the rebuild succeeds) — deliberately *not* fed into the solve the other four
sites feed (see that function's own doc comment: asserting a disjunctive
`compile_error!` into the solve lets Z3 satisfy it with an arbitrary disjunct,
which is what breaks uom). This reactive repair is a real, independent second
line of defense, not a duplicate implementation of the same veto — and before
this correction it was completely unguarded by `--no-compile-error-constraints`.

Confirmed live on `bulletproofs-bls-4.0.0` (KI-3's own crate, `rust` vs `blst`
backend selection): baseline, `--no-compile-error-constraints` alone, and that
flag combined with `--no-combo-search` all produced an *identical* `Success`
build with the same `--features bls12_381_plus,rust,custom_no_std_feature_enabled`
— the proactive veto (`compile_error_infeasible_backend_constraints`, §3.4's
own mechanism) was fully disabled in the second and third runs, and the
reactive repair below found and applied the same `rust` fix anyway, from the
build failure alone. **A single-mechanism ablation arm can look like a no-op
purely because a different, unguarded mechanism independently reaches the
same answer** — this is not specific to compile_error and is worth checking
for on any arm that shows no diff on a crate believed to exercise that
mechanism.

Fixed: `main.rs`'s call to `violated_compile_error_constraints`, and every
downstream recompute of that value (after the KI-30 build-enabler repair, the
R34-16 dep-edge retry, the R34-23 feature-removal repair, and the KI-34
transitive-package promotion all independently re-derive it once *they*
succeed), now returns empty under `ablation::flags().no_compile_error_constraints`
— which also transitively disables `compile_error_repair_features`, since
nothing calls it once `violated` never has anything in it.

### 3.5 cfg-gate / gateway resolution

What it does: two related passes that re-attribute a std usage to the
correct guarding condition when the naive per-span cfg doesn't tell the
whole story:

- `resolve_local_facade_gateways` ([driver.rs:455](src/driver.rs#L455)) —
  when a local module re-exports (facades) an externally-gated item, a std
  usage reached only through that re-export inherits the re-export's gate
  instead of being reported as ungated.
- `resolve_import_to_use_gateways` ([driver.rs:662](src/driver.rs#L662)) —
  joins a routeless bare std use (e.g. a `HashMap` pulled in by a glob
  re-export) back to the `use` statement that actually names it, by
  `def_path`, so it can inherit that import's gate. Needs the `ModNode` tree,
  so it only runs inside `analyze_crate`
  ([driver.rs:5710](src/driver.rs#L5710)), not in
  `run_default_features_pass`.

Both are called from four sites total: [driver.rs:959](src/driver.rs#L959)
and [driver.rs:2160](src/driver.rs#L2160) (facade resolution, two different
passes), [driver.rs:5710](src/driver.rs#L5710) and
[driver.rs:5851](src/driver.rs#L5851) (import-to-use resolution).

**Proposed "disabled" semantics:** no-op both functions (skip the call, or
have them return immediately without mutating `FeatureRunOutput`/without
incrementing `telemetry.routed_import_anchors`) at all four sites. A std
usage that would have inherited a gate now reports as ungated (i.e., as
unconditionally std) instead.

**What to measure:** this is the mechanism your `visitor-cfg-gate-coverage`
and `cfg-gate-false-positive-cases` work already characterized as a false-positive
source — with it off, expect an increase in crates the tool now reports as
"unconditionally uses std" (false negative for no_std-capability) that the
full pipeline correctly resolved as conditionally gated.

### 3.6 Cross-crate impl/path requirement propagation

What it does: when a dependency's own no_std answer is already cached in
the DB, the tool normally trusts that cached answer. This mechanism is the
check that decides *not* to trust it: if the main crate's own code calls a
method on a dependency's type (`dep_carries_impl_requirements`,
[driver.rs:4222](src/driver.rs#L4222)) or names an item from that dependency
(`dep_carries_path_requirements`, [driver.rs:4254](src/driver.rs#L4254)) in
a way that could require a feature the dependency-alone analysis wouldn't
know to force on, the DB is bypassed and the dependency is re-analyzed with
that context. `transitive_impl_requirements` /
`transitive_path_requirements` ([driver.rs:4166](src/driver.rs#L4166),
[driver.rs:4518](src/driver.rs#L4518)) do the same thing across an edge one
level further removed (a dependency-of-a-dependency).

Call site: [main.rs:136-163](src/bin/main.rs#L136), inside
`process_dep_crate_wrapper` — `has_impl_requirements` and
`has_path_requirements` gate whether the DB lookup at
[main.rs:161](src/bin/main.rs#L161) is allowed to short-circuit the analysis.

**Proposed "disabled" semantics:** force `has_impl_requirements` and
`has_path_requirements` to `false` unconditionally (skip calling
`dep_carries_impl_requirements`/`dep_carries_path_requirements`, or call them
and discard the result) when the flag is set — this makes every dependency
eligible for the DB shortcut regardless of what the main crate does with its
types, restoring the pre-KI-27 behavior.

**What to measure:** cases where a dependency needed a feature (e.g.
`zeroize/alloc`) *because of how the main crate uses it*, not because of
anything in the dependency's own source — those regress specifically when a
DB entry already exists for that dependency from a different, less-demanding
caller. This means the effect is easiest to see on a corpus where the DB
already has entries for shared dependencies (i.e. don't wipe `db.bin` between
building the corpus and running this specific arm's comparison baseline —
see §6 for how this interacts with the general cache-cold rule).

### 3.7 DB result cache

What it does: `db.bin` stores, per `name_with_version`, the `(enable,
disable)` feature pair the tool decided a crate needs for no_std — main
crates and dependencies alike, both written back through the same table.
Flow in [main.rs](src/bin/main.rs):

- `db::read_db_file()` ([main.rs:357](src/bin/main.rs#L357)) loads the whole
  file into `exchange.db_data` once, at the start of the process.
- `db::get_from_db_data` is consulted twice: inside
  `process_dep_crate_wrapper` ([main.rs:162](src/bin/main.rs#L162), gated by
  §3.6's `has_impl_requirements`/`has_path_requirements` check) to possibly
  skip a dependency's analysis entirely, and inside `parser::parse_deps_crate`
  ([main.rs:910](src/bin/main.rs#L910)).
- `db::add_to_db_data` ([main.rs:1785](src/bin/main.rs#L1785)) records the
  *main* crate's own result once its analysis finishes successfully.
- `db::write_db_file` ([main.rs:1813](src/bin/main.rs#L1813)) persists
  everything back to disk at the end of every run — so the file accumulates
  across every crate ever analyzed, in any process invocation, whether that
  crate was analyzed as someone's main crate or reached as a dependency.

This is the mechanism behind the `db-cache-invalidates-verification` concern
already on file: a hit means the analysis under test didn't run at all for
that crate, it just reused whatever an earlier, possibly differently-scoped
run decided.

**Proposed `--no-db` semantics:** when set, skip `read_db_file` (start
`exchange.db_data` empty), make both `get_from_db_data` call sites always
return `None` (or just don't call them), and skip `write_db_file` at the end
— i.e. this run neither reads nor writes the shared cache, full isolation.
Skipping the write matters: without it, a `--no-db` run would still
pollute `db.bin` for every *other* arm/run sharing the file, which is exactly
the contamination §6 warns about.

Note this is simpler to wire than mechanisms 1-6: `db_data` never leaves
[main.rs](src/bin/main.rs) — it isn't threaded down into `driver.rs`/`phases.rs`
the way the HIR output and solver context are. A single `Cli` flag read at
the two `main.rs` call sites and the one write site is enough; it doesn't
need to go through the `AblationFlags` global described in §4 for the other
six (though putting it there too, for uniformity and so `Telemetry` records
it, is fine).

**What this arm is for:** unlike §6's cache-cold rule (which controls the
cache so the *other six* arms are comparable to each other), this arm asks a
different question — does the cache change results at all, on top of what it
obviously changes (runtime)? Compare it against the baseline (§7 arm 1, run
with the cache behaving normally, not wiped):

- If verdicts and feature sets are identical between baseline and
  `--no-db`, the cache is sound for this corpus — it's purely a speed
  optimization, and #6 is successfully catching every case where reusing a
  cached answer would have been wrong.
- Any verdict or feature-set diff is a case #6 (or the DB's
  parent-independent design generally) is missing — a real finding, not
  noise, since a `(enable, disable)` pair keyed only by `name_with_version`
  is a stated approximation the tool already knows can be wrong (§3.6).
- Expect a substantial wall-time increase with `--no-db` on any corpus
  with shared dependencies (`serde`, `zeroize`, etc. reachable from many
  crates) — that cost is the price of the accuracy (or lack of accuracy
  difference) found above, and is the other half of this arm's answer to
  "does the DB help."

Run this arm on a corpus slice where common dependencies actually recur
across multiple crates — on a corpus where every crate's dependency set is
unique, the cache never gets a hit anyway and the arm proves nothing.

### 3.8 In-process `cargo hir` cache (L1)

What it does: every call to `run_cargo_hir_cached`
([driver.rs:1748](src/driver.rs#L1748)) — the one function every `cargo hir`
invocation in the whole tool goes through, called from
`run_default_features_pass` ([driver.rs:900](src/driver.rs#L900)) and from
`run_rustc_plugin_pass_with` ([driver.rs:2032](src/driver.rs#L2032)) — first
checks a static in-memory map, `CARGO_HIR_CACHE`
([driver.rs:1401](src/driver.rs#L1401), a
`Mutex<HashMap<CargoHirCacheKey, CargoHirCacheValue>>`). `CargoHirCacheKey`
is `{manifest_hash, feats_sorted, target, has_lib}` — so a hit means "this
exact process already ran `cargo hir` with this exact manifest, feature set,
and target once already," which happens constantly: the combo search (#2)
and dep analysis (#3) both re-derive overlapping feature sets across many
solver iterations and many dependencies. This map lives only for the
lifetime of one process (one top-level crate's whole analysis, main crate
plus its full dependency recursion) — it's never written to disk and
disappears when the process exits.

**Proposed `--no-local-cache` semantics:** in `run_cargo_hir_cached`, skip
the `CARGO_HIR_CACHE.lock().unwrap().get(k)` check at
[driver.rs:1769](src/driver.rs#L1769) and the two
`CARGO_HIR_CACHE.lock().unwrap().insert(...)` calls that follow a miss
(after an L2 hit at [driver.rs:1794](src/driver.rs#L1794), and after a fresh
compile at [driver.rs:1801](src/driver.rs#L1801)/
[driver.rs:1814](src/driver.rs#L1814)) — i.e. the function behaves as if L1
never has anything in it, on every call, for the whole run. L2 (the
persistent cache, §3.9) is untouched by this flag unless `--no-global-cache`
is also set.

This is the cheapest of all nine flags to implement: it's a handful of
early-return checks inside one already-small function, no new call sites
elsewhere in the tool.

**What to measure:** primarily wall time (every repeat `cargo hir` call
within one process now actually recompiles). A verdict/feature-set diff
here is not an expected finding — investigate it as a cache-key bug (see §2)
rather than reporting it as a design tradeoff the way §3.6/§3.7 findings are.

### 3.9 Cross-process `cargo hir` cache (L2)

What it does: on an L1 miss, `run_cargo_hir_cached` checks a *disk*-backed
cache shared by every process on the machine, rooted at
`consts::CARGO_HIR_CACHE_DIR` (`/evaldisk/sourag/cargo_hir_cache` —
[consts.rs:11](src/consts.rs#L11), overridable via the
`NO_STD_TOOL_TEST_CARGO_HIR_CACHE_DIR` env var, currently only used by
tests). The cache key adds a `plugin_version_stamp`
([driver.rs:1547](src/driver.rs#L1547), a hash of the installed
`cargo-hir`/`hir-driver` binaries' size+mtime) on top of
`CargoHirCacheKey`, so a rebuilt plugin can't serve a stale answer under the
same filename. `persistent_cache_claim_and_compute`
([driver.rs:1667](src/driver.rs#L1667)) handles the miss path: a per-key
lock file (`fs::TryLock`, not a correctness requirement — just avoids two
processes compiling the same key at once) is taken, the entry is computed
and written via temp-file-then-atomic-rename, and a successful lookup or
computation is also inserted into L1 ([driver.rs:1794](src/driver.rs#L1794),
[driver.rs:1801](src/driver.rs#L1801)) so a third call in the same process
hits L1 instead of touching disk again.

**Proposed `--no-global-cache` semantics:** in `run_cargo_hir_cached`, skip
the whole L2 block ([driver.rs:1783-1806](src/driver.rs#L1783)) on a
declared cache miss and fall straight to the "no key, or persistent cache
unavailable" branch at [driver.rs:1811](src/driver.rs#L1811)
(`compile_cargo_hir_uncached`), still populating L1 with the fresh result
(unless `--no-local-cache` is also set). This means never reading
`persistent_cache_dir()` and never calling
`write_persistent_cache_entry` — so, unlike simply pointing at an empty
directory, this run also never pollutes the real shared cache for anyone
else's concurrent or later process.

**This cache is the one place in the whole study where "just start with an
empty directory" (§6's usual cache-cold rule) is not enough on its own,**
because `CARGO_HIR_CACHE_DIR` is one fixed absolute path shared by every
process on the box — including runs that have nothing to do with this
study. Two consequences:

1. For the **baseline** arm (cache behaving normally), don't point it at the
   real shared directory if you want a clean, attributable hit-rate/speedup
   number — an unknown fraction of hits would come from whatever unrelated
   work already warmed that directory, not from this corpus's own
   cross-crate sharing. Use the existing test-only override,
   `NO_STD_TOOL_TEST_CARGO_HIR_CACHE_DIR`, pointed at a private directory
   for the whole study (warm it once by running the corpus through
   uninstrumented, then treat that as the "cache-warm" starting point for
   every arm that wants one).
2. For the **`--no-global-cache`** arm itself, the flag already makes this
   moot (it never touches the directory, real or overridden) — but confirm
   the implementation actually takes the early-return path and doesn't, say,
   still call `persistent_cache_dir()` for a metadata check that happens to
   touch the shared filesystem.

**What to measure:** same shape as §3.8 — wall time is the primary signal
(expect a larger effect than L1 alone, since L2 is what lets a *second*
process, e.g. a later corpus crate that happens to share a dependency's
exact feature set, skip a compile entirely). Also report L1-vs-L2 hit
counts separately in the baseline (`cargo_hir_cache_hits_for_test` /
`cargo_hir_persistent_cache_hits_for_test` in
[driver.rs:1838-1872](src/driver.rs#L1838) are test-only accessors for
exactly these counters — either reuse them directly if the ablation harness
can link against the crate as a lib, or add non-test-gated equivalents) so
`--no-local-cache` and `--no-global-cache`'s individual contributions aren't
conflated into one number.

## 4. Implementation approach

Six of the nine toggles (#1-#6) need to reach deeply-nested functions (some
4-5 calls down from `main`) without threading six new parameters through
every intermediate signature in a 6000-line file. The codebase already has a
precedent for this: cross-cutting per-run state that many nested functions
need is kept in a small number of `static`/`Mutex`-guarded globals, reset
once per crate — see `CRATE_REACHED_BARE_METAL` and `CRATE_BARE_METAL_SETS`
in [driver.rs](src/driver.rs) and `set_explicit_target` in
[target_cfg.rs:177](src/target_cfg.rs#L177). The other three are simpler:
#7 (`--no-db`) doesn't need this at all — `exchange.db_data` never leaves
`main.rs`, so a plain `Cli` field checked at its three call sites is enough
(see §3.7). #8 and #9 (`--no-local-cache`/`--no-global-cache`) are checked
in exactly one place, inside `run_cargo_hir_cached`
([driver.rs:1748](src/driver.rs#L1748)) — they still need to read the same
process-wide flag store as #1-#6 (that function has no other way to learn
what the CLI was invoked with), but nothing about their own logic is
nested or threaded (see §3.8/§3.9).

Follow that pattern:

1. Add an `AblationFlags` struct (nine `bool` fields, one per mechanism
   above — including `no_db`, `no_local_cache`, and `no_global_cache`, for
   uniformity and so `Telemetry` can record all nine even though #7's own
   implementation doesn't strictly need the global) in a small new module
   (or next to `consts.rs`), with a `OnceLock` (or the same
   `Mutex`-guarded-static style already used) holding the process-wide
   value, set once at startup from CLI flags and read via a `fn ablation() ->
   AblationFlags` accessor.
2. In [main.rs](src/bin/main.rs), add nine new `#[arg(long)]` fields to
   `Cli` following the existing `no_recursive` pattern (same doc-comment
   style, same naming convention — e.g. `no_std_finding`, `no_combo_search`,
   `no_dep_analysis`, `no_compile_error_constraints`, `no_gateway_resolution`,
   `no_cross_crate_propagation`, `no_db`, `no_local_cache`,
   `no_global_cache`). Set the global `AblationFlags` from these right after
   `Cli::parse()`, before anything else runs.
3. At each call site named in §3, check the relevant flag and take the
   degraded path described. Prefer an early return / skip at the call site
   over modifying the called function's internals, so the normal (flags-off)
   code path is untouched and a `diff` of the PR is easy to audit against
   this doc.
4. Telemetry: add a field recording which flags were active for a given run
   (e.g. `Telemetry::ablation_flags: Vec<String>`) so every dumped result
   self-describes which arm produced it — this matters once results from
   different arms live in the same `results/` directory or get merged by
   `aggregate_results.py`.

## 5. Corpus and run isolation

- Pick a **fixed** crate subset up front and use the same list for every
  arm. `crates-list-no-std.txt` / `supported_july_17.txt` (used by
  [eval.py](eval.py) and [ablation.py](ablation.py) respectively — note
  `ablation.py` is an unrelated existing script, a raw `cargo build` vs
  `cargo build --no-default-features` comparison against undownloaded
  crates; don't confuse it with this study) are the existing candidate
  lists. Given the covering-set search is combinatorial
  (`sweep-space-explosion`-style cost), keep the subset modest — a few
  hundred crates, weighted toward ones already known to exercise dependency
  chains and conditional no_std (the `REVIEW_INFO.md`/`ALL_TARGET_FAILURES.md`
  corpus already used for other triage is a reasonable source).
- Each arm's output should land in its own `results/` directory (mirror the
  structure `aggregate_results.py` already expects: one JSON per crate,
  named `<name>:<version>_args.json`), e.g. `results-baseline/`,
  `results-no-combo-search/`, etc.

## 6. The cache is the single biggest way to invalidate this study

This section is about controlling `db.bin` as a *confound* across arms 1-6
(the study's methodology) — a separate concern from §3.7's `--no-db` arm,
which studies that cache as a *mechanism in its own right*. §3.7's arm still
needs this section's rules applied to whatever baseline it's compared
against. The `cargo hir` compile caches (§3.8/§3.9) have their own,
different contamination story — the L2 cache in particular is a single
directory shared box-wide, not per-study — covered where they're discussed
rather than here; see §3.9's callout specifically.

`db.bin` caches "what does dependency X need to be no_std," independent of
which arm computed it. Two failure modes if you don't control for this:

1. **Cross-arm contamination**: if arm A (baseline) populates the DB for a
   shared dependency, then arm B (e.g. "cross-crate propagation off," §3.6)
   runs and hits that same DB entry, arm B's result reflects arm A's
   computation, not arm B's degraded logic. The whole point of §3.6 is to
   measure when the DB shortcut *shouldn't* be trusted — a warm shared cache
   silently defeats exactly the comparison it's meant to produce.
2. **Within-arm skip**: a cache hit means "the analysis under test didn't
   run at all for this crate," not "the analysis ran and produced this
   result" — this is a general property of the cache in this tool, not
   specific to the ablation study.

**Rule: give every arm its own empty `db.bin`, and run each arm's crates
serially** (not concurrently against a shared cache) unless a specific arm's
measurement (like §3.6's) deliberately calls for a pre-warmed cache as its
baseline — in which case warm it once, snapshot it, and restore that
snapshot before every arm that needs the same starting point, rather than
letting arms mutate a shared live cache.

Also isolate `CARGO_TARGET_DIR` per crate per arm (existing per-crate target
dir pattern used elsewhere in this tool's eval scripts) so concurrent arms
don't lock each other out at the cargo level even though the DB is arm-local.

## 7. Suggested run matrix

Baseline + one arm per mechanism, single toggle only, is the minimum useful
set — 10 runs:

1. Baseline (no flags; `db.bin` and both `cargo hir` caches behaving
   normally, per §6/§3.9's private-warmed-cache setup — not the real shared
   `/evaldisk/sourag/cargo_hir_cache`)
2. `--no-std-finding` (§3.1 — declared-attribute-only verdict)
3. `--no-combo-search` (§3.2 — default-features-only, no negation loop)
4. `--no-dep-analysis` (§3.3 — deps left at their own defaults)
5. `--no-compile-error-constraints` (§3.4)
6. `--no-gateway-resolution` (§3.5)
7. `--no-cross-crate-propagation` (§3.6)
8. `--no-db` (§3.7 — every dependency re-analyzed fresh, no DB reads or
   writes)
9. `--no-local-cache` (§3.8 — every `cargo hir` call recompiles within a
   process, no L1 reuse)
10. `--no-global-cache` (§3.9 — no cross-process `cargo hir` reuse; L1 still
    active unless arm 9 is combined in)

Beyond that, only add a combined arm when it isolates something a single
arm can't show — e.g. `--no-dep-analysis --no-cross-crate-propagation`
together is redundant with #4 alone (per §2, #6 has no effect once #3 is
off), so skip it. A combination worth adding: `--no-compile-error-constraints
--no-combo-search`, to see whether the combo search's negation loop was
independently finding (and rejecting) the same bad feature sets the
compile_error constraints rule out directly, or whether the two catch
different cases. Another worth adding once arms 7 and 8 both exist:
`--no-cross-crate-propagation` run against a corpus/cache state where
`--no-db` has already shown a real (non-zero) diff from baseline — that's
the sharpest test of whether #6 is actually catching the cases the raw
cache-vs-no-cache diff surfaced, rather than just plausible in theory. And
`--no-local-cache --no-global-cache` together (both compile caches off at
once) isolates the combined `cargo hir` compile cost from everything else —
useful as the "how slow would this tool be with no compile memoization at
all" number, distinct from either layer's individual contribution.

## 8. Comparison metrics

For each arm vs. baseline, over the fixed corpus:

- **Verdict flips**: crates where the top-level no_std verdict
  (`Telemetry::no_std`) changed.
- **Feature-set drift**: for crates where the verdict didn't flip, did
  `final_features_length` / the actual emitted `--features` list change?
  (Emitted argv, not the manifest's declared defaults — see
  `compilation_results.json`'s `args` field, the same source of truth used
  elsewhere in this tool's own verification.)
- **New build failures**: crates that built successfully in baseline and
  fail in the ablated arm — and *why* (grep stderr for the specific
  mechanism's signature, e.g. the `compile_error!` message text for §3.4,
  "cannot find" for a dropped gateway resolution in §3.5).
- **Runtime/solver cost**: wall time and Z3 call count per crate (the
  `timing.rs` scopes already emit this — `timing::crate_scope("dep_analysis",
  ...)`, `timing::scope("coverage", ...)`, etc. are already in place at
  exactly the boundaries this study cares about, so no new instrumentation
  should be needed here, just aggregation across arms).

Report all four per mechanism, not just verdict flips — a mechanism that
never flips a verdict but silently pads the feature list, or one that costs
90% of total runtime for a 1% verdict-flip rate, are both results worth
having.

For arm 8 (`--no-db`) specifically, also report:

- **Cache hit rate in the baseline run**, on the same corpus (count of
  `get_from_db_data` hits vs. total dependency lookups) — without this
  number a zero-diff result is ambiguous between "the cache is sound" and
  "the cache was barely used on this corpus." Pick a corpus where the hit
  rate is meaningfully non-zero before trusting either conclusion.
- **Wall-time ratio** (`--no-db` total time / baseline total time) as the
  cache's speed contribution, reported alongside whatever accuracy diff (or
  lack of one) arm 8 shows — the two numbers together are the actual answer
  to "does the DB help."

For arms 9 and 10 (`--no-local-cache` / `--no-global-cache`), the framing
inverts: **a verdict or feature-set diff is the surprising result, not the
expected one** (§2, §3.8/§3.9) — these caches are supposed to be
transparent. Report:

- **Wall-time ratio** for each arm individually, plus the combined
  `--no-local-cache --no-global-cache` arm, against the same private-warmed
  baseline used everywhere else in this study (§7 arm 1) — this is the
  actual "does the compile cache help" number, and is expected to be the
  dominant effect (likely much larger than #7's, since #8/#9 sit under all
  of #1/#2/#3, not just dependency lookups).
- **Any verdict/feature-set diff at all**, flagged for follow-up as a
  `CargoHirCacheKey` completeness bug rather than folded into the "results
  changed" comparison the other eight arms use — something the compile
  depends on (an env var, a file outside the manifest, timing/nondeterminism
  in the plugin itself) isn't captured by
  `{manifest_hash, feats_sorted, target, has_lib}`.
- **L1 vs. L2 hit counts in the baseline**, so arm 9's and arm 10's
  individual wall-time contributions can be told apart from each other
  (see §3.9's last paragraph for the counters to reuse).

use log::debug;
use std::collections::HashSet;
use z3::Context;
use z3::SatResult;
use z3::Solver;
use z3::ast::Bool;

use crate::types::*;

use crate::driver::run_rustc_plugin_pass;
use crate::solver;
use crate::timing;

pub fn solve_with_negation<'a>(
    ctx: &'a Context,
    hard_constraints: &[Bool<'a>],
    negated_gate: &Bool<'a>,
    all_constraints: &[Bool<'a>],
) -> SolveResult {
    let solver = Solver::new(ctx);
    for hc in hard_constraints {
        solver.assert(hc);
    }
    solver.assert(negated_gate);
    match solver.check() {
        SatResult::Sat => {
            // Hard constrains + ¬gate is satisfiable. Now add all the other solvable constraints
            for ac in all_constraints {
                solver.push();
                solver.assert(ac);
                if let SatResult::Unsat = solver.check() {
                    solver.pop(1);
                }
            }
            // `pop` discards the model the preceding `check` produced, so when
            // the *last* optional constraint is the one that failed, the solver
            // is left holding no model at all and `get_model` returns `None`
            // ("sat without model", agnostic-0.7.2). The surviving assertion set
            // is satisfiable by construction — every unsatisfiable addition was
            // popped back off — so re-check to rebuild the model.
            let model = match solver.check() {
                SatResult::Sat => solver.get_model(),
                other => {
                    debug!("solve_with_negation: re-check after pop returned {other:?}");
                    None
                }
            };
            match model {
                Some(model) => {
                    let (enabled, disabled) = crate::solver::model_to_features(&Some(model));
                    SolveResult::Sat(enabled, disabled)
                }
                None => SolveResult::Unsat,
            }
        }
        SatResult::Unsat | SatResult::Unknown => SolveResult::Unsat,
    }
}

/// Everything accumulated for one source position across all covering runs.
struct SpanAcc {
    /// Representative record. Preferentially a std one — see `note_record`.
    exemplar: PathRecord,
    exemplar_is_std: bool,
    std_cfgs: Vec<Vec<String>>,
    non_std_cfgs: Vec<Vec<String>>,
    /// *Which* runs saw std here. `std_cfgs` pushes once per record, so a run
    /// with several std records at one span would be counted multiple times —
    /// the set keeps run identity.
    std_run_idxs: std::collections::HashSet<usize>,
    /// Distinct non-std crates that resolved at this position.
    alts: std::collections::BTreeSet<String>,
}

impl SpanAcc {
    fn new(rec: &PathRecord) -> Self {
        SpanAcc {
            exemplar: rec.clone(),
            exemplar_is_std: rec.span.usage_crate.as_deref() == Some("std"),
            std_cfgs: Vec::new(),
            non_std_cfgs: Vec::new(),
            std_run_idxs: std::collections::HashSet::new(),
            alts: std::collections::BTreeSet::new(),
        }
    }

    fn note_record(&mut self, rec: &PathRecord, run_idx: usize, features: &[String]) {
        match rec.span.usage_crate.as_deref() {
            Some("std") => {
                // A span can hold std and non-std records at once (below), and
                // which one arrives first is arbitrary. Phase 3 reads the
                // exemplar's `context` to route the span — imports are probed
                // under a `PathContext::ImportDeclaration` filter, so a non-std
                // exemplar could send a std *usage* down the import path, where
                // it can never appear in the filtered candidates and would be
                // excused as "disappeared". `is_local_reexport` reads the
                // exemplar's `local_route` with the same effect. So whenever the
                // span is std at all, the exemplar must be one of its std records.
                if !self.exemplar_is_std {
                    self.exemplar = rec.clone();
                    self.exemplar_is_std = true;
                }
                self.std_cfgs.push(features.to_vec());
                self.std_run_idxs.insert(run_idx);
            }
            Some(other) => {
                self.non_std_cfgs.push(features.to_vec());
                self.alts.insert(other.to_string());
            }
            None => {} // unresolved; ignore for classification
        }
    }
}

pub fn classify_spans(runs: &[CoveringRun]) -> Vec<SpanAnalysis> {
    let mut index: std::collections::HashMap<ReadableSpan, SpanAcc> =
        std::collections::HashMap::new();

    for (run_idx, run) in runs.iter().enumerate() {
        for rec in &run.output.records {
            index
                .entry(rec.span.clone())
                .or_insert_with(|| SpanAcc::new(rec))
                .note_record(rec, run_idx, &run.features);
        }
    }

    index
        .into_iter()
        .map(|(span, acc)| {
            // The verdict is per-run, not per-record. `ReadableSpan` equality
            // ignores `usage_crate`, so one source position routinely holds std
            // and non-std records *from the same run* — a `#[derive(...)]`
            // attribute span collects its whole expansion under one position,
            // and `impl std::error::Error` lands there next to a handful of
            // core paths. Those co-located records are not evidence that the
            // std-ness is avoidable; they are a different path that happens to
            // share a span.
            //
            // The only evidence that counts is a successful run in which this
            // span produced no std record at all. Every CoveringRun is a
            // successful compile, so such a run is a working configuration in
            // which the span is not std usage. Absent that, the span is std no
            // matter what is enabled, whatever else sits on top of it.
            let std_in_every_run =
                !acc.std_cfgs.is_empty() && acc.std_run_idxs.len() == runs.len();

            let verdict = if acc.std_cfgs.is_empty() {
                SpanVerdict::NeverStd
            } else if std_in_every_run {
                SpanVerdict::AlwaysStd
            } else {
                // Conditional both keeps the span out of `all_hard` and lets it
                // feed `final_condition` through the normal conditional-probe
                // path. `alternate_crates` is empty when nothing else ever
                // resolved here — the span was absent from those runs, not
                // resolved elsewhere.
                SpanVerdict::Conditional {
                    alternate_crates: acc.alts.into_iter().collect(),
                }
            };
            SpanAnalysis {
                span,
                verdict,
                exemplar: acc.exemplar,
                std_configs: acc.std_cfgs,
                non_std_configs: acc.non_std_cfgs,
                std_in_every_run,
            }
        })
        .collect()
}

pub fn get_always_std_imports(analyses: &[SpanAnalysis]) -> Vec<&SpanAnalysis> {
    analyses
        .iter()
        .filter(|a| {
            matches!(a.verdict, SpanVerdict::AlwaysStd)
                && a.exemplar.context == PathContext::ImportDeclaration
        })
        .collect()
}

pub fn get_always_std_others(analyses: &[SpanAnalysis]) -> Vec<&SpanAnalysis> {
    analyses
        .iter()
        .filter(|a| {
            matches!(a.verdict, SpanVerdict::AlwaysStd)
                && !matches!(a.exemplar.context, PathContext::ImportDeclaration)
        })
        .collect()
}

pub fn get_conditional_spans(analyses: &[SpanAnalysis]) -> Vec<&SpanAnalysis> {
    analyses
        .iter()
        .filter(|a| matches!(a.verdict, SpanVerdict::Conditional { .. }))
        .collect()
}

/// Is this condition already contradicted by a run the tool performed?
///
/// A conditional span with a non-empty `non_std_configs` has a **witness**: a
/// successful compile in which the span was present and resolved to something
/// other than std. A condition that is *false* in that witness cannot be what
/// makes the span non-std — the witness is non-std without it — so imposing it
/// only takes features away.
///
/// That is what a gate negation which merely deletes the code always produces.
/// uom's `si/angle.rs` spans sit inside `#[cfg(feature = "f32")]` storage
/// modules, so negating `f32` removes them from the run and the prober reads
/// that as "not std"; the witness run has `f32` **on** and the span resolving to
/// `core`, which says plainly that `f32` is not the culprit. Vetoing here rather
/// than inside the prober keeps the check independent of which gate the walk
/// happened to reach first, and of how the covering sets came out on the day —
/// `lps28dfw` built or failed run to run on exactly that nondeterminism.
///
/// Spans whose other runs merely *lacked* the span (`non_std_configs` empty)
/// have no witness and are left alone: for those, deleting the code really is
/// the only demonstrated route to no_std.
pub fn condition_contradicted_by_runs<'a>(
    ctx: &'a Context,
    a: &SpanAnalysis,
    condition: &Bool<'a>,
    known_features: &HashSet<String>,
) -> bool {
    a.non_std_configs.iter().any(|cfg| {
        let assignment: Vec<Bool<'a>> = known_features
            .iter()
            .map(|f| {
                let var = Bool::new_const(ctx, f.as_str());
                if cfg.contains(f) { var } else { var.not() }
            })
            .collect();
        let solver = Solver::new(ctx);
        for lit in &assignment {
            solver.assert(lit);
        }
        solver.assert(condition);
        solver.check() == SatResult::Unsat
    })
}

/// The feature a conditional span's std-ness actually turns on, read off the
/// covering runs that have already been performed.
///
/// `probe_conditional_spans` answers the same question by negating the span's
/// syntactic ancestor gates one at a time and taking the first negation after
/// which the span is no longer std. That accepts **the code disappeared** as
/// proof **the code is not std**: negating any feature that merely *contains*
/// the span deletes it, so a span inside a `#[cfg(feature = "f32")]` module
/// yields `¬f32` — a true statement about that compile and a wrong statement
/// about no_std. uom 0.36 loses `f32`, `f64` and `si` exactly that way, and the
/// crates depending on it then fail uom's own `compile_error!` for having no
/// storage type left (ALL_TARGET_FAILURES T4a).
///
/// The runs already separate the two kinds of feature, but only under a
/// **biconditional** reading: the answer is a feature that is on in every run
/// where the span was std *and* off in every run where it was present and not
/// std. "On in every std run" alone is mere correlation — the covering sets are
/// chosen to cover items, not to vary one feature at a time, so an unrelated
/// feature is easily on in all of them. wg 0.9.2 is the case that catches it:
/// `src/sync.rs`'s `Mutex` is std exactly when `parking_lot` is *off*, and
/// `triomphe` happened to be on in each of those runs. Requiring the other half
/// rejects it, because a run with `parking_lot` on and `triomphe` on resolves
/// the span to `parking_lot`.
///
/// A feature that merely contains the span fails the same half: the span cannot
/// be observed with it off, so it is on in the non-std runs too. That is what
/// leaves `std` as uom's answer and rules out `f32`, `si` and `f64`.
///
/// Returns `None` unless exactly one feature qualifies: with two, the runs do
/// not say which one carries the std-ness, and the probe — which compiles — is
/// the better answer. `None` therefore means "unchanged behaviour".
///
/// The condition is always `¬feature`, so only a feature whose *presence*
/// brings std is reportable. wg's `parking_lot` — std when it is **off** — is
/// deliberately not expressible here and stays with the probe, which already
/// gets it right by enabling the feature.
pub fn feature_explaining_std(a: &SpanAnalysis) -> Option<String> {
    if a.std_configs.is_empty() || a.non_std_configs.is_empty() {
        return None;
    }

    // On in *every* config the span resolved to std under, so `std ⟹ feature`
    // at this span and `¬feature` rules the std resolution out.
    let mut necessary: Vec<&String> = a.std_configs[0].iter().collect();
    for cfg in &a.std_configs[1..] {
        necessary.retain(|f| cfg.contains(*f));
    }

    // ...and off in *every* config where the span was present and not std, so
    // `feature ⟹ std` too. Both halves together are what separate the feature
    // the resolution turns on from one that is only correlated with it.
    let mut candidates = necessary
        .into_iter()
        .filter(|f| a.non_std_configs.iter().all(|cfg| !cfg.contains(*f)));

    match (candidates.next(), candidates.next()) {
        (Some(only), None) => Some(only.clone()),
        _ => None,
    }
}

/// The std spans of a configuration that **satisfies** the span's own gate,
/// under the crate's hard constraints. `None` when no such configuration exists
/// or it does not compile — in both cases the run yields no evidence.
///
/// [`condition_contradicted_by_runs`] vetoes a probe condition when a covering
/// run already holds a witness: the span present, and not std, with the
/// condition false. Some spans never get one. zeno 0.3.2's covering runs are
/// `[std, eval]` and `[libm]`, and `src/stroke.rs:723`'s `.sqrt()` sits inside
/// an `eval`-gated block, so the only run that compiles it also has `std` on.
/// The probe then negates the one gate it can see, the code disappears, and
/// `¬eval` is emitted — zeno converts and builds, having quietly lost a feature
/// its author had on by default.
///
/// So compile the witness instead of waiting for one. Asserting the gate rather
/// than negating it asks the question that actually distinguishes the two cases:
///
/// * zeno — `[eval, libm]` compiles and `.sqrt()` resolves to zeno's own
///   `F32Ext`. The gate is not what makes the span non-std, so the condition
///   goes and `eval` survives.
/// * tarfs 0.2.7 — `[builtin_devices]` with `std` off does not compile at all
///   (`use std::fs::File`, `E0433`). No evidence, condition kept, and
///   `builtin_devices` is still correctly turned off.
/// * wg 0.9.2 — satisfying `not(feature = "triomphe")` means `triomphe` off,
///   which leaves `use std::sync::Arc` under `no_std`. Also fails, so the
///   requirement that `triomphe` be *on* survives.
///
/// This is a refutation, never an attribution: it can only drop a condition the
/// probe proposed, never name a feature of its own. Both weaker readings of the
/// run evidence — including one tried and reverted here — got wg or tarfs wrong
/// precisely by attributing.
pub fn gate_satisfied_std_spans<'a>(
    ctx: &'a Context,
    crate_name: &str,
    manifest: &str,
    ancestors: &[Bool<'a>],
    hard_constraints: &[Bool<'a>],
    all_constraints: &[Bool<'a>],
) -> Option<Vec<ReadableSpan>> {
    let refs: Vec<&Bool<'a>> = ancestors.iter().collect();
    let satisfied = Bool::and(ctx, &refs);

    match solve_with_negation(ctx, hard_constraints, &satisfied, all_constraints) {
        SolveResult::Unsat => {
            debug!("Gate {} cannot be satisfied under the hard constraints", satisfied);
            None
        }
        SolveResult::Sat(features, _) => {
            debug!("Gate-satisfying configuration for {}: {:?}", satisfied, features);
            match run_rustc_plugin_pass(manifest, crate_name, &features, None) {
                PassOutcome::Success { std_spans, .. } => Some(std_spans),
                // A configuration that keeps the gate and drops std does not
                // build: the gate really is the way out, so leave the probe's
                // condition alone.
                _ => {
                    debug!("Gate-satisfying configuration {:?} did not compile", features);
                    None
                }
            }
        }
    }
}

pub fn probe_conditional_spans<'a>(
    ctx: &'a z3::Context,
    crate_name: &str,
    manifest: &str,
    probe_targets: Vec<ProbeTarget<'a>>,
    hard_constraints: &[Bool<'a>],
    all_constraints: &[Bool<'a>],
) -> Vec<ProbeResult<'a>> {
    // Same body as `probe_usages`, but entered under its own timing phase:
    // conditional spans and always-std spans cost very different amounts and
    // reusing the caller's phase would merge them into one number.
    probe_usages_with(
        ctx,
        crate_name,
        manifest,
        probe_targets,
        hard_constraints,
        all_constraints,
        "probe_conditional_phase",
        "conditional",
    )
}

/// Seed results for targets that will not be probed.
///
/// Externally gated targets are taken first and regardless of `ancestors`: a
/// non-feature cfg anywhere above the span means the whole region is off the
/// axis we control, so even a nested `#[cfg(feature = "std")]` inside it is
/// moot. Everything else with no gate at all stays `StillStd` as before.
fn initial_ungated_results<'a>(probe_targets: &[ProbeTarget<'a>]) -> Vec<ProbeResult<'a>> {
    probe_targets
        .iter()
        .filter(|t| t.externally_gated || t.ancestors.is_none())
        .map(|t| ProbeResult {
            target: t.clone(),
            decision: if t.externally_gated {
                ProbeDecision::ExternallyGated {
                    reason: "Guarded by a cfg naming no feature; not on the feature axis"
                        .to_string(),
                }
            } else {
                ProbeDecision::StillStd {
                    reason: "No gate ancestors; Cannot disable".to_string(),
                }
            },
            condition: None,
            history: Vec::new(),
        })
        .collect()
}

/// Returns a fingerprint for a gate ancestor sequence — used to group probe
/// targets that will produce identical Z3 queries and compiles.
fn gate_fingerprint(ancestors: &[Bool]) -> Vec<String> {
    ancestors.iter().map(|b| b.to_string()).collect()
}

/// Probes each ancestor gate of a single target by negating it, checking
/// satisfiability, running the plugin with the resulting feature set, and
/// delegating success classification to the caller via `classify_success(std_spans, full_output)`.
///
/// When a feature set is SAT but fails to compile, a blocking constraint is added
/// (CEGAR) and `solve_with_negation` is retried for the same gate. This repeats
/// until UNSAT (no more valid models) or until a compile succeeds.
///
/// UNSAT with no prior blocking → StillStd (gate is unsatisfiable; can't be negated).
/// UNSAT after ≥1 blocking clause → CompileFailed (all models tried, all failed to compile).
///
/// Returns `(history, final_decision, last_gate_condition)`.
fn probe_one_target<'a, F>(
    ctx: &'a z3::Context,
    crate_name: &str,
    manifest: &str,
    ancestors: &[Bool<'a>],
    hard_constraints: &[Bool<'a>],
    all_constraints: &[Bool<'a>],
    context_filter: Option<PathContext>,
    mut classify_success: F,
) -> (Vec<ProbeOneStep>, ProbeDecision, Option<Bool<'a>>)
where
    F: FnMut(Vec<ReadableSpan>, FeatureRunOutput) -> ProbeDecision,
{
    let mut index = 0;
    let mut last_decision = ProbeDecision::StillStd {
        reason: "Initial assumption".to_string(),
    };
    let mut history = Vec::new();
    let mut current_condition: Option<Bool<'a>> = None;

    'gate: loop {
        if index >= ancestors.len() {
            break;
        }
        let gate = ancestors[index].not();
        current_condition = Some(gate.clone());
        debug!("Negating gate: {}", gate);
        let gate_scope = timing::scope("probe_gate", format!("{}", gate));
        gate_scope.meta("gate_index", index.to_string());

        let mut blocking: Vec<Bool<'a>> = vec![];

        // Each blocking clause eliminates exactly one Z3 assignment. With N boolean
        // feature variables there are 2^N models; without a cap a crate with many
        // features hits millions of compile attempts. Cap at 10.
        const MAX_CEGAR_RETRIES: usize = 10;

        'retry: loop {
            if blocking.len() >= MAX_CEGAR_RETRIES {
                debug!("CEGAR: hit retry cap for gate {} — CompileFailed", gate);
                last_decision = ProbeDecision::CompileFailed;
                index += 1;
                break 'retry;
            }

            let extended: Vec<Bool<'a>> = hard_constraints
                .iter()
                .chain(blocking.iter())
                .cloned()
                .collect();

            // Timed apart from the compile below so a probe that is slow because
            // Z3 is grinding is not mistaken for one that is slow because cargo is.
            let solved = {
                let s = timing::scope("probe_solve", format!("{}", gate));
                s.meta("cegar_retry", blocking.len().to_string());
                solve_with_negation(ctx, &extended, &gate, all_constraints)
            };
            match solved {
                SolveResult::Unsat => {
                    if blocking.is_empty() {
                        // Gate is genuinely unsatisfiable — can't negate it.
                        debug!("Negating this gate is unsatisfiable");
                        last_decision = ProbeDecision::StillStd {
                            reason: "Negating this gate did not change satisfiability".to_string(),
                        };
                        history.push(ProbeOneStep {
                            gate_description: format!("{}", gate),
                            features: vec![],
                            classification: last_decision.clone(),
                        });
                    } else {
                        // All models tried; all failed to compile.
                        debug!(
                            "CEGAR: exhausted all models for gate {} — CompileFailed",
                            gate
                        );
                        last_decision = ProbeDecision::CompileFailed;
                    }
                    index += 1;
                    break 'retry;
                }
                SolveResult::Sat(features, disabled_feats) => {
                    debug!(
                        "Negating this gate is satisfiable with features: {:?}",
                        features
                    );
                    match run_rustc_plugin_pass(manifest, crate_name, &features, context_filter) {
                        PassOutcome::CompileFailed { stderr, .. } => {
                            // CEGAR: block this exact assignment and retry the same gate.
                            // The first error line is the only record of *why* a probe
                            // never compiled — without it a `CompileFailed` verdict is
                            // untriageable from the log (bucket T2).
                            debug!(
                                "CEGAR: compile failed for {:?} ({}); blocking and retrying",
                                features,
                                stderr
                                    .lines()
                                    .find(|l| l.starts_with("error"))
                                    .unwrap_or_else(|| stderr.lines().next().unwrap_or(""))
                                    .trim()
                            );
                            let block =
                                solver::build_forbidden_constraint(ctx, &features, &disabled_feats);
                            blocking.push(block);
                            // continue 'retry
                        }
                        PassOutcome::PluginMissingOutput { .. } => {
                            last_decision = ProbeDecision::CompileFailed;
                            index += 1;
                            break 'retry;
                        }
                        PassOutcome::Success {
                            std_spans,
                            full_output,
                            std_inconclusive,
                            ..
                        } => {
                            let mut decision = classify_success(std_spans, full_output);
                            // The negated gate compiled only on the host, and only
                            // because no bare-metal attempt got past a dependency.
                            // `--no-default-features` does not take std out of the
                            // dependency graph there, so a shim like `core2` is
                            // still built with its own default `std` feature and
                            // its items resolve to std whatever this gate says.
                            // "Still std" in that environment is the dependency's
                            // configuration talking, not proof the gate is no way
                            // out — report it as unproven instead, which keeps the
                            // span out of `all_hard` and counts it.
                            // `NonStd` is left alone: a span that resolved to
                            // another crate here resolves there on bare metal too.
                            if std_inconclusive
                                && let ProbeDecision::StillStd { reason } = &decision
                            {
                                debug!(
                                    "Probe compiled only on the host with every bare-metal attempt \
                                     failing inside a dependency; not treating '{}' as proof of \
                                     hardness",
                                    reason
                                );
                                decision = ProbeDecision::CompileFailed;
                            }
                            let is_nonstd = matches!(decision, ProbeDecision::NonStd { .. });
                            last_decision = decision.clone();
                            history.push(ProbeOneStep {
                                gate_description: format!("{}", gate),
                                features,
                                classification: decision,
                            });
                            if is_nonstd {
                                break 'gate;
                            }
                            index += 1;
                            break 'retry;
                        }
                    }
                }
            }
        }
    }

    (history, last_decision, current_condition)
}

pub fn probe_usages<'a>(
    ctx: &'a z3::Context,
    crate_name: &str,
    manifest: &str,
    probe_targets: Vec<ProbeTarget<'a>>,
    hard_constraints: &[Bool<'a>],
    all_constraints: &[Bool<'a>],
) -> Vec<ProbeResult<'a>> {
    probe_usages_with(
        ctx,
        crate_name,
        manifest,
        probe_targets,
        hard_constraints,
        all_constraints,
        "probe_usages_phase",
        "usages",
    )
}

/// Body of [`probe_usages`], parameterised by the timing phase it reports under.
///
/// `phase` is the scope wrapping the whole sweep; `kind` labels each individual
/// probe so a span's cost can be traced back to which sweep paid for it.
#[allow(clippy::too_many_arguments)]
fn probe_usages_with<'a>(
    ctx: &'a z3::Context,
    crate_name: &str,
    manifest: &str,
    probe_targets: Vec<ProbeTarget<'a>>,
    hard_constraints: &[Bool<'a>],
    all_constraints: &[Bool<'a>],
    phase: &'static str,
    kind: &'static str,
) -> Vec<ProbeResult<'a>> {
    let _phase = timing::scope(phase, crate_name);
    let mut results = initial_ungated_results(&probe_targets);

    // Group gated targets by gate fingerprint so each unique gate sequence is
    // probed only once. All targets in a group share the same gate, so they
    // compile/fail together; the representative's verdict is correct for all.
    let mut groups: Vec<(Vec<String>, Vec<ProbeTarget<'a>>)> = Vec::new();
    for target in probe_targets
        .into_iter()
        .filter(|t| !t.externally_gated && t.ancestors.is_some())
    {
        let fp = gate_fingerprint(target.ancestors.as_ref().unwrap());
        match groups.iter_mut().find(|(k, _)| *k == fp) {
            Some(g) => g.1.push(target),
            None => groups.push((fp, vec![target])),
        }
    }

    for (_, group) in groups {
        let rep = group[0].clone();
        let ancestors = rep.ancestors.clone().unwrap();
        debug!("Current target: {:?}", rep.analysis.span);
        // One event per *group*, not per span: the grouping above probes a single
        // representative and copies its verdict to the rest, so a group is the
        // smallest thing that has a measurable cost. `group_size` is what lets the
        // report amortise it back over the spans it covers.
        let probe = timing::scope("probe_span", rep.analysis.span.key());
        probe.meta("kind", kind);
        probe.meta("group_size", group.len().to_string());
        let (history, last_decision, condition) = probe_one_target(
            ctx,
            crate_name,
            manifest,
            &ancestors,
            hard_constraints,
            all_constraints,
            None,
            |std_spans, _| match std_spans
                .iter()
                .find(|s| *s == &rep.analysis.span)
                .filter(|s| s.usage_crate.as_deref() == Some("std"))
            {
                None => ProbeDecision::NonStd {
                    reason: "Negating this gate caused the span to disappear from std spans"
                        .to_string(),
                    alternate_crate: "unknown".to_string(),
                },
                Some(_) => ProbeDecision::StillStd {
                    reason: "Negating this gate did not change the span's std usage".to_string(),
                },
            },
        );
        probe.meta("decision", decision_name(&last_decision));
        drop(probe);
        results.push(ProbeResult {
            target: rep,
            decision: last_decision.clone(),
            history: history.clone(),
            condition: condition.clone(),
        });
        for target in group.into_iter().skip(1) {
            results.push(ProbeResult {
                target,
                decision: last_decision.clone(),
                history: history.clone(),
                condition: condition.clone(),
            });
        }
    }
    results
}

/// Short tag for a decision, for the timing record.
fn decision_name(d: &ProbeDecision) -> &'static str {
    match d {
        ProbeDecision::NonStd { .. } => "NonStd",
        ProbeDecision::StillStd { .. } => "StillStd",
        ProbeDecision::CompileFailed => "CompileFailed",
        ProbeDecision::ExternallyGated { .. } => "ExternallyGated",
    }
}

pub fn probe_candidates<'a>(
    ctx: &'a z3::Context,
    crate_name: &str,
    manifest: &str,
    probe_targets: Vec<ProbeTarget<'a>>,
    usages: &mut [SpanAnalysis],
    hard_constraints: &[Bool<'a>],
    all_constraints: &[Bool<'a>],
) -> Vec<ProbeResult<'a>> {
    let _phase = timing::scope("probe_imports_phase", crate_name);
    let mut results = initial_ungated_results(&probe_targets);

    // Group gated targets by gate fingerprint — one compile per unique gate sequence.
    let mut groups: Vec<(Vec<String>, Vec<ProbeTarget<'a>>)> = Vec::new();
    for target in probe_targets
        .into_iter()
        .filter(|t| !t.externally_gated && t.ancestors.is_some())
    {
        let fp = gate_fingerprint(target.ancestors.as_ref().unwrap());
        match groups.iter_mut().find(|(k, _)| *k == fp) {
            Some(g) => g.1.push(target),
            None => groups.push((fp, vec![target])),
        }
    }

    for (_, group) in groups {
        let rep = group[0].clone();
        let ancestors = rep.ancestors.clone().unwrap();
        let probe = timing::scope("probe_span", rep.analysis.span.key());
        probe.meta("kind", "imports");
        probe.meta("group_size", group.len().to_string());
        // TODO: Do we need to get the other groups as well here to do a full run?
        let (history, last_decision, condition) = probe_one_target(
            ctx,
            crate_name,
            manifest,
            &ancestors,
            hard_constraints,
            all_constraints,
            Some(PathContext::ImportDeclaration),
            |std_spans, full_output| {
                update_always_std_usages(usages, full_output);
                // TODO: Are we sure this is the case? What if the feature set itself never had that Span?
                match std_spans.iter().find(|s| *s == &rep.analysis.span) {
                    None => ProbeDecision::NonStd {
                        reason: "Negating this gate caused the span to disappear from std imports"
                            .to_string(),
                        alternate_crate: "unknown".to_string(),
                    },
                    Some(found) if found.usage_crate.as_deref() == Some("std") => {
                        ProbeDecision::StillStd {
                            reason: "Negating this gate did not change the span's std usage"
                                .to_string(),
                        }
                    }
                    Some(found) => ProbeDecision::NonStd {
                        reason: "Negating this gate caused the span to resolve to a non-std crate"
                            .to_string(),
                        alternate_crate: found
                            .usage_crate
                            .clone()
                            .unwrap_or_else(|| "unknown".to_string()),
                    },
                }
            },
        );
        probe.meta("decision", decision_name(&last_decision));
        drop(probe);
        results.push(ProbeResult {
            target: rep,
            decision: last_decision.clone(),
            history: history.clone(),
            condition: condition.clone(),
        });
        for target in group.into_iter().skip(1) {
            results.push(ProbeResult {
                target,
                decision: last_decision.clone(),
                history: history.clone(),
                condition: condition.clone(),
            });
        }
    }

    results
}

/// During probing of imports, we may find that some AlwaysStd spans are actually Conditional
/// (i.e., they might show up as non-std under some feature combinations). When we find such a span, we update its verdict in the analyses
/// to reflect this new information, so that it won't be considered a hard std import in future iterations.
///
/// **Currently inert**: `driver::analyze_crate` consumes `always_std_others`
/// without re-reading the verdict, so a demotion here changes nothing. Leave it
/// that way unless the rule is made run-aware first. The test below is
/// `find(span)` on the records of a *single* probe run, which demotes on a
/// co-located non-std record from that same run — exactly the fallacy
/// `classify_spans` was fixed to stop making. Wiring it up as written would
/// reopen the hole one level down.
fn update_always_std_usages(usages: &mut [SpanAnalysis], output: FeatureRunOutput) {
    let all_spans = output
        .records
        .into_iter()
        .map(|rec| rec.span)
        .collect::<Vec<_>>();

    for analysis in usages.iter_mut() {
        let alt_crate = all_spans
            .iter()
            .find(|s| *s == &analysis.span)
            .filter(|s| s.usage_crate.as_deref() != Some("std"))
            .map(|s| {
                s.usage_crate
                    .clone()
                    .unwrap_or_else(|| "unknown".to_string())
            });

        if let Some(alt_crate) = alt_crate {
            analysis.verdict = SpanVerdict::Conditional {
                alternate_crates: vec![alt_crate],
            };
        }
    }
}

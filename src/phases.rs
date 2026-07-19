use log::debug;
use z3::Context;
use z3::SatResult;
use z3::Solver;
use z3::ast::Bool;

use crate::types::*;

use crate::driver::run_rustc_plugin_pass;
use crate::solver;

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
            let model = solver.get_model().expect("sat without model");
            let (enabled, disabled) = crate::solver::model_to_features(&Some(model));
            SolveResult::Sat(enabled, disabled)
        }
        SatResult::Unsat | SatResult::Unknown => SolveResult::Unsat,
    }
}

pub fn classify_spans(runs: &[CoveringRun]) -> Vec<SpanAnalysis> {
    let mut index: std::collections::HashMap<
        ReadableSpan,
        (
            PathRecord,
            Vec<Vec<String>>,
            Vec<Vec<String>>,
            std::collections::HashSet<usize>,
        ),
    > = std::collections::HashMap::new();

    for (run_idx, run) in runs.iter().enumerate() {
        for rec in &run.output.records {
            let entry = index.entry(rec.span.clone()).or_insert_with(|| {
                (
                    rec.clone(),
                    Vec::new(),
                    Vec::new(),
                    std::collections::HashSet::new(),
                )
            });

            match rec.span.usage_crate.as_deref() {
                Some("std") => {
                    entry.1.push(run.features.clone());
                    // Track *which* runs saw std here. `std_cfgs` pushes once per
                    // record, so a run with several std records at one span would
                    // be counted multiple times — the set keeps run identity.
                    entry.3.insert(run_idx);
                }
                Some(_) => entry.2.push(run.features.clone()),
                None => {} // unresolved; ignore for classification
            }
        }
    }

    index
        .into_iter()
        .map(|(span, (exemplar, std_cfgs, non_std_cfgs, std_run_idxs))| {
            let verdict = match (std_cfgs.is_empty(), non_std_cfgs.is_empty()) {
                (true, _) => SpanVerdict::NeverStd,
                (false, true) => {
                    // Seen as std, never as another crate. That alone does NOT make
                    // it unconditionally std: the span may simply be ABSENT from
                    // some runs (the code wasn't compiled, or a macro expanded to
                    // something else). Every CoveringRun is a successful compile,
                    // so if any run has no std record here, there exists a working
                    // configuration in which this span is not std usage — i.e. the
                    // std-ness is avoidable. Only call it AlwaysStd when *every*
                    // run recorded std at this span.
                    //
                    // Marking the rest Conditional both keeps them out of
                    // `all_hard` and lets them feed `final_condition` through the
                    // normal conditional-probe path. `alternate_crates` is empty
                    // here because nothing else resolved at this span — it was
                    // absent, not resolved elsewhere.
                    if std_run_idxs.len() == runs.len() {
                        SpanVerdict::AlwaysStd
                    } else {
                        SpanVerdict::Conditional {
                            alternate_crates: Vec::new(),
                        }
                    }
                }
                (false, false) => {
                    // Collect the distinct alternate crates seen
                    let mut alts: Vec<String> = runs
                        .iter()
                        .flat_map(|r| r.output.records.iter())
                        .filter(|r| r.span == span)
                        .filter_map(|r| r.span.usage_crate.as_deref())
                        .filter(|c| *c != "std")
                        .map(String::from)
                        .collect();
                    alts.sort();
                    alts.dedup();
                    SpanVerdict::Conditional {
                        alternate_crates: alts,
                    }
                }
            };
            SpanAnalysis {
                span,
                verdict,
                exemplar,
                std_configs: std_cfgs,
                non_std_configs: non_std_cfgs,
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

pub fn probe_conditional_spans<'a>(
    ctx: &'a z3::Context,
    crate_name: &str,
    manifest: &str,
    probe_targets: Vec<ProbeTarget<'a>>,
    hard_constraints: &[Bool<'a>],
    all_constraints: &[Bool<'a>],
) -> Vec<ProbeResult<'a>> {
    probe_usages(
        ctx,
        crate_name,
        manifest,
        probe_targets,
        hard_constraints,
        all_constraints,
    )
}

/// Builds the initial result set for probe targets that have no ancestor gates —
/// these are unconditionally hard std without any way to disable them.
fn initial_ungated_results<'a>(probe_targets: &[ProbeTarget<'a>]) -> Vec<ProbeResult<'a>> {
    probe_targets
        .iter()
        .filter(|t| t.ancestors.is_none())
        .map(|t| ProbeResult {
            target: t.clone(),
            decision: ProbeDecision::StillStd {
                reason: "No gate ancestors; Cannot disable".to_string(),
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

            match solve_with_negation(ctx, &extended, &gate, all_constraints) {
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
                        PassOutcome::CompileFailed { .. } => {
                            // CEGAR: block this exact assignment and retry the same gate.
                            debug!(
                                "CEGAR: compile failed for {:?}; blocking and retrying",
                                features
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
                            ..
                        } => {
                            let decision = classify_success(std_spans, full_output);
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
    let mut results = initial_ungated_results(&probe_targets);

    // Group gated targets by gate fingerprint so each unique gate sequence is
    // probed only once. All targets in a group share the same gate, so they
    // compile/fail together; the representative's verdict is correct for all.
    let mut groups: Vec<(Vec<String>, Vec<ProbeTarget<'a>>)> = Vec::new();
    for target in probe_targets.into_iter().filter(|t| t.ancestors.is_some()) {
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

pub fn probe_candidates<'a>(
    ctx: &'a z3::Context,
    crate_name: &str,
    manifest: &str,
    probe_targets: Vec<ProbeTarget<'a>>,
    usages: &mut [SpanAnalysis],
    hard_constraints: &[Bool<'a>],
    all_constraints: &[Bool<'a>],
) -> Vec<ProbeResult<'a>> {
    let mut results = initial_ungated_results(&probe_targets);

    // Group gated targets by gate fingerprint — one compile per unique gate sequence.
    let mut groups: Vec<(Vec<String>, Vec<ProbeTarget<'a>>)> = Vec::new();
    for target in probe_targets.into_iter().filter(|t| t.ancestors.is_some()) {
        let fp = gate_fingerprint(target.ancestors.as_ref().unwrap());
        match groups.iter_mut().find(|(k, _)| *k == fp) {
            Some(g) => g.1.push(target),
            None => groups.push((fp, vec![target])),
        }
    }

    for (_, group) in groups {
        let rep = group[0].clone();
        let ancestors = rep.ancestors.clone().unwrap();
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

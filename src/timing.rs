//! Wall-clock accounting for a single tool run.
//!
//! # Why an event tree and not more `Telemetry` fields
//!
//! The scalar `*_time_ms` fields this replaces did not survive contact with the
//! pipeline they were measuring, for two structural reasons:
//!
//! * **Re-entrancy.** `driver::analyze_crate_wrapper` runs for dependencies too,
//!   and `parser` calls it in one place with a *throwaway* `Telemetry` whose
//!   fields are dropped on the floor. Timing threaded through `&mut Telemetry`
//!   is silently lost there.
//! * **Fan-out.** One "coverage run" is not one compile: `run_rustc_plugin_pass_with`
//!   sweeps up to `TARGET_LIST.len()` triples before one links, and the whole point
//!   of the `LAST_GOOD_TARGET` cache is that the first pass costs far more than the
//!   rest. A scalar cannot show that; a nested event can.
//!
//! So this records *events*, not totals: a flat list where each entry names its
//! parent. Every total is derived by summing the tree at dump time, which means
//! no aggregate can drift out of sync with what actually ran, and adding a new
//! phase costs one [`scope`] call.
//!
//! # Usage
//!
//! ```ignore
//! let _g = timing::scope("coverage_run", features.join(","));
//! _g.meta("kind", "baseline");
//! ```
//!
//! The guard closes the event when it drops, so an early `return` or `?` cannot
//! leak a half-open scope. Events nest by guard lifetime alone — callers never
//! pass ids around.

use std::collections::BTreeMap;
use std::sync::Mutex;
use std::time::Instant;

use serde::Serialize;

/// One completed (or in-flight) timed region.
#[derive(Debug, Clone, Serialize)]
pub struct Event {
    pub id: u32,
    /// Enclosing event, by id. `None` for a top-level phase.
    pub parent: Option<u32>,
    /// Nesting depth, so the flat list can be printed as a tree without a join.
    pub depth: u16,
    /// Phase name — the key aggregates are grouped by. See the module docs of
    /// the instrumented functions for the vocabulary.
    pub phase: &'static str,
    /// What this occurrence was about: a span key, a feature list, a target
    /// triple, a `crate:version`.
    pub label: String,
    /// Which crate's analysis this happened under. Dependencies are analysed by
    /// the same code as the main crate, so without this their cost is
    /// indistinguishable from the main crate's.
    pub crate_ctx: String,
    /// Milliseconds from process start to the opening of this scope.
    pub start_ms: u128,
    /// Wall time of the scope. `None` only if the run died inside it.
    pub dur_ms: Option<u128>,
    #[serde(skip_serializing_if = "BTreeMap::is_empty")]
    pub meta: BTreeMap<&'static str, String>,
}

struct Recorder {
    origin: Instant,
    events: Vec<Event>,
    /// Ids of the currently open scopes, outermost first.
    stack: Vec<u32>,
    /// Innermost `crate_ctx` pushed so far; inherited by every event opened
    /// under it.
    crate_stack: Vec<String>,
}

impl Recorder {
    fn new() -> Self {
        Recorder {
            origin: Instant::now(),
            events: Vec::new(),
            stack: Vec::new(),
            crate_stack: Vec::new(),
        }
    }
}

static REC: Mutex<Option<Recorder>> = Mutex::new(None);

/// Start the clock. Called once from `main`; a second call is ignored so a test
/// or library embedding cannot reset the origin mid-run.
pub fn init() {
    let mut guard = REC.lock().unwrap();
    if guard.is_none() {
        *guard = Some(Recorder::new());
    }
}

/// Milliseconds since [`init`], i.e. the total run time so far.
pub fn total_ms() -> u128 {
    let guard = REC.lock().unwrap();
    guard
        .as_ref()
        .map(|r| r.origin.elapsed().as_millis())
        .unwrap_or(0)
}

/// Every event recorded so far, in the order the scopes were opened.
pub fn events() -> Vec<Event> {
    let guard = REC.lock().unwrap();
    guard.as_ref().map(|r| r.events.clone()).unwrap_or_default()
}

/// Open a timed region. The returned guard closes it on drop.
///
/// Uninitialised recorder (any entry point that never called [`init`], e.g. a
/// unit test) yields an inert guard: no allocation, no record, no panic.
pub fn scope(phase: &'static str, label: impl Into<String>) -> Guard {
    let mut guard = REC.lock().unwrap();
    let Some(rec) = guard.as_mut() else {
        return Guard { id: None };
    };
    let id = rec.events.len() as u32;
    let parent = rec.stack.last().copied();
    let depth = rec.stack.len() as u16;
    let start_ms = rec.origin.elapsed().as_millis();
    let crate_ctx = rec.crate_stack.last().cloned().unwrap_or_default();
    rec.events.push(Event {
        id,
        parent,
        depth,
        phase,
        label: label.into(),
        crate_ctx,
        start_ms,
        dur_ms: None,
        meta: BTreeMap::new(),
    });
    rec.stack.push(id);
    Guard { id: Some(id) }
}

/// As [`scope`], but also makes `crate_name` the ambient crate for everything
/// opened inside. Used at the two entry points that analyse a whole crate.
pub fn crate_scope(phase: &'static str, crate_name: &str) -> CrateGuard {
    let guard = scope(phase, crate_name);
    if let Some(id) = guard.id {
        let mut rec = REC.lock().unwrap();
        if let Some(rec) = rec.as_mut() {
            rec.crate_stack.push(crate_name.to_string());
            // The scope's own `crate_ctx` was resolved from the *outer* stack,
            // before this push. Fix it up so the event names the crate it opens.
            rec.events[id as usize].crate_ctx = crate_name.to_string();
        }
    }
    CrateGuard {
        id: guard.into_raw_crate_scope(),
    }
}

/// Handle for an open scope. Dropping it stamps the duration.
pub struct Guard {
    id: Option<u32>,
}

/// Same as [`Guard`], but also pops the crate context. Kept as a distinct type
/// so the two cannot be confused at a call site.
pub struct CrateGuard {
    id: Option<u32>,
}

impl Guard {
    /// Attach a key/value fact to this event. Overwrites a previous value for
    /// the same key, so a scope can revise a field (e.g. `outcome`) as it learns.
    pub fn meta(&self, key: &'static str, value: impl Into<String>) {
        let Some(id) = self.id else { return };
        let mut guard = REC.lock().unwrap();
        if let Some(rec) = guard.as_mut() {
            rec.events[id as usize].meta.insert(key, value.into());
        }
    }

    /// Internal: hand the id to a `CrateGuard`-flavoured `Guard` without
    /// running `Drop` twice.
    fn into_raw_crate_scope(self) -> Option<u32> {
        let id = self.id;
        std::mem::forget(self);
        id
    }
}

fn close(id: Option<u32>, pop_crate: bool) {
    let Some(id) = id else { return };
    let mut guard = REC.lock().unwrap();
    let Some(rec) = guard.as_mut() else { return };
    let now = rec.origin.elapsed().as_millis();
    let ev = &mut rec.events[id as usize];
    ev.dur_ms = Some(now.saturating_sub(ev.start_ms));
    // Guards drop in reverse order of creation, so the id being closed is the
    // top of the stack. Truncate rather than pop to stay consistent even if a
    // panic unwound past an inner guard.
    if let Some(pos) = rec.stack.iter().position(|&s| s == id) {
        rec.stack.truncate(pos);
    }
    if pop_crate {
        rec.crate_stack.pop();
    }
}

impl Drop for Guard {
    fn drop(&mut self) {
        close(self.id, false);
    }
}

impl Drop for CrateGuard {
    fn drop(&mut self) {
        close(self.id, true);
    }
}

/// A phase's share of the run: how often it happened and how long it took.
#[derive(Debug, Default, Clone, Serialize)]
pub struct PhaseRollup {
    pub count: usize,
    /// Sum of the scopes' durations. Nested occurrences of the *same* phase
    /// (a `cargo_hir` inside a probe inside a dep analysis) are each counted, so
    /// compare this against `total_ms` only for phases that cannot nest.
    pub total_ms: u128,
    /// `total_ms` minus the time attributed to child events — the phase's own
    /// unaccounted work. A large value means the tree is missing a scope.
    pub self_ms: u128,
    pub max_ms: u128,
}

/// One `cargo hir` / `cargo build` invocation.
#[derive(Debug, Clone, Serialize)]
pub struct CompileEntry {
    pub target: String,
    pub ms: u128,
    pub success: bool,
}

/// One covering run: a feature set, and the compiles it took to get records for it.
#[derive(Debug, Clone, Serialize)]
pub struct CoverageRunEntry {
    pub crate_ctx: String,
    pub features: String,
    pub kind: String,
    pub ms: u128,
    pub targets_tried: usize,
    pub compile_ms: u128,
    pub outcome: String,
}

/// One probed gate group.
///
/// `phases::probe_usages` and friends group targets by gate fingerprint and probe
/// a single representative for the whole group, so the measured unit is a group,
/// not a span. `group_size` is how many spans that verdict covers, and
/// `ms_per_span` divides the group cost evenly across them — an amortisation, not
/// a measurement of any one span.
#[derive(Debug, Clone, Serialize)]
pub struct ProbeSpanEntry {
    pub crate_ctx: String,
    /// `file:line:col-line:col` of the representative span.
    pub span: String,
    pub phase: String,
    pub group_size: usize,
    pub ms: u128,
    pub ms_per_span: u128,
    /// Z3 time inside this probe (`solve_with_negation`, including CEGAR retries).
    pub solve_ms: u128,
    /// Plugin-compile time inside this probe.
    pub compile_ms: u128,
    pub compiles: usize,
    pub gates: usize,
    pub decision: String,
}

/// Everything the run spent, derived from the event tree.
#[derive(Debug, Clone, Serialize)]
pub struct TimingReport {
    pub crate_name: String,
    /// Wall time of the whole process, up to the moment stats were dumped.
    pub total_ms: u128,
    /// Time inside `cargo` (plugin passes and verification builds) — usually
    /// nearly all of `total_ms`.
    pub compile_total_ms: u128,
    pub by_phase: BTreeMap<String, PhaseRollup>,
    pub compiles: CompileSummary,
    pub coverage: CoverageSummary,
    pub probing: ProbeSummary,
    /// The raw tree. Every aggregate above is derived from it, so a question the
    /// summaries do not answer can still be answered here.
    pub events: Vec<Event>,
}

#[derive(Debug, Default, Clone, Serialize)]
pub struct CompileSummary {
    pub count: usize,
    pub total_ms: u128,
    /// Failed attempts are the target sweep's cost: a pass that ends up on
    /// triple #7 paid for six failures first.
    pub failed: usize,
    pub failed_ms: u128,
    pub by_target: BTreeMap<String, PhaseRollup>,
}

#[derive(Debug, Default, Clone, Serialize)]
pub struct CoverageSummary {
    pub total_ms: u128,
    pub runs: Vec<CoverageRunEntry>,
}

#[derive(Debug, Default, Clone, Serialize)]
pub struct ProbeSummary {
    pub total_ms: u128,
    pub spans: Vec<ProbeSpanEntry>,
}

/// Phases whose scopes wrap a `cargo` invocation.
const COMPILE_PHASES: [&str; 2] = ["cargo_hir", "verify_target"];

fn dur(ev: &Event) -> u128 {
    ev.dur_ms.unwrap_or(0)
}

/// Total duration of the events directly inside `id`.
fn children_ms(events: &[Event], id: u32) -> u128 {
    events
        .iter()
        .filter(|e| e.parent == Some(id))
        .map(dur)
        .sum()
}

/// Walk the subtree rooted at `id` (excluding `id` itself), visiting each
/// descendant once.
fn for_each_descendant(events: &[Event], id: u32, mut f: impl FnMut(&Event)) {
    // Events are appended in scope-open order, so a descendant always has a
    // higher id than its ancestor: one forward pass suffices.
    let mut inside: Vec<u32> = vec![id];
    for ev in events.iter().skip(id as usize + 1) {
        match ev.parent {
            Some(p) if inside.contains(&p) => {
                inside.push(ev.id);
                f(ev);
            }
            _ => {}
        }
    }
}

/// Collapse the event tree into the summaries written to `timing.json`.
pub fn report(crate_name: &str) -> TimingReport {
    let events = events();
    let total_ms = total_ms();

    let mut by_phase: BTreeMap<String, PhaseRollup> = BTreeMap::new();
    for ev in &events {
        let entry = by_phase.entry(ev.phase.to_string()).or_default();
        entry.count += 1;
        entry.total_ms += dur(ev);
        entry.self_ms += dur(ev).saturating_sub(children_ms(&events, ev.id));
        entry.max_ms = entry.max_ms.max(dur(ev));
    }

    let mut compiles = CompileSummary::default();
    for ev in events.iter().filter(|e| COMPILE_PHASES.contains(&e.phase)) {
        let ok = ev.meta.get("success").map(|s| s == "true").unwrap_or(false);
        compiles.count += 1;
        compiles.total_ms += dur(ev);
        if !ok {
            compiles.failed += 1;
            compiles.failed_ms += dur(ev);
        }
        let t = compiles.by_target.entry(ev.label.clone()).or_default();
        t.count += 1;
        t.total_ms += dur(ev);
        t.self_ms += dur(ev);
        t.max_ms = t.max_ms.max(dur(ev));
    }

    let mut coverage = CoverageSummary::default();
    coverage.total_ms = events
        .iter()
        .filter(|e| e.phase == "coverage")
        .map(dur)
        .sum();
    for ev in events.iter().filter(|e| e.phase == "coverage_run") {
        let mut targets_tried = 0;
        let mut compile_ms = 0;
        for_each_descendant(&events, ev.id, |d| {
            if d.phase == "cargo_hir" {
                targets_tried += 1;
                compile_ms += dur(d);
            }
        });
        coverage.runs.push(CoverageRunEntry {
            crate_ctx: ev.crate_ctx.clone(),
            features: ev.label.clone(),
            kind: ev.meta.get("kind").cloned().unwrap_or_default(),
            ms: dur(ev),
            targets_tried,
            compile_ms,
            outcome: ev.meta.get("outcome").cloned().unwrap_or_default(),
        });
    }

    let mut probing = ProbeSummary::default();
    probing.total_ms = events
        .iter()
        .filter(|e| e.phase.starts_with("probe_") && e.phase.ends_with("_phase"))
        .map(dur)
        .sum();
    for ev in events.iter().filter(|e| e.phase == "probe_span") {
        let (mut solve_ms, mut compile_ms, mut compiles_n, mut gates) = (0, 0, 0, 0);
        for_each_descendant(&events, ev.id, |d| match d.phase {
            "probe_solve" => solve_ms += dur(d),
            "cargo_hir" => {
                compile_ms += dur(d);
                compiles_n += 1;
            }
            "probe_gate" => gates += 1,
            _ => {}
        });
        let group_size = ev
            .meta
            .get("group_size")
            .and_then(|s| s.parse::<usize>().ok())
            .unwrap_or(1)
            .max(1);
        probing.spans.push(ProbeSpanEntry {
            crate_ctx: ev.crate_ctx.clone(),
            span: ev.label.clone(),
            phase: ev.meta.get("kind").cloned().unwrap_or_default(),
            group_size,
            ms: dur(ev),
            ms_per_span: dur(ev) / group_size as u128,
            solve_ms,
            compile_ms,
            compiles: compiles_n,
            gates,
            decision: ev.meta.get("decision").cloned().unwrap_or_default(),
        });
    }
    probing
        .spans
        .sort_by(|a, b| b.ms.cmp(&a.ms).then_with(|| a.span.cmp(&b.span)));

    TimingReport {
        crate_name: crate_name.to_string(),
        total_ms,
        compile_total_ms: compiles.total_ms,
        by_phase,
        compiles,
        coverage,
        probing,
        events,
    }
}

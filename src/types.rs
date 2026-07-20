use serde::{Deserialize, Serialize};
use std::hash::{Hash, Hasher};
use std::path::PathBuf;
use z3::ast::Bool;

/// Coverage comparison between a single default-features run and the full covering-set union.
/// Used to quantify how much code a default-only static analysis tool would miss.
#[derive(Debug, Clone, Serialize)]
pub struct CoverageComparison {
    /// Unique spans visible in a single default-features compilation
    pub default_unique_spans: usize,
    /// Unique spans visible across the union of all covering-set runs
    pub covering_unique_spans: usize,
    /// Spans reachable only via non-default feature combinations (missed by default-only tools)
    pub spans_only_in_covering: usize,
    /// Std-resolving spans in the default run
    pub default_std_spans: usize,
    /// Std-resolving spans in the covering union
    pub covering_std_spans: usize,
    /// Std-resolving spans missed by the default run
    pub std_spans_only_in_covering: usize,
    /// Number of covering runs used
    pub num_covering_runs: usize,
}

/// A vector of (String, String) tuples.
pub type TupleVec = Vec<(String, String)>;

pub type DoubleTupleVecString = (Vec<String>, Vec<String>);
pub type TripleTupleVecString = (Vec<String>, Vec<String>, Vec<String>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SpanVerdict {
    /// Span resolves to std in every run where it appears, AND appears in
    /// at least one run. Candidate for Phase 2 probing.
    AlwaysStd,
    /// Span resolves to std in some runs and to a non-std crate in others.
    /// No probing needed — the crate itself has demonstrated a non-std path.
    Conditional { alternate_crates: Vec<String> },
    /// Span never resolves to std in any observed run.
    /// Not a std dependency at all.
    NeverStd,
}

#[derive(Debug, Clone)]
pub struct SpanAnalysis {
    pub span: ReadableSpan,
    pub verdict: SpanVerdict,
    /// Representative record for the span (first seen). Used by Phase 3
    /// for context/local_route/path_text.
    pub exemplar: PathRecord,
    /// Features under which this span was observed resolving to std.
    pub std_configs: Vec<Vec<String>>,
    /// Features under which this span was observed resolving elsewhere.
    pub non_std_configs: Vec<Vec<String>>,
}

#[derive(Debug, Clone, Serialize)]
pub struct CoveringRun {
    pub features: Vec<String>,
    pub output: FeatureRunOutput,
}

/// The only part of a `PathRecord` that cross-crate item analysis needs: which
/// dep an item comes from, its bare name, and where it was referenced.
///
/// Covering runs produce one `PathRecord` per *reference*, which for generated
/// crates means hundreds of thousands of records carrying fields nobody reads
/// downstream (web-sys yields ~865k). Projecting to this at the point the runs
/// are consumed lets the fat records be dropped instead of copied, and lets the
/// set dedup — the same item referenced from many modules collapses to one entry.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CrossCrateRef {
    /// `definition_crate`, already normalized (`-` → `_`).
    pub dep: String,
    /// Last `::` segment of `path_text`.
    pub item: String,
    pub span: ReadableSpan,
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq)]
pub enum PathContext {
    ImportDeclaration, // This path is part of a `use` statement (The Facade Map)
    Expression,        // This path is used in code (The Usage Map)
    Type,              // This path is a type signature
    Other,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct PathRecord {
    pub path_text: String,
    // gateway_crate: String,    // e.g., "std", "core", or "LOCAL"
    pub definition_crate: String, // e.g., "alloc", "core"
    pub context: PathContext,
    pub span: ReadableSpan,
    pub local_route: Option<String>,
    pub defining_module: Option<String>,
    /// Raw cfg predicate strings (e.g. `feature = "ZLIB_DEBUG"`) found inside
    /// the body of the macro that expanded to this path. Empty when the path is
    /// not from a macro expansion or the macro body has no `#[cfg(…)]`.
    #[serde(default)]
    pub macro_body_cfgs: Vec<String>,
    /// True when this record was produced by an `extern crate X` declaration
    /// (as opposed to a `use` path). Used by post-processing to identify
    /// module-level extern-crate anchors for local facade gateway resolution.
    #[serde(default)]
    pub is_extern_crate: bool,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct FeatureRunOutput {
    // active_feature: String,
    pub records: Vec<PathRecord>,
    pub macro_module_imports: Vec<(String, String)>, // filename, module name
    /// The crate's `OUT_DIR` for this run, if any build script set one. Used to
    /// resolve `include!(concat!(env!("OUT_DIR"), …))` generated files, whose
    /// real path is unknowable to the syn pass.
    #[serde(default)]
    pub out_dir: Option<String>,
}

#[derive(Debug)]
pub enum PassOutcome {
    Success {
        /// Newly discovered macro-emitted modules, for the fixed-point loop.
        macro_modules: Vec<(String, String)>,
        /// Std-candidate spans after filtering: records resolving to
        /// `usage_crate == "std"`, filtered by the `context_filter` passed to
        /// `run_rustc_plugin_pass`. Not local re-exports (crate::/self::/super::).
        std_spans: Vec<ReadableSpan>,
        /// Full output retained for cross-run comparison (Phase 1).
        full_output: FeatureRunOutput,
    },
    CompileFailed {
        stderr: String,
        exit_code: Option<i32>,
    },
    /// Plugin exited successfully but didn't write the expected JSON.
    /// Treat as a bug/infrastructure failure, not a signal about std usage.
    PluginMissingOutput { expected_path: PathBuf },
}

impl PassOutcome {
    pub fn is_success(&self) -> bool {
        matches!(self, PassOutcome::Success { .. })
    }
}

pub enum SolveResult {
    Sat(Vec<String>, Vec<String>), // (enabled features, disabled features)
    Unsat,
}

/// Input to the probing phase for a single candidate span.
#[derive(Debug, Clone)]
pub struct ProbeTarget<'a> {
    pub analysis: SpanAnalysis,
    /// Ancestors of the span's gate, deepest-first.
    /// Produced by `find_ancestors_for_span`.
    pub ancestors: Option<Vec<Bool<'a>>>,
    /// The span sits under a `#[cfg(...)]` whose predicate names no feature —
    /// `target_arch`, `target_os`, `test`, a build-script `--cfg`, etc. Such a
    /// gate is real but not on an axis we control, so the span is neither
    /// reported nor probed. See `ProbeDecision::ExternallyGated`.
    pub externally_gated: bool,
}

#[derive(Debug, Clone)]
pub struct ProbeResult<'a> {
    pub target: ProbeTarget<'a>,
    pub decision: ProbeDecision,
    pub history: Vec<ProbeOneStep>,
    /// This stores the condition under which the probe build succeeded.
    /// It shows the gate condition based on the Decision. If the decision is StillStd,
    /// this condition is the negation that caused std to still be present.
    /// If the decision is NonStd, this condition is the negation that caused std to be avoided.
    pub condition: Option<Bool<'a>>,
}

#[derive(Debug, Clone)]
pub struct ProbeOneStep {
    /// Which gate (in the ancestor chain) was negated this iteration.
    pub gate_description: String,
    /// Features Z3 produced for this probe.
    pub features: Vec<String>,
    pub classification: ProbeDecision,
}

impl<'a> std::fmt::Display for ProbeResult<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "ProbeResult {{ target: {:?}, decision: {:?} }}",
            self.target, self.decision
        )
    }
}

#[derive(Debug, Clone)]
pub enum ProbeDecision {
    /// The probe build succeeded and the span still resolved to std. Evidence of hardness, but not conclusive on its own.
    StillStd {
        reason: String,
    },
    /// The probe build succeeded and the span resolved to a non-std crate. Strong evidence that negating the witness_gate would avoid this span.
    NonStd {
        reason: String,
        alternate_crate: String,
    },
    CompileFailed,
    /// The span is guarded, but by a cfg predicate naming no feature — e.g.
    /// `#[cfg(all(target_arch = "x86_64", target_os = "linux"))]`. Features are
    /// the axis this tool controls; the target is the consumer's choice, so such
    /// a gate is accepted as a guard without being proved.
    ///
    /// This decision is deliberately neither `StillStd` nor `NonStd`, which is
    /// what gives it the semantics we want: `all_hard` collects `StillStd` so
    /// the span is not reported, and `final_condition` collects `NonStd` so it
    /// contributes nothing to the feature condition the crate must satisfy.
    ///
    /// It cannot be probed: the probe compiles for the host, so a target-gated
    /// arm is present in every covering run and would classify AlwaysStd
    /// unanimously. Compiling for a bare-metal target instead is no help —
    /// every std usage becomes a hard error, so nothing is observable.
    ExternallyGated {
        reason: String,
    },
}

#[derive(Debug, Clone)]
pub struct UsageProbeTarget<'a> {
    pub record: PathRecord,
    pub ancestors: Vec<Option<Bool<'a>>>,
}

#[derive(Debug, Clone)]
pub enum UsageProbeClassification {
    /// Usage's span is absent — code unreachable under ¬gate.
    /// For usages this is a win: the usage is avoidable.
    SpanAbsent,
    /// Same span still resolves to std — gate didn't control this usage.
    SpanStillStd,
    /// Span resolves to a non-std crate. Also a win.
    SpanNonStd {
        alternate_crate: String,
    },
    CompileFailed {
        related: bool,
    },
    PluginMissing,
}

/// Probe runs are kept in a separate bucket from covering runs — they
/// must never feed back into Phase 1 classification (see case 5).
#[derive(Debug)]
pub struct ProbeRun {
    pub features: Vec<String>,
    pub negated_gate_description: String,
    pub outcome: PassOutcome,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReadableSpan {
    pub file: String,
    pub start_line: usize,
    pub start_col: usize,
    pub end_line: usize,
    pub end_col: usize,
    // This field is used only during the ast visitor call to track the usage crate of the current span.
    // It is then used to filter out spans where there are conflicting usage_crates.
    pub usage_crate: Option<String>,
}

impl PartialEq for ReadableSpan {
    fn eq(&self, other: &Self) -> bool {
        self.file == other.file
            && self.start_line == other.start_line
            && self.start_col == other.start_col
            && self.end_line == other.end_line
            && self.end_col == other.end_col
    }
}

impl Eq for ReadableSpan {}

impl Hash for ReadableSpan {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.file.hash(state);
        self.start_line.hash(state);
        self.start_col.hash(state);
        self.end_line.hash(state);
        self.end_col.hash(state);
    }
}

impl ReadableSpan {
    pub fn contains(&self, other: &ReadableSpan) -> bool {
        if self.file != other.file {
            return false;
        }
        if self.start_line > other.start_line || self.end_line < other.end_line {
            return false;
        }
        if self.start_line == other.start_line && self.start_col > other.start_col {
            return false;
        }
        if self.end_line == other.end_line && self.end_col < other.end_col {
            return false;
        }
        true
    }
}

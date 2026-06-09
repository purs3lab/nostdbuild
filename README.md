# Automatic `no_std` Cargo feature solver

Given a crate that supports `no_std`, this tool automatically finds the Cargo feature flags needed to compile it in `no_std` mode and verifies the result by actually building it.

## How it works

The tool runs in two broad stages.

### Stage 1 — Determine the no_std condition for the main crate

The goal is to find the feature combination under which all `std` usage disappears.

**Coverage pass.** A syn-based AST visitor parses `cfg` attributes across the crate and encodes them as Z3 boolean constraints. A CEGAR loop drives the Z3 solver to find a minimal set of feature combinations (covering runs) such that every `#[cfg(…)]`-gated block is exercised by at least one run. Each covering run invokes the `cargo-hir` rustc plugin, which instruments the compiler to record every path resolution, specifically which crate each symbol resolves to (`std`, `core`, `alloc`, a local path, or an external dep crate).

**Span classification.** After all covering runs complete, each span that resolves to `std` is classified:
- **AlwaysStd** — resolves to `std` in every covering run that activates it
- **Conditional** — resolves to `std` in some runs, non-std in others
- **NeverStd** — never resolves to `std`

**Probing.** For each `AlwaysStd` import or usage, the tool negates its cfg gate in Z3 and re-runs the plugin. If negating the gate produces a run where the span is absent or resolves to non-std, the negated condition is the required no_std enabler. CEGAR is used here too: if a candidate feature set fails to compile, that model is blocked and Z3 picks a different one. The final no_std condition is the conjunction of all discovered gate conditions.

If any `std` span cannot be gated away, the crate is reported as incompatible.

### Stage 2 — Resolve dependencies

Each direct dependency is processed similarly. The tool:
1. Checks the results DB for a cached feature set; otherwise runs the full analysis.
2. Reconciles the dependency's required features with what the main crate already enables, adding or removing features as needed.
3. Minimizes the final feature list by removing features that are redundant given the crate's feature implication graph.
4. Handles optional dependencies that get pulled in transitively.

After all dependencies are resolved, the combined feature set is compiled against the requested target to verify correctness. Successful results are cached in `db.bin`.

## Setup

Install the rustc plugin (required before running):
```sh
cargo install --path . --bin cargo-hir --bin hir-driver --force
```

Update [src/consts.rs](src/consts.rs):
- `DOWNLOAD_PATH` — where crates and dependencies are downloaded
- `RESULTS_PATH` — where JSON results are written

## Usage

```sh
cargo run --bin main -- --name <crate-name> --version <version> --target <target>
```

Example:
```sh
cargo run --bin main -- --name log --version 0.4.29 --target x86_64-unknown-none
```

Supported targets are listed in `consts::TARGET_LIST`. Pass `--no-recursive` to skip the final recursive dependency check.

## Output

Results are written to `$RESULTS_PATH/<crate_name>-<version>/`:
- `compilation_results.json` — per-target build status and the derived `--features` / `--no-default-features` args
- `crate_info.json` — dependency tree with resolved features
- `std_usages.json` — hard std spans that could not be gated away (non-empty means failure)
- `telemetry.json` — detailed diagnostics about the analysis

## Tests

```sh
cargo test
```

| Suite | What it tests |
|-------|--------------|
| `tests/driver_tests.rs` | Unit tests for `is_local_reexport` and `resolve_local_facade_gateways` |
| `tests/std_tests.rs` | Fixture crates: verifies hard vs non-hard std span classification against known-correct answers |
| `tests/cegar_tests.rs` | Regression tests for the CEGAR retry loop and covering-set pairing recovery |
| `tests/main_tests.rs` | End-to-end: runs the full binary on real crates and diffs the JSON output against saved fixtures |

`main_tests` requires `DOWNLOAD_PATH` and `RESULTS_PATH` to be set and `cargo-hir` to be installed.

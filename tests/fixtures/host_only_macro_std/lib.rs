// bp-wococo 0.3.0 in miniature, with its own control built in.
//
//   // src/lib.rs:68 — the whole of what the crate wrote
//   decl_bridge_finality_runtime_apis!(wococo, grandpa);
//
//   exemplar  path_text: "std::result::Result",
//             expansion_crate: Some("bp_runtime")
//
// Every bare-metal target dies inside `macros` before this crate is compiled, so
// the only covering run is a host build and its std records are inconclusive
// (O-7). The question O-16 asks is which of the two spans below that rule may
// weaken: the text at `api` is the *dependency's*, and the text at `label` is
// this crate's own, in the same run, under the same flag.

#![cfg_attr(not(feature = "std"), no_std)]

/// The O-16 span. Nothing here names `std` — the macro does, in a crate this one
/// only calls. Ungated, so `initial_ungated_results` short-circuits it to
/// `StillStd` without compiling and only the host-only rule can answer it.
macros::declare_api!(api);

/// The control, and it must not move. Ungated std this crate spelled itself: no
/// feature set removes it, and the fact that no run left the host says nothing
/// about it (xous-ipc 0.10.4, xous-api-names 0.9.65).
pub fn label() -> std::string::String {
    std::string::String::new()
}

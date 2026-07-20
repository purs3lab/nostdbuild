// An INNER cfg_attr: crate-level configuration, not a proc-macro. It generates no
// code, so it must NOT record a gate — doing so pollutes the covering-set pool and
// `all_constraints` with a stray `not(std)` and perturbs the solver (regressed
// tarfs: the lost constraint let `builtin_devices` be enabled in a no_std build).
#![cfg_attr(not(feature = "std"), no_std)]

// Fixture: `#[cfg_attr(pred, some_proc_macro_attr(..))]` on an *unconditional*
// item. The applied proc-macro generates code that rustc maps back to a span
// INSIDE the cfg_attr's tokens, so that region must be gated by `pred` — the item
// itself is correctly ungated (cfg_attr does not gate the item).
// See base58_monero (derive) and accesskit (pyo3 `pyclass` attribute).
//
// NOTE: the marker identifiers below must not appear anywhere else in this file,
// including in comments — the tests locate them by first textual occurrence.

// Derive form: the generated error impl is mapped onto the derive's name token.
#[derive(Debug)]
#[cfg_attr(feature = "std", derive(MarkerGatedDerive))]
pub enum Error {
    Invalid,
}

// Attribute-macro form spanning multiple lines (accesskit shape).
#[cfg_attr(
    feature = "pyo3",
    pyclass(module = "demo", rename_all = "SCREAMING_SNAKE_CASE", marker_gated_eq)
)]
pub enum Role {
    Button,
}

// Control: a plain (non-cfg_attr) derive must NOT be gated.
#[derive(MarkerUngatedDerive)]
pub enum Plain {
    Thing,
}

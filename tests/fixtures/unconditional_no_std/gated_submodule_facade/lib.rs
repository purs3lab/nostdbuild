//! Control: the same facade, but the module that holds it is itself gated. The
//! declaration's real condition is then `feature = "extras" ∧ feature = "std"`,
//! and folding a module gate into the OR is what O-1 warns about — an erased
//! non-feature atom inside it is read the wrong way once the OR is negated.
//! Nothing is claimed for this shape.
#![no_std]

#[cfg(feature = "extras")]
pub mod details;

pub fn nothing() {}

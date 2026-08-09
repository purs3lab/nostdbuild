//! Control, two levels down: the crate root has a gated `extern crate std`, so
//! on its own it would yield `¬std` — but a module the tree reaches
//! unconditionally links std whatever the features do. The OR is not the whole
//! story, so the inference is vetoed, exactly as an ungated declaration in the
//! root vetoes it.
//!
//! Also the proof that the fold recurses: the ungated declaration is a
//! grandchild, not a child.
#![no_std]

#[cfg(feature = "std")]
extern crate std;

pub mod outer;

pub fn nothing() {}

//! Control: macaw 0.20.0, renderling 0.4.9, xous-ipc 0.10.4 — a `cfg_attr` that
//! erases exactly like `not(test)` does but says the opposite thing. The crate
//! is no_std only where the target cfg holds; on the targets this tool actually
//! compiles for, that atom is true (`target_os = "none"`) or unknown, never
//! known-false. Nothing is claimed for this shape.
#![cfg_attr(target_arch = "spirv", no_std)]

pub fn nothing() {}

// Fixture reproducing the `cfg_if::cfg_if!` pattern (all_is_cubes, backtrace,
// bevy_platform). The `#[cfg(..)]` predicate sits after `if` and gates a brace
// group, so the std paths inside each arm must be recognised by parsing the
// cfg_if arm grammar rather than as leading segment attributes.

// Module-level cfg_if with an if / else-if / else chain.
cfg_if::cfg_if! {
    if #[cfg(feature = "std")] {
        use std::path::PathPlain;
    } else if #[cfg(feature = "alloc")] {
        use alloc::vec::VecPlain;
    } else {
        use core::fmt::CoreFallback;
    }
}

// Qualified path form plus a nested cfg_if inside the std arm.
cfg_if::cfg_if! {
    if #[cfg(feature = "std")] {
        type OuterStd = std::collections::HashMapNested;
        cfg_if::cfg_if! {
            if #[cfg(windows)] {
                type InnerWin = std::os::windows::NestedWin;
            } else {
                type InnerOther = std::os::unix::NestedUnix;
            }
        }
    }
}

// std path living in an `else if` arm (bevy_platform time/mod.rs pattern).
cfg_if::cfg_if! {
    if #[cfg(all(target_arch = "wasm32", feature = "web"))] {
        use web_time::TimeWeb;
    } else if #[cfg(feature = "std")] {
        use std::time::ElseIfStd;
    } else {
        type FallbackTime = core::fmt::ElseFallback;
    }
}

// An ungated std path OUTSIDE any cfg_if must remain ungated.
pub type Unconditional = std::sync::UngatedArc;

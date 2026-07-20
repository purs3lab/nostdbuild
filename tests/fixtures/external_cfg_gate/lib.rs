// Target-gated module: the canopen_rust shape. `mod std_items` exists only on
// x86_64-linux; on a bare-metal target the no_std arm supplies the same names.
#[cfg(all(target_arch = "x86_64", target_os = "linux"))]
mod std_items {
    pub use std::collections::HashMap;
}

#[cfg(all(target_arch = "arm", target_os = "none"))]
mod no_std_items {
    pub use hashbrown::HashMap;
}

// cfg_if else-arm: the hpm_rt shape. The gate on `mod host` is the *negation*
// of a target predicate, which must be treated identically to the positive form.
cfg_if::cfg_if! {
    if #[cfg(all(target_arch = "riscv32", target_os = "none"))] {
        fn platform_halt() -> ! { loop {} }
    } else {
        fn platform_halt() -> ! { std::process::abort() }
    }
}

// Build-script cfg: the discord_indexmap shape. `has_std` is set by build.rs,
// not by a feature, so it is equally off the axis.
#[cfg(has_std)]
use std::collections::hash_map::RandomState;

// Statement-level target gate — no LocalItem was pushed for this shape at all
// before, since the cfg produces no feature Bool.
fn read_it() {
    #[cfg(target_os = "linux")]
    let _f = std::fs::File::open("/etc/hostname");
}

// CONTROL: a real feature gate must keep behaving as a feature gate.
#[cfg(feature = "std")]
use std::vec::Vec;

// CONTROL: mixed predicate. The non-feature atom is projected out
// existentially, leaving `std` — so this stays probe-able, not excused.
#[cfg(all(target_os = "linux", feature = "std"))]
use std::fs::File;

// CONTROL: no gate at all — must remain hard std.
use std::io::Write;

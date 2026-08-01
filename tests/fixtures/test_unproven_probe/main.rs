#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(feature = "std")]
pub fn locked() -> std::sync::Mutex<u32> {
    std::sync::Mutex::new(0)
}

// The no_std arm does not compile, and nothing in the manifest says so. The
// solver negates `feature = "std"` happily, the probe build then fails on an
// unresolved name, CEGAR exhausts its models, and the `locked` spans land on
// `ProbeDecision::CompileFailed`: never proven std, never proven avoidable.
#[cfg(not(feature = "std"))]
pub fn broken() {
    no_such_function_anywhere();
}

fn main() {}

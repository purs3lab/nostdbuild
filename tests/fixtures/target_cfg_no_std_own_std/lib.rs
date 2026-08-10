// xous-ipc 0.10.4 / xous-api-names 0.9.65 in miniature.
//
//   #![cfg_attr(target_os = "none", no_std)]
//   use std::marker::PhantomData;                 // xous-ipc, src/buffer.rs:2
//   impl std::hash::Hash for XousServerName { … } // xous-api-names, src/api.rs:203
//
// The same shape as `target_cfg_no_std`, and the same host-only run: this crate
// is `#![no_std]` on 25 of the 26 targets in `TARGET_LIST` and a std crate on
// the host, so the run that compiled is not a no_std environment and every
// covering run is flagged inconclusive.
//
// The verdict must not move all the same. Unlike a method call that could bind
// something else somewhere else, this crate *spells* `std` — under the no_std it
// declares for `target_os = "none"` there is no `extern crate std` to bind the
// name, so the path cannot resolve on any target where its own attribute
// applies. `crate_named_std_in_path` is the guard, and this fixture is what
// stops the rule from silencing the two crates in the bucket that are genuinely
// std.

#![cfg_attr(target_os = "none", no_std)]

/// Ungated, spelled-out std. No feature set removes it and no target rescues it.
pub fn label() -> std::string::String {
    std::string::String::new()
}

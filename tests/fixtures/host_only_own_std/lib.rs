// The control for `host_only_shim`, and the case the host fallback exists for
// (tarfs enabling `builtin_devices`): no bare-metal target compiles here
// either, but the compile dies in *this* crate — `error: could not compile
// `host_only_own_std`` — because the std usage below is this crate's own and
// ungated.
//
// So the host build is the only place that usage surfaces, its records are the
// evidence, and it must keep being reported. A rule that discounted every
// host-only run rather than only the ones that never reached this crate would
// silence this.

#![no_std]

extern crate std;

/// Ungated, unavoidable std. No feature set removes it.
pub fn label() -> std::string::String {
    std::string::String::new()
}

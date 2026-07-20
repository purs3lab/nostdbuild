// std usage gated on target_os, not on a feature flag.
// The target is the consumer's choice, not an axis this tool controls, so the
// gate is accepted as a guard: these spans are excused rather than reported.
// See std_tests::test_cfg_target_os_gates_std_is_excused.

#[cfg(target_os = "linux")]
use std::fs;

fn main() {
    #[cfg(target_os = "linux")]
    let _ = std::fs::read_to_string("/dev/null");
}

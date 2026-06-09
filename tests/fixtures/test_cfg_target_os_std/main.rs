// std usage gated on target_os, not on a feature flag.
// The tool controls feature combinations but not the target platform,
// so this std usage cannot be avoided by any feature setting.

#[cfg(target_os = "linux")]
use std::fs;

fn main() {
    #[cfg(target_os = "linux")]
    let _ = std::fs::read_to_string("/dev/null");
}

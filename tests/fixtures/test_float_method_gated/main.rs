// The same std-only method call as `test_float_method_std`, behind a feature
// gate. Turning `std` off deletes the function, so no covering run resolves the
// call into std and the crate is convertible by disabling the feature.
//
// The point of the fixture is that the new method-call records are subject to
// the ordinary gating machinery: the record's span sits inside the `#[cfg]`, so
// the tree finds an ancestor for it and the span is conditional, not hard. A
// record that skipped the call-site/ancestor treatment would land outside the
// ModNode tree, find no gate, and sink a crate that converts cleanly.

#[cfg(feature = "std")]
pub fn ratio_to_register(division_ratio: u8) -> u8 {
    f32::from(division_ratio).log2().round() as u8
}

// Reachable with the feature off, and resolves into core rather than std — so
// the crate has code to cover in both configurations.
pub fn halve(value: u8) -> u8 {
    value.wrapping_div(2)
}

fn main() {
    let _ = halve(8);
}

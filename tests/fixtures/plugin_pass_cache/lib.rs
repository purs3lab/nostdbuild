// Deliberately trivial: no std, no external deps, arch-independent, so it
// compiles for a bare-metal target in well under a second. Its only job is to
// be cheap to compile twice — the point of the test that uses it.
#![no_std]

pub fn add(a: u32, b: u32) -> u32 {
    a + b
}

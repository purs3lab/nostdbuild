// feat_a is defined as feat_a = ["feat_b"] in Cargo.toml.
// Cargo automatically enables feat_b whenever feat_a is enabled.
// std usage is gated on feat_b, so enabling only feat_a is enough to trigger it.
// Without either feature, no std usage is present.

#[cfg(feature = "feat_b")]
extern crate std;

struct Wrapper;

#[cfg(feature = "feat_b")]
impl std::fmt::Display for Wrapper {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Wrapper")
    }
}

fn main() {}

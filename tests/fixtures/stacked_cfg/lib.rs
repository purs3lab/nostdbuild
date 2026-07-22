#![no_std]

struct Foo;

// Two `#[cfg(..)]` attributes stacked on one item AND together in Rust: the impl
// is present only when *both* `alpha` and `std` are enabled. Taking only the
// first attribute drops the second gate, so negating the (wrong) `alpha` gate
// re-enables std and manufactures a false positive (inout stacks
// block-padding and std the same way).
#[cfg(feature = "alpha")]
#[cfg(feature = "std")]
impl std::error::Error for Foo {}

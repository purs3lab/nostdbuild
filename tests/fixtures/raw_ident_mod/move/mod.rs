struct Foo;

// Gated std usage that is only discoverable if `move/mod.rs` is actually walked.
// The gate lives on the impl (mirroring ref_kind's `#[cfg(feature = "std")] impl
// std_crate::error::Error for MoveError {}`).
#[cfg(feature = "std")]
impl Foo {
    pub fn bar() {
        let _ = std::string::String::new();
    }
}

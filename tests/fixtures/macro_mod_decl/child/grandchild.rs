// Unconditional std usage; gated only by inheriting the parent chain
// (feature = "parent") through the macro-declared modules.
pub fn g() -> std::vec::Vec<u8> {
    std::vec::Vec::new()
}

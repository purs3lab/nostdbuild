// UNGATED std usage: the only gate that can reach it is the `#[cfg]` on the
// macro invocation that declared this module.
pub fn uses_std() -> std::string::String {
    std::string::String::new()
}

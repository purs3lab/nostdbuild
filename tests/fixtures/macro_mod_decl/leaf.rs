// std usage gated by an inner item cfg — clears once this file is walked.
#[cfg(feature = "std")]
pub fn uses_std() -> std::string::String {
    std::string::String::new()
}

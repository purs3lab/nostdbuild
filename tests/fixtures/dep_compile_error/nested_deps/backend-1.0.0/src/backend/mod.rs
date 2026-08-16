#[cfg(not(any(feature = "u32_backend", feature = "u64_backend")))]
compile_error!("no backend cargo feature enabled! please enable one of: u32_backend, u64_backend");

pub fn nothing() {}

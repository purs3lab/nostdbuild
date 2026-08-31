#[cfg(any(feature = "cannot", feature = "wasm"))]
compile_error!("`cannot` and `wasm` cannot be used with `enable`");

pub fn nothing() {}

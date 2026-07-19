// Pulled in under `#[cfg(feature = "std")]`; its std usage is ungated *here* and
// is gated only by the include site.
mod gated_stdlib {
    pub use std::string::MarkerGatedInclude;
}

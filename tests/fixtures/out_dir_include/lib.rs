// Fixture mirroring akd_core: a cfg-gated module whose contents are pulled in
// from a build-script-generated file via `include!(concat!(env!("OUT_DIR"), …))`.
// syn can't resolve OUT_DIR, so the driver must splice the generated file in
// gated by this module's condition.

#[cfg(all(feature = "protobuf", not(feature = "nostd")))]
pub mod proto;

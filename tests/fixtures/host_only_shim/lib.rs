// bitstream-io 4.0.0 in miniature.
//
//   #![no_std]
//   #[cfg(not(feature = "std"))] use core2::io;
//   #[cfg(feature = "std")]      use std::io;
//   ...
//   fn read(...) -> io::Result<u8>          // no cfg on the use sites
//
// `io` is a cfg-selected alias, and `resolve_import_to_use_gateways` does anchor
// the use sites to the std-gated one — the gate is found. What fails is the
// evidence: with `std` off the shim is *still* built with its own default `std`
// feature, so no bare-metal target compiles and the host is the only build that
// survives. On the host `shim::io` IS `std::io`, so the std-off run reports the
// very std it was run to rule out, and the ¬std probe comes back "still std".
//
// Nothing here is std usage this crate could avoid by picking features — the
// shim's configuration is what puts std in the answer.

#![no_std]

#[cfg(feature = "std")]
extern crate std;

#[cfg(not(feature = "std"))]
use shim::io;
#[cfg(feature = "std")]
use std::io;

/// The ungated use site — bitstream-io's `-> io::Result<u8>`. `io` resolves to
/// `shim::io` with `std` off and to `std::io` with `std` on, and on the host
/// those are the same type. The name `io` is bound by a std-gated import, so
/// `resolve_import_to_use_gateways` anchors this span and it is probed.
pub fn describe(_e: &io::Error) -> bool {
    true
}

/// The other half of bitstream-io's residue: a *method* on a shim trait
/// (`writer.write_all(…)` against `W: io::Write`). Nothing in the source spells
/// `io` at the call site, so no import anchors it — the span is ungated, and
/// `initial_ungated_results` short-circuits it to `StillStd` without compiling
/// anything. Only "no run ever left the host" can answer it.
pub fn flush_it<W: io::Write>(mut w: W) -> io::Result<()> {
    w.flush()
}

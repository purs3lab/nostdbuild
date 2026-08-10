// cuda_std 0.2.2 / macaw 0.20.0 in miniature.
//
//   #![cfg_attr(target_arch = "spirv", no_std)]   // macaw, renderling, saft-sdf
//   #![cfg_attr(target_os   = "cuda",  no_std)]   // cuda_std
//   ...
//   x.trunc()                                     // no cfg on the expression
//
// The `#![no_std]` hangs off a *target* predicate. Policy G erases the atom, so
// the attribute yields no feature equation and the crate gets no no_std
// condition — but that is only half of why it fails.
//
// The other half is what every build does. On a bare-metal target the predicate
// is false, so the attribute does not apply, so this is a plain std crate — and
// bare metal has no std, so the compile dies here with `can't find crate for
// std` and cargo says ``could not compile `target_cfg_no_std```. O-7's
// discriminator reads that as "bare metal reached the crate" and lets the host
// fallback keep its authority. But on the host the predicate is false too: the
// crate compiled there is *not* no_std, and `x.trunc()` binding std's inherent
// `f32::trunc` in a std build was never in question.
//
// So the one run that exists proves nothing about no_std-ness, and the span must
// come back unproven rather than as hard unguarded std.

#![cfg_attr(target_arch = "spirv", no_std)]

/// The ungated method call. `initial_ungated_results` short-circuits it to
/// `StillStd` without compiling anything, so only "no run was ever a no_std
/// environment" can answer it. On spirv it would bind the shader intrinsic;
/// there is no such target in `TARGET_LIST`, and rustc has none at all.
pub fn round_down(x: f32) -> f32 {
    x.trunc()
}

// Three optional dependencies, three answers to "does deleting the dep entry out
// of the feature that names it turn this import's gate off?". Parsed by
// `ModCollector` only — no build.

// bevy_input-0.16.0. `smol_str` is an optional dep AND an explicitly declared
// feature (`smol_str = ["dep:smol_str", "bevy_reflect/smol_str"]`), so the
// feature survives the deletion with its other value, stays on the command line,
// and this gate stays true. Unlinking is unsafe.
#[cfg(feature = "smol_str")]
use smol_str::SmolStr;

// watchface-0.4.0. `chrono` is optional with no `[features]` entry of its own, so
// Cargo's implicit feature is the only thing that can turn this gate on — and
// deleting `chrono` out of `std` does turn it off. Unlinking is safe.
#[cfg(feature = "chrono")]
use chrono::Utc;

// a7105-0.1.0. Implicit feature again, but the gate names a *different* feature
// that the deletion cannot touch. Unlinking is unsafe.
#[cfg(feature = "async")]
use embedded_hal_async::spi::SpiDevice;

pub fn use_them(_: SmolStr, _: Utc, _: &dyn SpiDevice) {}

// ---------------------------------------------------------------------------
// Below: the same question for crates the code names without importing them.
// R31-2 — 24 of its 49 crates reference their dependency only this way, and the
// import-only evidence read that as "nobody names it", so `minimize` unlinked it
// under a feature the crate's own `default` turns on.
// ---------------------------------------------------------------------------

// mutex-1.0.0. One call, no import anywhere in the crate, under a declared
// feature that the deletion cannot turn off. Unlinking is unsafe.
#[cfg(feature = "impl-critical-section")]
pub fn locked() {
    critical_section::with(|_| {});
}

// icu_calendar-1.5.2. The reference is the *macro's own path* — there is no
// import and no ordinary path either. Unlinking is unsafe.
#[cfg(feature = "compiled_data")]
pub fn baked() {
    icu_calendar_data::make_provider!(Baked);
}

// pallet-revive-uapi-0.4.0. The reference lives inside a `cfg_attr`, which syn
// hands over as opaque tokens; the derive only exists when `scale` is on, which
// is exactly the gate. Unlinking is unsafe.
#[cfg_attr(feature = "scale", derive(scale_info::TypeInfo))]
pub struct Flags;

// Control, watchface's shape in path form: the only gate is the dependency's own
// implicit feature, so the deletion turns it off and unlinking stays safe. Without
// this the fix would be "pin everything", which is not a fix.
#[cfg(feature = "yazi")]
pub fn decode() -> yazi::Error {
    yazi::Error::Underflow
}

// Control: a doc comment is a string literal, not a path. `deflate` is named
// nowhere else, so nothing may pin it — this is the mechanism that cost
// watchface its build the last time spans were resolved this way.
/// Compresses with deflate::deflate_bytes_zlib when the feature is on.
pub fn documented() {}

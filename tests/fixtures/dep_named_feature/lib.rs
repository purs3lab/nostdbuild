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

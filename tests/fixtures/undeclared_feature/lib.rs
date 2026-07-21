#![cfg_attr(not(feature = "forced"), no_std)]

// `forced` is NOT in the fixture manifest's [features]; only a build script can
// set it. `declared` IS a real feature.

#[cfg(feature = "forced")]
use std::sync::Mutex;

#[cfg(feature = "declared")]
use std::collections::BTreeMap;

#[cfg(all(not(feature = "declared"), feature = "forced"))]
mod mixed {
    use std::sync::Once;
}

#[cfg(not(feature = "forced"))]
mod forced_negated {
    use std::process::abort;
}

mod ungated {
    use std::fs::File;
}

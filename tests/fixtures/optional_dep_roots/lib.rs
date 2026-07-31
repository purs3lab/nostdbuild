// Every way a crate says "under this cfg I need that dependency linked", as seen
// by `visitor::collect_gated_extern_roots`. Parsed by `ModCollector` only — a
// real cargo build would need cfg-if and the backend crates on the registry.

// 1. Plain gated `use` (alexcrichton-cranelift-codegen's lib.rs:52).
#[cfg(not(feature = "std"))]
use hashbrown::{HashMap, HashSet};

// 2. Gated `extern crate`, the edition-2015 spelling (fatfs's lib.rs:80).
#[cfg(not(feature = "std"))]
extern crate core_io;

// 3. Renamed gated `use` — syn's UseTree::Rename, root is still the crate
//    (fatfs's lib.rs:101).
#[cfg(not(feature = "std"))]
use core_io as io;

// 4. `use` inside a cfg_if! arm (caches-0.3.0's polyfill.rs). The arm reaches the
//    visitor as one opaque token group, so this root only exists if the arm's
//    tokens are scanned.
cfg_if! {
    if #[cfg(feature = "std")] {
        pub fn ceil(v: f64) -> f64 { v.ceil() }
    } else {
        use libm;
        pub fn ceil(v: f64) -> f64 { libm::ceil(v) }
    }
}

// 5. Ungated `use` — always present, so it constrains nothing.
use always_there::Widget;

// 6. Gated by a cfg naming no feature. The atom is erased to a constant, so the
//    edge must NOT be emitted: `true => dep` would link it on every target.
#[cfg(target_arch = "wasm32")]
use wasm_only::Binding;

pub fn use_them(_: HashMap<u32, u32>, _: HashSet<u32>, _: Widget) {}

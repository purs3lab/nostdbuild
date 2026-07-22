// The canopen_rust shape. The crate is std on the host and no_std on a
// bare-metal target, split by a *non-feature* cfg. A prelude re-exports the std
// item behind that target gate; every module does `use crate::prelude::*` and
// then names the item bare.
//
// On the host build (where the plugin compiles), `HashMap` resolves to
// `std::collections::HashMap` through the target-gated `std_items` module, but
// the bare use carries no `local_route` and no `defining_module`, so
// `resolve_local_facade_gateways` never links it back to the import. The
// import, however, lives in an externally-gated (target-cfg) module, so
// `resolve_import_to_use_gateways` joins use → import on the bound name
// `HashMap` and lets the use inherit that gate. No span should be hard std.

mod prelude {
    #[cfg(not(target_os = "none"))]
    mod std_items {
        pub use std::collections::HashMap;
    }
    #[cfg(not(target_os = "none"))]
    pub use std_items::*;

    // The no_std arm (not compiled on the host) would bind `HashMap` to a
    // no_std replacement; it is here to show the arms do not contaminate.
    #[cfg(target_os = "none")]
    pub use core::marker::PhantomData as HashMap;
}

use crate::prelude::*;

pub fn count(m: &HashMap<u8, u8>) -> usize {
    let mut m2 = HashMap::new();
    m2.insert(1u8, 2u8);
    m.len() + m2.len()
}

fn main() {
    let m = HashMap::new();
    let _ = count(&m);
}

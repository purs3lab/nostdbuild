// The regalloc2 shape. A `#[cfg(feature = "std")] extern crate std;` at the
// crate root is the ordinary way for a no_std crate to name std, not a facade.
// `extern crate alloc;` next to it binds `alloc` in the root namespace, so
// `crate::alloc::vec::Vec` below is a genuine alloc path — the resolver reports
// it as `alloc` with a bare `crate` local_route.
//
// resolve_local_facade_gateways used to walk that route's prefixes down to the
// bare `crate` prefix, match the root `extern crate std`, and stamp the import
// `usage_crate = "std"`. The import carries no #[cfg], so the probe
// short-circuits it to StillStd without compiling and the crate fails.

#[cfg(feature = "std")]
extern crate std;

extern crate alloc;

mod cfg {
    use crate::alloc::vec::Vec;

    pub fn make() -> Vec<i32> {
        Vec::new()
    }
}

fn main() {
    let _v = cfg::make();
}

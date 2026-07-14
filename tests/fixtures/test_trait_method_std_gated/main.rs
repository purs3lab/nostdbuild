// Mimics zerocopy's `FromBytes::read_from_io` / `IntoBytes::write_to_io`
// pattern: a trait with *provided* (default-body) methods that are each gated
// by `#[cfg(feature = "std")]` and use `std::io` inside their bodies/signatures.
//
// The std usage lives inside a `TraitItemFn`, not an `ImplItemFn`. The visitor
// must record the method's own `#[cfg(feature = "std")]` gate so the probe can
// negate it and see the std usage disappear. Without that handling the gate is
// invisible, the span looks ungated, and it is misclassified as hard/unguarded
// std usage even though no feature-free build ever reaches it.

pub trait Bytes {
    fn as_bytes(&self) -> &[u8];

    #[cfg(feature = "std")]
    fn write_to_io<W>(&self, mut dst: W) -> std::io::Result<()>
    where
        W: std::io::Write,
    {
        dst.write_all(self.as_bytes())
    }

    #[cfg(feature = "std")]
    fn read_from_io<R>(mut src: R) -> std::io::Result<usize>
    where
        R: std::io::Read,
        Self: Sized,
    {
        let mut buf = [0u8; 8];
        src.read(&mut buf)
    }
}

// An independent std-gated item with no std usage in its body. This forces the
// coverage engine to schedule a build with the `std` feature enabled (mirroring
// how zerocopy's many std-gated items force a std run), so the std usages inside
// the gated trait methods above are actually observed during analysis. Without a
// std run, those usages would never be seen and the bug would stay hidden.
#[cfg(feature = "std")]
pub fn force_std_run() {}

struct Thing([u8; 8]);

// A std-gated associated const in an inherent impl. The `#[cfg(feature = "std")]`
// sits directly on an `ImplItem::Const`, so the std path in its type/value must be
// recorded via `visit_impl_item_const` (mirror of the trait-item case).
impl Thing {
    #[cfg(feature = "std")]
    const IO_ERR_KIND: std::io::ErrorKind = std::io::ErrorKind::Other;
}

impl Bytes for Thing {
    fn as_bytes(&self) -> &[u8] {
        &self.0
    }
}

fn main() {}

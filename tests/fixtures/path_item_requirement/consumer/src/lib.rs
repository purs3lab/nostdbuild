#![no_std]

// The import that does not resolve once the edge's defaults come off. Note it
// is a `use`, not a call: nothing here is ever type checked, so the obligation
// walk sees nothing and `impl_records` is empty.
use mathdep::float::Float;

pub fn hypot<T: Float>(a: T) -> T {
    a.sqrt()
}

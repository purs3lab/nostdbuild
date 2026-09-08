#![no_std]

use core::marker::PhantomData;

/// Stands in for hashbrown's `HashMap`: same call shape (`HashMap::<K, V>::new()`)
/// as `std::collections::HashMap`, minus everything this fixture never exercises.
#[derive(Default)]
pub struct HashMap<K, V>(PhantomData<(K, V)>);

impl<K, V> HashMap<K, V> {
    pub fn new() -> Self {
        HashMap(PhantomData)
    }
}

pub mod hash_map {
    /// Stands in for `std::collections::hash_map::RandomState` — real std's
    /// version is non-generic too, so the two-hop routed import
    /// (`use crate::hash_map::RandomState;`) has the same shape on both sides
    /// of the `std` cfg.
    #[derive(Default)]
    pub struct RandomState;

    impl RandomState {
        pub fn new() -> Self {
            RandomState
        }
    }
}

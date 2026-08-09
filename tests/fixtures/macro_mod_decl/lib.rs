// Fixture: modules declared from inside a macro_rules! invocation as passthrough
// `$item` args (agnostic_lite cfg_time!/cfg_time_with_docsrs! pattern). syn can't
// expand the macro, and the plugin skips these (their spans are not
// from_expansion), so the visitor must recognise the `mod X;` tokens directly.

macro_rules! cfg_time {
    ( $($item:item)* ) => {
        $(
            #[cfg(feature = "time")]
            $item
        )*
    };
}

// `leaf` module declared at crate root via the macro. Its file has an item
// gated by an inner `#[cfg(feature = "std")]`.
cfg_time! {
    mod leaf;
}

// A parent module declared normally but *gated* (feature = "parent"); its child
// is declared via the macro and must inherit the parent's gate.
#[cfg(feature = "parent")]
pub mod child;

// A macro invocation carrying its OWN `#[cfg]`. The module it declares must
// inherit that gate: only the ambient condition stack and the macro
// *definition*'s gate used to reach the child, so serde_json's
//     #[cfg(not(any(feature = "std", feature = "alloc")))]
//     hide_from_rustfmt! { mod error; }
// registered `features_check/error.rs` with no gate at all.
macro_rules! passthrough {
    ( $($item:item)* ) => { $( $item )* };
}

#[cfg(feature = "invocation")]
passthrough! {
    mod invoked;
}

// Control: the same macro without an attribute still yields no gate, so an
// ungated std usage below it stays ungated.
passthrough! {
    mod ungated;
}

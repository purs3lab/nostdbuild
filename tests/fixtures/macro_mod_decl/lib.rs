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

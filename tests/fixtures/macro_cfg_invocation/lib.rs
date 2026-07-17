// Fixture reproducing valuable's pattern: a list-style macro captures
// `$(#[$attrs:meta])*` and the cfg gates live at the *invocation* site.
// syn cannot see inside the invocation, so the std paths those gates cover
// must be recognised by parsing the invocation tokens.

macro_rules! value {
    (
        $(
            $(#[$attrs:meta])*
            $variant:ident($ty:ty),
        )*
    ) => {
        pub enum Value<'a> {
            $(
                $(#[$attrs])*
                $variant($ty),
            )*
            Unit,
        }
    };
}

value! {
    Bool(bool),
    #[cfg(feature = "std")]
    Path(&'a std::path::Path),
    #[cfg(feature = "std")]
    Error(&'a (dyn std::error::Error + 'static)),
    Debuggable(&'a dyn core::fmt::Debug),
}

macro_rules! collection {
    ( $( $(#[$attrs:meta])* ($($g:tt)*) $ty:ty, )* ) => {
        $(
            $(#[$attrs])*
            impl<$($g)*> Listable for $ty {}
        )*
    };
}

pub trait Listable {}

collection! {
    #[cfg(feature = "std")]
    (T: core::hash::Hash, H: core::hash::BuildHasher) std::collections::HashSet<T, H>,
    (T: Clone) UngatedWrapper<T>,
}

pub struct UngatedWrapper<T>(T);

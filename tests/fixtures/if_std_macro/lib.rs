// The gate lives in the definition; every invocation below carries no attribute.
macro_rules! if_std {
    ($($i:item)*) => ($(
        #[cfg(feature = "std")]
        $i
    )*)
}

// Control: a passthrough macro that applies no cfg at all.
macro_rules! passthrough {
    ($($i:item)*) => ($( $i )*)
}

// Control: rules that disagree on the gate — no single gate can be named.
macro_rules! disagreeing {
    (a $($i:item)*) => ($( #[cfg(feature = "std")] $i )*);
    (b $($i:item)*) => ($( #[cfg(feature = "alloc")] $i )*);
}

// A multiplexer (proptest's `multiplex_alloc!`): the transcriber leads with one
// cfg and then emits a SECOND item under a different one. The leading gate
// governs only the first branch, so no gate can be named for the invocation.
macro_rules! multiplex {
    ($($alloc:path, $std:path),*) => {
        $(
            #[cfg(all(feature = "alloc", not(feature = "std")))]
            pub use $alloc;
            #[cfg(feature = "std")]
            pub use $std;
        )*
    };
}

// Control for the same rule: the leading gate DOES govern everything the macro
// emits, and the cfg inside the item's own body is a further restriction under
// it — not a sibling. The gate must survive.
macro_rules! nested_cfg_inside {
    ($($i:item)*) => ($(
        #[cfg(feature = "std")]
        mod wrapper {
            #[cfg(feature = "extra")]
            pub use std::io::Read;

            $i
        }
    )*)
}

multiplex! {
    alloc::borrow::Cow, ::std::borrow::Cow
}

nested_cfg_inside! {
    use std::sync::RwLock;
}

if_std! {
    mod gated_mod;

    use std::sync::Mutex;
}

passthrough! {
    use std::fs::File;
}

disagreeing! {
    a
    use std::net::TcpStream;
}

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

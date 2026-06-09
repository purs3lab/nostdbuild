mod lib {
    #[cfg(not(feature = "use_std"))]
    pub use core::*;
    #[cfg(feature = "use_std")]
    pub use std::*;

    pub use std::vec::Vec;
}

#[cfg(feature = "si")]
mod si {
    struct MyStruct;

    #[cfg(feature = "f64")]
    impl MyStruct {
        fn my_method(&self) -> crate::lib::primitive::f64 {
            crate::lib::f64::consts::PI
        }
    }
}

fn main() {
    // #[cfg(feature = "use_std")]
    let err: crate::lib::error::Error;
    let vec: crate::lib::Vec<i32> = crate::lib::Vec::new();
}

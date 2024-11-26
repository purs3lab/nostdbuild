// no_std is an attribute (#![no_std]), and if we include it we cannot build bc println not in scope
// in the case that either "bar" or "no_std" is defined, this line becomes #![no_std]
#![cfg_attr(any(feature = "bar", feature = "no_std"), no_std)]

#[cfg_attr(any(all(featu = "joe", f = "woeifjawoeijfowijfwoeifj"), feature = "bar", feature = "no_std", not(feature = "no_std")), no_std)]
use std::array;

// even if we build for no_std, we can still bring in no_std with this line
extern crate std;

#[cfg_attr(feature = "sparkle", magic, buddy)]
#[cfg(any(all(foo, not(bar))))]
use core::panic::PanicInfo;

// #[cfg(...)] says include what is under this code only if the predicate is true
#[cfg(not( 
    feature =     "bar"))]
#[cfg(feature = "no_std")]
#[allow(dead_code)]
fn main() {
    #![cfg(not(features = "bar"))]

    // this is at runtime, not compile time
    if cfg!(not(feature = "bar")) {
        println!("Hello, world!");
    } else {
        println!("hi there, world");
    }

    // must include either feature "foo" or feature "bar" in cargo.toml
    #[cfg(any(feature = "bar", all(feature = "foo", feature="bar"), feature="joe"))]
    std::println!("hi");

    #[cfg(all(target_family="unix", target_pointer_width = "64"))]
    std::println!("unix?");

    let mut i = 0;
    while i < 10 {
        println!("hello");
        #[cfg(all(target_family="unix"))]
        println!("on unix");

        i = i + 1;
    }

    let mut vec = std::vec![0, 1, 2];

    // because this is not specified in cargo.toml as an option, we must pass it via "billy"
    #[cfg(feature = "billy")]
    println!("this must be specified at the command line: rustc --cfg=feature=\"billy\" src/main.rs && ./main");


}

#[cfg(feature = "bar")]
fn main() {}

// if we misspell feature, it will give a warning but still compile
#[cfg(featue = "no_std")]
fn no_std() {
    weofj
}

#[cfg_attr(all(any(feature = "foo", feature = "bar"), feature = "joe"), allow(dead_code))]
struct HStderr {}

impl HStderr {
    fn new() -> HStderr {
        return HStderr{}
    }
}


#[cfg(feature = "bar")]
#[panic_handler]
fn panic(info: &PanicInfo) -> ! {
    let mut host_stderr = HStderr::new();

    // logs "panicked at '$reason', src/main.rs:27:4" to the host stderr
    // writeln!(host_stderr, "{}", info).ok();

    loop {}
}

/*
Learnings: 
must put feature = "foo" and then define that feature in cargo.toml
we set the default feature to "foo", so the "hi" printline is not grayed out
but we can pass "--no-default-features" to disable that
this command adds the configuration to the compilation: rustc --cfg=feature=\"bar\" src/main.rs
*/
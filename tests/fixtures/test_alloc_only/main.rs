extern crate alloc;

fn main() {
    let _v: alloc::vec::Vec<i32> = alloc::vec::Vec::new();
    let _s: alloc::string::String = alloc::string::String::from("hello");
    let _b: alloc::boxed::Box<i32> = alloc::boxed::Box::new(42);
}

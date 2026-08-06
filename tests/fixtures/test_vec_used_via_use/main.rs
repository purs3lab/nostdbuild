use std::vec::Vec;

fn main() {
    // Annotated for the same reason as `test_vec_direct_usage`: an unconstrained
    // element type is `E0282`, raised during analysis.
    let mut v: Vec<u8> = Vec::new();
    v.push(1);
}
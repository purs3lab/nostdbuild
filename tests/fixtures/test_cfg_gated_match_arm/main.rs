// An independent std-gated item forces a covering build with the `std` feature,
// so the gated arm below is actually observed during analysis (see the note in
// test_trait_method_std_gated).
#[cfg(feature = "std")]
pub fn force_std_run() {}

// The std usage lives only inside a `#[cfg(feature = "std")]`-gated match arm.
// The enclosing function is NOT gated, and the arm body is an `Expr` (not a
// `Stmt`), so only `visit_arm` can record the gate. Without it the usage looks
// ungated and is misclassified as hard/unguarded std usage.
pub fn classify(x: u8) -> u8 {
    match x {
        #[cfg(feature = "std")]
        0 => {
            let _ = std::io::stdin();
            0
        }
        _ => 1,
    }
}

fn main() {}

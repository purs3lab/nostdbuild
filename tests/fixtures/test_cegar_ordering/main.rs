#![no_std]
// A `#![no_std]` bin needs no `main` and must supply its own panic handler.
// Both are checked during analysis, not during expansion — so before the plugin
// pass ran through type checking, this fixture "compiled" on every target
// without either of them.
#![no_main]
#![allow(dead_code)]

#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    loop {}
}

// eq_a — depends on b_helper at compile time; must appear first in AST
// so the greedy seed is eq_a and eq_c_not_b poisons the set before eq_b can join
#[cfg(feature = "feat_a")]
pub fn a_function() {
    b_helper();
}

// eq_c_not_b — compatible with eq_a alone but incompatible with eq_b (feat_b=false)
// appears second so it gets greedily added to the eq_a set before eq_b is tested
#[cfg(all(feature = "feat_c", not(feature = "feat_b")))]
pub fn c_only() {}

// eq_b — defines b_helper needed by a_function; appears last so it's blocked
// by eq_c_not_b from pairing with eq_a in the greedy algorithm
#[cfg(feature = "feat_b")]
pub fn b_helper() {}

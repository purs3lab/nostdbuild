// afe4404 0.2.4's shape, reduced: `f32::from(ratio).log2().round()`.
//
// `log2` and `round` are inherent methods on `f32` defined in
// `library/std/src/f32.rs`; `core` has no counterpart, so this crate cannot be
// no_std. Nothing here is a *path* — the receiver is a plain `f32` and the
// method is reached by dot syntax, so the call has no entry in the resolver's
// `partial_res_map` and the AST pass records nothing at all. Only
// `type_dependent_def_id`, which exists after type checking, names `std`.

pub fn ratio_to_register(division_ratio: u8) -> u8 {
    f32::from(division_ratio).log2().round() as u8
}

fn main() {
    let _ = ratio_to_register(8);
}

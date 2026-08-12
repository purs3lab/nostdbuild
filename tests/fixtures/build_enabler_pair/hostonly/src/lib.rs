//! Deliberately NOT no_std: on a bare-metal target this fails with
//! `can't find crate for `std``, which is what makes the all-on enabler trial
//! fail for a reason that has nothing to do with the feature pair being sought.

pub fn name() -> String {
    String::from("hostonly")
}

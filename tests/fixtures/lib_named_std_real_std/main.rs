// Control for `lib_named_std`. Same `[lib] name = "std"`, but this one really
// does use the sysroot std.
//
// A crate's own name is not in its extern prelude, so a path starting `std::`
// here reaches the injected sysroot crate, whose `DefId`s are not local. The
// fix keys on that — `def_id.krate == LOCAL_CRATE` — and not on the name, so
// this usage must still be reported even though the local crate answers to the
// same name.

pub mod os {
    pub struct OsStr {
        _tbd: (),
    }
}

pub fn counts() -> std::collections::HashMap<u8, u8> {
    std::collections::HashMap::new()
}

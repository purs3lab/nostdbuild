// stdworld 0.1.1's shape, reduced. The package is `lib_named_std` but its
// library is named `std`, so `tcx.crate_name(LOCAL_CRATE)` answers "std" for
// everything below — the modules, the types, and the generic parameter `W`.
//
// None of it is the sysroot std: the crate is `#![no_std]`, has no
// dependencies, and defines every name it uses. A verdict of "unguarded std"
// here can never be escaped by any feature set, because there is no feature
// that removes a crate's own definitions.

#![no_std]

pub mod os {
    pub struct OsStr {
        _tbd: (),
    }

    pub struct OsString {
        _tbd: (),
    }
}

pub trait EnvAccess<W> {
    fn var_os(&self, key: &crate::os::OsStr) -> Option<crate::os::OsString>;
}

pub struct MainWorld {
    _tbd: (),
}

impl MainWorld {
    pub fn new() -> MainWorld {
        MainWorld { _tbd: () }
    }
}

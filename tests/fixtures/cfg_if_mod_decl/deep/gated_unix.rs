// Reached only through a target-gated cfg_if arm, so its std usage is excused —
// but only if the tree resolves this file's path correctly enough to find it.
pub fn read_it() {
    let _ = std::fs::File::open("/etc/hostname");
}

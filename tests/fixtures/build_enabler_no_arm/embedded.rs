/// The no_std arm's replacement for `std::string::String` — no std, no alloc,
/// so it compiles for a bare-metal target.
#[derive(Copy, Clone)]
pub struct Text(pub &'static str);

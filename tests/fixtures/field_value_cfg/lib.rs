// Fixture: `#[cfg(...)]` on a field of a struct *literal* (array_section,
// bevy_ecs pattern). These are `syn::FieldValue`, not `syn::Field`, so they need
// their own visit handler or the std expression in the value looks unconditional.

pub struct Error {
    pub section: u32,
    #[cfg(feature = "std")]
    pub backtrace: std::backtrace::Backtrace,
    pub always: u32,
}

impl Error {
    pub fn new(section: u32) -> Self {
        Self {
            section,
            #[cfg(feature = "std")]
            backtrace: std::backtrace::GatedCapture::capture(),
            // Control: an ungated field whose value mentions std must stay ungated.
            always: std::mem::UngatedSize::of(),
        }
    }
}

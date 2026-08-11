#![no_std]

/// The crate writes no std. Everything std about it arrives at this one span,
/// out of the derive's expansion.
#[derive(injector::Inject)]
pub struct Message;

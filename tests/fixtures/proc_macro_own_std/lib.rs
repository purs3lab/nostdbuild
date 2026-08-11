#![no_std]

/// The expansion is plain: nothing std reaches this crate, so nothing about the
/// macro's features is this crate's business.
#[derive(hostonly::Plain)]
pub struct Message;

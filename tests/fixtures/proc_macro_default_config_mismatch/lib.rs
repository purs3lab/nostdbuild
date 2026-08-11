#![no_std]

#[cfg(feature = "alt")]
pub struct AltReceipt;

#[derive(dual::Dual)]
pub struct Message;

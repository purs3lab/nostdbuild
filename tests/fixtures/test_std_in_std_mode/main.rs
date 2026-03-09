#![no_std]
#![no_main]

use core::panic::PanicInfo;

use std::fs;

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}
pub const DOWNLOAD_PATH: &str = "/tmp/downloads";
pub const CRATE_IO: &str = "https://crates.io";

pub const TARGET_LIST: [&str; 26] = [
    "aarch64-unknown-none",
    "aarch64-unknown-none-softfloat",
    "armebv7r-none-eabi",
    "armebv7r-none-eabihf",
    "armv7a-none-eabi",
    "armv7r-none-eabi",
    "armv7r-none-eabihf",
    "loongarch64-unknown-none",
    "loongarch64-unknown-none-softfloat",
    "nvptx64-nvidia-cuda",
    "riscv32i-unknown-none-elf",
    "riscv32im-unknown-none-elf",
    "riscv32imac-unknown-none-elf",
    "riscv32imafc-unknown-none-elf",
    "riscv32imc-unknown-none-elf",
    "riscv64gc-unknown-none-elf",
    "riscv64imac-unknown-none-elf",
    "thumbv6m-none-eabi",
    "thumbv7em-none-eabi",
    "thumbv7em-none-eabihf",
    "thumbv7m-none-eabi",
    "thumbv8m.base-none-eabi",
    "thumbv8m.main-none-eabi",
    "thumbv8m.main-none-eabihf",
    "wasm32v1-none",
    "x86_64-unknown-none",
];

/// Check if the target is valid
/// # Arguments
/// * `target` - The target to check
/// # Returns
/// * `bool` - Whether the target is valid
pub fn is_valid_target(target: &str) -> bool {
    TARGET_LIST.contains(&target)
}

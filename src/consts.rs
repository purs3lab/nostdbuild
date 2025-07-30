pub const DOWNLOAD_PATH: &str = "/evaldisk/sourag/downloads";
pub const CRATE_IO: &str = "https://crates.io";
pub const DB_FILE_NAME: &str = "db.bin";
pub const RESULTS_JSON_PREFIX: &str = "/evaldisk/sourag/results/";
pub const RESULTS_JSON_SUFFIX: &str = "_args.json";

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

pub const CUSTOM_FEATURES: &str = "custom_default_features";

/// Check if the target is valid
/// # Arguments
/// * `target` - The target to check
/// # Returns
/// * `bool` - Whether the target is valid
pub fn is_valid_target(target: &str) -> bool {
    TARGET_LIST.contains(&target)
}

/// Get the target names which start with the given architecture
/// # Arguments
/// * `arch` - The architecture to get the target names for
/// # Returns
/// * `Vec<String>` - The target names which start with the given architecture
pub fn get_target_names_from_arch(arch: &str) -> Vec<String> {
    TARGET_LIST
        .iter()
        .filter(|target| target.starts_with(arch))
        .map(|&target| target.to_string())
        .collect()
}

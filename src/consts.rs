use const_format::concatcp;

pub const DOWNLOAD_PATH: &str = "/evaldisk/sourag/downloads";
pub const CRATE_IO: &str = "https://crates.io";
pub const DB_FILE_NAME: &str = "db.bin";
pub const RESULTS_PATH: &str = "/evaldisk/sourag/results/";

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

/// When we remove a feature from default enabled features, we add it to
/// this custom feature list to preserve the original functionality
/// of the crate when used in std mode.
pub const CUSTOM_FEATURES_DISABLED: &str = "custom_default_features";

/// When a dependency is required to enable a feature but main crate does
/// not provide a feature  that enables that feature, we add it to this
/// custom feature and enable it while in no_std mode.
/// Or if a main feature got disable since it was enabling something that
/// was not required, and if that feature also enabled something required
/// by a dependency, we add it here.
pub const CUSTOM_FEATURES_ENABLED: &str = "custom_no_std_feature_enabled";

/// If after processing a dependency, we find that main enables some
/// feature for the dependency which is uncessary and might cause
/// compilation issues, we add it to this list so as to preserve
/// original functionality in std mode.
pub const DEP_UNNECESSARY_FEATURES: &str = "dep_unnecessary_features";

/// Some crates have known syntex failures but are `no_std`.
/// We need to ignore these crates to prevent false negatives.
pub const KNOWN_SYN_FAILURES: [&str; 1] = ["nb:0.1.3"];

/// We do hir analysis once. It is done if the crate fails
/// to build for all targets. This determines if the tool made a mistake and
/// accidentally included std usages.
/// The results of the hir visit are stored in this file.
pub const HIR_VISITOR_SPAN_DUMP: &str = concatcp!(RESULTS_PATH, "hir_visitor_span_dump.json");

/// Check if the target is valid
/// # Arguments
/// * `target` - The target to check
/// # Returns
/// * `bool` - Whether the target is valid
pub fn is_valid_target(target: &str) -> bool {
    TARGET_LIST.contains(&target)
}

pub const DOWNLOAD_PATH: &str = "/evaldisk/sourag/downloads";
pub const INDEX_CRATES_IO: &str = "https://index.crates.io";
pub const STATIC_CRATES_IO: &str = "https://static.crates.io";
pub const DB_FILE_NAME: &str = "db.bin";
pub const RESULTS_PATH: &str = "/evaldisk/sourag/results/";

pub const TARGET_LIST: [&str; 33] = [
    "aarch64-unknown-none",
    "aarch64-unknown-none-softfloat",
    // "armebv7r-none-eabi",
    // "armebv7r-none-eabihf",
    "armv7a-none-eabi",
    "armv7a-none-eabihf",
    "armv7r-none-eabi",
    "armv7r-none-eabihf",
    "armv8r-none-eabihf",
    "loongarch32-unknown-none",
    "loongarch32-unknown-none-softfloat",
    "loongarch64-unknown-none",
    "loongarch64-unknown-none-softfloat",
    "nvptx64-nvidia-cuda",
    "riscv32i-unknown-none-elf",
    "riscv32im-unknown-none-elf",
    "riscv32imac-unknown-none-elf",
    "riscv32imafc-unknown-none-elf",
    "riscv32imc-unknown-none-elf",
    "riscv64gc-unknown-none-elf",
    // "riscv64im-unknown-none-elf", // rustc knows it (`--print target-list`), but no
    // prebuilt `rust-std` on this pinned nightly (2026-08-24) — `rustup target add`
    // fails "no prebuilt artifacts available"; re-add once the component ships.
    "riscv64imac-unknown-none-elf",
    // "s390x-unknown-none-softfloat", // same gap: recognised by rustc, no prebuilt
    // `rust-std` for this nightly channel yet.
    "thumbv6m-none-eabi",
    "thumbv7a-none-eabi",
    "thumbv7a-none-eabihf",
    "thumbv7em-none-eabi",
    "thumbv7em-none-eabihf",
    "thumbv7m-none-eabi",
    "thumbv7r-none-eabi",
    "thumbv7r-none-eabihf",
    "thumbv8m.base-none-eabi",
    "thumbv8m.main-none-eabi",
    "thumbv8m.main-none-eabihf",
    "thumbv8r-none-eabihf",
    "wasm32v1-none",
    "x86_64-unknown-none",
];

/// R34-17's post-failure probe (`bin/main.rs`): representative OS targets tried,
/// in order, first build wins, once every `TARGET_LIST` member and every repair
/// has failed. Not a claim about which OS the crate actually needs — the point
/// is "does any OS make this compile" — but chosen to cover the OS families the
/// R34-17 family's crates gate on (`sc`/`atomic-wait`: linux, android, macos,
/// windows, freebsd). See `Telemetry::os_target_probe`.
pub const OS_TARGET_PROBES: [&str; 6] = [
    "x86_64-unknown-linux-gnu",
    "aarch64-unknown-linux-gnu",
    "x86_64-pc-windows-msvc",
    "x86_64-apple-darwin",
    "aarch64-linux-android",
    "x86_64-unknown-freebsd",
];

pub const PLUGIN_OUTPUT_ENV: &str = "NO_STD_TOOL_OUTPUT_PATH";

/// Sysroot crate names the pipeline reads as identities rather than as labels:
/// `usage_crate == "std"` is the thing that fails a crate, and `core`/`alloc`
/// are what mark a resolution as no_std-clean. A package that names its own
/// library one of these (`[lib] name = "std"`) would otherwise have every item
/// it defines mistaken for the sysroot crate — see
/// `hir_driver::reported_crate_name`.
pub const SYSROOT_CRATE_NAMES: [&str; 3] = ["std", "core", "alloc"];

/// What the plugin reports instead of a crate name when a resolution stayed
/// inside the crate being analysed. Not a crate name: no dependency is looked up
/// under it, and `is_local_reexport` keys on it.
pub const LOCAL_CRATE_SENTINEL: &str = "LOCAL";

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

/// Check if the target is valid
/// # Arguments
/// * `target` - The target to check
/// # Returns
/// * `bool` - Whether the target is valid
pub fn is_valid_target(target: &str) -> bool {
    TARGET_LIST.contains(&target)
}

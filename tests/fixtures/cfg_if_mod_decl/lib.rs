// Target-gated arms declaring the same module name via #[path] — the backtrace
// `src/symbolize/gimli.rs` shape. Every arm says `mod mmap;`, so without #[path]
// support they all collapse onto a nonexistent `mmap.rs`.
cfg_if::cfg_if! {
    if #[cfg(windows)] {
        #[path = "inner/mmap_windows.rs"]
        mod mmap;
    } else {
        #[path = "inner/mmap_unix.rs"]
        mod mmap;
    }
}

// Feature-gated arm with a plain `mod X;` — the bug is NOT target-specific, so
// this must be registered too, gated by the feature.
cfg_if::cfg_if! {
    if #[cfg(feature = "std")] {
        mod featmod;
    }
}

// Default path resolution (no #[path]) under a target gate — the hpm_rt
// `mod host;` shape, resolving to `host/mod.rs`.
cfg_if::cfg_if! {
    if #[cfg(all(target_arch = "riscv32", target_os = "none"))] {
        mod bare;
    } else {
        mod host;
    }
}

// Child file exercising #[path] resolution from a non-mod-rs file.
mod sub;

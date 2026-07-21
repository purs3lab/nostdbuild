// A NON-mod-rs child file containing #[path] mod declarations — the backtrace
// `src/symbolize/gimli.rs` shape, and the case the lib.rs fixtures above cannot
// reach. A bare `mod x;` here resolves into `sub/`, but a `#[path]` resolves
// relative to the directory holding *this file*, so these live in `deep/`, not
// `sub/deep/`. Resolving both the same way produced a ModNode for a file that
// does not exist, silently un-guarding everything in the real one.
cfg_if::cfg_if! {
    if #[cfg(windows)] {
        #[path = "deep/gated_win.rs"]
        mod gated;
    } else {
        #[path = "deep/gated_unix.rs"]
        mod gated;
    }
}

// The same base-directory rule outside cfg_if — `visit_item_mod`'s file-based
// branch had the identical confusion.
#[path = "deep/plain.rs"]
mod plain;

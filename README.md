# Automatic `no_std` compilation of crates

This tool automatically finds the features that should be enabled/disabled for a crate to be compiled in `no_std` mode.  
This requires that the crate already supports `no_std` build.

## Usage

The tool depends on an hir visitor plugin. So you will need to install the visitor first.
Run the following command from the root of the repo.
```sh
cargo install --path . --bin cargo-hir --bin hir-driver --force
```

You will need to update the following constans from [consts](./src/consts.rs):  
- `DOWNLOAD_PATH`: Location where the main crate and its dependencies will be downloaded
- `RESULTS_JSON_PREFIX`: Result json will be dumped here
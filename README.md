# Automatic `no_std` compilation of crates

This tool automatically finds the features that should be enabled/disabled for a crate to be compiled in `no_std` mode.  
This requires that the crate already supports `no_std` build.

The tool runs in three main phases moving to the next only if the previous one fails.

### Phase 1
Run the ast visitor to find std usages. Parse the attributes and find a starting point. This starting point depends on the kind of attribute that enables `no_std`. If is a conditional attribute, that condition is used as the starting point. If it is an unconditional attribute, we find any std usages gated by an attribute and use that as the starting point.  

Once we have all attributes and the starting point, we convert all the conditions into `z3` equation and solve them. This gives the required condition for `no_std`.  

> If there are conflicting conditions, for example, main crate a feature for a dependency that should be disabled to make the dependency `no_std`, we update the main crate manifest to solve this confict. Refer to `move_unnecessary_dep_feats` for more details.

### Phase 2
If the above phase fails to find list of features, we check the reason for failure. This phase checks for any std usages that are not gated by any attribute. This means the crate is configured such that it is impossible to build in `no_std` mode. This reuses the ast visitor result from the previous phase.

### Phase 3
Possibly the most time consuming and most likely to give false positive. This phase goes through the chain of dependencies and check if there is a dependency at some depth where it is impossible to make it `no_std` without modifying the manigest of its parent. We need to check this because we can only edit the manifest for the main crate. Meaning crates at depth > 1 is considered immutable.

## Usage

The tool depends on an ast visitor plugin. So you will need to install the visitor first.
Run the following command from the root of the repo.
```sh
cargo install --path . --bin cargo-hir --bin hir-driver --force
```
> The name is `hir` because that's what it used to do.

You will need to update the following constants from [consts](./src/consts.rs):  
- `DOWNLOAD_PATH`: Location where the main crate and its dependencies will be downloaded
- `RESULTS_PATH`: Location where the results are stored

## Tests
There are two sets of tests available for this crate:
- `./test-all hir`: Test the ast visitor with example crates
- `./test-all main`: Test the main crate functionality

> `cargo test` depends on the above paths as well. So update them before running tests.
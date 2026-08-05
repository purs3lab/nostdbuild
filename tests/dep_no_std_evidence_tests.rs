#![feature(rustc_private)]

//! Tests for `parser::no_std_evidence` — the three-way answer that replaced the
//! `check_for_no_std` boolean.
//!
//! The bucket this exists for (T5, `DEP_NOT_NO_STD_NO_DEP_NAMED`) is crates
//! where the tool set `dep_not_no_std` and named nobody. Half of that is
//! attribution; the other half is that "we parsed the crate root and it has no
//! `no_std`" and "we parsed nothing at all" used to be the same `false`. Only
//! the first is a statement about the crate — the second is why
//! `consts::KNOWN_SYN_FAILURES` exists, one crate name at a time.

use std::fs;
use std::path::PathBuf;

use nostd::consts::DOWNLOAD_PATH;
use nostd::parser::{NoStdEvidence, no_std_evidence};

/// Lay a crate out where `determine_manifest_file` looks for a main crate:
/// `<DOWNLOAD_PATH>/<name>-<version>/`.
fn write_crate(name: &str, lib_rs: &str) -> PathBuf {
    let dir = PathBuf::from(DOWNLOAD_PATH).join(format!("{name}-0.1.0"));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(dir.join("src")).unwrap();
    fs::write(dir.join("src/lib.rs"), lib_rs).unwrap();
    fs::write(
        dir.join("Cargo.toml"),
        format!(
            r#"[package]
name = "{name}"
version = "0.1.0"
edition = "2018"

[lib]
name = "{name}"
path = "src/lib.rs"
"#
        ),
    )
    .unwrap();
    dir
}

fn evidence_for(name: &str, lib_rs: &str) -> NoStdEvidence {
    let dir = write_crate(name, lib_rs);
    let ctx = z3::Context::new(&z3::Config::new());
    let evidence = no_std_evidence(&format!("{name}:0.1.0"), &ctx, None, None);
    fs::remove_dir_all(&dir).unwrap();
    evidence
}

#[test]
fn crate_root_declaring_no_std_is_supported() {
    assert_eq!(
        evidence_for(
            "nostd_evidence_supported",
            "#![no_std]\npub fn f() -> u8 { 1 }\n"
        ),
        NoStdEvidence::Supported
    );
}

#[test]
fn conditional_no_std_is_supported() {
    assert_eq!(
        evidence_for(
            "nostd_evidence_conditional",
            "#![cfg_attr(not(feature = \"std\"), no_std)]\npub fn f() -> u8 { 1 }\n"
        ),
        NoStdEvidence::Supported
    );
}

#[test]
fn parsed_crate_root_without_the_attribute_is_absent() {
    assert_eq!(
        evidence_for("nostd_evidence_absent", "pub fn f() -> u8 { 1 }\n"),
        NoStdEvidence::Absent
    );
}

/// The distinction the bucket is about: `syn` rejects the file, so the
/// attribute list is empty for want of evidence. Answering `Absent` here is
/// what let a dependency be reported as std-only — and the whole dependency
/// walk truncated — on the strength of a parse that read nothing.
#[test]
fn unparseable_source_is_not_evidence_of_std() {
    assert_eq!(
        evidence_for(
            "nostd_evidence_unparseable",
            "this is not rust at all ((( \n"
        ),
        NoStdEvidence::NoSources
    );
}

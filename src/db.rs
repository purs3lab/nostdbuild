use anyhow::Context;
use bincode::config;
use log::debug;
use std::{
    fs::{self, File},
    io::Read,
    path::Path,
    sync::Mutex,
};

use crate::{DBData, consts::DB_FILE_NAME};

/// How many times `process_dep_crate_wrapper` considered serving a
/// dependency from `db.bin` (mechanism #7 in `ablation_plan.md`) — the
/// denominator for `db_cache_hits`/`db_cache_bypassed` in `telemetry.json`.
/// Incremented once per dependency, regardless of outcome. See
/// `evaluation_plan.md` §6.1.
static DB_CACHE_ATTEMPTS: Mutex<u64> = Mutex::new(0);

/// How many of those attempts were actually served from `db.bin` instead of
/// running the full analysis.
static DB_CACHE_HITS: Mutex<u64> = Mutex::new(0);

/// How many attempts were forced past the cache by mechanism #6
/// (`dep_carries_impl_requirements`/`dep_carries_path_requirements`) even
/// though a `db.bin` entry may have existed — not a miss (no entry), a
/// deliberate bypass.
static DB_CACHE_BYPASSED: Mutex<u64> = Mutex::new(0);

/// Record one dependency reaching the DB-cache decision point in
/// `process_dep_crate_wrapper`, and whether mechanism #6 forced past it.
/// Call once per dependency, before checking [`get_from_db_data`].
pub fn record_db_cache_attempt(bypassed: bool) {
    *DB_CACHE_ATTEMPTS.lock().unwrap() += 1;
    if bypassed {
        *DB_CACHE_BYPASSED.lock().unwrap() += 1;
    }
}

/// Record that a dependency was actually served from `db.bin`. Call once per
/// dependency, only on the hit path.
pub fn record_db_cache_hit() {
    *DB_CACHE_HITS.lock().unwrap() += 1;
}

/// Production accessor for `telemetry.json`.
pub fn db_cache_attempts() -> u64 {
    *DB_CACHE_ATTEMPTS.lock().unwrap()
}

/// Production accessor for `telemetry.json`.
pub fn db_cache_hits() -> u64 {
    *DB_CACHE_HITS.lock().unwrap()
}

/// Production accessor for `telemetry.json`.
pub fn db_cache_bypassed() -> u64 {
    *DB_CACHE_BYPASSED.lock().unwrap()
}

/// Read the db file and return the data
/// # Returns
/// * `Vec<DBData>` - The data from the db file
///
/// This function fails silently if the db file cannot be decoded
pub fn read_db_file() -> anyhow::Result<Vec<DBData>> {
    if !Path::new(DB_FILE_NAME).exists() {
        fs::write(DB_FILE_NAME, Vec::new()).context("Failed to create db file")?;
    }
    let mut file = File::open(DB_FILE_NAME).context("Failed to open db file")?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)
        .context("Failed to read db file")?;

    if buffer.is_empty() {
        return Ok(Vec::new());
    }

    let (db_data, decoded) = match bincode::decode_from_slice(&buffer, config::standard()) {
        Ok(data) => data,
        Err(e) => {
            debug!("Failed to decode db file: {}", e);
            return Ok(Vec::new());
        }
    };
    debug!("Decoded {} bytes", decoded);

    Ok(db_data)
}

/// Write the db file with the given data
/// # Arguments
/// * `db_data` - The data to write to the db file
/// # Returns
/// * `Result<(), anyhow::Error>` - The result of writing the db file
///
/// This function fails silently if the data given cannot be encoded
pub fn write_db_file(db_data: Vec<DBData>) -> anyhow::Result<()> {
    debug!("Saving {:?} ", db_data);
    let buffer = match bincode::encode_to_vec(db_data, config::standard()) {
        Ok(data) => data,
        Err(e) => {
            debug!("Failed to encode db file: {}", e);
            return Ok(());
        }
    };
    debug!("Encoded {} bytes", buffer.len());
    std::fs::write(DB_FILE_NAME, buffer).context("Failed to write db file")?;
    Ok(())
}

/// Get the db data from the list of db data
/// # Arguments
/// * `db_data` - The list of db data
/// * `name` - The name to get from the db data
/// # Returns
/// * `Option<&DBData>` - The db data if found
///
/// This function returns an immutable reference to the db data if found
pub fn get_from_db_data<'a>(db_data: &'a [DBData], name: &str) -> Option<&'a DBData> {
    db_data
        .iter()
        .find(|dbdata| dbdata.name_with_version == name)
}

/// Add the given data to the list of db data
/// # Arguments
/// * `db_data` - The list of db data
/// * `name` - The name to add to the db data
/// * `features` - The features to add to the db data
pub fn add_to_db_data(
    db_data: &mut Vec<DBData>,
    name: &str,
    features: (&Vec<String>, &Vec<String>),
) {
    db_data.push(DBData {
        name_with_version: name.to_string(),
        features: (features.0.clone(), features.1.clone()),
    });
}

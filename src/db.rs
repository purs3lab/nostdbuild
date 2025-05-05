use anyhow::Context;
use bincode::config;
use log::debug;
use serde_json;
use std::{
    fs::{self, File},
    io::Read,
    path::Path,
};

use crate::{
    consts::{DB_FILE_NAME, RESULTS_JSON_PREFIX, RESULTS_JSON_SUFFIX},
    DBData, Results,
};

/// Read the db file and return the data
/// # Returns
/// * `Vec<DBData>` - The data from the db file
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
/// This function returns an immutable reference to the db data if found
pub fn get_from_db_data<'a>(db_data: &'a Vec<DBData>, name: &str) -> Option<&'a DBData> {
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

/// Write the final json file with the given name and results
/// # Arguments
/// * `name_with_version` - The name with version to write to the json file
/// * `results` - The results to write to the json file
pub fn write_final_json(name_with_version: &str, results: &Vec<Results>) {
    let json_file_name = format!(
        "{}{}{}",
        RESULTS_JSON_PREFIX, name_with_version, RESULTS_JSON_SUFFIX
    );

    if let Some(parent) = Path::new(&json_file_name).parent() {
        fs::create_dir_all(parent).unwrap();
    }

    let json_data = serde_json::to_string_pretty(&results).unwrap();
    fs::write(json_file_name, json_data).unwrap();
}

use anyhow::Context;
use bincode::config;
use log::debug;
use std::io::Read;

use crate::{consts::DB_FILE_NAME, DBData};

/// Read the db file and return the data
/// # Returns
/// * `Vec<DBData>` - The data from the db file
/// This function fails silently if the db file cannot be decoded
pub fn read_db_file() -> anyhow::Result<Vec<DBData>> {
    if !std::path::Path::new(DB_FILE_NAME).exists() {
        std::fs::write(DB_FILE_NAME, Vec::new()).context("Failed to create db file")?;
    }
    let mut file = std::fs::File::open(DB_FILE_NAME).context("Failed to open db file")?;
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

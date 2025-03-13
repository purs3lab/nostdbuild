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
    let mut file = std::fs::File::open(DB_FILE_NAME).context("Failed to open db file")?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)
        .context("Failed to read db file")?;
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

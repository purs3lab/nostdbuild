use crates_io_api::{Error, SyncClient};
use std::fs::File;
use std::io::{self, BufRead, Read, Write};
use std::path::Path;

fn get_downloads(client: &SyncClient, path: String) -> io::Result<Vec<u64>> {
    let file = File::open(path)?;
    let reader = io::BufReader::new(file);
    let mut downloads: Vec<u64> = vec![];
    let mut file = File::create("./downloads.txt")?;

    for line in reader.lines() {
        let line = line?; // Unwrap the Result for each line
        let line = line.trim_end(); // Remove trailing newline or whitespace

        // Split the version
        if let Some((package, version)) = split_package_version(line) {
            let res = client.get_crate(package);
            if res.is_err() {
                continue;
            }
            let res = res.unwrap();
            let ds = res.crate_data.downloads;
            downloads.push(ds);

            let mut str = package.to_string();
            str.push_str(" ");
            str.push_str(ds.to_string().as_str());
            str.push_str("\n");
            file.write(str.as_bytes());
        }
    }

    Ok(downloads)
}

fn main() {
    let base = "https://crates.io".to_string();

    let crates: Vec<(String, String)> = read_crates("./nostd_failed_build.txt").unwrap();
    dbg!(&crates);

    let client = SyncClient::new(
        "my-user-agent gnocera@purdue.edu",
        std::time::Duration::from_millis(10000),
    )
    .unwrap();

    let _ = get_downloads(&client, "./nostd_failed_build.txt".to_string());
    return;

    let mut file = File::create("./nostd_failed_build_urls_2.txt").unwrap();
    let mut results: Vec<String> = vec![];
    let mut i: i32 = 0;
    for (package, version) in crates {
        if i < 46 {
            i += 1;
            continue;
        }
        let res = client.get_crate(package.as_str());
        if res.is_err() {
            i += 1;
            continue;
        }
        let res = res.unwrap();
        let path: String = res
            .versions
            .into_iter()
            .filter(|x| x.num == version)
            .map(|x| x.dl_path)
            .collect::<Vec<String>>()[0]
            .clone();
        let mut res = base.clone();
        res.push_str(path.as_str());
        dbg!(&res);
        results.push(res.clone());
        dbg!(&path);
        res.push_str("\n");
        file.write_all(res.as_bytes()).unwrap();
        i += 1;
    }
    let final_string = results.join("\n");
    let mut file = File::create("./no_std_failed_build_urls.txt").unwrap();
    file.write_all(final_string.as_bytes()).unwrap();
}

fn read_crates(path: &str) -> io::Result<Vec<(String, String)>> {
    let file = File::open(path)?;
    let reader = io::BufReader::new(file);
    let mut packages = Vec::new();

    for line in reader.lines() {
        let line = line?; // Unwrap the Result for each line
        let line = line.trim_end(); // Remove trailing newline or whitespace

        // Split the version
        if let Some((package, version)) = split_package_version(line) {
            packages.push((package.to_string(), version.to_string()));
        }
    }

    Ok(packages)
}

// Helper function to split a package string into name and version
fn split_package_version(package: &str) -> Option<(&str, &str)> {
    if let Some(index) = package.rfind('-') {
        let (name, version) = package.split_at(index);
        Some((name, &version[1..])) // Skip the leading '-'
    } else {
        None
    }
}

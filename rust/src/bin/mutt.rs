// MIT License

// Copyright (c) 2021 David Pollak

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

use chrono::{DateTime, Utc};
use clap::{App, Arg};
use mutt::party::{AssetType, PartyType};
use mutt::transaction::Transaction;
use mutt::world::World;
use serde_json::{json, to_string_pretty, Deserializer};
use std::io::{BufRead, BufReader, Read};
use std::sync::Arc;
#[tokio::main]
pub async fn main() {
    let input_name = "Input Name";
    let extra_defaults = "Extra Defaults";
    let defaults_file = "Defaults File";
    let start_time = "Start Time";

    let matches = App::new("mutt")
        .version("0.1.0")
        .author("David Pollak <feeder.of.the.bears@gmail.com>")
        .about("An economy simulator")
        .arg(
            Arg::with_name(start_time)
                .long("start-time")
                .takes_value(true)
                .help("The Beginning time for the world")
                .validator(validate_date),
        )
        .arg(
            Arg::with_name(input_name)
                .long("event-source")
                .value_name("File Name or '^^' ")
                .help("Define the input for transactions... either a filename or `^^` for STDIN")
                .validator(validate_file)
                .takes_value(true),
        )
        .arg(
            Arg::with_name(extra_defaults)
                .long("extra-defaults")
                .help("When building world, use defaults from test suite"),
        )
        .arg(
            Arg::with_name(defaults_file)
                .long("party-defaults-file")
                .help("The name of the file with party defaults")
                .takes_value(true)
                .validator(validate_file),
        )
        .get_matches();

    let mut defaults = read_defaults(matches.value_of(defaults_file)).clone();

    if matches.is_present(extra_defaults) {
        let mut v2 = World::get_test_party_stuff();
        defaults.append(&mut v2);
    }
    let start_date = matches
        .value_of(start_time)
        .map(|v| DateTime::parse_from_rfc3339(v).unwrap().with_timezone(&Utc));
    let w = World::new_with_preload(start_date, defaults).await;

    if matches.is_present(input_name) {
        let rr: Box<dyn BufRead> = match matches.value_of(input_name) as Option<&str> {
            Some("^^") => Box::new(BufReader::new(std::io::stdin())),
            Some(name) => Box::new(BufReader::new(std::fs::File::open(name).unwrap())),
            None => panic!(),
        };

        process_json_stream(rr, &w).await.unwrap();
    }

    let balance = w.generate_balance_sheet().await;
    println!(
        "Balance Sheet:\n{}",
        to_string_pretty(&json!(balance)).unwrap()
    );
}

fn read_defaults(file_name: Option<&str>) -> Vec<(String, PartyType, Vec<AssetType>)> {
    match file_name {
        None => vec![],
        Some(the_name) => {
            let p = std::path::Path::new(the_name);
            std::fs::File::open(p)
                .map(|f| serde_json::from_reader(f))
                .unwrap()
                .unwrap()
        }
    }
}

fn validate_date(date: String) -> Result<(), String> {
    match DateTime::parse_from_rfc3339(&date) {
        Ok(_) => Ok(()),
        Err(e) => Err(format!("{}", e)),
    }
}

fn validate_file(name: String) -> Result<(), String> {
    let st: &str = &name;
    match st {
        "^^" => Ok(()),
        file_name if std::path::Path::new(file_name).exists() => Ok(()),
        _ => Err(format!("File {} does not exist", name)),
    }
}

pub async fn process_json_stream<R: Read>(reader: R, world: &Arc<World>) -> Result<(), String> {
    let stream = Deserializer::from_reader(reader).into_iter::<Transaction>();

    let mut to_wait = Vec::new();
    for v in stream {
        let v = v.map_err(|se| format!("Serde Error {}", se))?;
        println!(
            "Processing Transaction:\n{}",
            to_string_pretty(&json!(v)).unwrap()
        );
        let w2 = world.clone();
        to_wait.push(tokio::spawn(async move {
            w2.process_transaction(&v).await.unwrap();
        }));
    }

    // wait for completion
    for v in to_wait {
        v.await.unwrap();
    }

    Ok(())
}

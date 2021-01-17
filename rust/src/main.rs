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

#[macro_use]
extern crate lazy_static;
extern crate serde_derive;

pub mod misc;
pub mod party;
pub mod transaction;
pub mod world;

use crate::transaction::Transaction;
use crate::world::World;
use serde_json::{json, to_string_pretty, Deserializer};
use std::io::{BufReader, Read};
use std::sync::Arc;

#[tokio::main]
pub async fn main() {
    let w = Arc::new(World::new_with_preload(World::get_test_party_stuff()));
    process_json_stream(BufReader::new(std::io::stdin()), &w)
        .await
        .unwrap();

    let balance = w.generate_balance_sheet();
    println!(
        "Balance Sheet:\n{}",
        to_string_pretty(&json!(balance)).unwrap()
    );
}

pub async fn process_json_stream<R: Read>(
    reader: BufReader<R>,
    world: &Arc<World>,
) -> Result<(), String> {
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

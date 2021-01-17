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

use chrono::{DateTime, FixedOffset, Utc};
use fixed::{types::extra::U64, FixedI128};
use im::Vector;
use uuid::Uuid;

pub struct Misc {
    _cant_create_one: String,
}

pub type FixedNum = FixedI128<U64>;

impl Misc {
    /// Create a stable UUID given a string
    pub fn compute_uuid_for(string: &str) -> Uuid {
        Uuid::new_v5(&Uuid::NAMESPACE_DNS, string.as_bytes())
    }

    pub fn append<T: Clone>(src: &Vector<T>, other: T) -> Vector<T> {
        let mut sc = src.clone();
        sc.push_back(other);
        sc
    }

    pub fn now() -> DateTime<FixedOffset> {
        DateTime::from(Utc::now())
    }
}

#[test]
fn test_numbers() {
    let a = FixedNum::from_num(22.2);
    let b = FixedNum::from_num(1) / FixedNum::from_num(2);
    println!("A is {} and b {}", a, b);
    assert_eq!(a * b, FixedNum::from_num(11.1));
}

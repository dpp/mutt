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
use rlua::Error as LuaError;
use serde::{Deserialize, Serialize};
use std::error::Error;
use std::fmt::{Debug, Display, Formatter, Result as FResult};
use std::ops::{Add, Div, Mul, Sub};
use tokio::sync::mpsc::error::SendError;
use tokio::sync::oneshot::error::RecvError;
use uuid::Uuid;
pub struct Misc {
    _cant_create_one: String,
}
pub trait GenericError: Error + Sync + Send {}
#[derive(Debug)]
struct ErrorWrapper<T: Display + Debug + Sync + Send> {
    v: T,
}
impl<T: Display + Debug + Sync + Send> GenericError for ErrorWrapper<T> {}
impl<T: Display + Debug + Sync + Send> Error for ErrorWrapper<T> {}

//impl<T> Error for ErrorWrapper<T> {}
impl<T: Display + Debug + Sync + Send> Display for ErrorWrapper<T> {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> FResult {
        write!(fmt, "Error Wrapper {}", self.v)
    }
}
impl<T: Debug + Sync + Send + 'static> From<SendError<T>> for Box<dyn GenericError> {
    fn from(x: SendError<T>) -> Self {
        Box::new(ErrorWrapper { v: x })
    }
}

impl From<RecvError> for Box<dyn GenericError> {
    fn from(x: RecvError) -> Self {
        Box::new(ErrorWrapper { v: x })
    }
}

impl From<serde_json::Error> for Box<dyn GenericError> {
    fn from(x: serde_json::Error) -> Self {
        Box::new(ErrorWrapper { v: x })
    }
}

impl From<String> for Box<dyn GenericError> {
    fn from(x: String) -> Self {
        // let _z: ErrorWrapper<dyn Error + Sync + Send> = ErrorWrapper {
        //     v: Box::new(x.into()),
        // };
        Box::new(ErrorWrapper { v: x })
    }
}

impl From<Box<dyn Error + Send + Sync>> for Box<dyn GenericError> {
    fn from(_x: Box<dyn Error + Send + Sync>) -> Self {
        Box::new(ErrorWrapper { v: Box::new(_x) })
    }
}

impl From<LuaError> for Box<dyn GenericError> {
    fn from(x: LuaError) -> Self {
        Box::new(ErrorWrapper { v: x })
    }
}

pub type MResult<A> = Result<A, Box<dyn GenericError>>;
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
pub struct FixedNum(FixedI128<U64>);
impl From<FixedI128<U64>> for FixedNum {
    fn from(v: FixedI128<U64>) -> Self {
        FixedNum(v)
    }
}
impl From<FixedNum> for FixedI128<U64> {
    fn from(v: FixedNum) -> Self {
        v.0
    }
}
impl From<isize> for FixedNum {
    fn from(v: isize) -> Self {
        FixedNum(FixedI128::from_num(v))
    }
}
impl From<f32> for FixedNum {
    fn from(v: f32) -> Self {
        FixedNum(FixedI128::from_num(v))
    }
}
impl From<f64> for FixedNum {
    fn from(v: f64) -> Self {
        FixedNum(FixedI128::from_num(v))
    }
}
impl Display for FixedNum {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> FResult {
        write!(fmt, "{}", self.0)
    }
}
impl Sub for FixedNum {
    type Output = FixedNum;
    fn sub(self, rhs: FixedNum) -> Self::Output {
        FixedNum(self.0 - rhs.0)
    }
}
impl Mul for FixedNum {
    type Output = FixedNum;
    fn mul(self, rhs: FixedNum) -> Self::Output {
        FixedNum(self.0 * rhs.0)
    }
}
impl Add for FixedNum {
    type Output = FixedNum;
    fn add(self, rhs: FixedNum) -> Self::Output {
        FixedNum(self.0 + rhs.0)
    }
}
impl Div for FixedNum {
    type Output = FixedNum;
    fn div(self, rhs: FixedNum) -> Self::Output {
        FixedNum(self.0 / rhs.0)
    }
}
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

/// Log Error
///
/// If the parameter is an error, log it and return `None`,
/// otherwise return `Some`

#[macro_export]
macro_rules! log_error {
    ( $input:expr ) => {
        use log::error;
        match $input {
            Ok(v) => Some(v),
            Err(e) => {
                error!("file: {} line: {} error: {:?}", file!(), line!(), e);
                None
            }
        }
    };
}

#[test]
fn test_numbers() {
    let a: FixedNum = 22.2.into();
    let one: FixedNum = 1.into();
    let b: FixedNum = one / 2.into();
    println!("A is {} and b {}", a, b);
    assert_eq!(a * b, 11.1.into());
}

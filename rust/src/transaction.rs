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

use crate::misc::Misc;
use crate::party::AssetType;
use chrono::{DateTime, FixedOffset};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Transaction {
    #[serde(default)]
    pub id: Option<Uuid>,
    pub description: String,
    pub from: AssetType,
    pub to: AssetType,
    pub from_party: Uuid,
    pub to_party: Uuid,
    #[serde(with = "format_optional_dates")]
    #[serde(default)]
    pub when: Option<DateTime<FixedOffset>>,
}

impl Transaction {
    /// If date and/or id is missing insert them
    pub fn fix_date_and_id(&self) -> Transaction {
        Transaction {
            id: match self.id {
                None => Some(Uuid::new_v4()),
                s => s,
            },
            description: self.description.clone(),
            from: self.from.clone(),
            to: self.to.clone(),
            from_party: self.from_party,
            to_party: self.to_party,
            when: match self.when {
                None => Some(Misc::now()),
                s => s,
            },
        }
    }
}

// From https://serde.rs/custom-date-format.html
mod format_optional_dates {
    use chrono::{DateTime, FixedOffset};
    use serde::{self, Deserialize, Deserializer, Serializer};

    // The signature of a serialize_with function must follow the pattern:
    //
    //    fn serialize<S>(&T, S) -> Result<S::Ok, S::Error>
    //    where
    //        S: Serializer
    //
    // although it may also be generic over the input types T.
    pub fn serialize<S>(
        date: &Option<DateTime<FixedOffset>>,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match date {
            None => serializer.serialize_none(),
            Some(dt) => serializer.serialize_str(&dt.to_rfc3339()),
        }
    }

    // The signature of a deserialize_with function must follow the pattern:
    //
    //    fn deserialize<'de, D>(D) -> Result<T, D::Error>
    //    where
    //        D: Deserializer<'de>
    //
    // although it may also be generic over the output types T.
    pub fn deserialize<'de, D>(deserializer: D) -> Result<Option<DateTime<FixedOffset>>, D::Error>
    where
        D: Deserializer<'de>,
    {
        match String::deserialize(deserializer) {
            Err(_) => Ok(None),
            Ok(s) => {
                let ps: DateTime<FixedOffset> =
                    DateTime::parse_from_rfc3339(&s).map_err(serde::de::Error::custom)?;
                Ok(Some(ps))
            }
        }
    }
}

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
use crate::misc::{FixedNum, MResult, Misc};
use crate::party::{AssetType, AssetTypeIdentifier, Party};
use crate::transaction::Transaction;

use chrono::{DateTime, Utc};
use im::{HashMap, Vector};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::sync::Arc;
use uuid::Uuid;
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PartySnapshot {
    pub id: Uuid,
    pub name: String,
    pub state: PartyState,
    pub now: DateTime<Utc>,
}

pub type ArcParty = Arc<Party>;
pub type PartyAssets = HashMap<AssetTypeIdentifier, AssetType>;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct PartyState {
    pub cash_balance: FixedNum,
    pub hash: Vec<u8>,
    pub transactions: Vector<Arc<Transaction>>,
    pub assets: PartyAssets,
}

impl PartyState {
    pub fn new() -> PartyState {
        PartyState {
            cash_balance: FixedNum::from(0),
            hash: vec![],
            transactions: Vector::new(),
            assets: HashMap::new(),
        }
    }

    pub fn new_with_baseline(baseline: &Vec<AssetType>) -> PartyState {
        let cash = baseline.iter().fold(FixedNum::from(0), |acc, v| match v {
            AssetType::USD(x) => acc + *x,
            _ => acc,
        });

        let a: PartyAssets = baseline.iter().fold(HashMap::new(), |acc, at| {
            let asset_type_id = at.to_identifier();
            match acc.get(&asset_type_id) {
                Some(other_asset) => match other_asset.combine_with(at, false) {
                    Some(na) => acc.update(asset_type_id, na),
                    _ => acc,
                },
                None => acc.update(asset_type_id, at.clone()),
            }
        });

        PartyState {
            cash_balance: cash,
            hash: vec![],
            transactions: Vector::new(),
            assets: a,
        }
    }

    pub fn process(
        &self,
        xa: &Arc<Transaction>,
        mine: &AssetType,
        theirs: &AssetType,
    ) -> MResult<PartyState> {
        self.process_or_rollback(xa, mine, theirs, false)
    }

    pub fn rollback(
        &self,
        xa: &Arc<Transaction>,
        mine: &AssetType,
        theirs: &AssetType,
    ) -> MResult<PartyState> {
        self.process_or_rollback(xa, mine, theirs, true)
    }

    fn process_or_rollback(
        &self,
        xa: &Arc<Transaction>,
        mine: &AssetType,
        theirs: &AssetType,
        rollback: bool,
    ) -> MResult<PartyState> {
        let new_xa = if rollback {
            let mut v2 = Vector::new();
            // remove the offending transaction
            for i in self.transactions.iter() {
                if i.id != xa.id {
                    v2.push_back(i.clone());
                }
            }
            v2
        } else {
            Misc::append(&self.transactions, xa.clone())
        };

        let new_hash = if rollback {
            PartyState::build_new_hash(&new_xa)
        } else {
            PartyState::update_hash(&self.hash, xa)
        };

        fn fix_cash_balance(current: FixedNum, at: &AssetType, rollback: bool) -> FixedNum {
            match at {
                AssetType::USD(amount) => {
                    if rollback {
                        current - *amount
                    } else {
                        current + *amount
                    }
                }
                _ => current,
            }
        }
        let new_cash_balance = fix_cash_balance(
            fix_cash_balance(self.cash_balance, mine, rollback),
            theirs,
            !rollback,
        );
        fn fix_assets(
            assets: &PartyAssets,
            at: &AssetType,
            rollback: bool,
        ) -> MResult<PartyAssets> {
            let asset_type_id = at.to_identifier();
            match assets.get(&asset_type_id) {
                Some(other_asset) => match other_asset.combine_with(at, rollback) {
                    Some(na) => Ok(assets.update(asset_type_id, na)),
                    _ => Err(format!(
                        "Unable to combine asset {:?} with new asset {:?}",
                        other_asset, at
                    )
                    .into()),
                },
                None if !rollback => Ok(assets.update(asset_type_id, at.clone())),
                None => Ok(assets.update(asset_type_id, at.negative())),
            }
        }
        let new_assets = match fix_assets(&self.assets, mine, rollback) {
            Ok(na) => match fix_assets(&na, theirs, !rollback) {
                Ok(na) => na,
                Err(msg) => return Err(msg),
            },
            Err(msg) => return Err(msg),
        };

        Ok(PartyState {
            cash_balance: new_cash_balance,
            hash: new_hash,
            transactions: new_xa,
            assets: new_assets,
        })
    }

    pub fn build_new_hash(xa: &Vector<Arc<Transaction>>) -> Vec<u8> {
        let mut v = vec![];
        for i in xa {
            v = PartyState::update_hash(&v, i);
        }
        v
    }

    pub fn update_hash(old_hash: &Vec<u8>, xa: &Transaction) -> Vec<u8> {
        let this_one = format!("{:?}", xa);
        let mut hasher = Sha256::new();
        hasher.update(old_hash);
        hasher.update(this_one);
        hasher.finalize().to_vec()
    }
}

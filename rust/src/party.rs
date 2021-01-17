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

use crate::misc::{FixedNum, Misc};
use crate::transaction::Transaction;

use arc_swap::ArcSwap;
use chrono::{DateTime, Duration, Utc};
use im::{HashMap, Vector};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::sync::Arc;
use uuid::Uuid;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum PartyType {
    CurrencyIssuer = 1,
    CurrencyUser = 2,
    ForeignCurrencyIssuer = 3,
    ForeignCurrencyUser = 4,
}
#[derive(Debug, Eq, PartialEq, Clone, Hash, Copy)]
pub struct Loan {
    amount_usd: FixedNum,
    id: Uuid,
    to: Uuid,
    interest_per_period: FixedNum,
    periods: usize,
    initial_date: DateTime<Utc>,
    period_duration: Duration,
    last_serviced: DateTime<Utc>,
    balance_usd: FixedNum,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum AssetType {
    Labor(FixedNum),
    USD(FixedNum),
    LoanBalance(FixedNum),
    Food(FixedNum),
    Materials(FixedNum), // Loans(HashMap<Uuid, Loan>),
}
impl AssetType {
    pub fn to_identifier(&self) -> AssetTypeIdentifier {
        AssetTypeIdentifier::from_asset_type(self)
    }

    pub fn negative(&self) -> AssetType {
        let n1 = FixedNum::from(-1);
        match self {
            &AssetType::Labor(x) => AssetType::Labor(x * n1),
            &AssetType::USD(x) => AssetType::USD(x * n1),
            &AssetType::LoanBalance(x) => AssetType::LoanBalance(x * n1),
            &AssetType::Food(x) => AssetType::Food(x * n1),
            &AssetType::Materials(x) => AssetType::Materials(x * n1),
        }
    }

    pub fn combine_with(&self, other: &AssetType, rollback: bool) -> Option<AssetType> {
        match (self, other) {
            (&AssetType::Labor(a), &AssetType::Labor(b)) => {
                Some(AssetType::Labor(if rollback { a - b } else { a + b }))
            }
            (&AssetType::USD(a), &AssetType::USD(b)) => {
                Some(AssetType::USD(if rollback { a - b } else { a + b }))
            }
            (&AssetType::LoanBalance(a), &AssetType::LoanBalance(b)) => {
                Some(AssetType::LoanBalance(if rollback { a - b } else { a + b }))
            }
            (&AssetType::Food(a), &AssetType::Food(b)) => {
                Some(AssetType::Food(if rollback { a - b } else { a + b }))
            }
            (&AssetType::Materials(a), &AssetType::Materials(b)) => {
                Some(AssetType::Materials(if rollback { a - b } else { a + b }))
            }
            _ => None,
        }
    }

    /// Is this the special case of asset types
    pub fn is_usd(&self) -> bool {
        match self {
            AssetType::USD(_) => true,
            _ => false,
        }
    }
}
#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub enum AssetTypeIdentifier {
    Labor = 1,
    USD,
    LoanBalance,
    Materials,
    Food,
}

impl AssetTypeIdentifier {
    /// Convert an AssetType into an AssetTypeIdentifier
    pub fn from_asset_type(asset_type: &AssetType) -> AssetTypeIdentifier {
        match asset_type {
            &AssetType::Labor(_) => AssetTypeIdentifier::Labor,
            &AssetType::USD(_) => AssetTypeIdentifier::USD,
            &AssetType::LoanBalance(_) => AssetTypeIdentifier::LoanBalance,
            &AssetType::Materials(_) => AssetTypeIdentifier::Materials,
            &AssetType::Food(_) => AssetTypeIdentifier::Food, // &AssetType::Loans(_) => 3,
        }
    }
}

pub type ArcParty = Arc<Party>;
pub type PartyAssets = HashMap<AssetTypeIdentifier, AssetType>;

#[derive(Debug, Clone, PartialEq)]
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

    pub fn new_with_baseline(baseline: &Vec<AssetType>) -> ArcSwap<PartyState> {
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

        ArcSwap::new(Arc::new(PartyState {
            cash_balance: cash,
            hash: vec![],
            transactions: Vector::new(),
            assets: a,
        }))
    }

    pub fn new_swap() -> ArcSwap<PartyState> {
        ArcSwap::new(Arc::new(PartyState::new()))
    }

    pub fn process(
        &self,
        xa: &Arc<Transaction>,
        mine: &AssetType,
        theirs: &AssetType,
    ) -> Result<PartyState, String> {
        self.process_or_rollback(xa, mine, theirs, false)
    }

    pub fn rollback(
        &self,
        xa: &Arc<Transaction>,
        mine: &AssetType,
        theirs: &AssetType,
    ) -> Result<PartyState, String> {
        self.process_or_rollback(xa, mine, theirs, true)
    }

    fn process_or_rollback(
        &self,
        xa: &Arc<Transaction>,
        mine: &AssetType,
        theirs: &AssetType,
        rollback: bool,
    ) -> Result<PartyState, String> {
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
                        current - amount
                    } else {
                        current + amount
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
        ) -> Result<PartyAssets, String> {
            let asset_type_id = at.to_identifier();
            match assets.get(&asset_type_id) {
                Some(other_asset) => match other_asset.combine_with(at, rollback) {
                    Some(na) => Ok(assets.update(asset_type_id, na)),
                    _ => Err(format!(
                        "Unable to combine asset {:?} with new asset {:?}",
                        other_asset, at
                    )),
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

#[derive(Debug)]
pub struct Party {
    pub id: Uuid,
    pub name: String,
    pub state: ArcSwap<PartyState>,
    pub party_type: PartyType,
}

impl Party {
    pub fn get_cash_balance(&self) -> FixedNum {
        self.state.load().cash_balance
    }

    pub fn get_asset(&self, assert_type: AssetTypeIdentifier) -> Option<AssetType> {
        self.state
            .load()
            .assets
            .get(&assert_type)
            .map(|v| v.clone())
    }

    pub async fn process(
        &self,
        xa: &Arc<Transaction>,
        mine: &AssetType,
        theirs: &AssetType,
    ) -> Result<(), String> {
        let mut error_option: Option<Result<(), String>> = None;
        self.state.rcu(|st| match st.process(xa, mine, theirs) {
            Ok(new) => new,
            e => {
                error_option = Some(match e {
                    Ok(_) => Ok(()),
                    Err(msg) => Err(msg),
                });
                let v: &PartyState = st;
                v.clone()
            }
        });
        match error_option {
            Some(r) => return r,
            _ => (),
        }
        Ok(())
    }

    pub async fn rollback(
        &self,
        xa: &Arc<Transaction>,
        mine: &AssetType,
        theirs: &AssetType,
    ) -> Result<(), String> {
        let mut error_option: Option<Result<(), String>> = None;
        self.state.rcu(|st| match st.rollback(xa, mine, theirs) {
            Ok(new) => new,
            e => {
                error_option = Some(match e {
                    Ok(_) => Ok(()),
                    Err(msg) => Err(msg),
                });
                let v: &PartyState = st;
                v.clone()
            }
        });
        match error_option {
            Some(r) => return r,
            _ => (),
        }
        Ok(())
    }
    pub fn new(name: &str, party_type: PartyType) -> Party {
        Party {
            id: Uuid::new_v4(),
            name: name.to_string(),
            party_type: party_type,
            state: PartyState::new_swap(),
        }
    }

    pub fn new_entity(name: &str, baseline: &Vec<AssetType>) -> Party {
        Party {
            id: Misc::compute_uuid_for(name),
            name: name.to_string(),
            party_type: PartyType::CurrencyUser,
            state: PartyState::new_with_baseline(baseline),
        }
    }
    pub fn new_issuer(name: &str) -> Party {
        Party {
            id: Misc::compute_uuid_for(name),
            name: name.to_string(),
            party_type: PartyType::CurrencyIssuer,
            state: PartyState::new_swap(),
        }
    }
}
impl Clone for Party {
    fn clone(&self) -> Party {
        Party {
            id: self.id.clone(),
            name: self.name.clone(),
            party_type: self.party_type.clone(),
            state: ArcSwap::new(self.state.load().clone()),
        }
    }
}
impl PartialEq for Party {
    fn eq(&self, other: &Party) -> bool {
        self.id == other.id && self.name == other.name && self.party_type == other.party_type && {
            let me: &PartyState = &self.state.load();
            let them: &PartyState = &other.state.load();
            me == them
        }
    }
}

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
use crate::world::World;

use async_trait::async_trait;
use chrono::{DateTime, Duration, Utc};
use im::{HashMap, Vector};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::sync::Arc;
use tokio::sync::mpsc::{channel as mpsc_channel, Receiver as MPSCReceiver, Sender as MPSCSender};
use tokio::sync::oneshot::{channel as one_channel, Sender as OneShotSender};
use uuid::Uuid;

#[derive(Debug, Clone)]
pub struct PartySnapshot {
    pub id: Uuid,
    pub name: String,
    pub state: PartyState,
}

pub enum PartyMessage {
    Snapshot(OneShotSender<PartySnapshot>),
    Process {
        xa: Arc<Transaction>,
        mine: AssetType,
        theirs: AssetType,
        rollback: bool,
        response_chan: OneShotSender<Result<(), String>>,
    },
}

#[async_trait]
pub trait Processor {
    async fn get_state(&self) -> Result<PartyState, String> {
        let (tx, rx) = one_channel();
        self.get_channel()
            .send(PartyMessage::Snapshot(tx))
            .await
            .map_err(|v| format!("{:?}", v))?;
        rx.await.map(|v| v.state).map_err(|v| format!("{:?}", v))
    }

    async fn get_cash_balance(&self) -> Result<FixedNum, String> {
        self.get_state().await.map(|v| v.cash_balance)
    }

    async fn get_asset(
        &self,
        asset_type: AssetTypeIdentifier,
    ) -> Result<Option<AssetType>, String> {
        self.get_state()
            .await
            .map(|v| v.assets.get(&asset_type).map(|v| v.clone()))
    }
    async fn process(
        &self,
        xa: &Arc<Transaction>,
        mine: &AssetType,
        theirs: &AssetType,
    ) -> Result<(), String> {
        let (tx, rx) = one_channel();
        let msg = PartyMessage::Process {
            xa: xa.clone(),
            mine: mine.clone(),
            theirs: theirs.clone(),
            rollback: false,
            response_chan: tx,
        };
        Misc::fe(self.get_channel().send(msg).await)?;

        match Misc::fe(rx.await) {
            Ok(msg) => msg,
            Err(st) => Err(st),
        }
    }

    fn get_channel(&self) -> MPSCSender<PartyMessage>;

    async fn rollback(
        &self,
        xa: &Arc<Transaction>,
        mine: &AssetType,
        theirs: &AssetType,
    ) -> Result<(), String> {
        let (tx, rx) = one_channel();
        let msg = PartyMessage::Process {
            xa: xa.clone(),
            mine: mine.clone(),
            theirs: theirs.clone(),
            rollback: true,
            response_chan: tx,
        };
        Misc::fe(self.get_channel().send(msg).await)?;

        let resp: Result<Result<(), String>, String> = Misc::fe(rx.await);
        match resp {
            Ok(msg) => msg,
            Err(st) => Err(st),
        }
    }
}
#[derive(Debug, Clone)]
pub struct PartyProxy {
    pub id: Uuid,
    pub channel: MPSCSender<PartyMessage>,
}

impl Processor for PartyProxy {
    fn get_channel(&self) -> MPSCSender<PartyMessage> {
        self.channel.clone()
    }
}

impl PartyProxy {}
impl core::fmt::Debug for PartyMessage {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            PartyMessage::Snapshot(_) => f.write_str("Snapshot()"),
            PartyMessage::Process {
                xa,
                mine,
                theirs,
                rollback: rb,
                response_chan: _,
            } => f
                .debug_struct("Process")
                .field("xa", &xa)
                .field("mine", &mine)
                .field("theirs", &theirs)
                .field("rollback", rb)
                .finish(),
        }
    }
}

impl core::fmt::Display for PartyMessage {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            PartyMessage::Snapshot(_) => f.write_str("Snapshot"),
            PartyMessage::Process {
                xa,
                mine,
                theirs,
                rollback: rb,
                response_chan: _,
            } => f
                .debug_struct("Process")
                .field("xa", &xa)
                .field("mine", &mine)
                .field("theirs", &theirs)
                .field("rollback", rb)
                .finish(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum PartyType {
    CurrencyIssuer = 1,
    CurrencyUser = 2,
    ForeignCurrencyIssuer = 3,
    ForeignCurrencyUser = 4,
}
#[derive(Debug, Eq, PartialEq, Clone, Hash, Copy)]
pub struct Loan {
    pub amount_usd: FixedNum,
    pub id: Uuid,
    pub to: Uuid,
    pub interest_per_period: FixedNum,
    pub periods: usize,
    pub initial_date: DateTime<Utc>,
    pub period_duration: Duration,
    pub last_serviced: DateTime<Utc>,
    pub balance_usd: FixedNum,
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
    // pub state: ArcSwap<PartyState>,
    channel: MPSCSender<PartyMessage>,
    pub party_type: PartyType,
}

impl Processor for Party {
    fn get_channel(&self) -> MPSCSender<PartyMessage> {
        self.channel.clone()
    }
}

impl Party {
    pub fn get_message_chan(&self) -> MPSCSender<PartyMessage> {
        self.channel.clone()
    }

    fn create_channels() -> (MPSCSender<PartyMessage>, MPSCReceiver<PartyMessage>) {
        mpsc_channel(100)
    }

    async fn start_loop(
        _world: Arc<World>,
        myself: Arc<Party>,
        initial_state: PartyState,
        mut rx: MPSCReceiver<PartyMessage>,
    ) {
        let sc = myself.clone();
        tokio::spawn(async move {
            let sp: &Party = &sc;
            let mut snapshot = initial_state;

            fn log_error<T, V: std::fmt::Debug>(_result: Result<T, V>) {
                // FIXME log error
            }

            let build_snapshot = |ss: &PartyState| PartySnapshot {
                id: sp.id.clone(),
                name: sp.name.clone(),
                state: ss.clone(),
            };

            loop {
                match rx.recv().await {
                    None => break,
                    Some(PartyMessage::Snapshot(reply)) => {
                        log_error(reply.send(build_snapshot(&snapshot)));
                    }
                    Some(PartyMessage::Process {
                        xa,
                        mine,
                        theirs,
                        rollback,
                        response_chan,
                    }) => {
                        match if rollback {
                            snapshot.rollback(&xa, &mine, &theirs)
                        } else {
                            snapshot.process(&xa, &mine, &theirs)
                        } {
                            Ok(new) => {
                                log_error(response_chan.send(Ok(())));
                                snapshot = new.clone();
                            }
                            Err(e) => {
                                log_error(response_chan.send(Err(e)));
                            }
                        }
                    }
                }
            }
        });
    }
    pub async fn new(name: &str, party_type: PartyType, world: Arc<World>) -> Arc<Party> {
        let (tx, rx) = Party::create_channels();
        let ap = Arc::new(Party {
            id: Uuid::new_v4(),
            name: name.to_string(),
            party_type: party_type,
            channel: tx,
        });

        Party::start_loop(world, ap.clone(), PartyState::new(), rx).await;

        ap
    }

    pub async fn new_entity(
        name: &str,
        baseline: &Vec<AssetType>,
        world: Arc<World>,
    ) -> Arc<Party> {
        let (tx, rx) = Party::create_channels();
        let ap = Arc::new(Party {
            id: Misc::compute_uuid_for(name),
            name: name.to_string(),
            party_type: PartyType::CurrencyUser,
            channel: tx,
        });

        Party::start_loop(
            world,
            ap.clone(),
            PartyState::new_with_baseline(baseline),
            rx,
        )
        .await;

        ap
    }
    pub async fn new_issuer(name: &str, world: Arc<World>) -> Arc<Party> {
        let (tx, rx) = Party::create_channels();
        let ap = Arc::new(Party {
            id: Misc::compute_uuid_for(name),
            name: name.to_string(),
            party_type: PartyType::CurrencyIssuer,
            channel: tx,
        });

        Party::start_loop(world, ap.clone(), PartyState::new(), rx).await;

        ap
    }
}

impl PartialEq for Party {
    fn eq(&self, other: &Party) -> bool {
        self.id == other.id && self.name == other.name && self.party_type == other.party_type
    }
}

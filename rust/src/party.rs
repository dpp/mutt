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

use crate::log_error;
use crate::lua_env::create_lua_runtime;
use crate::misc::{FixedNum, MResult, Misc};
use crate::party_func::TickFunc;
use crate::party_state::{PartySnapshot, PartyState};
use crate::transaction::Transaction;
use crate::world::World;

use async_trait::async_trait;
use chrono::{DateTime, Duration, Utc};
use derivative::Derivative;
use im::{hashmap, Vector};
use rlua::Lua;
use serde::{Deserialize, Serialize};
use std::sync::{Arc, RwLock};
use tokio::sync::mpsc::{channel as mpsc_channel, Receiver as MPSCReceiver, Sender as MPSCSender};
use tokio::sync::oneshot::{channel as one_channel, Sender as OneShotSender};
use uuid::Uuid;

pub enum PartyMessage {
    Snapshot(OneShotSender<PartySnapshot>),
    Process {
        xa: Arc<Transaction>,
        mine: AssetType,
        theirs: AssetType,
        rollback: bool,
        response_chan: OneShotSender<MResult<()>>,
    },
    Tick(DateTime<Utc>, MPSCSender<PartySnapshot>),
}

#[async_trait]
pub trait Processor {
    async fn get_state(&self) -> MResult<PartyState> {
        let (tx, rx) = one_channel();
        self.get_channel().send(PartyMessage::Snapshot(tx)).await?;
        rx.await.map(|v| v.state).map_err(|e| e.into())
    }

    async fn get_cash_balance(&self) -> MResult<FixedNum> {
        self.get_state().await.map(|v| v.cash_balance)
    }

    async fn get_asset(&self, asset_type: AssetTypeIdentifier) -> MResult<Option<AssetType>> {
        self.get_state()
            .await
            .map(|v| v.assets.get(&asset_type).map(|v| v.clone()))
    }
    async fn process(
        &self,
        xa: &Arc<Transaction>,
        mine: &AssetType,
        theirs: &AssetType,
    ) -> MResult<()> {
        let (tx, rx) = one_channel();
        let msg = PartyMessage::Process {
            xa: xa.clone(),
            mine: mine.clone(),
            theirs: theirs.clone(),
            rollback: false,
            response_chan: tx,
        };
        self.get_channel().send(msg).await?;

        rx.await?
    }

    fn get_channel(&self) -> MPSCSender<PartyMessage>;

    async fn rollback(
        &self,
        xa: &Arc<Transaction>,
        mine: &AssetType,
        theirs: &AssetType,
    ) -> MResult<()> {
        let (tx, rx) = one_channel();
        let msg = PartyMessage::Process {
            xa: xa.clone(),
            mine: mine.clone(),
            theirs: theirs.clone(),
            rollback: true,
            response_chan: tx,
        };
        self.get_channel().send(msg).await?;

        let resp = rx.await.map(|v| v.map_err(|e| e.into()));
        match resp {
            Ok(msg) => msg,
            Err(st) => Err(st.into()),
        }
    }
}
#[derive(Debug, Clone)]
pub struct PartyProxy {
    pub id: Uuid,
    pub name: String,
    pub party_type: PartyType,
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
            PartyMessage::Tick(when, _) => f.debug_struct("Tick").field("when", when).finish(),
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
            PartyMessage::Tick(when, _) => f.debug_struct("Tick").field("when", when).finish(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Serialize, Deserialize)]
pub enum PartyType {
    CurrencyIssuer = 1,
    CurrencyUser = 2,
    ForeignCurrencyIssuer = 3,
    ForeignCurrencyUser = 4,
    LaborFairy = 5,
    CurrencyFairy = 6,
    Labor = 7,
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
    Labor(FixedNum, String),
    USD(FixedNum),
    LoanBalance(FixedNum, Uuid),
    Food(FixedNum),
    Materials(FixedNum, String), // Loans(HashMap<Uuid, Loan>),
}

impl AssetType {
    pub fn labor<A: Into<FixedNum>, B: Into<String>>(v: A, description: B) -> AssetType {
        AssetType::Labor(v.into(), description.into())
    }

    pub fn usd<A: Into<FixedNum>>(v: A) -> AssetType {
        AssetType::USD(v.into())
    }

    pub fn loan_balance<A: Into<FixedNum>>(v: A, loan_id: Uuid) -> AssetType {
        AssetType::LoanBalance(v.into(), loan_id)
    }

    pub fn food<A: Into<FixedNum>>(v: A) -> AssetType {
        AssetType::Food(v.into())
    }
    pub fn materials<A: Into<FixedNum>, B: Into<String>>(v: A, s: B) -> AssetType {
        AssetType::Materials(v.into(), s.into())
    }
}

impl AssetType {
    pub fn to_identifier(&self) -> AssetTypeIdentifier {
        AssetTypeIdentifier::from_asset_type(self)
    }

    pub fn negative(&self) -> AssetType {
        let n1 = FixedNum::from(-1);
        match self.clone() {
            AssetType::Labor(x, st) => AssetType::Labor(x * n1, st.clone()),
            AssetType::USD(x) => AssetType::USD(x * n1),
            AssetType::LoanBalance(x, id) => AssetType::LoanBalance(x * n1, id),
            AssetType::Food(x) => AssetType::Food(x * n1),
            AssetType::Materials(x, st) => AssetType::Materials(x * n1, st.clone()),
        }
    }

    pub fn combine_with(&self, other: &AssetType, rollback: bool) -> Option<AssetType> {
        match (self.clone(), other) {
            (AssetType::Labor(a, sa), &AssetType::Labor(b, _)) => Some(AssetType::Labor(
                if rollback { a - b } else { a + b },
                sa.clone(),
            )),
            (AssetType::USD(a), &AssetType::USD(b)) => {
                Some(AssetType::USD(if rollback { a - b } else { a + b }))
            }
            (AssetType::LoanBalance(a, id), &AssetType::LoanBalance(b, _)) => Some(
                AssetType::LoanBalance(if rollback { a - b } else { a + b }, id),
            ),
            (AssetType::Food(a), &AssetType::Food(b)) => {
                Some(AssetType::Food(if rollback { a - b } else { a + b }))
            }
            (AssetType::Materials(a, st), &AssetType::Materials(b, _)) => Some(
                AssetType::Materials(if rollback { a - b } else { a + b }, st.clone()),
            ),
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
#[derive(Debug, Clone, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub enum AssetTypeIdentifier {
    Labor(String),
    USD,
    LoanBalance(Uuid),
    Materials(String),
    Food,
}

impl AssetTypeIdentifier {
    /// Convert an AssetType into an AssetTypeIdentifier
    pub fn from_asset_type(asset_type: &AssetType) -> AssetTypeIdentifier {
        match asset_type.clone() {
            AssetType::Labor(_, st) => AssetTypeIdentifier::Labor(st.clone()),
            AssetType::USD(_) => AssetTypeIdentifier::USD,
            AssetType::LoanBalance(_, id) => AssetTypeIdentifier::LoanBalance(id),
            AssetType::Materials(_, st) => AssetTypeIdentifier::Materials(st.clone()),
            AssetType::Food(_) => AssetTypeIdentifier::Food, // &AssetType::Loans(_) => 3,
        }
    }
}

#[derive(Derivative)]
//#[derivative(Debug)]
pub struct Party {
    pub id: Uuid,
    pub name: String,
    // pub state: ArcSwap<PartyState>,
    channel: MPSCSender<PartyMessage>,
    pub party_type: PartyType,
    tick_func: RwLock<Vector<Arc<dyn TickFunc>>>,
}

impl core::fmt::Debug for Party {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("Party")
            .field("id", &self.id)
            .field("name", &self.name)
            .field("party_type", &self.party_type)
            .finish()
    }
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
    pub fn add_tick_func(&self, func: Arc<dyn TickFunc>) {
        match log_error!(self.tick_func.write()) {
            Some(mut v) => {
                let mut v2 = v.clone();
                v2.push_back(func);
                *v = v2; // v.clone().push_back(func);
                ()
            }
            None => (),
        }
    }
    fn create_channels() -> (MPSCSender<PartyMessage>, MPSCReceiver<PartyMessage>) {
        mpsc_channel(100)
    }
    async fn perform_tick(
        &self,
        world: &Arc<World>,
        old_time: DateTime<Utc>,
        new_time: DateTime<Utc>,
        state: &PartyState,
    ) -> Option<PartyState> {
        let fo: Option<Vector<Arc<dyn TickFunc>>> =
            log_error!(self.tick_func.read().map(|v| v.clone()));
        let mut state_ret: Option<PartyState> = None;
        match fo {
            Some(functions) => {
                let f2 = functions;
                for the_fn in f2 {
                    state_ret = match state_ret {
                        Some(ts) => the_fn.tick(world, self, old_time, new_time, &ts).await,
                        None => the_fn.tick(world, self, old_time, new_time, state).await,
                    }
                }
                ()
            }
            None => (),
        }
        state_ret
    }
    async fn start_loop(
        world: Arc<World>,
        myself: Arc<Party>,
        _lua: Arc<Lua>,
        initial_state: PartyState,
        mut rx: MPSCReceiver<PartyMessage>,
    ) {
        let sc = myself.clone();
        tokio::spawn(async move {
            let sp: &Party = &sc;
            let mut snapshot = initial_state;
            let mut real_now = world.get_time();

            let build_snapshot = |ss: &PartyState, the_now| PartySnapshot {
                id: sp.id.clone(),
                name: sp.name.clone(),
                state: ss.clone(),
                now: the_now,
            };

            loop {
                match rx.recv().await {
                    None => break,
                    Some(PartyMessage::Snapshot(reply)) => {
                        log_error!(reply.send(build_snapshot(&snapshot, real_now)));
                    }
                    Some(PartyMessage::Tick(now, tx)) => {
                        match myself.perform_tick(&world, real_now, now, &snapshot).await {
                            Some(v) => snapshot = v,
                            None => (),
                        }
                        real_now = now;
                        log_error!(tx.send(build_snapshot(&snapshot, real_now)).await);
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
                                log_error!(response_chan.send(Ok(())));
                                snapshot = new.clone();
                            }
                            Err(e) => {
                                log_error!(response_chan.send(Err(e)));
                            }
                        }
                    }
                }
            }

            ()
        });
    }

    pub async fn new(name: &str, party_type: PartyType, world: Arc<World>) -> MResult<Arc<Party>> {
        let (tx, rx) = Party::create_channels();
        let ap = Arc::new(Party {
            id: Uuid::new_v4(),
            name: name.to_string(),
            party_type: party_type,
            tick_func: RwLock::new(Vector::new()),
            channel: tx,
        });

        let arc_lua = create_lua_runtime(Some(
            hashmap! {"party_type".into() => serde_json::to_string(&party_type)?,
                "party_id".into() => serde_json::to_string(&ap.id)?,
            "name".into() => name.into()},
        ))?;
        Party::start_loop(world, ap.clone(), arc_lua, PartyState::new(), rx).await;

        Ok(ap)
    }

    pub async fn new_entity(
        name: &str,
        baseline: &Vec<AssetType>,
        the_type: PartyType,
        world: Arc<World>,
    ) -> MResult<Arc<Party>> {
        let (tx, rx) = Party::create_channels();
        let ap = Arc::new(Party {
            id: Misc::compute_uuid_for(name),
            name: name.to_string(),
            tick_func: RwLock::new(Vector::new()),
            party_type: the_type,
            channel: tx,
        });

        Party::start_loop(
            world,
            ap.clone(),
            create_lua_runtime(Some(
                hashmap! {"party_type".into() => serde_json::to_string(&the_type)?,
                    "party_id".into() => serde_json::to_string(&ap.id)?,
                "name".into() => name.into()},
            ))?,
            PartyState::new_with_baseline(baseline),
            rx,
        )
        .await;

        Ok(ap)
    }
    pub async fn new_issuer(name: &str, world: Arc<World>) -> MResult<Arc<Party>> {
        let (tx, rx) = Party::create_channels();
        let ap = Arc::new(Party {
            id: Misc::compute_uuid_for(name),
            name: name.to_string(),
            tick_func: RwLock::new(Vector::new()),
            party_type: PartyType::CurrencyIssuer,
            channel: tx,
        });

        Party::start_loop(
            world,
            ap.clone(),
            create_lua_runtime(Some(
                hashmap! {"party_type".into() => serde_json::to_string(&PartyType::CurrencyIssuer)?,
                    "party_id".into() => serde_json::to_string(&ap.id)?,
                "name".into() => name.into()},
            ))?,
            PartyState::new(),
            rx,
        )
        .await;

        Ok(ap)
    }
}

impl PartialEq for Party {
    fn eq(&self, other: &Party) -> bool {
        self.id == other.id && self.name == other.name && self.party_type == other.party_type
    }
}

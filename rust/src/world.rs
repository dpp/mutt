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
use crate::party::{AssetType, Party, PartyMessage, PartyProxy};
use crate::transaction::Transaction;
use arc_swap::ArcSwap;
use im::{HashMap, HashSet};
use std::sync::Arc;
use uuid::Uuid;

pub const US_GOVERNMENT_NAME: &str = "US Government";
pub const LABOR_NAME: &str = "Labor";
pub const BANK_NAME: &str = "Bank";
pub const RAW_MATERIALS_NAME: &str = "Raw Material";
pub const FOOD_PRODUCER_NAME: &str = "Food Producer";
pub const MAGIC_LABOR_NAME: &str = "The Magic Place Labor Hours Come From";
lazy_static! {
    pub static ref US_GOVERNMENT_UUID: Uuid = Misc::compute_uuid_for(US_GOVERNMENT_NAME);
    pub static ref LABOR_UUID: Uuid = Misc::compute_uuid_for(LABOR_NAME);
    pub static ref BANK_UUID: Uuid = Misc::compute_uuid_for(BANK_NAME);
    pub static ref RAW_MATERIALS_UUID: Uuid = Misc::compute_uuid_for(RAW_MATERIALS_NAME);
    pub static ref FOOD_PRODUCER_UUID: Uuid = Misc::compute_uuid_for(FOOD_PRODUCER_NAME);
    pub static ref MAGIC_LABOR_UUID: Uuid = Misc::compute_uuid_for(MAGIC_LABOR_NAME);
}

#[test]
fn uuids_are_stable() {
    assert_eq!(*LABOR_UUID, Misc::compute_uuid_for(&LABOR_NAME.to_string()));
}

#[derive(Debug)]
pub struct World {
    parties: ArcSwap<HashMap<Uuid, PartyProxy>>,
    transactions: ArcSwap<HashMap<Uuid, Arc<Transaction>>>,
    failed_transactions: ArcSwap<HashMap<Uuid, (Result<(), String>, Arc<Transaction>)>>,
}
impl World {
    pub async fn new() -> Arc<World> {
        let ret = Arc::new(World {
            parties: ArcSwap::new(Arc::new(HashMap::new())),
            transactions: ArcSwap::new(Arc::new(HashMap::new())),
            failed_transactions: ArcSwap::new(Arc::new(HashMap::new())),
        });
        ret.add_party(&Party::new_issuer(US_GOVERNMENT_NAME, ret.clone()).await);
        ret
    }

    pub fn get_transactions(&self) -> Arc<HashMap<Uuid, Arc<Transaction>>> {
        self.transactions.load().clone()
    }

    pub async fn generate_balance_sheet(
        &self,
    ) -> Result<Vec<(Uuid, String, Vec<AssetType>)>, String> {
        let snapshot = self.parties.load();
        let mut v: Vec<(Uuid, String, Vec<AssetType>)> = vec![];
        for p in snapshot.values() {
            let (s, r) = tokio::sync::oneshot::channel();

            p.channel
                .send(PartyMessage::Snapshot(s))
                .await
                .map_err(|v| format!("{:?}", v))?;

            let snapshot = r.await.map_err(|v| format!("{:?}", v))?;
            v.push((
                snapshot.id.clone(),
                snapshot.name.clone(),
                snapshot.state.assets.values().map(|v| v.clone()).collect(),
            ));
        }

        v.sort_by(|a, b| a.1.cmp(&b.1));

        Ok(v)
    }

    pub async fn new_with_preload(info: Vec<(&str, Vec<AssetType>)>) -> Arc<World> {
        let ret = World::new().await;
        for (name, preload) in info {
            ret.add_party(&Party::new_entity(name, &preload, ret.clone()).await);
        }
        ret
    }

    pub fn get_test_party_stuff() -> Vec<(&'static str, Vec<AssetType>)> {
        let mut ret = Vec::new();

        ret.push((LABOR_NAME, vec![AssetType::Labor(FixedNum::from(1000))]));

        ret.push((BANK_NAME, vec![]));
        ret.push((
            RAW_MATERIALS_NAME,
            vec![AssetType::Materials(FixedNum::from(10000))],
        ));
        ret.push((
            FOOD_PRODUCER_NAME,
            vec![AssetType::Food(FixedNum::from(25000))],
        ));
        ret
    }

    /// Process a transaction. Given the async nature of stuff, there's
    /// no way to get a
    pub async fn process_transaction(&self, xaction: &Transaction) -> Result<(), String> {
        use crate::party::Processor;

        let xaction = &Arc::new(xaction.fix_date_and_id());
        async fn process_stuff(self1: &World, xa2: &Arc<Transaction>) -> Result<(), String> {
            let parties = self1.parties.load();
            let from_party = xa2.from_party;
            let to_party = xa2.to_party;
            let from = parties
                .get(&from_party)
                .ok_or(format!("Couldn't find 'from' party with id {}", from_party))?;
            let to = parties
                .get(&to_party)
                .ok_or(format!("Couldn't find 'to' party with id {}", to_party))?;
            from.process(xa2, &xa2.to, &xa2.from).await?;
            match to.process(xa2, &xa2.from, &xa2.to).await {
                Ok(_) => Ok(()),
                e @ Err(_) => {
                    from.rollback(xa2, &xa2.to, &xa2.from).await?;
                    e.clone()
                }
            }
        }
        let res = process_stuff(self, xaction).await;
        match res {
            err @ Err(_) => {
                self.failed_transactions.rcu(|t| {
                    t.update(
                        xaction.id.unwrap_or_else(|| Uuid::new_v4()),
                        (err.clone(), xaction.clone()),
                    )
                });
                err.clone()
            }
            _ => {
                self.transactions.rcu(|t| {
                    t.update(
                        xaction.id.unwrap_or_else(|| Uuid::new_v4()),
                        xaction.clone(),
                    )
                });
                Ok(())
            }
        }
    }

    pub fn get_uuids(&self) -> HashSet<Uuid> {
        let the_map: &HashMap<Uuid, PartyProxy> = &self.parties.load();
        let mut ret = HashSet::new();
        for v in the_map.keys() {
            ret = ret.update(*v);
        }
        ret
    }

    pub fn party_for(&self, id: &Uuid) -> Option<PartyProxy> {
        self.parties.load().get(id).map(|v| v.clone())
    }

    pub fn add_party(&self, p: &Arc<Party>) {
        self.parties.rcu(|pts| {
            if pts.contains_key(&p.id) {
                // do nothing
                pts.clone()
            } else {
                let chan = p.get_message_chan();
                Arc::new(pts.update(
                    p.id.clone(),
                    PartyProxy {
                        id: p.id.clone(),
                        channel: chan,
                    },
                ))
            }
        });
    }
}

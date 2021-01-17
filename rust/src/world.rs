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
use crate::party::{AssetType, Party};
use crate::transaction::Transaction;
use arc_swap::ArcSwap;
use im::{HashMap, HashSet, Vector};
use std::sync::Arc;
use uuid::Uuid;

pub const US_GOVERNMENT_NAME: &str = "US Government";
pub const LABOR_NAME: &str = "Labor";
pub const BANK_NAME: &str = "Bank";
pub const RAW_MATERIALS_NAME: &str = "Raw Material";
pub const FOOD_PRODUCER_NAME: &str = "Food Producer";

lazy_static! {
    static ref US_GOVERNMENT_UUID: Uuid = Misc::compute_uuid_for(US_GOVERNMENT_NAME);
    static ref LABOR_UUID: Uuid = Misc::compute_uuid_for(LABOR_NAME);
    static ref BANK_UUID: Uuid = Misc::compute_uuid_for(BANK_NAME);
    static ref RAW_MATERIALS_UUID: Uuid = Misc::compute_uuid_for(RAW_MATERIALS_NAME);
    static ref FOOD_PRODUCER_UUID: Uuid = Misc::compute_uuid_for(FOOD_PRODUCER_NAME);
}

#[derive(Debug)]
pub struct World {
    parties: ArcSwap<HashMap<Uuid, Party>>,
    transactions: ArcSwap<Vector<Arc<Transaction>>>,
    failed_transactions: ArcSwap<Vector<(Result<(), String>, Arc<Transaction>)>>,
}
impl World {
    pub fn new() -> World {
        let ret = World {
            parties: ArcSwap::new(Arc::new(HashMap::new())),
            transactions: ArcSwap::new(Arc::new(Vector::new())),
            failed_transactions: ArcSwap::new(Arc::new(Vector::new())),
        };
        ret.add_party(&Party::new_issuer(US_GOVERNMENT_NAME));
        ret
    }

    pub fn generate_balance_sheet(&self) -> Vec<(Uuid, String, Vec<AssetType>)> {
        let snapshot = self.parties.load();
        let mut v: Vec<(Uuid, String, Vec<AssetType>)> = snapshot
            .values()
            .map(|v| {
                (
                    v.id.clone(),
                    v.name.clone(),
                    v.state.load().assets.values().map(|a| a.clone()).collect(),
                )
            })
            .collect();

        v.sort_by(|a, b| a.1.cmp(&b.1));

        v
    }

    pub fn new_with_preload(info: Vec<(&str, Vec<AssetType>)>) -> World {
        let ret = World::new();
        for (name, preload) in info {
            ret.add_party(&Party::new_entity(name, &preload));
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

    pub fn new_arc() -> Arc<World> {
        Arc::new(World::new())
    }

    /// Process a transaction. Given the async nature of stuff, there's
    /// no way to get a
    pub async fn process_transaction(&self, xaction: &Transaction) -> Result<(), String> {
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
                self.failed_transactions
                    .rcu(|t| Misc::append(t, (err.clone(), xaction.clone())));
                err.clone()
            }
            _ => {
                self.transactions.rcu(|t| Misc::append(t, xaction.clone()));
                Ok(())
            }
        }
    }

    pub fn get_uuids(&self) -> HashSet<Uuid> {
        let the_map: &HashMap<Uuid, Party> = &self.parties.load();
        let mut ret = HashSet::new();
        for v in the_map.keys() {
            ret = ret.update(*v);
        }
        ret
    }

    pub fn party_for(&self, id: &Uuid) -> Option<Party> {
        self.parties.load().get(id).map(|v| v.clone())
    }

    pub fn add_party(&self, p: &Party) {
        self.parties.rcu(|pts| {
            if pts.contains_key(&p.id) {
                // do nothing
                pts.clone()
            } else {
                Arc::new(pts.update(p.id.clone(), p.clone()))
            }
        });
    }
}

#[test]
fn test_world_building() {
    let w = World::new();
    let ids = w.get_uuids();
    assert!(ids.len() > 0, "At least someone in the world");

    // construct a second world
    let w2 = World::new();
    let ids2 = w2.get_uuids();
    assert_eq!(ids, ids2, "The ids must be stable");

    assert!(
        w2.party_for(&US_GOVERNMENT_UUID).is_some(),
        "Has a US_GOVERNMENT UUID"
    )
}
#[test]
fn test_transaction_reading_from_json() {
    println!("{}", Misc::now().to_rfc3339());

    let xa = r#"{
    "description": "Labor buys food from Bank",
    "from": {
        "USD": "5"
    },
    "from_party": "757de9b5-337b-5fe2-906c-143f26984912",
    "id": "1b8c528c-10b1-4f79-8cd4-2f74cd10ab40",
    "to": {
        "Food": "3"
    },
    "to_party": "e4266d2d-d6e2-527a-a2ab-67c384cea8b3",
    "when": "2021-01-10T19:04:34.994945272+00:00"
}
"#;
    let t: Transaction = serde_json::from_str(xa).unwrap();

    assert_eq!(t.from, AssetType::USD(FixedNum::from(5)), "Got $5");

    let xa2 = r#"{
    "description": "Labor buys food from Bank",
    "from": {
        "USD": "2"
    },
    "from_party": "757de9b5-337b-5fe2-906c-143f26984912",
    "id": "1b8c528c-10b1-4f79-8cd4-2f74cd10ab40",
    "to": {
        "Food": "3"
    },
    "to_party": "e4266d2d-d6e2-527a-a2ab-67c384cea8b3"
}
"#;

    let t2: Transaction = serde_json::from_str(xa2).unwrap();

    assert_eq!(t2.from, AssetType::USD(FixedNum::from(2)), "Got $2");
}
//#[test]
#[tokio::test]
async fn test_a_transaction() {
    use crate::party::AssetTypeIdentifier;
    use serde_json::{json, to_string_pretty};

    let w = World::new_with_preload(World::get_test_party_stuff());
    let t = Transaction {
        description: "Government buys Labor".to_string(),
        from_party: *US_GOVERNMENT_UUID,
        to_party: *LABOR_UUID,
        id: None,
        when: None,
        from: AssetType::USD(FixedNum::from(50)),
        to: AssetType::Labor(FixedNum::from(2)),
    };
    w.process_transaction(&t).await.unwrap();
    assert!(
        w.party_for(&LABOR_UUID).unwrap().get_cash_balance() > FixedNum::from(0),
        "Labor has income"
    );
    assert!(
        w.party_for(&US_GOVERNMENT_UUID).unwrap().get_cash_balance() < FixedNum::from(0),
        "Government has a deficit"
    );

    assert!(
        match w
            .party_for(&US_GOVERNMENT_UUID)
            .unwrap()
            .get_asset(AssetTypeIdentifier::Labor)
            .unwrap()
        {
            AssetType::Labor(x) => x,
            _ => panic!("Expected labor"),
        } > FixedNum::from(0),
        "Government has labor"
    );
    assert_eq!(
        match w
            .party_for(&LABOR_UUID)
            .unwrap()
            .get_asset(AssetTypeIdentifier::Labor)
            .unwrap()
        {
            AssetType::Labor(x) => x,
            _ => panic!("Expected labor"),
        },
        FixedNum::from(998),
        "Labor sold some"
    );
    let t = Transaction {
        description: "Labor buys food from Bank".to_string(),
        from_party: *LABOR_UUID,
        to_party: *BANK_UUID,
        id: None,
        when: None,
        from: AssetType::USD(FixedNum::from(5)),
        to: AssetType::Food(FixedNum::from(3)),
    };

    w.process_transaction(&t).await.unwrap();
    assert_eq!(
        match w
            .party_for(&LABOR_UUID)
            .unwrap()
            .get_asset(AssetTypeIdentifier::Food)
            .unwrap()
        {
            AssetType::Food(x) => x,
            _ => panic!("Expected food"),
        },
        FixedNum::from(3),
        "Labor bought food"
    );
    assert_eq!(
        match w
            .party_for(&BANK_UUID)
            .unwrap()
            .get_asset(AssetTypeIdentifier::Food)
            .unwrap()
        {
            AssetType::Food(x) => x,
            _ => panic!("Expected food"),
        },
        FixedNum::from(-3),
        "Bank sold food it didn't have"
    );

    for t in w.transactions.load().iter() {
        let t2: &Transaction = t;
        println!("{}", to_string_pretty(&json!(t2)).unwrap());
    }
}

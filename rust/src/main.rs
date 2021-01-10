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

use arc_swap::ArcSwap;
use chrono::{DateTime, Duration, FixedOffset, Utc};
use im::{HashMap, HashSet, Vector};
use serde::{Deserialize, Serialize};
use serde_json::{json, to_string_pretty, Deserializer};
use sha2::{Digest, Sha256};
use std::io::{BufReader, Read};
use std::sync::Arc;
use uuid::Uuid;

use fixed::{types::extra::U64, FixedI128};
pub type Fix = FixedI128<U64>;

#[test]
fn test_numbers() {
    let a = Fix::from_num(22.2);
    let b = Fix::from_num(1) / Fix::from_num(2);
    println!("A is {} and b {}", a, b);
    assert_eq!(a * b, Fix::from_num(11.1));
}

#[tokio::main]
pub async fn main() {
    // println!("US Gov uuid {}", US_GOVERNMENT_UUID.to_string());
    // println!("Labor uuid {}", LABOR_UUID.to_string());
    // println!("Bank uuid {}", BANK_UUID.to_string());
    // println!("Raw Materials uuid {}", RAW_MATERIALS_UUID.to_string());
    // println!("Food uuid {}", FOOD_PRODUCER_UUID.to_string());

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

        ret.push((LABOR_NAME, vec![AssetType::Labor(Fix::from(1000))]));

        ret.push((BANK_NAME, vec![]));
        ret.push((
            RAW_MATERIALS_NAME,
            vec![AssetType::Materials(Fix::from(10000))],
        ));
        ret.push((FOOD_PRODUCER_NAME, vec![AssetType::Food(Fix::from(25000))]));
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

    assert_eq!(t.from, AssetType::USD(Fix::from(5)), "Got $5");

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

    assert_eq!(t2.from, AssetType::USD(Fix::from(2)), "Got $2");
}
//#[test]
#[tokio::test]
async fn test_a_transaction() {
    let w = World::new_with_preload(World::get_test_party_stuff());
    let t = Transaction {
        description: "Government buys Labor".to_string(),
        from_party: *US_GOVERNMENT_UUID,
        to_party: *LABOR_UUID,
        id: None,
        when: None,
        from: AssetType::USD(Fix::from(50)),
        to: AssetType::Labor(Fix::from(2)),
    };
    w.process_transaction(&t).await.unwrap();
    assert!(
        w.party_for(&LABOR_UUID).unwrap().get_cash_balance() > Fix::from(0),
        "Labor has income"
    );
    assert!(
        w.party_for(&US_GOVERNMENT_UUID).unwrap().get_cash_balance() < Fix::from(0),
        "Government has a deficit"
    );

    assert!(
        match w
            .party_for(&US_GOVERNMENT_UUID)
            .unwrap()
            .get_asset(AssetTypeIdentifer::Labor)
            .unwrap()
        {
            AssetType::Labor(x) => x,
            _ => panic!("Expected labor"),
        } > Fix::from(0),
        "Government has labor"
    );
    assert_eq!(
        match w
            .party_for(&LABOR_UUID)
            .unwrap()
            .get_asset(AssetTypeIdentifer::Labor)
            .unwrap()
        {
            AssetType::Labor(x) => x,
            _ => panic!("Expected labor"),
        },
        Fix::from(998),
        "Labor sold some"
    );
    let t = Transaction {
        description: "Labor buys food from Bank".to_string(),
        from_party: *LABOR_UUID,
        to_party: *BANK_UUID,
        id: None,
        when: None,
        from: AssetType::USD(Fix::from(5)),
        to: AssetType::Food(Fix::from(3)),
    };

    w.process_transaction(&t).await.unwrap();
    assert_eq!(
        match w
            .party_for(&LABOR_UUID)
            .unwrap()
            .get_asset(AssetTypeIdentifer::Food)
            .unwrap()
        {
            AssetType::Food(x) => x,
            _ => panic!("Expected food"),
        },
        Fix::from(3),
        "Labor bought food"
    );
    assert_eq!(
        match w
            .party_for(&BANK_UUID)
            .unwrap()
            .get_asset(AssetTypeIdentifer::Food)
            .unwrap()
        {
            AssetType::Food(x) => x,
            _ => panic!("Expected food"),
        },
        Fix::from(-3),
        "Bank sold food it didn't have"
    );

    for t in w.transactions.load().iter() {
        let t2: &Transaction = t;
        println!("{}", to_string_pretty(&json!(t2)).unwrap());
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
    amount_usd: Fix,
    id: Uuid,
    to: Uuid,
    interest_per_period: Fix,
    periods: usize,
    initial_date: DateTime<Utc>,
    period_duration: Duration,
    last_serviced: DateTime<Utc>,
    balance_usd: Fix,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum AssetType {
    Labor(Fix),
    USD(Fix),
    LoanBalance(Fix),
    Food(Fix),
    Materials(Fix), // Loans(HashMap<Uuid, Loan>),
}
impl AssetType {
    pub fn to_identifier(&self) -> AssetTypeIdentifer {
        AssetTypeIdentifer::from_asset_type(self)
    }

    pub fn negative(&self) -> AssetType {
        let n1 = Fix::from(-1);
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
pub enum AssetTypeIdentifer {
    Labor = 1,
    USD,
    LoanBalance,
    Materials,
    Food,
}

impl AssetTypeIdentifer {
    /// Convert an AssetType into an AssetTypeIdentifier
    pub fn from_asset_type(asset_type: &AssetType) -> AssetTypeIdentifer {
        match asset_type {
            &AssetType::Labor(_) => AssetTypeIdentifer::Labor,
            &AssetType::USD(_) => AssetTypeIdentifer::USD,
            &AssetType::LoanBalance(_) => AssetTypeIdentifer::LoanBalance,
            &AssetType::Materials(_) => AssetTypeIdentifer::Materials,
            &AssetType::Food(_) => AssetTypeIdentifer::Food, // &AssetType::Loans(_) => 3,
        }
    }
}

pub type ArcParty = Arc<Party>;

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

pub type PartyAssets = HashMap<AssetTypeIdentifer, AssetType>;

#[derive(Debug, Clone, PartialEq)]
pub struct PartyState {
    pub cash_balance: Fix,
    pub hash: Vec<u8>,
    pub transactions: Vector<Arc<Transaction>>,
    pub assets: PartyAssets,
}

impl PartyState {
    pub fn new() -> PartyState {
        PartyState {
            cash_balance: Fix::from(0),
            hash: vec![],
            transactions: Vector::new(),
            assets: HashMap::new(),
        }
    }

    pub fn new_with_baseline(baseline: &Vec<AssetType>) -> ArcSwap<PartyState> {
        let cash = baseline.iter().fold(Fix::from(0), |acc, v| match v {
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

        fn fix_cash_balance(current: Fix, at: &AssetType, rollback: bool) -> Fix {
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
    pub fn get_cash_balance(&self) -> Fix {
        self.state.load().cash_balance
    }

    pub fn get_asset(&self, assert_type: AssetTypeIdentifer) -> Option<AssetType> {
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

/// This is a struct that cannot be created. It's mostly to
/// create a namespace for miscellaneous functions
pub struct Misc {
    _cant_create_one: String,
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

use arc_swap::ArcSwap;
// use async_channel::{bounded, Receiver, Sender};
use im::{HashMap, Vector};
use std::sync::Arc;
use std::time::Instant;
use uuid::Uuid;

fn main() {
    println!("Hello, world!");
}

pub struct World {
    parties: ArcSwap<HashMap<Uuid, Party>>,
}

impl World {
    pub fn new() -> World {
        let ret = World {
            parties: ArcSwap::new(Arc::new(HashMap::new())),
        };
        ret.add_party(&Party::new_issuer("US Government"));
        ret
    }

    pub fn get_uuids(&self) -> Vec<Uuid> {
        let the_map: &HashMap<Uuid, Party> = &self.parties.load();
        let mut ret = vec![];
        for v in the_map.keys() {
            ret.push(*v)
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
}
#[derive(Debug, Clone, PartialEq)]
pub enum PartyType {
    CurrencyIssuer = 1,
    CurrencyUser = 2,
    ForeignCurrencyIssuer = 3,
    ForeignCurrencyUser = 4,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum AssetType {
    Labor,
    USD,
}
pub type ArcParty = Arc<Party>;

#[derive(Debug, Clone, PartialEq)]
pub struct Transaction {
    pub id: Uuid,
    pub description: String,
    pub from_type: AssetType,
    pub from_qnty: i64,
    pub to_type: AssetType,
    pub to_qnty: i64,
    pub from_party: Uuid,
    pub to_party: Uuid,
    pub when: Instant,
    pub sub_transactions: Vector<Transaction>,
}
#[derive(Debug, Clone, PartialEq)]
pub struct PartyState {
    pub cash_balance: i128,
    pub hash: Vec<u8>,
    pub transactions: Vector<Arc<Transaction>>,
    pub assets: HashMap<AssetType, i128>,
}

impl PartyState {
    pub fn new() -> PartyState {
        PartyState {
            cash_balance: 0,
            hash: vec![],
            transactions: Vector::new(),
            assets: HashMap::new(),
        }
    }

    pub fn new_swap() -> ArcSwap<PartyState> {
        ArcSwap::new(Arc::new(PartyState::new()))
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
    pub fn new(name: &str, party_type: PartyType) -> Party {
        Party {
            id: Uuid::new_v4(),
            name: name.to_string(),
            party_type: party_type,
            state: PartyState::new_swap(),
        }
    }

    pub fn new_issuer(name: &str) -> Party {
        Party {
            id: Uuid::new_v5(&Uuid::NAMESPACE_DNS, name.as_bytes()),
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

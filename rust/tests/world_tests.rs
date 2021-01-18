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

use mutt::misc::{FixedNum, Misc};
use mutt::party::{AssetType, Processor};
use mutt::transaction::Transaction;
use mutt::world::*;

#[tokio::test]
async fn test_world_building() {
    let w = World::new().await;
    let ids = w.get_uuids();
    assert!(ids.len() > 0, "At least someone in the world");

    // construct a second world
    let w2 = World::new().await;
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
    use mutt::party::AssetTypeIdentifier;
    use serde_json::{json, to_string_pretty};

    let w = World::new_with_preload(World::get_test_party_stuff()).await;
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
        w.party_for(&LABOR_UUID)
            .unwrap()
            .get_cash_balance()
            .await
            .unwrap()
            > FixedNum::from(0),
        "Labor has income"
    );
    assert!(
        w.party_for(&US_GOVERNMENT_UUID)
            .unwrap()
            .get_cash_balance()
            .await
            .unwrap()
            < FixedNum::from(0),
        "Government has a deficit"
    );

    assert!(
        match w
            .party_for(&US_GOVERNMENT_UUID)
            .unwrap()
            .get_asset(AssetTypeIdentifier::Labor)
            .await
            .unwrap()
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
            .await
            .unwrap() // Unwrap the result
            .unwrap() // unwrap the option
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
            .await
            .unwrap()
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
            .await
            .unwrap()
            .unwrap()
        {
            AssetType::Food(x) => x,
            _ => panic!("Expected food"),
        },
        FixedNum::from(-3),
        "Bank sold food it didn't have"
    );

    for t in w.get_transactions().values() {
        let t2: &Transaction = t;
        println!("{}", to_string_pretty(&json!(t2)).unwrap());
    }
}

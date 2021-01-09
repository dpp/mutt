# Rust Simulator

This Rust code simulates a series of transactions.

Transactions are JSON objects that look like:

```json
{
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
    "when": {
        "nanos_since_epoch": 643453891,
        "secs_since_epoch": 1610151893
    }
}
```

The parties have stable UUIDs. Here are the pre-loaded UUIDs:

```
US Gov uuid 13a08b9b-2a5f-5e09-90aa-b0dc31038e34
Labor uuid 757de9b5-337b-5fe2-906c-143f26984912
Bank uuid e4266d2d-d6e2-527a-a2ab-67c384cea8b3
Raw Materials uuid 31eb7edf-8f4d-5df7-b7e7-320a09228efb
Food uuid 1fbb6953-a525-5f92-bdd7-09b54d6fa6d7
```

The `from` and `to` fields are the one of the following keys:

```rust
Labor
USD
LoanBalance
Food
Materials
```

To run a sample simulator: `cargo run < sample_transactions.txt` and you should
see a balance sheet emitted.

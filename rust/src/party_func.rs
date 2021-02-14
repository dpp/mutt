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
use crate::party::{AssetType, Party};
use crate::party_state::PartyState;
use crate::transaction::Transaction;
use crate::world::World;
use async_trait::async_trait;
use chrono::{DateTime, Duration, Utc};
use std::sync::Arc;

#[async_trait]
pub trait TickFunc: Send + Sync {
    async fn tick(
        &self,
        world: &Arc<World>,
        party: &Party,
        old: DateTime<Utc>,
        new: DateTime<Utc>,
        state: &PartyState,
    ) -> Option<PartyState>;

    fn name(&self) -> String;
}

pub struct RefreshLabor {}

#[async_trait]
impl TickFunc for RefreshLabor {
    fn name(&self) -> String {
        "Refresh Labor".into()
    }

    async fn tick(
        &self,
        world: &Arc<World>,
        party: &Party,
        old: DateTime<Utc>,
        new: DateTime<Utc>,
        _state: &PartyState,
    ) -> Option<PartyState> {
        // only refresh hours if we can find a labor fairy
        let fairy = world.get_labor_fairy().front().map(|v| v.clone());
        match fairy {
            Some(v) => {
                let duration: Duration = new - old;
                let hours = duration.num_hours() as f64;
                let hours_per_week = 7.0 * 24.0;
                let hours_to_add = 40.0 * (hours / hours_per_week);
                let ot_hours_to_add = 20.0 * (hours / hours_per_week);
                let ta = Transaction::new(
                    "Refresh periodic hours",
                    AssetType::labor(hours_to_add, "normal hours"),
                    AssetType::labor(hours_to_add, "normal hours"),
                    v.id,
                    party.id,
                );
                log_error!(world.process_transaction(&ta).await.as_ref());
                let ta2 = Transaction::new(
                    "Refresh periodic overtime hours",
                    AssetType::labor(ot_hours_to_add, "overtime hours"),
                    AssetType::labor(ot_hours_to_add, "overtime hours"),
                    v.id,
                    party.id,
                );
                log_error!(world.process_transaction(&ta2).await.as_ref());
                None
            }
            None => None,
        }
    }
}

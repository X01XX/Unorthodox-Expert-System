//! A filter between the internal states/functions, and a
//! function that does something externally.
//!
//! Storing a function pointer in SomeAction runs into problems with the parallel crate
//! and the serialization crate.
use crate::actions;
use crate::state::SomeState;
use rand::Rng;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[readonly::make]
#[derive(Serialize, Deserialize)]
pub struct ActionInterface {
    /// Store states and most-recent change step. Only used for the demonstration action dom0_act0.
    pub ahash: HashMap<SomeState, usize>,
}

impl ActionInterface {
    /// Return a new, empty, ActionInterface.
    pub fn new() -> Self {
        Self {
            ahash: HashMap::new(),
        }
    }

    /// Given a domain number, action number and domain current state,
    /// take an action, filter and massage as needed, return a new domain
    /// current state.
    pub fn take_action(
        &mut self,
        cur_state: &SomeState,
        dom_num: usize,
        act_num: usize,
    ) -> SomeState {
        let mut sample_hint = 0;

        if dom_num == 0 && act_num == 0 {
            if let Some(val) = self.ahash.get_mut(cur_state) {
                sample_hint = *val;
                *val = (sample_hint + 1) % 4;
            } else {
                // Start a new state counter at a random place in the cycle.
                sample_hint = rand::thread_rng().gen_range(0..4);
                self.ahash.insert(cur_state.clone(), (sample_hint + 1) % 4);
            };
        }
        actions::take_action(dom_num, act_num, cur_state, sample_hint)
    }
}

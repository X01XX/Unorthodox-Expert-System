//! A filter between the internal states/functions, and a
//! function that does something externally.
//!
//! Storing a function pointer in SomeAction runs into problems with the parallel crate
//! and the serialization crate.
use crate::actions::take_action;
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
        if dom_num == 0 && act_num == 0 {
            let anum = if let Some(x) = self.ahash.get(cur_state) {
                *x
            } else {
                rand::thread_rng().gen_range(0..4)
            };

            let ret_state = crate::actions::take_action(dom_num, act_num, cur_state, anum);
            self.ahash.insert(cur_state.clone(), (anum + 1) % 4);
            ret_state
        } else {
            take_action(dom_num, act_num, cur_state, 0)
        }
    }
}

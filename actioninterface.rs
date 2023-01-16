//! A filter between the internal states/functions, and a
//! function that does something externally.
//!
//! Storing a function pointer in SomeAction runs into problems with the parallel crate
//! and the serialization crate.
use crate::actions::take_action;
use crate::mask::SomeMask;
use crate::state::SomeState;

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[readonly::make]
#[derive(Serialize, Deserialize)]
pub struct ActionInterface {
    /// Store states and most-recent change step. Only used for the demonstration action dom0_act0.
    pub ahash: HashMap<SomeState, SomeMask>,
    /// A stand-in for a function pointer.
    dom_num: usize,
    act_num: usize,
}

impl ActionInterface {
    /// Return a new, empty, ActionInterface.
    pub fn new(dom_num: usize, act_num: usize) -> Self {
        Self {
            ahash: HashMap::new(),
            dom_num,
            act_num,
        }
    }

    /// Given a domain number, action number and domain current state,
    /// take an action, filter and massage as needed, return a new domain
    /// current state.
    pub fn take_action(&mut self, cur_state: &SomeState) -> SomeState {
        let ret_state;

        if self.dom_num == 0 && self.act_num == 0 {
            let amsk = self.ahash.get(cur_state);
            ret_state = crate::actions::take_action(self.dom_num, self.act_num, cur_state, amsk);
            let dif = cur_state.bitwise_xor(&ret_state).to_mask();
            self.ahash.insert(cur_state.clone(), dif);
        } else {
            ret_state = take_action(self.dom_num, self.act_num, cur_state, None);
        }

        ret_state
    }
}

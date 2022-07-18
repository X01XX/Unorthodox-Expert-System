use crate::actions::take_action;
use crate::mask::SomeMask;
/// A filter between the internal states/functions, and a
/// function that does something.
use crate::state::SomeState;

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[readonly::make]
#[derive(Serialize, Deserialize)]
pub struct ActionInterface {
    /// Store states and most-recent change seen
    pub ahash: HashMap<SomeState, SomeMask>,
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
        dom_num: usize,
        act_num: usize,
        cur_state: &SomeState,
    ) -> SomeState {
        let ret_state;

        if dom_num == 0 && act_num == 0 {
            let amsk = self.ahash.get(&cur_state);
            ret_state = take_action(dom_num, act_num, &cur_state, amsk);
            let dif = SomeMask::new(cur_state.bts.b_xor(&ret_state.bts));
            self.ahash.insert(cur_state.clone(), dif);
        } else {
            ret_state = take_action(dom_num, act_num, &cur_state, None);
        }

        ret_state
    }
}

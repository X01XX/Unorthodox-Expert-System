/// A filter between the internal states/functions, and a
/// function that does something.

use crate::state::SomeState;
use crate::mask::SomeMask;
use crate::actions::take_action;

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

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
    pub fn take_action(&mut self, dom_num: usize, act_num: usize, cur_state: &SomeState) -> SomeState {

        let amsk = self.ahash.get(&cur_state);
        let ret_state = take_action(dom_num, act_num, &cur_state, amsk);
        let dif = cur_state.s_xor(&ret_state).to_mask();
        self.ahash.insert(cur_state.clone(), dif);

        ret_state
    }
}

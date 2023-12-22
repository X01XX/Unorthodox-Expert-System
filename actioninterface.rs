//! A filter between the internal states/functions, and a
//! function that does something externally.
//!
//! Storing a function pointer in SomeAction runs into problems with the parallel crate
//! and the serialization crate.

use crate::rule::SomeRule;
use crate::rulestore::RuleStore;
use crate::sample::SomeSample;
use crate::state::SomeState;

use rand::Rng;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[readonly::make]
#[derive(Serialize, Deserialize)]
pub struct ActionInterface {
    /// Store states and most-recent change step. Only used for the demonstration action dom0_act0.
    pub ahash: HashMap<SomeState, usize>,
    pub rules: Vec<RuleStore>,
}

impl ActionInterface {
    /// Return a new, ActionInterface.
    pub fn new(rules: Vec<RuleStore>) -> Self {
        // Check rules have the same initial region.
        for rulesx in rules.iter().skip(1) {
            for rulex in rulesx.iter() {
                //println!("{} == {} ?", rulex.initial_region(), rules[0].initial_region());
                assert!(rulex.initial_region() == rulesx[0].initial_region());
            }
        }
        Self {
            ahash: HashMap::new(),
            rules,
        }
    }

    /// Given a domain number, action number and domain current state,
    /// take an action, filter and massage as needed, return a new domain
    /// current state.
    pub fn take_action(
        &mut self,
        cur_state: &SomeState,
        dom_id: usize,
        act_id: usize,
    ) -> SomeState {
        let mut new_state = cur_state.clone();
        'next_rs: for rsx in self.rules.iter() {
            if rsx.is_not_empty() && rsx[0].initial_region().is_superset_of(cur_state) {
                if rsx.len() == 1 {
                    new_state = rsx[0].result_from_initial_state(cur_state);
                } else {
                    let rule_hint = if let Some(val) = self.ahash.get_mut(cur_state) {
                        *val += 1;
                        if *val == rsx.len() {
                            *val = 0;
                        }
                        *val
                    } else {
                        // Start a new state counter at a random place in the cycle.
                        let sample_hint = rand::thread_rng().gen_range(0..rsx.len());
                        self.ahash.insert(cur_state.clone(), sample_hint);
                        sample_hint
                    };
                    new_state = rsx[rule_hint].result_from_initial_state(cur_state);
                }
                break 'next_rs;
            }
        } // next rsx
        println!(
            "\nDom {} {} -{}-> {} R[{}]",
            dom_id,
            cur_state,
            act_id,
            new_state,
            SomeRule::new(&SomeSample::new(cur_state.clone(), new_state.clone()))
        );
        new_state
    }

    /// Return the number of RuleStores.
    pub fn len(&self) -> usize {
        self.rules.len()
    }
}

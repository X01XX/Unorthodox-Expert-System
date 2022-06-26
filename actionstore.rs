//! The ActionStore struct, a vector of SomeAction structs.
//!
//! This stores a vector of SomeAction structs, for a Domain struct.
//!
use crate::action::SomeAction;
use crate::change::SomeChange;
use crate::needstore::NeedStore;
use crate::state::SomeState;
use crate::stepstore::StepStore;
use std::collections::VecDeque;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::{Index, IndexMut};

use rayon::prelude::*;

impl fmt::Display for ActionStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut rc_str = String::new();

        for actx in &self.avec {
            rc_str.push_str(&format!("\n  {}", &actx));
        }

        write!(f, "{}", rc_str)
    }
}

#[readonly::make]
#[derive(Serialize, Deserialize)]
pub struct ActionStore {
    /// A vector of SomeAction structs
    pub avec: Vec<SomeAction>,
}

impl ActionStore {
    /// Return a new, empty ActionStore.
    pub fn new() -> Self {
        ActionStore {
            avec: Vec::<SomeAction>::with_capacity(5),
        }
    }

    /// Return the length of an ActionStore.
    pub fn len(&self) -> usize {
        self.avec.len()
    }

    /// Add a SomeAction struct to the store.
    pub fn push(&mut self, mut actx: SomeAction) {
        actx.set_num(self.avec.len());
        self.avec.push(actx);
    }

    /// Get needs for all actions in the store.
    pub fn get_needs(
        &mut self,
        cur: &SomeState,
        agg_chgs: &SomeChange,
        dom: usize,
        memory: &VecDeque<SomeState>,
    ) -> NeedStore {
        // Run a get_needs thread for each action
        //println!("actionstore: get_needs");

        let mut vecx: Vec<NeedStore> = self
            .avec
            .par_iter_mut() // par_iter_mut for parallel, .iter_mut for easier reading of diagnostic messages
            .map(|actx| actx.get_needs(cur, agg_chgs, dom, memory))
            .collect::<Vec<NeedStore>>();

        // Aggregate the results into one NeedStore
        let mut nds_agg = NeedStore::new();

        for mut nst in vecx.iter_mut() {
            nds_agg.append(&mut nst);
        }

        nds_agg
    }

    /// Return steps that make at least one needed bit change.
    pub fn get_steps(&self, achange: &SomeChange) -> StepStore {
        // Run a thread for each action
        let mut stps: Vec<StepStore> = self
            .avec
            .par_iter() // par_iter for parallel, .iter for easier reading of diagnostic messages
            .map(|actx| actx.get_steps(achange))
            .collect::<Vec<StepStore>>();

        // Aggregate the results into one StepStore
        let mut stps_agg = StepStore::new();

        for stp in stps.iter_mut() {
            stps_agg.append(stp);
        }

        //println!("actionstore:get_steps possible steps: {}", stps.str());
        stps_agg
    }

    /// Return the number of integers used for low-level bit patterns.
    fn num_ints(&self) -> Option<usize> {
        if self.len() == 0 {
            return None;
        }
        Some(self.avec[0].num_ints())
    }

    /// Return the maximum reachable region for all actions
    pub fn aggregate_changes(&self) -> Option<SomeChange> {
        if let Some(num_ints) = self.num_ints() {
            let mut agg_chg = SomeChange::new_low(num_ints);

            // Or each action change
            for actx in self.avec.iter() {
                agg_chg = agg_chg.c_or(&actx.aggregate_changes);
            }

            return Some(agg_chg);
        }
        None
    }
} // end impl ActionStore

impl Index<usize> for ActionStore {
    type Output = SomeAction;
    fn index<'a>(&'a self, i: usize) -> &'a SomeAction {
        &self.avec[i]
    }
}

impl IndexMut<usize> for ActionStore {
    fn index_mut<'a>(&mut self, i: usize) -> &mut Self::Output {
        &mut self.avec[i]
    }
}

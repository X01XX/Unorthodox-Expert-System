//! The ActionStore struct, a vector of SomeAction structs.
//!
//! This stores a vector of SomeAction structs, for a Domain struct.
//!
use crate::action::SomeAction;
use crate::change::SomeChange;
use crate::mask::SomeMask;
use crate::needstore::NeedStore;
use crate::state::SomeState;
use crate::stepstore::StepStore;

use std::collections::VecDeque;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::{Index, IndexMut};
use std::slice::Iter;

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
    pub num_ints: usize,
}

impl ActionStore {
    /// Return a new, empty ActionStore.
    pub fn new(num_ints: usize) -> Self {
        ActionStore {
            avec: Vec::<SomeAction>::with_capacity(5),
            num_ints,
        }
    }

    /// Return the length of an ActionStore.
    pub fn len(&self) -> usize {
        self.avec.len()
    }

    /// Add a SomeAction struct to the store.
    pub fn push(&mut self, actx: SomeAction) {
        self.avec.push(actx);
    }

    /// Get needs for all actions in the store.
    pub fn get_needs(
        &mut self,
        cur: &SomeState,
        dom: usize,
        memory: &VecDeque<SomeState>,
        changes_mask: &SomeMask,
    ) -> NeedStore {
        // Run a get_needs thread for each action
        //println!("actionstore: get_needs");

        let mut vecx: Vec<NeedStore> = self
            .avec
            .par_iter_mut() // par_iter_mut for parallel, .iter_mut for easier reading of diagnostic messages
            .map(|actx| actx.get_needs(cur, dom, memory, changes_mask))
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

    /// Return an iterator
    pub fn iter(&self) -> Iter<SomeAction> {
        self.avec.iter()
    }

    /// Return a mask of bit positions that can be changed.
    pub fn aggregate_changes_mask(&self) -> SomeMask {
        let mut chgs = SomeChange::new_low(self.num_ints);

        for actx in self.avec.iter() {
            chgs = chgs.c_or(actx.aggregate_changes());
        }

        chgs.b01.m_and(&chgs.b10)
    }

    /// Check the limited flags on groups due to new bit position that can be changed.
    pub fn check_limited(&mut self, new_chgs: &SomeMask) {
        for actx in self.avec.iter_mut() {
            actx.check_limited(new_chgs);
        }
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

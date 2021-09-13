//! The ActionStore struct, a vector of SomeAction structs.
//!
//! This stores a vector of SomeAction structs, for a Domain struct.
//!
use crate::action::SomeAction;
use crate::change::SomeChange;
use crate::mask::SomeMask;
use crate::needstore::NeedStore;
use crate::state::SomeState;
use crate::region::SomeRegion;
use crate::stepstore::StepStore;

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
        actx.num = self.avec.len();
        self.avec.push(actx);
    }

    /// Get an predictable_mask for all actions.
    /// Indicates bit position that can predictably change
    pub fn get_predictable_mask(&self, num_ints: usize) -> SomeMask {
        let mut cngx = SomeChange::new_low(num_ints);

        for actx in &self.avec {
            //println!("act {} change {}", &actx.num, &actx.predictable_bit_changes);
            cngx = cngx.union(&actx.predictable_bit_changes);
            //println!("cngx   change {}", &cngx);
        }

        cngx.x_mask()
    }

    /// Get needs for all actions in the store.
    pub fn get_needs(&mut self, cur: &SomeState, x_mask: &SomeMask, dom: usize) -> NeedStore {
        // Run a get_needs thread for each action
        let mut vecx: Vec<NeedStore> = self
            .avec
            .par_iter_mut() // par_iter_mut for parallel, .iter_mut for easier reading of diagnostic messages
            .map(|actx| actx.get_needs(cur, x_mask, dom))
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

        // Run a get_needs thread for each action
        let mut stps: Vec<StepStore> = self
            .avec
            .par_iter() // par_iter for parallel, .iter for easier reading of diagnostic messages
            .map(|actx| actx.get_steps(achange))
            .collect::<Vec<StepStore>>();

        // Aggregate the results into one NeedStore
        let mut stps_agg = StepStore::new();

        for stp in stps.iter_mut() {
            stps_agg.append(stp);
        }

        //println!("actionstore:get_steps possible steps: {}", stps.str());
        stps_agg
    }

    /// Return steps that change a given state to a state closer
    /// to a goal region.
    pub fn steps_to(&self, from_reg: &SomeRegion, goal_reg: &SomeRegion) -> StepStore {

        // Run a get_needs thread for each action
        let mut stps: Vec<StepStore> = self
            .avec
            .par_iter() // par_iter for parallel, .iter for easier reading of diagnostic messages
            .map(|actx| actx.steps_to(from_reg, goal_reg))
            .collect::<Vec<StepStore>>();

        // Aggregate the results into one NeedStore
        let mut stps_agg = StepStore::new();

        for stp in stps.iter_mut() {
            stps_agg.append(stp);
        }

        //println!("actionstore:steps_to possible steps: {}", stps.str());
        stps_agg
    }

    /// Return steps that change a region closer to a state to a goal
    pub fn steps_from(&self, goal_reg: &SomeRegion, from_reg: &SomeRegion) -> StepStore {

        // Run a get_needs thread for each action
        let mut stps: Vec<StepStore> = self
            .avec
            .par_iter() // par_iter for parallel, .iter for easier reading of diagnostic messages
            .map(|actx| actx.steps_from(goal_reg, from_reg))
            .collect::<Vec<StepStore>>();

        // Aggregate the results into one NeedStore
        let mut stps_agg = StepStore::new();

        for stp in stps.iter_mut() {
            stps_agg.append(stp);
        }

        //println!("actionstore:steps_from possible steps: {}", stps.str());
        stps_agg
        
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

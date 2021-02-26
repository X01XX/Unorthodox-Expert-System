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

    /// Return the length of the store.
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

    /// Get needs for all actions.
    pub fn get_needs(&mut self, cur: &SomeState, x_mask: &SomeMask, dom: usize) -> NeedStore {
        // Run a get_needs thread for each action
        let mut vecx: Vec<NeedStore> = self
            .avec
            .par_iter_mut() // par_iter_mut for parallel, .iter for easier reading of diagnostic messages
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
        let mut stps = StepStore::new();

        for actx in &self.avec {
            stps.append(actx.get_steps(achange));
        }

        //println!("possible steps: {}", stps.str());
        stps
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

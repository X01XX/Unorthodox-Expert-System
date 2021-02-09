//! The ActionStore struct, for an Unorthodox Expert System.
//!
//! This stores a vector of Action structs, for a Domain struct.
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
    pub avec: Vec<SomeAction>,
}

impl ActionStore {
    pub fn new() -> Self {
        ActionStore {
            avec: Vec::<SomeAction>::with_capacity(5),
        }
    }

    pub fn len(&self) -> usize {
        self.avec.len()
    }

    pub fn push(&mut self, mut val: SomeAction) {
        val.num = self.avec.len();
        self.avec.push(val);
    }

    // Get an x_mask for all actions.
    // Indicates bit position that can predictably change
    pub fn get_x_mask(&self, num_ints: usize) -> SomeMask {
        let mut cngx = SomeChange::new_low(num_ints);

        for actx in &self.avec {
            cngx = cngx.union(&actx.predictable_bit_changes);
        }

        cngx.x_mask()
    }

    // Get needs for all actions
    pub fn get_needs(&mut self, cur: &SomeState, x_mask: &SomeMask) -> NeedStore {
        // Run a get_needs thread for each action
        let mut vecx: Vec<NeedStore> = self
            .avec
            .par_iter_mut() // .iter for easier reading of diagnostic messages
            .map(|actx| actx.get_needs(cur, x_mask))
            .collect::<Vec<NeedStore>>();

        // Aggregate the results into one NeedStore
        let mut nds_agg = NeedStore::new();

        for mut nst in vecx.iter_mut() {
            nds_agg.append(&mut nst);
        }

        // For testing to make the terminal output squential, comment out the code above.
        //        let mut nds_agg = NeedStore::new();
        //        for actx in self.avec.iter_mut() {
        //			let mut ndsx = actx.get_needs(cur, x_mask);
        //			if ndsx.len() > 0 {
        //				nds_agg.append(&mut ndsx);
        //			}
        //		}

        nds_agg
    }

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

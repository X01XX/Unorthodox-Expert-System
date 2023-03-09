//! The ActionStore struct, a vector of SomeAction structs.
//!
//! This stores a vector of SomeAction structs, for a Domain struct.
//!
use crate::action::SomeAction;
use crate::change::SomeChange;
use crate::mask::SomeMask;
use crate::needstore::NeedStore;
use crate::sample::SomeSample;
use crate::state::SomeState;
use crate::stepstore::StepStore;

use std::collections::VecDeque;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::fmt::Write as _; // import without risk of name clashing

use std::ops::{Index, IndexMut};
use std::slice::Iter;

use rayon::prelude::*;

impl fmt::Display for ActionStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut rc_str = String::new();

        for actx in &self.avec {
            let _ = write!(rc_str, "\n  {}", &actx);
        }

        write!(f, "{rc_str}")
    }
}

#[readonly::make]
#[derive(Serialize, Deserialize)]
pub struct ActionStore {
    /// A vector of SomeAction structs
    pub avec: Vec<SomeAction>,
    pub aggregate_changes: SomeChange,
}

impl ActionStore {
    /// Return a new, empty ActionStore.
    pub fn new(num_ints: usize) -> Self {
        assert!(num_ints > 0);
        ActionStore {
            avec: Vec::<SomeAction>::with_capacity(5),
            aggregate_changes: SomeChange::new_low(num_ints),
        }
    }

    /// Return the length of an ActionStore.
    pub fn len(&self) -> usize {
        self.avec.len()
    }

    /// Return the number of integers needed for a SomeBits instance.
    pub fn num_ints(&self) -> usize {
        self.aggregate_changes.num_ints()
    }

    /// Add a new action to the ActionStore.
    pub fn add_action(&mut self, dom_num: usize, num_ints: usize) {
        assert!(num_ints > 0);
        self.avec
            .push(SomeAction::new(dom_num, self.avec.len(), num_ints));
    }

    /// Check limited flage due to new changes.
    pub fn check_limited(&mut self, change_mask: &SomeMask) {
        for actx in self.avec.iter_mut() {
            actx.check_limited(change_mask);
        }
    }

    /// Get needs for all actions in the store.
    pub fn get_needs(
        &mut self,
        cur: &SomeState,
        dom: usize,
        memory: &VecDeque<SomeSample>,
    ) -> NeedStore {
        // Run a get_needs thread for each action
        //println!("actionstore: get_needs");

        self.calc_aggregate_changes();

        let mut vecx: Vec<NeedStore> = self
            .avec
            .par_iter_mut() // par_iter_mut for parallel, .iter_mut for easier reading of diagnostic messages
            .map(|actx| actx.get_needs(cur, dom, memory, &self.aggregate_changes))
            .filter(|ndstrx| ndstrx.is_not_empty())
            .collect::<Vec<NeedStore>>();

        // Aggregate the results into one NeedStore
        let mut nds_agg = NeedStore::new_with_capacity(vecx.iter().map(|ndsx| ndsx.len()).sum());

        for nst in vecx.iter_mut() {
            nds_agg.append(nst);
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
            .filter(|strx| strx.is_not_empty())
            .collect::<Vec<StepStore>>();

        // Aggregate the results into one StepStore
        let mut stps_agg = StepStore::new_with_capacity(stps.iter().map(|stpsx| stpsx.len()).sum());

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

    /// Calc all possible changes.
    pub fn calc_aggregate_changes(&mut self) {
        // Check for any action agg_chgs_updated set to true.
        let mut no_recalc = true;
        for actx in &self.avec {
            if actx.agg_chgs_updated() {
                no_recalc = false;
                break;
            }
        }

        // If no agg_chgs_updated are set to true, return.
        if no_recalc {
            return;
        }

        // Recalc ActionStore aggregate_changes.
        let mut new_chgs = SomeChange::new_low(self.num_ints());

        for actx in &self.avec {
            new_chgs = new_chgs.bitwise_or(actx.aggregate_changes());
        }

        // Check for new changes.
        let additions = new_chgs.minus(&self.aggregate_changes);

        // Reset agg_chgs_updated flags, as needed.
        for actx in &mut self.avec {
            if actx.agg_chgs_updated() {
                actx.reset_agg_chgs_updated();
            }
        }

        self.aggregate_changes = new_chgs;

        if additions.is_not_low() {
            self.check_limited(
                &additions
                    .b01
                    .bitwise_or(&additions.b01.bitwise_or(&additions.b10)),
            );
        }
    }
} // end impl ActionStore

impl Index<usize> for ActionStore {
    type Output = SomeAction;
    fn index(&self, i: usize) -> &SomeAction {
        &self.avec[i]
    }
}

impl IndexMut<usize> for ActionStore {
    fn index_mut<'a>(&mut self, i: usize) -> &mut Self::Output {
        &mut self.avec[i]
    }
}

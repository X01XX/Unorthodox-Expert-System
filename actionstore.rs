//! The ActionStore struct, a vector of SomeAction structs.
//!
//! This stores a vector of SomeAction structs, for a Domain struct.
//!
use crate::action::SomeAction;
use crate::change::SomeChange;
use crate::mask::SomeMask;
use crate::need::SomeNeed;
use crate::needstore::NeedStore;
use crate::region::SomeRegion;
use crate::rule::SomeRule;
use crate::state::SomeState;
use crate::step::SomeStep;
use crate::stepstore::StepStore;

use serde::{Deserialize, Serialize};
use std::fmt;

use std::ops::{Index, IndexMut};
use std::slice::Iter;

use rayon::prelude::*;

impl fmt::Display for ActionStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
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
    pub fn new(avec: Vec<SomeAction>, aggregate_changes: SomeChange) -> Self {
        ActionStore {
            avec,
            aggregate_changes,
        }
    }

    /// Return the length of an ActionStore.
    pub fn len(&self) -> usize {
        self.avec.len()
    }

    /// Add a new action to the ActionStore.
    pub fn add_action(&mut self, dom_num: usize, init_mask: SomeMask) {
        self.avec
            .push(SomeAction::new(dom_num, self.avec.len(), init_mask));
    }

    /// Check limited flag due to new changes.
    pub fn check_limited(&mut self, change_mask: &SomeMask) {
        for actx in self.avec.iter_mut() {
            actx.check_limited(change_mask);
        }
    }

    /// Get needs for all actions in the store.
    pub fn get_needs(&mut self, cur: &SomeState, dom: usize) -> NeedStore {
        // Run a get_needs thread for each action
        //println!("actionstore: get_needs");

        self.calc_aggregate_changes();

        let vecx: Vec<NeedStore> = self
            .avec
            .par_iter_mut() // par_iter_mut for parallel, .iter_mut for easier reading of diagnostic messages
            .map(|actx| actx.get_needs(cur, dom, &self.aggregate_changes))
            .collect::<Vec<NeedStore>>();

        // Consolidate need into one NeedStore.
        let num_items = vecx.iter().map(|ndsx| ndsx.len()).sum();
        let mut needs = NeedStore::new(Vec::<SomeNeed>::with_capacity(num_items));
        for needx in vecx {
            needs.append(needx);
        }
        needs
    }

    /// Return steps that make at least one needed bit change.
    /// Optional region reference indicating the steps must remain within the given region.
    pub fn get_steps(&self, achange: &SomeChange, within: Option<&SomeRegion>) -> StepStore {
        // Run a thread for each action
        let stps: Vec<StepStore> = self
            .avec
            .par_iter() // par_iter for parallel, .iter for easier reading of diagnostic messages
            .map(|actx| actx.get_steps(achange, within))
            .collect::<Vec<StepStore>>();

        // Consolidate steps into one StepStore.
        let num_items = stps.iter().map(|stpx| stpx.len()).sum();
        let mut stps2 = StepStore::new(Vec::<SomeStep>::with_capacity(num_items));
        for stpx in stps {
            stps2.append(stpx);
        }

        stps2
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
        let mut new_chgs = self.aggregate_changes.new_low();

        for actx in &self.avec {
            new_chgs = new_chgs.union(actx.aggregate_changes());
        }

        // Check for new changes.
        let additions = new_chgs.difference(&self.aggregate_changes);

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

    /// Return a String representation of an ActionStore.
    fn formatted_string(&self) -> String {
        let mut rc_str = String::new();

        for actx in &self.avec {
            rc_str.push_str(&format!("\n  {}", &actx));
        }

        rc_str
    }

    pub fn all_rules(&self) -> Vec<&SomeRule> {
        let mut ret = Vec::<&SomeRule>::new();
        for actx in self.avec.iter() {
            ret.extend(actx.all_rules());
        }
        ret
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

//! The ActionStore struct, a vector of SomeAction structs.
//!
//! This stores a vector of SomeAction structs, for a Domain struct.
//!
use crate::action::SomeAction;
use crate::change::SomeChange;
use crate::need::SomeNeed;
use crate::needstore::NeedStore;
use crate::region::SomeRegion;
use crate::sample::SomeSample;
use crate::state::SomeState;
use crate::step::SomeStep;
use crate::stepstore::StepStore;

use serde::{Deserialize, Serialize};
use std::fmt;

use std::ops::{Index, IndexMut};
use std::slice::{Iter, IterMut};

use rayon::prelude::*;

impl fmt::Display for ActionStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_str())
    }
}

#[readonly::make]
#[derive(Serialize, Deserialize)]
/// A vector of SomeAction structs, and SomeAction-specific functions.
pub struct ActionStore {
    /// A vector of SomeAction structs
    pub items: Vec<SomeAction>,
    /// A summary of all currently possible group-defined bit changes.
    /// This is used to limit needs under some circumstances.
    /// For example, a bit changes from zero to one, but no reverse change
    /// has been sampled.
    pub aggregate_changes: Option<SomeChange>,
}

impl ActionStore {
    /// Return a new, empty ActionStore.
    pub fn new(items: Vec<SomeAction>) -> Self {
        ActionStore {
            items,
            aggregate_changes: None,
        }
    }

    /// Return the length of an ActionStore.
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// Add a new action to the ActionStore.
    pub fn push(&mut self, actx: SomeAction) {
        self.items.push(actx);
    }

    /// Check limited flag due to new changes.
    pub fn check_limited(&mut self, max_reg: &SomeRegion) {
        for actx in self.items.iter_mut() {
            actx.check_limited(max_reg);
        }
    }

    /// Get needs for all actions in the store.
    pub fn get_needs(&mut self, cur_state: &SomeState) -> NeedStore {
        //println!("actionstore: get_needs");

        let max_reg_prev = self.reachable_region(cur_state);

        self.calc_aggregate_changes();

        let max_reg = self.reachable_region(cur_state);

        if max_reg != max_reg_prev {
            self.check_limited(&max_reg);
        }

        // Run a get_needs thread for each action
        let vecx: Vec<NeedStore> = self
            .items
            .par_iter_mut() // par_iter_mut for parallel, .iter_mut for easier reading of diagnostic messages
            .map(|actx| actx.get_needs(cur_state, &max_reg))
            .collect::<Vec<NeedStore>>();

        // Consolidate needs into one NeedStore.
        let num_items = vecx.iter().map(|ndsx| ndsx.len()).sum();
        let mut needs = NeedStore::new(Vec::<SomeNeed>::with_capacity(num_items));
        for needx in vecx {
            needs.append(needx);
        }

        needs
    }

    /// Return steps that make at least one needed bit change.
    /// Optional region reference indicating the steps must remain within the given region.
    pub fn get_steps(&self, wanted_changes: &SomeChange, within: &SomeRegion) -> StepStore {
        debug_assert!(wanted_changes.num_bits() == within.num_bits());

        // Run a thread for each action
        let stps: Vec<StepStore> = self
            .items
            .par_iter() // par_iter for parallel, .iter for easier reading of diagnostic messages
            .map(|actx| actx.get_steps(wanted_changes, within))
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
        self.items.iter()
    }

    /// Return a vector mut iterator.
    pub fn iter_mut(&mut self) -> IterMut<SomeAction> {
        self.items.iter_mut()
    }

    /// Return the expected maximum reachable region, based on the current state
    /// and known possible bit position changes.
    pub fn reachable_region(&self, cur_state: &SomeState) -> SomeRegion {
        if let Some(chgs) = &self.aggregate_changes {
            SomeRegion::new(vec![cur_state.clone(), chgs.apply_changes(cur_state)])
        } else {
            SomeRegion::new(vec![cur_state.clone()])
        }
    }

    /// Calc all possible changes.
    pub fn calc_aggregate_changes(&mut self) {
        // Check for any action agg_chgs_updated set to true.
        let mut recalc = false;
        for actx in &self.items {
            if !actx.agg_chgs_updated {
                recalc = true;
                break;
            }
        }

        // If no agg_chgs_updated are set to true, return.
        if !recalc {
            //println!("actionstore::calc_aggregate_changes: skipping");
            return;
        }

        // Recalc ActionStore aggregate_changes.
        let mut new_chgs: Option<SomeChange> = None;

        for actx in &mut self.items {
            if let Some(act_changes) = actx.aggregate_changes() {
                if let Some(tot_changes) = new_chgs {
                    new_chgs = Some(tot_changes.union(act_changes));
                } else {
                    new_chgs = Some(act_changes.clone());
                }
            }
        }

        // Calc changes from previous.
        let dif_changes = if let Some(old_changes) = &self.aggregate_changes {
            if let Some(new_changes) = &new_chgs {
                let dif = old_changes.difference(new_changes);
                if dif.is_low() {
                    None
                } else {
                    Some(dif)
                }
            } else {
                Some(old_changes.clone())
            }
        } else {
            // self.aggregate_changes = None.
            new_chgs.as_ref().cloned()
        };

        // Reset agg_chgs_updated flags, as needed.
        for actx in &mut self.items {
            if actx.agg_chgs_updated {
                actx.reset_agg_chgs_updated();
            }
        }

        if dif_changes.is_none() {
            return;
        }

        self.aggregate_changes = new_chgs;
    }

    /// Return a String representation of an ActionStore.
    fn formatted_str(&self) -> String {
        let mut rc_str = String::new();

        for actx in &self.items {
            rc_str.push_str(&format!("\n  {}", &actx));
        }

        rc_str
    }

    /// Take an action for a need, evaluate the resulting sample.
    /// It is assumed that a sample made for a need must be saved.
    pub fn take_action_need(&mut self, ndx: &SomeNeed, cur_state: &SomeState) -> SomeSample {
        let smpl = self.items[ndx.act_id()].take_action_need(cur_state, ndx);
        self.calc_aggregate_changes();
        smpl
    }

    /// Evaluate an arbitrary sample given by the user.
    /// This tends to break things for an action, unless all samples are arbitrary.
    /// Useful for testing a wholly different series of samples/results.
    /// Using the command: ss  action-number  initial-state  result-state
    /// e.g. ss  0  s0b1010  s0b1111
    pub fn eval_sample_arbitrary(&mut self, act_id: usize, smpl: &SomeSample) {
        //println!("max_reg {max_reg}");
        self.items[act_id].eval_sample_arbitrary(smpl);
        self.calc_aggregate_changes();
    }

    /// Take an action with the current state.
    /// Return a sample.
    pub fn take_action_step(&mut self, act_id: usize, cur_state: &SomeState) -> SomeSample {
        let smpl = self.items[act_id].take_action_step(cur_state);
        self.calc_aggregate_changes();
        smpl
    }

    /// Take an action with the current state, store the sample.
    /// Return a sample.
    pub fn take_action_arbitrary(&mut self, act_id: usize, cur_state: &SomeState) -> SomeSample {
        let smpl = self.items[act_id].take_action_arbitrary(cur_state);
        self.calc_aggregate_changes();
        smpl
    }
} // end impl ActionStore

impl Index<usize> for ActionStore {
    type Output = SomeAction;
    fn index(&self, i: usize) -> &SomeAction {
        &self.items[i]
    }
}

impl IndexMut<usize> for ActionStore {
    fn index_mut<'a>(&mut self, i: usize) -> &mut Self::Output {
        &mut self.items[i]
    }
}

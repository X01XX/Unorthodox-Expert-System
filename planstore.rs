//! The PlanStore struct, a vector of SomePlan structs.

use crate::plan::SomePlan;
use crate::region::SomeRegion;
use crate::regionstorecorr::RegionStoreCorr;
use crate::state::SomeState;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::Index; // IndexMut;
use std::slice::Iter;

impl fmt::Display for PlanStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut flg = 0;

        let mut rc_str = String::new();

        rc_str.push_str("\n(");

        for planx in &self.avec {
            if flg == 1 {
                rc_str.push_str(",\n ");
            }
            rc_str.push_str(&format!("{}", &planx));
            flg = 1;
        }
        rc_str.push(')');

        write!(f, "{rc_str}")
    }
}

#[readonly::make]
#[derive(Debug, Default, Serialize, Deserialize, Clone)]
pub struct PlanStore {
    /// A vector of SomePlan instances.
    pub avec: Vec<SomePlan>,
}

impl PlanStore {
    /// Return a new PlanStore instance.
    /// If more than one plan, plans will be run in order.
    /// If more than one plan, plans are in domain order, but that is not enforced.
    /// Since they will be run in order, there is the possiblity of splitting plans
    /// into one, or more, steps, and running them in any desired order,
    /// as long as the sequence of steps for any domain is preserved.
    pub fn new(avec: Vec<SomePlan>) -> Self {
        Self { avec }
    }

    /// Return the length of the SomePlan vector.
    pub fn len(&self) -> usize {
        self.avec.len()
    }

    /// Return true if the store is empty.
    pub fn is_empty(&self) -> bool {
        self.avec.is_empty()
    }

    /// Return true if the store is not empty.
    pub fn is_not_empty(&self) -> bool {
        !self.avec.is_empty()
    }

    /// Add a plan to the vector.
    pub fn push(&mut self, val: SomePlan) {
        self.avec.push(val);
    }

    /// Return the result region of the last plans.
    pub fn result_region(&self, all_states: &[&SomeState]) -> RegionStoreCorr {
        // Init return RegionStoreCorr.
        let mut ret = RegionStoreCorr::with_capacity(all_states.len());
        for stax in all_states.iter() {
            ret.push(SomeRegion::new(vec![(*stax).clone()]));
        }

        // Update return RegionStoreCorr as needed.
        for planx in self.avec.iter() {
            if planx.is_not_empty() {
                ret[planx.dom_num] = planx.result_region().clone();
            }
        }
        ret
    }

    /// Return the initial region of the plans.
    pub fn initial_region(&self, all_states: &[&SomeState]) -> RegionStoreCorr {
        // Init return RegionStoreCorr.
        let mut ret = RegionStoreCorr::with_capacity(all_states.len());
        for stax in all_states.iter() {
            ret.push(SomeRegion::new(vec![(*stax).clone()]));
        }

        let mut domains_done = Vec::<usize>::with_capacity(all_states.len());

        // Update return RegionStoreCorr as needed.
        for planx in self.avec.iter() {
            if planx.is_not_empty() && !domains_done.contains(&planx.dom_num) {
                ret[planx.dom_num] = planx.initial_region().clone();
                domains_done.push(planx.dom_num);
            }
        }
        ret
    }

    /// Return a vector iterator.
    pub fn iter(&self) -> Iter<SomePlan> {
        self.avec.iter()
    }

    /// Return a more restricted display version of a PlanStore.
    pub fn str_terse(&self) -> String {
        let mut rc_str = String::new();

        if self.avec.len() > 1 {
            rc_str.push('(');
        }

        for (inx, planx) in self.avec.iter().enumerate() {
            if inx > 0 {
                rc_str.push_str(", ");
            }
            rc_str.push_str(&planx.str_terse());
        }
        if self.avec.len() > 1 {
            rc_str.push(')');
        }
        rc_str
    }

    /// Return the number of steps in the plans of the PlanStore.
    pub fn number_steps(&self) -> usize {
        let mut ret = 0;
        for planx in &self.avec {
            ret += planx.len();
        }
        ret
    }

    /// Append a PlanStore to a PlanStore.
    pub fn append(&mut self, other: &mut PlanStore) {
        self.avec.append(&mut other.avec); // empties other.avec
    }
} // end impl PlanStore

impl Index<usize> for PlanStore {
    type Output = SomePlan;
    fn index(&self, i: usize) -> &SomePlan {
        &self.avec[i]
    }
}

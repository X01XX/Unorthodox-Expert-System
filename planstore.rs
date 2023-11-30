//! The PlanStore struct, a vector of SomePlan structs.

use crate::plan::SomePlan;
use crate::region::SomeRegion;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::Index;
use std::slice::Iter;

impl fmt::Display for PlanStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
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
    pub fn new(avec: Vec<SomePlan>) -> Self {
        Self { avec }
    }

    /// Return the length of the SomePlan vector.
    pub fn len(&self) -> usize {
        self.avec.len()
    }

    /// Return true if the store does not contain at least one non-empty plan.
    pub fn is_empty(&self) -> bool {
        if self.avec.is_empty() {
            return true;
        }
        for planx in self.avec.iter() {
            if planx.is_not_empty() {
                return false;
            }
        }
        true
    }

    /// Return true if the store is not empty.
    pub fn is_not_empty(&self) -> bool {
        !self.avec.is_empty()
    }

    /// Return the index of the last plan with a given domain number.
    fn last_dom(&self, dom_id: usize) -> Option<usize> {
        let mut ret: Option<usize> = None;
        for (inx, planx) in self.avec.iter().enumerate() {
            if planx.dom_id == dom_id {
                ret = Some(inx);
            }
        }
        ret
    }

    /// Add a plan to the vector.
    pub fn push(&mut self, planx: SomePlan) {
        // Verify a domain plan that is split into parts.
        if let Some(inx) = self.last_dom(planx.dom_id) {
            assert!(self[inx].result_region() == planx.initial_region());
        }
        self.avec.push(planx);
    }

    /// Return a vector iterator.
    pub fn iter(&self) -> Iter<SomePlan> {
        self.avec.iter()
    }

    /// Return a reference to the las plan.
    pub fn last(&self) -> Option<&SomePlan> {
        self.avec.last()
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

    /// Return a String representation of a PlanStore.
    fn formatted_string(&self) -> String {
        let mut flg = 0;

        let mut rc_str = String::new();

        rc_str.push_str("\n(");

        for planx in &self.avec {
            if flg == 1 {
                rc_str.push_str(",\n ");
            }
            rc_str.push_str(&planx.to_string());
            flg = 1;
        }
        rc_str.push(')');

        rc_str
    }

    /// Do swap_remove for a PlanStore.
    pub fn swap_remove(&mut self, inx: usize) -> SomePlan {
        self.avec.swap_remove(inx)
    }

    /// Extend a StepStore by emptying another StepStore.
    pub fn append(&mut self, mut other: Self) {
        self.avec.append(&mut other.avec);
    }

    /// Return true if the last plan for a domain is the given region.
    pub fn dom_result(&self, dom_id: usize, regx: &SomeRegion) -> bool {
        let mut rslt: Option<&SomeRegion> = None;
        for planx in self.iter() {
            if planx.dom_id == dom_id && planx.is_not_empty() {
                rslt = Some(planx.result_region());
            }
        }
        if let Some(regy) = rslt {
            return regy == regx;
        }
        false
    }
} // end impl PlanStore

impl Index<usize> for PlanStore {
    type Output = SomePlan;
    fn index(&self, i: usize) -> &SomePlan {
        &self.avec[i]
    }
}

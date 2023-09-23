//! The PlanStore struct, a vector of SomePlan structs.

use crate::plan::SomePlan;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::Index; // IndexMut;
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
} // end impl PlanStore

impl Index<usize> for PlanStore {
    type Output = SomePlan;
    fn index(&self, i: usize) -> &SomePlan {
        &self.avec[i]
    }
}
